#include "core/finite_state_factor.hpp"

// R supplies the LAPACK eigendecomposition and orthogonal QR factorizations.
// The finite-factor Joseph update and concatenated disturbance propagation are
// independently assembled from P=C C'; no numerical filter is translated.
#include <R_ext/Lapack.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace detail {
namespace {

constexpr std::size_t max_states = 32;
constexpr double residual_tolerance = 512 * std::numeric_limits<double>::epsilon();

void checkpoint(const Interrupt& interrupt) { if (interrupt) interrupt(); }

double checked(long double value) {
    const auto result = static_cast<double>(value);
    if (!std::isfinite(result)) throw std::overflow_error("finite state factor arithmetic produced a non-finite value");
    if (value != 0 && result == 0) throw std::underflow_error("finite state factor arithmetic underflows the finite numeric range");
    return result;
}

std::size_t initial_dimension(const DenseMatrix& covariance) {
    covariance.validate();
    if (covariance.rows() != covariance.columns() || covariance.rows() > max_states) {
        throw std::invalid_argument("finite state covariance must be square with at most 32 states");
    }
    return covariance.rows();
}

int workspace_size(double query, int minimum) {
    if (!std::isfinite(query) || query < 1 || query > std::numeric_limits<int>::max()) {
        throw std::length_error("finite state factor LAPACK workspace exceeds the integer range");
    }
    return std::max(minimum, static_cast<int>(std::ceil(query)));
}

void check_variance(double h) {
    if (!std::isfinite(h) || h < 0) throw std::invalid_argument("finite state observation variance must be finite and nonnegative");
}

} // namespace

FiniteStateFactor::FiniteStateFactor(const DenseMatrix& covariance,
                                     const Interrupt& interrupt)
    : factor_(initial_dimension(covariance), covariance.rows()) {
    const auto m = dimension();
    double scale = 0;
    for (const auto value : covariance.values()) scale = std::max(scale, std::abs(value));
    auto eigenvectors = covariance.values();
    for (std::size_t j = 0; j < m; ++j) {
        for (std::size_t i = 0; i < j; ++i) {
            const auto first = covariance(i, j), second = covariance(j, i);
            if (std::abs(static_cast<long double>(first) - second) >
                64 * std::numeric_limits<double>::epsilon() * static_cast<long double>(scale)) {
                throw std::invalid_argument("finite state covariance must be symmetric");
            }
            const auto average = checked(.5L * first + .5L * second);
            eigenvectors[i + m * j] = eigenvectors[j + m * i] = average;
        }
    }
    const int n = static_cast<int>(m), query_size = -1;
    int info = 0;
    double query = 0;
    std::vector<double> eigenvalues(m);
    checkpoint(interrupt);
    F77_CALL(dsyev)("V", "U", &n, eigenvectors.data(), &n, eigenvalues.data(),
                    &query, &query_size, &info FCONE FCONE);
    checkpoint(interrupt);
    if (info != 0) throw std::runtime_error("finite state covariance eigen workspace query failed");
    const auto work_size = workspace_size(query, std::max(1, 3 * n - 1));
    std::vector<double> work(static_cast<std::size_t>(work_size));
    F77_CALL(dsyev)("V", "U", &n, eigenvectors.data(), &n, eigenvalues.data(),
                    work.data(), &work_size, &info FCONE FCONE);
    checkpoint(interrupt);
    if (info != 0) throw std::runtime_error("finite state covariance eigendecomposition failed");
    const long double negative_bound = static_cast<long double>(residual_tolerance) * static_cast<long double>(m) * scale;
    for (std::size_t j = 0; j < m; ++j) {
        auto value = eigenvalues[j];
        if (!std::isfinite(value) || static_cast<long double>(value) < -negative_bound) {
            throw std::invalid_argument("finite state covariance is not numerically positive semidefinite");
        }
        if (value < 0) { value = 0; ++clipped_negative_count_; }
        const auto root = std::sqrt(value);
        for (std::size_t i = 0; i < m; ++i) {
            factor_(i, j) = checked(static_cast<long double>(eigenvectors[i + m * j]) * root);
        }
    }
    const auto reconstructed = this->covariance(interrupt);
    long double error = 0;
    for (std::size_t i = 0; i < covariance.values().size(); ++i) {
        error = std::max(error, std::abs(static_cast<long double>(reconstructed.values()[i]) - covariance.values()[i]));
    }
    initialization_residual_ = scale == 0 ? checked(error) : checked(error / scale);
    if (initialization_residual_ > residual_tolerance * static_cast<double>(m)) {
        throw std::runtime_error("finite state covariance factor failed its reconstruction residual check");
    }
}

std::vector<double> FiniteStateFactor::observation_loading(const std::vector<double>& z,
                                                           const Interrupt& interrupt) const {
    const auto m = dimension();
    if (z.size() != m) throw std::invalid_argument("finite state observation dimensions must agree");
    for (const auto value : z) if (!std::isfinite(value)) throw std::invalid_argument("finite state observation coefficients must be finite");
    std::vector<double> result(m, 0);
    for (std::size_t j = 0; j < m; ++j) {
        checkpoint(interrupt);
        long double value = 0;
        for (std::size_t i = 0; i < m; ++i) value += static_cast<long double>(z[i]) * factor_(i, j);
        result[j] = checked(value);
    }
    return result;
}

FiniteObservation FiniteStateFactor::observation(const std::vector<double>& z, double h,
                                                 const Interrupt& interrupt) const {
    check_variance(h);
    const auto w = observation_loading(z, interrupt);
    const auto m = dimension();
    double norm = std::sqrt(h);
    for (const auto value : w) norm = checked(std::hypot(norm, value));
    const auto variance = checked(static_cast<long double>(norm) * norm);
    if (norm != 0 && variance == 0) throw std::underflow_error("finite innovation variance underflows");
    std::vector<double> pz(m, 0);
    for (std::size_t i = 0; i < m; ++i) {
        long double value = 0;
        for (std::size_t j = 0; j < m; ++j) value += static_cast<long double>(factor_(i, j)) * w[j];
        pz[i] = checked(value);
    }
    return {variance, std::move(pz)};
}

void FiniteStateFactor::compress(const DenseMatrix& loading, const Interrupt& interrupt) {
    loading.validate(interrupt);
    const auto m = dimension(), columns = loading.columns();
    if (loading.rows() != m || columns < m || columns > 2 * max_states) {
        throw std::invalid_argument("finite state loading compression dimensions are unsupported");
    }
    // loading' = Q R, hence loading loading' = R' R. All m rows of R
    // are retained, including zero diagonals: this is not rank truncation.
    std::vector<double> transpose(columns * m, 0);
    for (std::size_t i = 0; i < m; ++i) {
        for (std::size_t j = 0; j < columns; ++j) transpose[j + columns * i] = loading(i, j);
    }
    const int rows = static_cast<int>(columns), cols = static_cast<int>(m), query_size = -1;
    int info = 0;
    double query = 0;
    std::vector<double> tau(m);
    checkpoint(interrupt);
    F77_CALL(dgeqrf)(&rows, &cols, transpose.data(), &rows, tau.data(), &query, &query_size, &info);
    checkpoint(interrupt);
    if (info != 0) throw std::runtime_error("finite state QR workspace query failed");
    const auto work_size = workspace_size(query, cols);
    std::vector<double> work(static_cast<std::size_t>(work_size));
    F77_CALL(dgeqrf)(&rows, &cols, transpose.data(), &rows, tau.data(), work.data(), &work_size, &info);
    checkpoint(interrupt);
    if (info != 0) throw std::runtime_error("finite state QR compression failed");
    DenseMatrix result(m, m);
    for (std::size_t j = 0; j < m; ++j) {
        for (std::size_t i = j; i < m; ++i) result(i, j) = checked(transpose[j + columns * i]);
    }
    factor_ = std::move(result);
    ++compression_count_;
}

void FiniteStateFactor::update(const std::vector<double>& gain,
                               const std::vector<double>& z, double h,
                               const Interrupt& interrupt) {
    check_variance(h);
    const auto m = dimension();
    if (gain.size() != m) throw std::invalid_argument("finite state gain dimensions must agree");
    for (const auto value : gain) if (!std::isfinite(value)) throw std::invalid_argument("finite state gain must be finite");
    const auto w = observation_loading(z, interrupt);
    DenseMatrix updated(m, m + 1);
    const auto root_h = std::sqrt(h);
    for (std::size_t i = 0; i < m; ++i) {
        for (std::size_t j = 0; j < m; ++j) updated(i, j) = checked(static_cast<long double>(factor_(i, j)) - static_cast<long double>(gain[i]) * w[j]);
        updated(i, m) = checked(static_cast<long double>(root_h) * gain[i]);
    }
    compress(updated, interrupt);
}

void FiniteStateFactor::predict(const DenseMatrix& transition,
                                const DenseMatrix& noise_loading,
                                const Interrupt& interrupt) {
    transition.validate(interrupt);
    noise_loading.validate(interrupt);
    const auto m = dimension();
    if (transition.rows() != m || transition.columns() != m ||
        noise_loading.rows() != m || noise_loading.columns() > max_states) {
        throw std::invalid_argument("finite state prediction dimensions must agree and have at most 32 disturbances");
    }
    DenseMatrix predicted(m, m + noise_loading.columns());
    for (std::size_t j = 0; j < m; ++j) {
        checkpoint(interrupt);
        for (std::size_t i = 0; i < m; ++i) {
            long double value = 0;
            for (std::size_t k = 0; k < m; ++k) value += static_cast<long double>(transition(i, k)) * factor_(k, j);
            predicted(i, j) = checked(value);
        }
    }
    for (std::size_t j = 0; j < noise_loading.columns(); ++j) {
        for (std::size_t i = 0; i < m; ++i) predicted(i, m + j) = noise_loading(i, j);
    }
    compress(predicted, interrupt);
}

DenseMatrix FiniteStateFactor::covariance(const Interrupt& interrupt) const {
    const auto m = dimension();
    DenseMatrix result(m, m);
    for (std::size_t j = 0; j < m; ++j) {
        checkpoint(interrupt);
        for (std::size_t i = 0; i <= j; ++i) {
            long double value = 0;
            for (std::size_t k = 0; k < m; ++k) value += static_cast<long double>(factor_(i, k)) * factor_(j, k);
            result(i, j) = result(j, i) = checked(value);
        }
    }
    return result;
}

} // namespace detail
} // namespace persephone
