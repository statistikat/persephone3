#include "core/sparse_history.hpp"
#include "core/linear_algebra.hpp"

// Thin QR is evaluated through R's declared LAPACK ABI. The factored
// homogeneous functions and their original-history normalization are
// independently derived; no filtering implementation is copied.
#include <R_ext/Lapack.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace detail {
namespace {

constexpr double rank_tolerance = 1e-12;
constexpr double normalization_tolerance = 512 * std::numeric_limits<double>::epsilon();

void checkpoint(const Interrupt& interrupt) { if (interrupt) interrupt(); }

double checked(long double value) {
    const auto result = static_cast<double>(value);
    if (!std::isfinite(result)) throw std::overflow_error("sparse history evaluation produced a non-finite value");
    if (value != 0 && result == 0) throw std::underflow_error("sparse history evaluation underflows the finite numeric range");
    return result;
}

std::int64_t checked_time(std::uint64_t time) {
    // At most three positive time knots enter a sum, and history evaluation
    // also uses the preceding 26 integer times. Reserve room for both signs.
    constexpr auto limit = static_cast<std::uint64_t>((std::numeric_limits<std::int64_t>::max() - 64) / 3);
    if (time == 0 || time > limit) throw std::out_of_range("sparse history time is outside the checked integer range");
    return static_cast<std::int64_t>(time);
}

} // namespace

SparseHistoryBasis::SparseHistoryBasis(int d, int seasonal_d, int period,
                                       const Interrupt& interrupt)
    : d_(d), seasonal_d_(seasonal_d), period_(period) {
    if (d < 0 || d > 2 || seasonal_d < 0 || seasonal_d > 2 ||
        (period != 4 && period != 12)) {
        throw std::invalid_argument("sparse history basis requires d,D in 0..2 and period 4 or 12");
    }
    rebuild(interrupt);
}

std::size_t SparseHistoryBasis::dimension() const {
    return static_cast<std::size_t>(d_ + period_ * seasonal_d_);
}

int SparseHistoryBasis::global_constraints() const {
    int extra = 0;
    std::int64_t first_sum = 0;
    bool qualified = false;
    for (std::size_t phase = 0; phase < static_cast<std::size_t>(period_); ++phase) {
        if (d_ > 0 && counts_[phase] >= seasonal_d_ + 1) {
            extra = std::max(extra, 1);
            if (d_ == 2 && (counts_[phase] >= seasonal_d_ + 2 ||
                (qualified && first_sum != sums_[phase]))) extra = 2;
            if (!qualified) first_sum = sums_[phase];
            qualified = true;
        }
    }
    return extra;
}

std::int64_t SparseHistoryBasis::global_knot_sum() const {
    for (std::size_t phase = 0; phase < static_cast<std::size_t>(period_); ++phase) {
        if (counts_[phase] >= seasonal_d_ + 1) return sums_[phase];
    }
    throw std::logic_error("sparse history has no qualifying global constraint");
}

std::size_t SparseHistoryBasis::identified_rank() const {
    std::size_t result = static_cast<std::size_t>(global_constraints());
    for (std::size_t phase = 0; phase < static_cast<std::size_t>(period_); ++phase) {
        result += static_cast<std::size_t>(std::min(counts_[phase], seasonal_d_));
    }
    return result;
}

std::size_t SparseHistoryBasis::rank() const { return identified_rank(); }

std::size_t SparseHistoryBasis::remaining_rank() const { return dimension() - identified_rank(); }

void SparseHistoryBasis::observe(std::uint64_t time, const Interrupt& interrupt) {
    const auto value = checked_time(time);
    if (time <= last_observed_) throw std::invalid_argument("sparse history observations must have increasing distinct times");
    auto candidate = *this;
    candidate.last_observed_ = time;
    const auto phase = static_cast<std::size_t>(value % period_);
    const auto count = candidate.counts_[phase];
    if (count < seasonal_d_) candidate.knots_[phase][static_cast<std::size_t>(count)] = value;
    if (count < seasonal_d_ + 1) candidate.sums_[phase] += value;
    if (count < seasonal_d_ + 2) ++candidate.counts_[phase];
    const auto before = rank(), after = candidate.rank();
    if (after < before || after > before + 1 || after > dimension()) {
        throw std::logic_error("sparse history observation has inconsistent exact rank");
    }
    if (after != before) candidate.rebuild(interrupt);
    *this = std::move(candidate);
}

std::vector<double> SparseHistoryBasis::functions(std::int64_t time) const {
    const auto current_phase = static_cast<std::size_t>((time % period_ + period_) % period_);
    std::vector<double> result;
    result.reserve(remaining_rank());
    for (std::size_t phase = 0; phase < static_cast<std::size_t>(period_); ++phase) {
        const auto count = std::min(counts_[phase], seasonal_d_);
        long double product = 1;
        for (int i = 0; i < count; ++i) product *= static_cast<long double>(time - knots_[phase][static_cast<std::size_t>(i)]);
        for (int power = 0; power < seasonal_d_ - count; ++power) {
            result.push_back(phase == current_phase ? checked(product) : 0);
            product *= static_cast<long double>(time);
        }
    }
    const auto extra = global_constraints();
    if (extra == d_) return result;
    const auto count = std::min(counts_[current_phase], seasonal_d_);
    long double product = 1;
    for (int i = 0; i < count; ++i) product *= static_cast<long double>(time - knots_[current_phase][static_cast<std::size_t>(i)]);
    const auto u = knots_[current_phase][0];
    const auto v = knots_[current_phase][1];
    const long double x = static_cast<long double>(time);
    const long double knot = static_cast<long double>(u);
    long double low = 1, high = x;
    // Residuals of t^D and t^(D+1) after interpolation at this phase's
    // first count knots: P_count(t) times their divided differences.
    if (seasonal_d_ == 1) {
        if (count == 0) { low = x; high = x * x; }
        else { low = 1; high = static_cast<long double>(time + u); }
    } else if (seasonal_d_ == 2) {
        if (count == 0) { low = x * x; high = low * x; }
        else if (count == 1) {
            low = static_cast<long double>(time + u);
            high = x * x + x * knot + knot * knot;
        } else { low = 1; high = static_cast<long double>(time + u + v); }
    }
    if (extra == 0) {
        result.push_back(checked(product * low));
        if (d_ == 2) result.push_back(checked(product * high));
    } else {
        // One surviving global coefficient: b, with a=-sum(knots)*b.
        // Factor its known root before floating conversion. This preserves
        // exact zeros at dependent observations without subtracting t^3 terms.
        const auto sum = global_knot_sum();
        long double remaining = 0;
        if (count == 0) remaining = low * static_cast<long double>(time - sum);
        else if (seasonal_d_ == 2 && count == 1) {
            remaining = static_cast<long double>(time - sum) * static_cast<long double>(time + u) +
                        knot * knot;
        } else {
            const auto knot_sum = count == 2 ? u + v : u;
            remaining = static_cast<long double>(time + knot_sum - sum);
        }
        result.push_back(checked(product * remaining));
    }
    if (result.size() != remaining_rank()) throw std::logic_error("sparse history function count disagrees with exact rank");
    return result;
}

std::vector<double> SparseHistoryBasis::evaluate(std::int64_t prediction_time,
                                                 const Interrupt& interrupt) const {
    const auto rows = dimension(), columns = remaining_rank();
    std::vector<double> result(rows * columns, 0);
    for (std::size_t i = 0; i < rows; ++i) {
        checkpoint(interrupt);
        const auto row = functions(prediction_time - 1 - static_cast<std::int64_t>(i));
        for (std::size_t j = 0; j < columns; ++j) result[i + rows * j] = row[j];
    }
    return result;
}

std::vector<double> SparseHistoryBasis::loading(std::uint64_t prediction_time,
                                                const Interrupt& interrupt) const {
    const auto time = checked_time(prediction_time);
    const auto rows = dimension(), columns = remaining_rank();
    if (columns == 0) return {};
    auto result = evaluate(time, interrupt);
    for (std::size_t j = 0; j < columns; ++j) {
        checkpoint(interrupt);
        for (std::size_t i = 0; i < rows; ++i) {
            long double value = static_cast<long double>(result[i + rows * j]) / column_scales_[j];
            for (std::size_t k = 0; k < j; ++k) value -= static_cast<long double>(result[i + rows * k]) * upper_[k + columns * j];
            result[i + rows * j] = checked(value / upper_[j + columns * j]);
        }
    }
    return result;
}

void SparseHistoryBasis::rebuild(const Interrupt& interrupt) {
    const auto rows = dimension(), columns = remaining_rank();
    column_scales_.assign(columns, 0);
    upper_.assign(columns * columns, 0);
    if (columns == 0) return;
    auto initial = evaluate(1, interrupt);
    for (std::size_t j = 0; j < columns; ++j) {
        for (std::size_t i = 0; i < rows; ++i) column_scales_[j] = std::hypot(column_scales_[j], initial[i + rows * j]);
        if (!std::isfinite(column_scales_[j]) || column_scales_[j] == 0) throw std::runtime_error("sparse history initial-coordinate function is numerically unresolved");
        for (std::size_t i = 0; i < rows; ++i) initial[i + rows * j] /= column_scales_[j];
    }
    const DenseMatrix matrix(rows, columns, initial), rhs(rows, 1);
    if (qr_least_squares(matrix, rhs, rank_tolerance, interrupt).rank != static_cast<int>(columns)) {
        throw std::runtime_error("sparse history initial-coordinate functions are numerically rank deficient");
    }
    const int m = static_cast<int>(rows), n = static_cast<int>(columns), query_size = -1;
    int info = 0;
    double query = 0;
    std::vector<double> tau(columns);
    checkpoint(interrupt);
    F77_CALL(dgeqrf)(&m, &n, initial.data(), &m, tau.data(), &query, &query_size, &info);
    checkpoint(interrupt);
    if (info != 0 || !std::isfinite(query) || query < 1 || query > std::numeric_limits<int>::max()) {
        throw std::runtime_error("sparse history QR workspace query failed");
    }
    const auto work_size = static_cast<int>(std::ceil(query));
    std::vector<double> work(static_cast<std::size_t>(work_size));
    F77_CALL(dgeqrf)(&m, &n, initial.data(), &m, tau.data(), work.data(), &work_size, &info);
    checkpoint(interrupt);
    if (info != 0) throw std::runtime_error("sparse history QR factorization failed");
    for (std::size_t j = 0; j < columns; ++j) {
        for (std::size_t i = 0; i <= j; ++i) upper_[i + columns * j] = initial[i + rows * j];
        if (!std::isfinite(upper_[j + columns * j]) || upper_[j + columns * j] == 0) {
            throw std::runtime_error("sparse history QR factor is numerically unresolved");
        }
    }
    const auto normalized = loading(1, interrupt);
    for (std::size_t j = 0; j < columns; ++j) {
        for (std::size_t k = 0; k <= j; ++k) {
            long double value = k == j ? -1 : 0;
            for (std::size_t i = 0; i < rows; ++i) value += static_cast<long double>(normalized[i + rows * j]) * normalized[i + rows * k];
            const auto residual = std::abs(checked(value));
            maximum_normalization_residual_ = std::max(maximum_normalization_residual_, residual);
            if (residual > normalization_tolerance * static_cast<double>(rows)) {
                throw std::runtime_error("sparse history initial-coordinate normalization failed its residual check");
            }
        }
    }
    ++rebuild_count_;
}

} // namespace detail
} // namespace persephone
