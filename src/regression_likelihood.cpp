#include "core/regression_likelihood.hpp"
#include "core/parallel.hpp"

// Numerical factorizations use R's declared LAPACK ABI. Regression whitening
// and concentrated Gaussian likelihood equations are independently assembled.
#include <R_ext/Lapack.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace {

void checkpoint(const Interrupt& interrupt) { if (interrupt) interrupt(); }

double checked(long double value) {
    const auto result = static_cast<double>(value);
    if (!std::isfinite(result)) throw std::overflow_error("regression likelihood arithmetic produced a non-finite value");
    if (value != 0 && result == 0) throw std::underflow_error("regression likelihood arithmetic underflows");
    return result;
}

double norm(const std::vector<double>& values) {
    double result = 0;
    for (const auto value : values) result = checked(std::hypot(result, value));
    return result;
}

double scaled_covariance(double value, long double scale) {
    const auto result = checked(static_cast<long double>(value) * scale);
    if (value != 0 && scale != 0 && result == 0) {
        throw std::underflow_error("regression coefficient covariance underflows");
    }
    return result;
}

void validate_design(const RegressionDesign& design, std::size_t rows,
                     const Interrupt& interrupt) {
    if (design.columns > 32 || design.coefficients.size() != design.columns ||
        design.fixed.size() != design.columns) {
        throw std::invalid_argument("regression coefficients must match at most 32 design columns");
    }
    const auto limit = static_cast<std::size_t>(std::numeric_limits<int>::max());
    if (rows > limit || (design.columns != 0 && rows > limit / design.columns) ||
        design.values.size() != rows * design.columns) {
        throw std::invalid_argument("regression design dimensions exceed the supported matrix range or do not match the response");
    }
    for (std::size_t j = 0; j < design.columns; ++j) {
        if (design.fixed[j] > 1 || !std::isfinite(design.coefficients[j])) {
            throw std::invalid_argument("regression coefficient states and fixed values are invalid");
        }
    }
    for (std::size_t i = 0; i < design.values.size(); ++i) {
        if ((i & 4095U) == 0) checkpoint(interrupt);
        if (!std::isfinite(design.values[i])) throw std::invalid_argument("regressors must be finite throughout the selected calendar domain");
    }
}

// A = Q R, so covariance at unit variance is R^-1 R^-T. A second thin
// factorization avoids forming A'A (and squaring its condition number), or an
// n-by-n identity for a pseudoinverse. DGELSY already certified full column rank.
std::vector<double> regression_covariance(const DenseMatrix& design,
                                          const Interrupt& interrupt) {
    const auto rows = design.rows(), columns = design.columns();
    auto qr = design.values();
    const int m = static_cast<int>(rows), n = static_cast<int>(columns), query_size = -1;
    int info = 0;
    double query = 0;
    std::vector<double> tau(columns);
    checkpoint(interrupt);
    F77_CALL(dgeqrf)(&m, &n, qr.data(), &m, tau.data(), &query, &query_size, &info);
    checkpoint(interrupt);
    if (info != 0 || !std::isfinite(query) || query < n || query > std::numeric_limits<int>::max()) {
        throw std::runtime_error("regression covariance QR workspace query failed");
    }
    const auto work_size = static_cast<int>(std::ceil(query));
    std::vector<double> work(static_cast<std::size_t>(work_size));
    F77_CALL(dgeqrf)(&m, &n, qr.data(), &m, tau.data(), work.data(), &work_size, &info);
    checkpoint(interrupt);
    if (info != 0) throw std::runtime_error("regression covariance QR factorization failed");
    std::vector<double> inverse(columns * columns, 0), covariance(columns * columns, 0);
    for (std::size_t j = 0; j < columns; ++j) {
        for (std::size_t reverse = 0; reverse < columns; ++reverse) {
            const auto i = columns - reverse - 1;
            long double value = i == j ? 1 : 0;
            for (std::size_t k = i + 1; k < columns; ++k) value -= static_cast<long double>(qr[i + rows * k]) * inverse[k + columns * j];
            if (!std::isfinite(qr[i + rows * i]) || qr[i + rows * i] == 0) throw std::runtime_error("regression covariance is numerically singular");
            inverse[i + columns * j] = checked(value / qr[i + rows * i]);
        }
    }
    for (std::size_t j = 0; j < columns; ++j) {
        checkpoint(interrupt);
        for (std::size_t i = 0; i <= j; ++i) {
            long double value = 0;
            for (std::size_t k = 0; k < columns; ++k) value += static_cast<long double>(inverse[i + columns * k]) * inverse[j + columns * k];
            const auto entry = checked(value);
            // A full-rank triangular inverse has a strictly positive squared
            // norm in every row. Underflow must not imply exact coefficient
            // certainty on platforms where long double is only double width.
            if (i == j && entry == 0) throw std::underflow_error("regression coefficient covariance underflows");
            covariance[i + columns * j] = covariance[j + columns * i] = entry;
        }
    }
    return covariance;
}

void whiten_columns(RegressionLikelihood& result, const RegressionDesign& design,
                    const SarimaLikelihood& baseline, double mean,
                    const Interrupt& interrupt) {
    const auto rows = result.adjusted_response.values.size();
    const auto states = baseline.model.transition.rows();
    const auto channels = result.free_columns.size() + 1;
    const auto count = result.finite_count;
    if (baseline.likelihood.observation_gains.size() != states * rows) {
        throw std::logic_error("regression whitening requires the complete state gain history");
    }
    std::vector<double> state(states * channels, 0), next(states * channels, 0);
    std::vector<std::size_t> finite_index(rows, count);
    for (std::size_t i = 0; i < count; ++i) finite_index[result.finite_rows[i]] = i;
    const auto& transition = baseline.model.transition.values();
    const auto& observation = baseline.model.observation;
    const auto& gains = baseline.likelihood.observation_gains;
    const auto& variances = baseline.likelihood.finite_variances;
    // Time recurrence is serial within each channel. Independent channels use
    // preallocated C++ memory and never call R, LAPACK or an interrupt in workers.
    // Main-thread cancellation occurs after at most 64 calendar positions.
    constexpr std::size_t time_chunk = 64;
    for (std::size_t first = 0; first < rows;) {
        const auto width = std::min(time_chunk, rows - first), end = first + width;
        parallel_for(channels, width * states * states, interrupt, [&](std::size_t channel) {
            const auto offset = channel * states;
            for (std::size_t t = first; t < end; ++t) {
                if (!result.adjusted_response.missing[t]) {
                    long double prediction = 0;
                    for (std::size_t i = 0; i < states; ++i) prediction += static_cast<long double>(observation[i]) * state[offset + i];
                    const auto value = channel == 0 ?
                        checked(static_cast<long double>(result.adjusted_response.values[t]) - mean) :
                        design.values[t + rows * result.free_columns[channel - 1]];
                    const auto innovation = checked(static_cast<long double>(value) - prediction);
                    if (finite_index[t] < count) {
                        const auto standardized = checked(innovation / std::sqrt(variances[t]));
                        if (innovation != 0 && standardized == 0) throw std::underflow_error("regression whitening underflows a finite innovation");
                        if (channel == 0) result.whitened_response[finite_index[t]] = standardized;
                        else result.whitened_design[finite_index[t] + count * (channel - 1)] = standardized;
                    }
                    for (std::size_t i = 0; i < states; ++i) {
                        state[offset + i] = checked(static_cast<long double>(state[offset + i]) + static_cast<long double>(gains[i + states * t]) * innovation);
                    }
                }
                if (t + 1 < rows) {
                    for (std::size_t i = 0; i < states; ++i) {
                        long double value = 0;
                        for (std::size_t j = 0; j < states; ++j) value += static_cast<long double>(transition[i + states * j]) * state[offset + j];
                        next[offset + i] = checked(value);
                    }
                    std::copy_n(next.data() + offset, states, state.data() + offset);
                }
            }
        });
        first = end;
    }
}

} // namespace

RegressionLikelihood sarima_regression_likelihood(
    const MaskedSeries& series, const RegressionDesign& design,
    const std::vector<double>& ar, const std::vector<double>& ma,
    const std::vector<double>& seasonal_ar, const std::vector<double>& seasonal_ma,
    int period, int d, int seasonal_d, double mean, double variance,
    double rank_tolerance, const Interrupt& interrupt) {
    series.validate();
    const auto rows = series.values.size();
    validate_design(design, rows, interrupt);
    if (!std::isfinite(rank_tolerance) || rank_tolerance <= 0 || rank_tolerance >= 1) {
        throw std::invalid_argument("regression rank tolerance must be strictly between zero and one");
    }
    if (!std::isfinite(mean)) throw std::invalid_argument("mean must be finite");
    if ((d > 0 || seasonal_d > 0) && mean != 0) throw std::invalid_argument("integrated SARIMA requires mean zero; use an explicit drift regressor");
    const double unavailable = std::numeric_limits<double>::quiet_NaN();
    RegressionLikelihood result{};
    result.coefficients.assign(design.columns, unavailable);
    result.fixed = design.fixed;
    result.fixed_effect.assign(rows, 0);
    result.fitted_effect.assign(rows, unavailable);
    result.adjusted_response = series;
    result.residual_series = {std::vector<double>(rows, 0), std::vector<std::uint8_t>(rows, 1)};
    for (std::size_t j = 0; j < design.columns; ++j) {
        if (design.fixed[j]) result.coefficients[j] = design.coefficients[j];
        else result.free_columns.push_back(j);
    }
    for (std::size_t i = 0; i < rows; ++i) {
        if ((i & 255U) == 0) checkpoint(interrupt);
        long double effect = 0;
        for (std::size_t j = 0; j < design.columns; ++j) {
            if (design.fixed[j]) effect += static_cast<long double>(design.values[i + rows * j]) * design.coefficients[j];
        }
        result.fixed_effect[i] = checked(effect);
        if (!series.missing[i]) result.adjusted_response.values[i] = checked(static_cast<long double>(series.values[i]) - effect);
    }
    // Covariance and gains do not depend on response values. A zero response
    // obtains them once without a premature quadratic form on unadjusted y/X.
    const MaskedSeries zero{std::vector<double>(rows, 0), series.missing};
    const auto baseline = sarima_innovations(zero, ar, ma, seasonal_ar,
                                             seasonal_ma, period, d, seasonal_d, 0, variance, interrupt, true);
    const auto& likelihood = baseline.likelihood;
    result.observed_count = likelihood.observed_count;
    result.diffuse_count = likelihood.diffuse_count;
    result.finite_count = likelihood.finite_count;
    result.initial_diffuse_rank = likelihood.initial_diffuse_rank;
    result.remaining_diffuse_rank = likelihood.remaining_diffuse_rank;
    result.sum_log_finite_variances = likelihood.sum_log_finite_variances;
    result.sum_log_diffuse_variances = likelihood.sum_log_diffuse_variances;
    result.difference_polynomial = baseline.difference;
    result.sum_squared_standardized = unavailable;
    result.fixed_log_likelihood = unavailable;
    result.profiled_variance = unavailable;
    result.profiled_log_likelihood = unavailable;
    result.status = likelihood.remaining_diffuse_rank ? "unresolved_diffuse" : "available";
    result.likelihood_status = result.status;
    result.profile_status = result.status;
    result.covariance_status = "unavailable";
    const auto free = result.free_columns.size(), count = result.finite_count;
    result.unscaled_covariance.assign(free * free, unavailable);
    result.fixed_covariance.assign(free * free, unavailable);
    result.ml_covariance.assign(free * free, unavailable);
    result.residual_covariance.assign(free * free, unavailable);
    result.whitened_design.assign(count * free, 0);
    result.whitened_response.assign(count, 0);
    result.whitening_norm_ratio.assign(free, unavailable);
    result.numerically_zero_columns.assign(free, 0);
    result.whitening_zero_tolerance = rank_tolerance;
    double maximum_inverse_sd = 0;
    for (std::size_t i = 0; i < rows; ++i) {
        if (likelihood.update_kind[i] == 2) {
            result.finite_rows.push_back(i);
            maximum_inverse_sd = std::max(maximum_inverse_sd, 1 / std::sqrt(likelihood.finite_variances[i]));
        }
    }
    if (result.finite_rows.size() != count) throw std::logic_error("regression finite-row accounting disagrees with the state model");
    whiten_columns(result, design, baseline, mean, interrupt);
    for (std::size_t j = 0; j < free; ++j) {
        checkpoint(interrupt);
        const auto column = result.free_columns[j];
        std::vector<double> observed;
        observed.reserve(result.observed_count);
        for (std::size_t i = 0; i < rows; ++i) {
            if (!series.missing[i]) {
                observed.push_back(design.values[i + rows * column]);
            }
        }
        const std::vector<double> whitened(result.whitened_design.begin() + static_cast<std::ptrdiff_t>(count * j),
                                          result.whitened_design.begin() + static_cast<std::ptrdiff_t>(count * (j + 1)));
        const auto original_norm = norm(observed), whitened_norm = norm(whitened);
        const long double reference_scale = static_cast<long double>(original_norm) * maximum_inverse_sd;
        result.whitening_norm_ratio[j] = reference_scale > 0 ? checked(whitened_norm / reference_scale) : 0;
        // A diffuse homogeneous column may leave only roundoff innovations.
        // Relative QR rank alone would call a single such column full rank.
        // Treat weak-to-input-scale integrated columns as numerically unresolved;
        // unavailable estimates/objectives below make this a fail-closed guard.
        if ((d > 0 || seasonal_d > 0) && whitened_norm <= rank_tolerance * reference_scale) result.numerically_zero_columns[j] = 1;
    }
    result.residual_df = count;
    if (free == 0) {
        // Fixed effects remain known even when the initial history has not
        // been identified and the complete likelihood is unavailable.
        result.fitted_effect = result.fixed_effect;
        result.residual_series = result.adjusted_response;
        result.covariance_status = "no_free_coefficients";
    }
    if (likelihood.remaining_diffuse_rank) return result;
    if (free > 0 && count == 0) {
        result.status = result.likelihood_status = result.profile_status = "no_finite_innovations";
        return result;
    }
    result.whitened_residuals = result.whitened_response;
    if (free > 0) {
        DenseMatrix whitened(count, free, result.whitened_design);
        for (std::size_t j = 0; j < free; ++j) {
            if (result.numerically_zero_columns[j]) std::fill_n(whitened.values().data() + count * j, count, 0);
        }
        const auto solved = qr_least_squares(whitened, DenseMatrix(count, 1, result.whitened_response), rank_tolerance, interrupt);
        result.rank = static_cast<std::size_t>(solved.rank);
        result.residual_df = count - result.rank;
        if (result.rank < free) {
            result.status = result.likelihood_status = result.profile_status = "rank_deficient";
            result.covariance_status = "rank_deficient";
            result.whitened_residuals.assign(count, unavailable);
            return result;
        }
        for (std::size_t j = 0; j < free; ++j) result.coefficients[result.free_columns[j]] = solved.coefficients(j, 0);
        result.whitened_residuals = solved.residuals.values();
        // Full row/column rank spans every finite response. Its concentrated
        // residual is mathematically zero, regardless of solution roundoff.
        if (result.residual_df == 0) std::fill(result.whitened_residuals.begin(), result.whitened_residuals.end(), 0);
        result.unscaled_covariance = regression_covariance(whitened, interrupt);
        result.covariance_status = result.residual_df ? "available" : "no_residual_degrees_of_freedom";
    } else result.covariance_status = "no_free_coefficients";
    result.fitted_effect.assign(rows, 0);
    result.residual_series = series;
    for (std::size_t i = 0; i < rows; ++i) {
        if ((i & 255U) == 0) checkpoint(interrupt);
        long double effect = 0;
        for (std::size_t j = 0; j < design.columns; ++j) effect += static_cast<long double>(design.values[i + rows * j]) * result.coefficients[j];
        result.fitted_effect[i] = checked(effect);
        if (!series.missing[i]) result.residual_series.values[i] = checked(static_cast<long double>(series.values[i]) - effect);
    }
    long double squares = 0;
    for (const auto value : result.whitened_residuals) {
        const long double term = static_cast<long double>(value) * value;
        if (value != 0 && term == 0) throw std::underflow_error("regression residual square underflows");
        squares += term;
    }
    result.sum_squared_standardized = checked(squares);
    const long double log_two_pi = std::log(2 * std::acos(-1.0L));
    const long double determinants = static_cast<long double>(result.sum_log_finite_variances) + result.sum_log_diffuse_variances;
    const auto finite_count = static_cast<long double>(count);
    result.fixed_log_likelihood = checked(-.5L * (determinants + finite_count * (log_two_pi + std::log(static_cast<long double>(variance))) + squares / variance));
    result.likelihood_status = result.observed_count == 0 ? "no_observations" : "available";
    result.profile_status = "no_finite_innovations";
    if (count > 0) {
        result.profiled_variance = checked(squares / finite_count);
        if (squares > 0 && result.profiled_variance == 0) throw std::underflow_error("profiled regression variance underflows");
        if (result.profiled_variance == 0) {
            result.profile_status = "zero_scale_boundary";
            result.profiled_log_likelihood = std::numeric_limits<double>::infinity();
        } else {
            result.profile_status = "available";
            result.profiled_log_likelihood = checked(-.5L * (determinants + finite_count * (log_two_pi + 1 + std::log(static_cast<long double>(result.profiled_variance)))));
        }
    }
    for (std::size_t i = 0; i < free * free; ++i) {
        const auto covariance = result.unscaled_covariance[i];
        result.fixed_covariance[i] = scaled_covariance(covariance, variance);
        result.ml_covariance[i] = scaled_covariance(covariance, result.profiled_variance);
        if (result.residual_df > 0) result.residual_covariance[i] = scaled_covariance(covariance, squares / static_cast<long double>(result.residual_df));
    }
    checkpoint(interrupt);
    return result;
}

} // namespace persephone
