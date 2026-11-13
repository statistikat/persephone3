#include "core/state_space.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace {

constexpr std::size_t max_states = 16;
constexpr double rank_tolerance = 1e-12;
constexpr double residual_tolerance = 1e-10;

void checkpoint(const Interrupt& interrupt) { if (interrupt) interrupt(); }

double finite(double value) {
    if (!std::isfinite(value)) {
        throw std::overflow_error("state-space arithmetic produced a non-finite value");
    }
    return value;
}

double finite(long double value) { return finite(static_cast<double>(value)); }

void validate(const StationaryStateSpace& model, const Interrupt& interrupt) {
    model.transition.validate(interrupt);
    model.noise_loading.validate(interrupt);
    const auto m = model.transition.rows();
    if (m > max_states || model.transition.columns() != m ||
        model.observation.size() != m || model.noise_loading.rows() != m ||
        model.noise_loading.columns() > max_states) {
        throw std::invalid_argument("state-space dimensions must agree and have at most 16 states and loading columns");
    }
    for (const auto value : model.observation) {
        if (!std::isfinite(value)) throw std::invalid_argument("observation coefficients must be finite");
    }
    if (!std::isfinite(model.observation_variance) || model.observation_variance < 0) {
        throw std::invalid_argument("observation variance must be finite and nonnegative");
    }
}

DenseMatrix multiply(const DenseMatrix& a, const DenseMatrix& b) {
    if (a.columns() != b.rows()) throw std::invalid_argument("matrix multiplication dimensions do not agree");
    DenseMatrix result(a.rows(), b.columns());
    for (std::size_t j = 0; j < b.columns(); ++j) {
        for (std::size_t i = 0; i < a.rows(); ++i) {
            long double total = 0;
            for (std::size_t k = 0; k < a.columns(); ++k) total += static_cast<long double>(a(i, k)) * b(k, j);
            result(i, j) = finite(total);
        }
    }
    return result;
}

// A B A', with symmetric output. Loop sums use extended precision where the
// platform provides it, but correctness/tolerances do not depend on its width.
DenseMatrix sandwich(const DenseMatrix& a, const DenseMatrix& b) {
    const auto ab = multiply(a, b);
    DenseMatrix result(a.rows(), a.rows());
    for (std::size_t j = 0; j < a.rows(); ++j) {
        for (std::size_t i = 0; i <= j; ++i) {
            long double total = 0;
            for (std::size_t k = 0; k < a.columns(); ++k) total += static_cast<long double>(ab(i, k)) * a(j, k);
            result(i, j) = result(j, i) = finite(total);
        }
    }
    return result;
}

DenseMatrix symmetric_solution(const DenseMatrix& coefficients, std::size_t column,
                                std::size_t m) {
    DenseMatrix result(m, m);
    for (std::size_t j = 0; j < m; ++j) {
        for (std::size_t i = 0; i <= j; ++i) {
            const long double left = coefficients(i + m * j, column);
            const long double right = coefficients(j + m * i, column);
            result(i, j) = result(j, i) = finite((left + right) / 2);
        }
    }
    return result;
}

double lyapunov_residual(const DenseMatrix& transition, const DenseMatrix& p,
                         const DenseMatrix& rhs) {
    const auto propagated = sandwich(transition, p);
    long double error = 0;
    long double scale = 0;
    for (std::size_t i = 0; i < p.values().size(); ++i) {
        const long double a = p.values()[i], b = propagated.values()[i], c = rhs.values()[i];
        error = std::max(error, std::abs(a - b - c));
        scale = std::max(scale, std::abs(a) + std::abs(b) + std::abs(c));
    }
    return scale == 0 ? 0 : finite(error / scale);
}

std::vector<double> lag_polynomial(const std::vector<double>& coefficients,
                                    int step, double sign) {
    if (step < 1 || coefficients.size() > max_states ||
        (!coefficients.empty() && static_cast<std::size_t>(step) > max_states / coefficients.size())) {
        throw std::invalid_argument("ARMA polynomial order exceeds the 16-state pilot bound");
    }
    std::vector<double> result(coefficients.size() * static_cast<std::size_t>(step) + 1, 0);
    result[0] = 1;
    for (std::size_t i = 0; i < coefficients.size(); ++i) {
        if (!std::isfinite(coefficients[i])) throw std::invalid_argument("ARMA coefficients must be finite");
        result[(i + 1) * static_cast<std::size_t>(step)] = sign * coefficients[i];
    }
    return result;
}

} // namespace

StationaryInitialization stationary_initialization(const StationaryStateSpace& model,
                                                    const Interrupt& interrupt) {
    validate(model, interrupt);
    const auto m = model.transition.rows();
    const auto dimension = m * m;
    DenseMatrix process(m, m);
    for (std::size_t j = 0; j < m; ++j) {
        checkpoint(interrupt);
        for (std::size_t i = 0; i <= j; ++i) {
            long double value = 0;
            for (std::size_t k = 0; k < model.noise_loading.columns(); ++k) {
                value += static_cast<long double>(model.noise_loading(i, k)) * model.noise_loading(j, k);
            }
            process(i, j) = process(j, i) = finite(value);
        }
    }
    DenseMatrix system(dimension, dimension), rhs(dimension, 2), identity(m, m);
    for (std::size_t j = 0; j < m; ++j) {
        checkpoint(interrupt);
        identity(j, j) = 1;
        for (std::size_t i = 0; i < m; ++i) {
            const auto row = i + m * j;
            rhs(row, 0) = process(i, j);
            rhs(row, 1) = i == j ? 1 : 0;
            for (std::size_t l = 0; l < m; ++l) {
                for (std::size_t k = 0; k < m; ++k) {
                    const auto column = k + m * l;
                    system(row, column) = finite(static_cast<long double>(row == column ? 1 : 0) -
                        static_cast<long double>(model.transition(i, k)) * model.transition(j, l));
                }
            }
        }
    }
    const auto solved = qr_least_squares(system, rhs, rank_tolerance, interrupt);
    if (solved.rank != static_cast<int>(dimension)) {
        throw std::invalid_argument("stationary initialization is singular or numerically rank deficient; diffuse initialization is unsupported");
    }
    auto covariance = symmetric_solution(solved.coefficients, 0, m);
    const auto certificate = symmetric_solution(solved.coefficients, 1, m);
    const double error = std::max(lyapunov_residual(model.transition, covariance, process),
                                  lyapunov_residual(model.transition, certificate, identity));
    if (!std::isfinite(error) || error > residual_tolerance) {
        throw std::runtime_error("stationary Lyapunov solve failed its scaled residual check");
    }
    // For real T, existence of positive definite S satisfying S-TST'=I is
    // equivalent to Schur stability. This tests every mode, including modes
    // with zero process noise or zero observation loading.
    CholeskyFactor factor = [&]() {
        try { return cholesky_factor(certificate, interrupt); }
        catch (const std::invalid_argument&) {
            throw std::invalid_argument("transition is not certified stationary: the Lyapunov stability certificate is not positive definite");
        }
    }();
    if (factor.reciprocal_condition <= rank_tolerance) {
        throw std::invalid_argument("stationary stability certificate is numerically ill-conditioned at the pilot tolerance");
    }
    return {std::move(covariance), std::move(process), error, factor.reciprocal_condition};
}

InnovationsLikelihood stationary_innovations(const MaskedSeries& series,
                                              const StationaryStateSpace& model,
                                              double mean, double variance,
                                              const Interrupt& interrupt) {
    series.validate();
    if (!std::isfinite(mean)) throw std::invalid_argument("observation mean must be finite");
    if (!std::isfinite(variance) || variance <= 0) throw std::invalid_argument("innovation scale variance must be finite and positive");
    auto initialization = stationary_initialization(model, interrupt);
    const auto m = model.transition.rows(), n = series.values.size();
    std::vector<double> state(m, 0), predictions(n, 0), variances(n, 0);
    MaskedSeries innovations{std::vector<double>(n, 0), series.missing};
    MaskedSeries standardized{std::vector<double>(n, 0), series.missing};
    DenseMatrix covariance = initialization.covariance;
    std::size_t observed = 0;
    long double squares = 0, log_variances = 0;
    for (std::size_t t = 0; t < n; ++t) {
        if ((t & 63U) == 0) checkpoint(interrupt);
        long double prediction = mean;
        std::vector<double> pz(m, 0);
        for (std::size_t i = 0; i < m; ++i) {
            prediction += static_cast<long double>(model.observation[i]) * state[i];
            long double total = 0;
            for (std::size_t j = 0; j < m; ++j) total += static_cast<long double>(covariance(i, j)) * model.observation[j];
            pz[i] = finite(total);
        }
        predictions[t] = finite(prediction);
        long double f = model.observation_variance;
        for (std::size_t i = 0; i < m; ++i) f += static_cast<long double>(model.observation[i]) * pz[i];
        variances[t] = finite(f);
        if (variances[t] < 0) throw std::runtime_error("negative innovation variance: stationary covariance is numerically unresolved");
        if (!series.missing[t]) {
            if (variances[t] == 0) throw std::invalid_argument("observed data require a positive innovation variance; deterministic observation likelihood is unsupported");
            const double innovation = finite(static_cast<long double>(series.values[t]) - prediction);
            const double standardized_value = finite(innovation / std::sqrt(variances[t]));
            const long double squared = static_cast<long double>(standardized_value) * standardized_value;
            if ((innovation != 0 && standardized_value == 0) ||
                (standardized_value != 0 && squared == 0)) {
                throw std::underflow_error("standardized innovation square underflows the finite numeric range");
            }
            innovations.values[t] = innovation;
            standardized.values[t] = standardized_value;
            squares += squared;
            log_variances += std::log(static_cast<long double>(variances[t]));
            ++observed;
            std::vector<double> gain(m, 0);
            DenseMatrix update(m, m);
            for (std::size_t i = 0; i < m; ++i) {
                gain[i] = finite(pz[i] / variances[t]);
                state[i] = finite(static_cast<long double>(state[i]) + static_cast<long double>(gain[i]) * innovation);
                for (std::size_t j = 0; j < m; ++j) {
                    update(i, j) = finite(static_cast<long double>(i == j ? 1 : 0) -
                                           static_cast<long double>(gain[i]) * model.observation[j]);
                }
            }
            // Joseph form: (I-KZ)P(I-KZ)' + h K K'.
            auto posterior = sandwich(update, covariance);
            for (std::size_t j = 0; j < m; ++j) {
                for (std::size_t i = 0; i <= j; ++i) {
                    posterior(i, j) = posterior(j, i) = finite(static_cast<long double>(posterior(i, j)) +
                        static_cast<long double>(model.observation_variance) * gain[i] * gain[j]);
                }
            }
            covariance = std::move(posterior);
        }
        if (t + 1 < n) {
            std::vector<double> next(m, 0);
            for (std::size_t i = 0; i < m; ++i) {
                long double value = 0;
                for (std::size_t j = 0; j < m; ++j) value += static_cast<long double>(model.transition(i, j)) * state[j];
                next[i] = finite(value);
            }
            state = std::move(next);
            covariance = sandwich(model.transition, covariance);
            for (std::size_t i = 0; i < covariance.values().size(); ++i) {
                covariance.values()[i] = finite(static_cast<long double>(covariance.values()[i]) +
                                                 initialization.process_covariance.values()[i]);
            }
        }
    }
    const double sum_squares = finite(squares), sum_logs = finite(log_variances);
    if (squares > 0 && sum_squares == 0) {
        throw std::underflow_error("standardized sum of squares underflows the finite numeric range");
    }
    const long double log_two_pi = std::log(2 * std::acos(-1.0L));
    const auto n_observed = static_cast<long double>(observed);
    const double fixed_loglik = finite(-0.5L * (n_observed * (log_two_pi + std::log(static_cast<long double>(variance))) +
                                               log_variances + squares / variance));
    double profile_variance = std::numeric_limits<double>::quiet_NaN();
    double profile_loglik = std::numeric_limits<double>::quiet_NaN();
    std::string status = "no_observations";
    if (observed > 0) {
        profile_variance = finite(squares / n_observed);
        if (squares > 0 && profile_variance == 0) {
            throw std::underflow_error("profiled variance underflows the finite numeric range");
        }
        if (profile_variance == 0) {
            status = "zero_scale_boundary";
            profile_loglik = std::numeric_limits<double>::infinity();
        } else {
            status = "available";
            profile_loglik = finite(-0.5L * (n_observed * (log_two_pi + 1 + std::log(static_cast<long double>(profile_variance))) + log_variances));
        }
    }
    checkpoint(interrupt);
    return {std::move(predictions), std::move(innovations), std::move(standardized),
            std::move(variances), std::move(state), std::move(covariance),
            std::move(initialization), observed, sum_squares, sum_logs, fixed_loglik,
            profile_variance, profile_loglik, std::move(status)};
}

StationaryArma stationary_arma(const std::vector<double>& ar, const std::vector<double>& ma,
                                const std::vector<double>& seasonal_ar,
                                const std::vector<double>& seasonal_ma,
                                int period, const Interrupt& interrupt) {
    if (period < 1) throw std::invalid_argument("seasonal period must be a positive integer");
    auto ar_poly = convolve(lag_polynomial(ar, 1, -1), lag_polynomial(seasonal_ar, period, -1), interrupt);
    auto ma_poly = convolve(lag_polynomial(ma, 1, 1), lag_polynomial(seasonal_ma, period, 1), interrupt);
    const auto m = std::max(ar_poly.size() - 1, ma_poly.size());
    if (m > max_states) throw std::invalid_argument("expanded ARMA model exceeds the 16-state pilot bound");
    std::vector<double> expanded_ar(ar_poly.size() - 1), expanded_ma(ma_poly.begin() + 1, ma_poly.end());
    DenseMatrix transition(m, m), loading(m, 1);
    std::vector<double> observation(m, 0);
    observation[0] = 1;
    for (std::size_t i = 0; i < expanded_ar.size(); ++i) transition(i, 0) = expanded_ar[i] = -ar_poly[i + 1];
    for (std::size_t i = 0; i + 1 < m; ++i) transition(i, i + 1) = 1;
    for (std::size_t i = 0; i < ma_poly.size(); ++i) loading(i, 0) = ma_poly[i];
    return {{std::move(transition), std::move(observation), std::move(loading), 0},
            std::move(expanded_ar), std::move(expanded_ma)};
}

} // namespace persephone
