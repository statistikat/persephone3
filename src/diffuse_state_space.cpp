#include "core/diffuse_state_space.hpp"
#include "core/exact_history.hpp"
#include "core/sparse_history.hpp"
#include "core/finite_state_factor.hpp"

// DLARFG is called through R's declared LAPACK ABI. Only its orthogonal
// reflector is used; no third-party filtering implementation is incorporated.
#include <R_ext/Lapack.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <memory>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace {

constexpr std::size_t max_states = 32;
constexpr double rank_tolerance = 1e-12;
constexpr double projection_tolerance = 512 * std::numeric_limits<double>::epsilon();

void checkpoint(const Interrupt& interrupt) { if (interrupt) interrupt(); }

double checked(long double value) {
    const auto result = static_cast<double>(value);
    if (!std::isfinite(result)) throw std::overflow_error("diffuse state-space arithmetic produced a non-finite value");
    if (value != 0 && result == 0) throw std::underflow_error("diffuse state-space arithmetic underflows the finite numeric range");
    return result;
}

double norm(const std::vector<double>& values) {
    double result = 0;
    for (const auto value : values) result = checked(std::hypot(result, value));
    return result;
}

double square(double value) {
    const long double product = static_cast<long double>(value) * value;
    if (value != 0 && product == 0) throw std::underflow_error("diffuse state-space square underflows the finite numeric range");
    return checked(product);
}

struct Basis {
    std::size_t rows;
    std::size_t columns;
    std::vector<double> values;
    double operator()(std::size_t i, std::size_t j) const { return values.at(i + rows * j); }
};

void validate_basis(const Basis& basis, const Interrupt& interrupt, const char* context) {
    if (basis.columns > basis.rows || basis.values.size() != basis.rows * basis.columns) {
        throw std::invalid_argument("diffuse loading dimensions do not match its stated rank");
    }
    for (const auto value : basis.values) {
        if (!std::isfinite(value)) throw std::invalid_argument("diffuse loading values must be finite");
    }
    if (basis.columns == 0) return;
    const DenseMatrix loading(basis.rows, basis.columns, basis.values);
    const DenseMatrix zero_rhs(basis.rows, 1);
    const auto solved = qr_least_squares(loading, zero_rhs, rank_tolerance, interrupt);
    if (solved.rank != static_cast<int>(basis.columns)) throw std::invalid_argument(context);
}

DenseMatrix gram(const Basis& loading) {
    DenseMatrix result(loading.rows, loading.rows);
    for (std::size_t i = 0; i < loading.rows; ++i) {
        double row_norm = 0;
        for (std::size_t k = 0; k < loading.columns; ++k) row_norm = checked(std::hypot(row_norm, loading(i, k)));
        result(i, i) = square(row_norm);
        for (std::size_t j = i + 1; j < loading.rows; ++j) {
            long double total = 0;
            for (std::size_t k = 0; k < loading.columns; ++k) total += static_cast<long double>(loading(i, k)) * loading(j, k);
            result(i, j) = result(j, i) = checked(total);
        }
    }
    return result;
}

DenseMatrix gram(const DenseMatrix& loading) {
    return gram(Basis{loading.rows(), loading.columns(), loading.values()});
}

DenseMatrix sandwich(const DenseMatrix& a, const DenseMatrix& p) {
    DenseMatrix ap(a.rows(), p.columns()), result(a.rows(), a.rows());
    for (std::size_t j = 0; j < p.columns(); ++j) {
        for (std::size_t i = 0; i < a.rows(); ++i) {
            long double value = 0;
            for (std::size_t k = 0; k < a.columns(); ++k) value += static_cast<long double>(a(i, k)) * p(k, j);
            ap(i, j) = checked(value);
        }
    }
    for (std::size_t j = 0; j < a.rows(); ++j) {
        for (std::size_t i = 0; i <= j; ++i) {
            long double value = 0;
            for (std::size_t k = 0; k < a.columns(); ++k) value += static_cast<long double>(ap(i, k)) * a(j, k);
            result(i, j) = result(j, i) = checked(value);
        }
    }
    return result;
}

std::vector<double> product(const DenseMatrix& a, const std::vector<double>& x) {
    std::vector<double> result(a.rows(), 0);
    for (std::size_t i = 0; i < a.rows(); ++i) {
        long double value = 0;
        for (std::size_t j = 0; j < a.columns(); ++j) value += static_cast<long double>(a(i, j)) * x[j];
        result[i] = checked(value);
    }
    return result;
}

std::vector<double> observation_loading(const std::vector<double>& z, const Basis& basis) {
    std::vector<double> result(basis.columns, 0);
    for (std::size_t j = 0; j < basis.columns; ++j) {
        long double value = 0;
        for (std::size_t i = 0; i < basis.rows; ++i) value += static_cast<long double>(z[i]) * basis(i, j);
        result[j] = checked(value);
    }
    return result;
}

Basis propagate(const DenseMatrix& transition, const Basis& basis,
                 const std::vector<double>& z, bool observation_processed) {
    Basis result{basis.rows, basis.columns, std::vector<double>(basis.values.size(), 0)};
    for (std::size_t j = 0; j < basis.columns; ++j) {
        for (std::size_t i = 0; i < basis.rows; ++i) {
            // After either observed update, Z B_post is mathematically zero.
            // An exactly identical transition row therefore has an exactly
            // zero propagated loading. This structural identity avoids
            // cancellation when SARIMA stores the newly observed history;
            // no approximate row match or small-value threshold is used.
            bool observed_history_row = observation_processed;
            for (std::size_t k = 0; observed_history_row && k < basis.rows; ++k) {
                observed_history_row = transition(i, k) == z[k];
            }
            if (observed_history_row) continue;
            long double value = 0;
            for (std::size_t k = 0; k < basis.rows; ++k) value += static_cast<long double>(transition(i, k)) * basis(k, j);
            result.values[i + basis.rows * j] = checked(value);
        }
    }
    return result;
}

// A canonical history frame is normalized in the ORIGINAL initial-history
// coordinates. Only its orientation changes after observations; this is not
// a renormalization of the propagated state loading or the diffuse measure.
Basis history_loading(const detail::SparseHistoryBasis& history, std::size_t states,
                       std::uint64_t time, const Interrupt& interrupt) {
    const auto k = history.dimension(), rank = history.remaining_rank();
    if (k > states) throw std::logic_error("history frame exceeds the state dimension");
    const auto values = history.loading(time, interrupt);
    if (values.size() != k * rank) throw std::logic_error("history frame has inconsistent dimensions");
    Basis result{states, rank, std::vector<double>(states * rank, 0)};
    for (std::size_t j = 0; j < rank; ++j) {
        for (std::size_t i = 0; i < k; ++i) result.values[states - k + i + states * j] = values[i + k * j];
    }
    return result;
}

// w H = [beta, 0,...,0] for an orthogonal LAPACK reflector H. Retaining the
// remaining columns of B H removes one identified coordinate without a
// subtractive P_inf update or an arbitrary covariance threshold.
Basis remove_observed_direction(const Basis& basis, std::vector<double> w,
                                 const std::vector<double>& z, double w_norm,
                                 double& maximum_residual, const Interrupt& interrupt) {
    if (basis.columns == 1) return Basis{basis.rows, 0, {}};
    for (auto& value : w) value /= w_norm;
    const int n = static_cast<int>(w.size()), increment = 1;
    double tau = 0;
    checkpoint(interrupt);
    F77_CALL(dlarfg)(&n, &w[0], w.data() + 1, &increment, &tau);
    checkpoint(interrupt);
    if (!std::isfinite(tau) || tau < 0 || tau > 2) throw std::runtime_error("LAPACK returned an invalid diffuse reflector");
    w[0] = 1;
    Basis result{basis.rows, basis.columns - 1,
                 std::vector<double>(basis.rows * (basis.columns - 1), 0)};
    for (std::size_t i = 0; i < basis.rows; ++i) {
        long double dot = 0;
        for (std::size_t k = 0; k < basis.columns; ++k) dot += static_cast<long double>(basis(i, k)) * w[k];
        for (std::size_t j = 0; j < result.columns; ++j) {
            result.values[i + result.rows * j] = checked(static_cast<long double>(basis(i, j + 1)) - tau * dot * w[j + 1]);
        }
    }
    validate_basis(result, interrupt, "diffuse projection is numerically rank deficient");
    const auto residual = norm(observation_loading(z, result));
    const long double scale = static_cast<long double>(norm(z)) * norm(result.values);
    const double relative = scale == 0 ? 0 : checked(residual / scale);
    maximum_residual = std::max(maximum_residual, relative);
    if (relative > projection_tolerance * static_cast<double>(basis.rows)) {
        throw std::runtime_error("diffuse orthogonal projection failed its residual check");
    }
    return result;
}

DenseMatrix joseph(const DenseMatrix& p, const std::vector<double>& gain,
                    const std::vector<double>& z, double h) {
    const auto m = p.rows();
    DenseMatrix update(m, m);
    for (std::size_t j = 0; j < m; ++j) {
        for (std::size_t i = 0; i < m; ++i) {
            update(i, j) = checked(static_cast<long double>(i == j ? 1 : 0) - static_cast<long double>(gain[i]) * z[j]);
        }
    }
    auto posterior = sandwich(update, p);
    for (std::size_t j = 0; j < m; ++j) {
        for (std::size_t i = 0; i <= j; ++i) {
            posterior(i, j) = posterior(j, i) = checked(static_cast<long double>(posterior(i, j)) +
                static_cast<long double>(h) * gain[i] * gain[j]);
        }
    }
    return posterior;
}

void validate_model(const DiffuseStateSpace& model, const std::vector<double>& state,
                     double mean, double variance, const Interrupt& interrupt) {
    model.transition.validate(interrupt);
    model.noise_loading.validate(interrupt);
    const auto m = model.transition.rows();
    if (m > max_states || model.transition.columns() != m || model.observation.size() != m ||
        model.noise_loading.rows() != m || model.noise_loading.columns() > max_states || state.size() != m) {
        throw std::invalid_argument("diffuse dimensions must agree and have at most 32 states and loading columns");
    }
    for (const auto value : state) if (!std::isfinite(value)) throw std::invalid_argument("initial state must be finite");
    for (const auto value : model.observation) if (!std::isfinite(value)) throw std::invalid_argument("observation coefficients must be finite");
    if (!std::isfinite(model.observation_variance) || model.observation_variance < 0) throw std::invalid_argument("observation variance must be finite and nonnegative");
    if (!std::isfinite(mean)) throw std::invalid_argument("mean must be finite");
    if (!std::isfinite(variance) || variance <= 0) throw std::invalid_argument("variance must be finite and positive");
}

DiffuseLikelihood filter(const MaskedSeries& series, const DiffuseStateSpace& model,
                          std::vector<double> state, DenseMatrix p, Basis basis,
                          double mean, double variance, const Interrupt& interrupt,
                          detail::CountsHistoryCertificate* history = nullptr,
                          detail::SparseHistoryBasis* stable_history = nullptr,
                          detail::FiniteStateFactor* finite_factor = nullptr) {
    series.validate();
    validate_model(model, state, mean, variance, interrupt);
    validate_basis(basis, interrupt, "initial diffuse loading is numerically rank deficient");
    const auto m = model.transition.rows(), n = series.values.size(), initial_rank = basis.columns;
    if (history) {
        if (history->dimension() != initial_rank || history->rank() != 0) {
            throw std::logic_error("exact history certificate disagrees with initial diffuse rank");
        }
        static_assert(std::numeric_limits<std::size_t>::digits <=
                      std::numeric_limits<std::uint64_t>::digits,
                      "exact history certificate requires size_t to fit uint64");
        history->validate_length(static_cast<std::uint64_t>(n));
    }
    if (stable_history && (!history || stable_history->dimension() != initial_rank ||
                           stable_history->rank() != 0)) {
        throw std::logic_error("stable history frame disagrees with the initial diffuse certificate");
    }
    if (finite_factor && (!stable_history || finite_factor->dimension() != m)) {
        throw std::logic_error("finite factor disagrees with the canonical state dimension");
    }
    const auto process = gram(model.noise_loading);
    const auto initial_finite = p;
    const auto initial_diffuse = gram(basis);
    std::vector<double> predictions(n, 0), finite_variances(n, 0), diffuse_variances(n, 0);
    std::vector<int> update_kind(n, 0);
    std::vector<std::size_t> rank_before(n, 0), rank_after(n, 0);
    MaskedSeries innovations{std::vector<double>(n, 0), series.missing};
    MaskedSeries standardized{std::vector<double>(n, 0), std::vector<std::uint8_t>(n, 1)};
    std::size_t observed = 0, diffuse_count = 0, finite_count = 0, last_diffuse = 0, rank_checks = 0;
    std::size_t exact_zero_count = 0, exact_pivot_count = 0, history_reconstructions = 0;
    long double squares = 0, finite_logs = 0, diffuse_logs = 0;
    double maximum_projection_residual = 0;
    const double z_norm = norm(model.observation);
    bool scalar_transition = true;
    for (std::size_t j = 0; j < m; ++j) {
        for (std::size_t i = 0; i < m; ++i) {
            const double required = i == j ? model.transition(0, 0) : 0;
            if (model.transition(i, j) != required) scalar_transition = false;
        }
    }
    bool observation_null_certified = false;
    for (std::size_t t = 0; t < n; ++t) {
        if ((t & 31U) == 0) checkpoint(interrupt);
        rank_before[t] = basis.columns;
        bool exact_independent = false;
        bool exact_zero = false;
        if (history && basis.columns > 0) {
            if (basis.columns != initial_rank - history->rank()) {
                throw std::runtime_error("floating diffuse rank disagrees with exact history certificate");
            }
            exact_independent = history->classify_current(!series.missing[t]);
            exact_zero = !exact_independent;
        }
        long double predicted = mean;
        for (std::size_t i = 0; i < m; ++i) predicted += static_cast<long double>(model.observation[i]) * state[i];
        predictions[t] = checked(predicted);
        auto pz = std::vector<double>(m, 0);
        long double finite_f = model.observation_variance;
        if (finite_factor) {
            auto projected = finite_factor->observation(model.observation, model.observation_variance, interrupt);
            pz = std::move(projected.pz);
            finite_f = projected.variance;
        } else {
            pz = product(p, model.observation);
            for (std::size_t i = 0; i < m; ++i) finite_f += static_cast<long double>(model.observation[i]) * pz[i];
        }
        finite_variances[t] = checked(finite_f);
        if (finite_variances[t] < 0) throw std::runtime_error("negative finite innovation variance: diffuse arithmetic is numerically unresolved");
        auto w = observation_loading(model.observation, basis);
        double w_norm = norm(w);
        // An observed update makes Z B_post = 0 exactly. T=cI preserves this
        // identity through any number of missing periods. Only the exact
        // scalar-identity pattern enables the certificate; approximate
        // proportionality is never used to discard weak observability.
        // Canonical SARIMA has an additional exact certificate from the
        // observed homogeneous-history design. It establishes dependence
        // without numerical rank thresholds or changing B's coordinates.
        if ((observation_null_certified || exact_zero) && basis.columns > 0) {
            const long double scale = static_cast<long double>(z_norm) * norm(basis.values);
            const double relative = scale == 0 ? 0 : checked(w_norm / scale);
            maximum_projection_residual = std::max(maximum_projection_residual, relative);
            if (relative > projection_tolerance * static_cast<double>(m)) {
                throw std::runtime_error("certified diffuse observation-null identity failed its residual check");
            }
            std::fill(w.begin(), w.end(), 0);
            w_norm = 0;
        }
        if (exact_independent && w_norm == 0) {
            throw std::runtime_error("exact history pivot is numerically unresolved in the diffuse loading");
        }
        diffuse_variances[t] = square(w_norm);
        if (!series.missing[t]) {
            ++observed;
            const double innovation = checked(static_cast<long double>(series.values[t]) - predicted);
            innovations.values[t] = innovation;
            std::vector<double> gain(m, 0);
            if (w_norm > 0) {
                const long double scale = static_cast<long double>(z_norm) * norm(basis.values);
                if (w_norm <= projection_tolerance * static_cast<long double>(m) * scale) {
                    throw std::invalid_argument("numerically ambiguous diffuse innovation; rescale the diffuse coordinates or model");
                }
                for (std::size_t i = 0; i < m; ++i) {
                    long double total = 0;
                    for (std::size_t j = 0; j < basis.columns; ++j) total += static_cast<long double>(basis(i, j)) * (w[j] / w_norm);
                    gain[i] = checked(total / w_norm);
                }
                if (finite_factor) finite_factor->update(gain, model.observation, model.observation_variance, interrupt);
                else p = joseph(p, gain, model.observation, model.observation_variance);
                basis = remove_observed_direction(basis, w, model.observation, w_norm,
                                                    maximum_projection_residual, interrupt);
                diffuse_logs += 2 * std::log(static_cast<long double>(w_norm));
                ++diffuse_count;
                if (history) ++exact_pivot_count;
                last_diffuse = t + 1;
                update_kind[t] = 1;
            } else {
                if (finite_variances[t] == 0) throw std::invalid_argument("finite observations require positive innovation variance; deterministic likelihood is unsupported");
                const double residual = checked(innovation / std::sqrt(finite_variances[t]));
                if (innovation != 0 && residual == 0) throw std::underflow_error("finite standardized innovation underflows");
                const double residual_square = square(residual);
                squares += residual_square;
                finite_logs += std::log(static_cast<long double>(finite_variances[t]));
                standardized.values[t] = residual;
                standardized.missing[t] = 0;
                for (std::size_t i = 0; i < m; ++i) gain[i] = checked(pz[i] / finite_variances[t]);
                if (finite_factor) finite_factor->update(gain, model.observation, model.observation_variance, interrupt);
                else p = joseph(p, gain, model.observation, model.observation_variance);
                ++finite_count;
                if (exact_zero) ++exact_zero_count;
                update_kind[t] = 2;
            }
            for (std::size_t i = 0; i < m; ++i) state[i] = checked(static_cast<long double>(state[i]) + static_cast<long double>(gain[i]) * innovation);
            observation_null_certified = true;
            if (stable_history) stable_history->observe(static_cast<std::uint64_t>(t) + 1, interrupt);
        }
        rank_after[t] = basis.columns;
        if (history && basis.columns != initial_rank - history->rank()) {
            throw std::runtime_error("diffuse update disagrees with exact history rank");
        }
        if (stable_history && stable_history->remaining_rank() != basis.columns) {
            throw std::runtime_error("stable history frame disagrees with the exact diffuse rank");
        }
        if (t + 1 < n) {
            if (history) history->advance();
            observation_null_certified = observation_null_certified && scalar_transition;
            state = product(model.transition, state);
            if (finite_factor) finite_factor->predict(model.transition, model.noise_loading, interrupt);
            else {
                p = sandwich(model.transition, p);
                for (std::size_t i = 0; i < p.values().size(); ++i) p.values()[i] = checked(static_cast<long double>(p.values()[i]) + process.values()[i]);
            }
            if (basis.columns > 0) {
                if (stable_history) {
                    basis = history_loading(*stable_history, m, static_cast<std::uint64_t>(t) + 2, interrupt);
                    ++history_reconstructions;
                } else basis = propagate(model.transition, basis, model.observation, !series.missing[t]);
                validate_basis(basis, interrupt, "transition loses diffuse rank or is numerically rank deficient; initial coordinates remain unidentified");
                ++rank_checks;
            }
        }
    }
    if (finite_factor) p = finite_factor->covariance(interrupt);
    const double sum_squares = checked(squares), sum_finite_logs = checked(finite_logs), sum_diffuse_logs = checked(diffuse_logs);
    const double unavailable = std::numeric_limits<double>::quiet_NaN();
    double fixed_loglik = unavailable, profiled_variance = unavailable, profiled_loglik = unavailable;
    std::string likelihood_status = "unresolved_diffuse", profile_status = "unresolved_diffuse";
    if (basis.columns == 0) {
        likelihood_status = observed == 0 ? "no_observations" : "available";
        profile_status = "no_finite_innovations";
        const auto n_finite = static_cast<long double>(finite_count);
        const long double log_two_pi = std::log(2 * std::acos(-1.0L));
        fixed_loglik = checked(-.5L * (diffuse_logs + finite_logs +
            n_finite * (log_two_pi + std::log(static_cast<long double>(variance))) + squares / variance));
        if (finite_count > 0) {
            profiled_variance = checked(squares / n_finite);
            if (squares > 0 && profiled_variance == 0) throw std::underflow_error("profiled diffuse variance underflows");
            if (profiled_variance == 0) {
                profile_status = "zero_scale_boundary";
                profiled_loglik = std::numeric_limits<double>::infinity();
            } else {
                profile_status = "available";
                profiled_loglik = checked(-.5L * (diffuse_logs + finite_logs +
                    n_finite * (log_two_pi + 1 + std::log(static_cast<long double>(profiled_variance)))));
            }
        }
    }
    checkpoint(interrupt);
    return {std::move(predictions), std::move(innovations), std::move(standardized),
            std::move(finite_variances), std::move(diffuse_variances), std::move(update_kind),
            std::move(rank_before), std::move(rank_after), observed, diffuse_count, finite_count,
            initial_rank, basis.columns, last_diffuse, sum_squares, sum_finite_logs, sum_diffuse_logs,
            fixed_loglik, profiled_variance, profiled_loglik, std::move(likelihood_status),
            std::move(profile_status), std::move(state), initial_finite, initial_diffuse,
            std::move(p), gram(basis),
            {rank_tolerance, projection_tolerance, rank_checks, maximum_projection_residual,
             history != nullptr, exact_zero_count, exact_pivot_count,
             stable_history != nullptr, history_reconstructions,
             stable_history ? stable_history->rebuild_count() : 0,
             stable_history ? stable_history->normalization_residual() : 0,
             finite_factor != nullptr,
             finite_factor ? finite_factor->compression_count() : 0,
             finite_factor ? finite_factor->clipped_negative_count() : 0,
             finite_factor ? finite_factor->initialization_residual() : 0}};
}

} // namespace

DiffuseLikelihood diffuse_innovations(const MaskedSeries& series, const DiffuseStateSpace& model,
                                      const DiffuseInitialState& initial, double mean,
                                      double variance, const Interrupt& interrupt) {
    validate_model(model, initial.state, mean, variance, interrupt);
    initial.finite_loading.validate(interrupt);
    const auto m = model.transition.rows();
    if (initial.finite_loading.rows() != m || initial.finite_loading.columns() > max_states) {
        throw std::invalid_argument("initial finite loading dimensions must agree and have at most 32 columns");
    }
    return filter(series, model, initial.state, gram(initial.finite_loading),
                  Basis{m, initial.diffuse_rank, initial.diffuse_loading}, mean, variance, interrupt);
}

SarimaLikelihood sarima_innovations(const MaskedSeries& series, const std::vector<double>& ar,
                                   const std::vector<double>& ma, const std::vector<double>& seasonal_ar,
                                   const std::vector<double>& seasonal_ma, int period, int d,
                                   int seasonal_d, double mean, double variance, const Interrupt& interrupt) {
    if (period != 4 && period != 12) throw std::invalid_argument("SARIMA period must be 4 or 12");
    if (d < 0 || d > 2 || seasonal_d < 0 || seasonal_d > 2) throw std::invalid_argument("d and seasonal_d must be between 0 and 2");
    if (!std::isfinite(mean)) throw std::invalid_argument("mean must be finite");
    if ((d > 0 || seasonal_d > 0) && mean != 0) throw std::invalid_argument("integrated SARIMA requires mean zero; drift is not implemented");
    auto arma = stationary_arma(ar, ma, seasonal_ar, seasonal_ma, period, interrupt);
    auto difference = difference_polynomial(d, seasonal_d, period, interrupt);
    const auto r = arma.state_space.transition.rows(), k = difference.size() - 1, m = r + k;
    if (m > max_states) throw std::invalid_argument("combined SARIMA model exceeds the 32-state diffuse pilot bound");
    const auto stationary = stationary_initialization(arma.state_space, interrupt);
    DenseMatrix transition(m, m), loading(m, 1), initial_finite(m, m);
    std::vector<double> z(m, 0);
    for (std::size_t i = 0; i < r; ++i) {
        z[i] = arma.state_space.observation[i];
        loading(i, 0) = arma.state_space.noise_loading(i, 0);
        for (std::size_t j = 0; j < r; ++j) {
            transition(i, j) = arma.state_space.transition(i, j);
            initial_finite(i, j) = stationary.covariance(i, j);
        }
    }
    Basis basis{m, k, std::vector<double>(m * k, 0)};
    for (std::size_t i = 0; i < k; ++i) {
        z[r + i] = -difference[i + 1];
        basis.values[r + i + m * i] = 1;
    }
    if (k > 0) {
        for (std::size_t j = 0; j < m; ++j) transition(r, j) = z[j];
        for (std::size_t i = 1; i < k; ++i) transition(r + i, r + i - 1) = 1;
    }
    DiffuseStateSpace model{std::move(transition), std::move(z), std::move(loading), 0};
    detail::CountsHistoryCertificate history(d, seasonal_d, period);
    detail::SparseHistoryBasis stable_history(d, seasonal_d, period, interrupt);
    std::unique_ptr<detail::FiniteStateFactor> finite_factor;
    if (k > 0) finite_factor = std::make_unique<detail::FiniteStateFactor>(initial_finite, interrupt);
    auto likelihood = filter(series, model, std::vector<double>(m, 0), std::move(initial_finite),
                              std::move(basis), mean, variance, interrupt,
                              k > 0 ? &history : nullptr, k > 0 ? &stable_history : nullptr,
                              finite_factor.get());
    return {std::move(model), std::move(arma.ar), std::move(arma.ma),
            std::move(difference), std::move(likelihood)};
}

} // namespace persephone
