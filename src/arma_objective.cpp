#include "core/arma_objective.hpp"
#include "core/parallel.hpp"

#include <algorithm>
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {

ArmaObjective evaluate_arma_objective(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean, double rank_tolerance, double log_jacobian,
    const Interrupt& interrupt) {
    validate_regression_inputs(series, design, mean, 1, rank_tolerance, interrupt);
    // Complete static validation before turning numerical failures into statuses.
    if (period != 4 && period != 12) throw std::invalid_argument("period must be 4 or 12");
    if (d < 0 || d > 2 || seasonal_d < 0 || seasonal_d > 2) throw std::invalid_argument("d and seasonal_d must be between zero and two");
    if ((d > 0 || seasonal_d > 0) && mean != 0) throw std::invalid_argument("integrated SARIMA requires mean zero; use an explicit drift regressor");
    if (!std::isfinite(log_jacobian)) throw std::invalid_argument("log Jacobian must be finite");
    const auto& blocks = map.blocks();
    const auto season = static_cast<std::size_t>(period);
    const auto states = std::max(blocks[0].initial.size() + season * blocks[2].initial.size(),
        1 + blocks[1].initial.size() + season * blocks[3].initial.size());
    if (states > 16 || states + static_cast<std::size_t>(d + period * seasonal_d) > 32) {
        throw std::invalid_argument("ARMA objective exceeds the 16 stationary / 32 total state bounds");
    }
    (void)parallel_info(); // Malformed thread configuration is not a bad candidate.
    const double unavailable = std::numeric_limits<double>::quiet_NaN();
    ArmaObjective result{"unavailable", "", map.decode(coordinates), unavailable, unavailable, std::nullopt};
    if (result.parameters.status != "admissible") {
        result.status = result.parameters.status;
        result.message = "ARMA proposal failed the declared factor admissibility check";
        return result;
    }
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt = [&] {
        if (!interrupt) return;
        try { interrupt(); }
        catch (...) { interrupted = std::current_exception(); throw; }
    };
    try {
        const auto& c = result.parameters.coefficients;
        result.likelihood = sarima_regression_likelihood(series, design, c[0], c[1], c[2], c[3],
            period, d, seasonal_d, mean, 1, rank_tolerance, checked_interrupt);
    } catch (const std::invalid_argument& error) {
        if (interrupted) std::rethrow_exception(interrupted);
        result.status = "numerical_failure"; result.message = error.what(); return result;
    } catch (const std::runtime_error& error) {
        if (interrupted) std::rethrow_exception(interrupted);
        result.status = "numerical_failure"; result.message = error.what(); return result;
    }
    const auto& fit = *result.likelihood;
    result.status = fit.profile_status;
    if (fit.likelihood_status != "available") result.status = fit.likelihood_status;
    if (result.status != "available") return result;
    const auto adjusted = fit.profiled_log_likelihood + log_jacobian;
    if (!std::isfinite(adjusted)) {
        result.status = "numerical_failure";
        result.message = "adjusted concentrated likelihood exceeds the finite numeric range";
        return result;
    }
    result.adjusted_log_likelihood = adjusted;
    result.value = -adjusted;
    return result;
}

} // namespace persephone
