#ifndef PERSEPHONE_ARMA_OBJECTIVE_HPP
#define PERSEPHONE_ARMA_OBJECTIVE_HPP

#include "arma_parameters.hpp"
#include "regression_likelihood.hpp"

#include <optional>

namespace persephone {

struct ArmaObjective {
    std::string status;
    std::string message;
    ArmaDecoded parameters;
    double value; // Negative adjusted concentrated ML; NaN unless available.
    double adjusted_log_likelihood;
    std::optional<RegressionLikelihood> likelihood;
};

void validate_arma_objective_inputs(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    int period, int d, int seasonal_d, double mean, double rank_tolerance,
    double log_jacobian, const Interrupt& interrupt = {});

// A single candidate, not an optimizer. Invalid static inputs throw; rejected
// candidates and numerical failures return explicit unavailable objectives.
// Interrupts, allocation failures and internal logic errors still propagate.
ArmaObjective evaluate_arma_objective(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean = 0, double rank_tolerance = 1e-10, double log_jacobian = 0,
    const Interrupt& interrupt = {});

} // namespace persephone
#endif
