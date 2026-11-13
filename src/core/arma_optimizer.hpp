#ifndef PERSEPHONE_ARMA_OPTIMIZER_HPP
#define PERSEPHONE_ARMA_OPTIMIZER_HPP

#include "arma_objective.hpp"
#include "optimizer.hpp"

namespace persephone {
struct ArmaOptimizationPoint {
    std::vector<double> coordinates; // The existing map's z/direct coordinates.
    ArmaObjective objective;
};

// The solver works in u=tanh(z) for fully free factors and in polynomial
// coefficients for mixed factors. This prevents transform saturation from
// looking like a small physical gradient. Constraints are enforced by rejection.
// Returned ObjectiveSample details own ArmaOptimizationPoint, including the
// final fit, without an uncounted extra likelihood evaluation.
OptimizationResult optimize_arma_objective(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& start, int period, int d, int seasonal_d,
    double mean=0, double rank_tolerance=1e-10, double log_jacobian=0,
    const OptimizerControl& control={}, const Interrupt& interrupt={});

} // namespace persephone
#endif
