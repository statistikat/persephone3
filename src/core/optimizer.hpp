#ifndef PERSEPHONE_OPTIMIZER_HPP
#define PERSEPHONE_OPTIMIZER_HPP

#include "numerics.hpp"

#include <map>
#include <memory>
#include <string>

namespace persephone {

struct ObjectiveSample {
    double value;
    std::string status; // Only "available" admits a finite value.
    // Optional owned caller data; retained only for initial/final points and
    // live stencils. The solver never inspects it or calls an external runtime.
    std::shared_ptr<const void> detail;
};
using ObjectiveFunction = std::function<ObjectiveSample(const std::vector<double>&)>;

struct OptimizerControl {
    int max_iterations = 100;
    int max_evaluations = 2000;
    int max_line_search = 30;
    int difference_refinements = 10;
    double gradient_tolerance = 1e-5;
    double step_tolerance = 1e-10;
    double function_tolerance = 1e-10;
    double difference_step = 1e-4;
    double armijo = 1e-4;
    double max_step = 1;
    void validate() const;
};

struct OptimizerTrace {
    int iteration, evaluations;
    double value, gradient_norm, gradient_error, step_norm, function_change, step_length, gradient_bound;
};

struct OptimizationResult {
    std::string status;
    bool converged = false;
    int iterations = 0, evaluations = 0, gradient_evaluations = 0, bfgs_resets = 0;
    std::vector<double> initial_coordinates, coordinates, gradient, gradient_error, difference_steps;
    std::vector<std::string> difference_schemes;
    ObjectiveSample initial, final;
    std::map<std::string, int> rejected;
    std::vector<OptimizerTrace> trace;
};

// Independent bounded-memory BFGS with feasible Armijo halving and qualified
// central/one-sided differences. All callbacks and evaluations stay sequential.
// Convergence is first-order numerical stationarity, not identification or a
// certificate of a local/global minimum. No inverse-Hessian covariance is exposed.
OptimizationResult minimize_bfgs(const ObjectiveFunction& objective,
    const std::vector<double>& start, const OptimizerControl& control = {},
    const Interrupt& interrupt = {});

} // namespace persephone
#endif
