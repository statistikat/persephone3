#ifndef PERSEPHONE_INFORMATION_HPP
#define PERSEPHONE_INFORMATION_HPP

#include "optimizer.hpp"
#include "linear_algebra.hpp"
#include <optional>

namespace persephone {

struct InformationControl {
    double difference_step = 1e-3;
    int difference_refinements = 8;
    int max_evaluations = 20000;
    double relative_tolerance = 1e-4;
    double absolute_tolerance = 1e-6;
    double gradient_tolerance = 1e-4;
    double rank_tolerance = 1e-10;
    double weak_tolerance = 1e-6;
    void validate() const;
};

struct ObservedInformation {
    std::string status = "unavailable";
    std::string derivative_status = "not_evaluated";
    std::string curvature_status = "not_assessed";
    ObjectiveSample center;
    int evaluations = 0, refinements = 0, rank = 0;
    bool stationary = false;
    std::vector<double> gradient, gradient_error, steps, normalized_eigenvalues;
    std::optional<DenseMatrix> hessian, hessian_error, covariance;
    double normalized_error = 0, eigenvalue_ratio = 0;
    std::map<std::string, int> rejected;
};

// Observed curvature of the supplied negative profile log likelihood. Three
// central stencils, Richardson checks, no one-sided information and no BFGS
// reuse. All objective evaluations, callbacks and LAPACK calls are sequential.
// Errors are numerical estimates, not rigorous bounds or statistical errors.
ObservedInformation observed_information(const ObjectiveFunction& objective,
    const std::vector<double>& coefficients, const InformationControl& control = {},
    const Interrupt& interrupt = {});

} // namespace persephone
#endif
