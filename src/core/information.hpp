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

// Shared signed spectrum and inverse qualification. The caller supplies an
// accuracy-qualified matrix/error and stationarity; it also labels whether the
// matrix is observed curvature or a residual-Jacobian approximation.
void qualify_information(ObservedInformation& result, const InformationControl& control,
    const Interrupt& interrupt = {});

struct ResidualSample {
    std::string status;
    std::vector<double> residuals;
    std::size_t effective_count = 0;
    std::shared_ptr<const void> detail;
};
using ResidualFunction = std::function<ResidualSample(const std::vector<double>&)>;
struct ResidualInformation {
    ObservedInformation diagnostics;
    ResidualSample center;
    std::optional<DenseMatrix> jacobian, jacobian_error;
};
// Gauss-Newton information n J'J/(z'z) for determinant-normalized residuals.
// Uses physical-coordinate three-scale central differences and never an
// optimizer's working/damped matrix. Covariance here is the inverse information;
// the caller supplies its declared residual-df multiplier.
ResidualInformation residual_information(const ResidualFunction& residuals,
    const std::vector<double>& coefficients, const InformationControl& control = {},
    const Interrupt& interrupt = {});

struct VectorSample {
    std::string status;
    std::vector<double> values;
};
using VectorFunction = std::function<VectorSample(const std::vector<double>&)>;
struct VectorJacobian {
    std::string status = "unavailable";
    VectorSample center;
    int evaluations = 0, refinements = 0;
    std::vector<double> steps;
    std::optional<DenseMatrix> jacobian, error;
    std::map<std::string, int> rejected;
};
// Dimensionless outputs with three central stencils and Richardson agreement.
// Unlike residual information this permits a zero center and does not infer a
// statistical score, scale or covariance. Errors are numerical estimates.
VectorJacobian vector_jacobian(const VectorFunction& function,
    const std::vector<double>& coefficients, const InformationControl& control = {},
    const Interrupt& interrupt = {});

} // namespace persephone
#endif
