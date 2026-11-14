#ifndef PERSEPHONE_INTERPOLATION_INFORMATION_HPP
#define PERSEPHONE_INTERPOLATION_INFORMATION_HPP
#include "arma_residuals.hpp"
#include "arma_information.hpp"
#include <optional>

namespace persephone {
struct InterpolationInformation {
    std::string status="point_unavailable",message;
    ArmaResiduals fit;
    double variance=0;
    std::optional<DenseMatrix> covariance,known_regression_covariance,regression_covariance;
    std::vector<double> standard_errors;
};
// Conditional on the supplied ARMA coefficients, including GLS regression
// estimation uncertainty. Scale uses the original observed likelihood count;
// missing-coordinate integration is not counted as extra fitted parameters.
InterpolationInformation interpolation_information(const MaskedSeries& series,
    const RegressionDesign& design,const ArmaParameterMap& map,
    const std::vector<double>& coordinates,int period,int d,int seasonal_d,
    double mean=0,double rank_tolerance=1e-10,double log_jacobian=0,
    ResidualScale scale=ResidualScale::ML,const Interrupt& interrupt={});

struct InterpolationArmaInformation {
    std::string status="conditional_unavailable",message;
    std::optional<ArmaInformation> arma;
    VectorJacobian derivatives;
    int evaluations=0;
    std::optional<DenseMatrix> covariance,increment,sensitivity,sensitivity_error;
    std::vector<double> standard_errors;
};
// Delta-method propagation G Vtheta G'. `conditional` must describe these
// same inputs and use ML scale. Only accurate observed profile-ML information
// is admissible; the conditional result is immutable and remains usable when
// ARMA curvature or point sensitivities cannot be qualified.
InterpolationArmaInformation interpolation_arma_information(
    const InterpolationInformation& conditional,const MaskedSeries& series,
    const RegressionDesign& design,const ArmaParameterMap& map,
    const std::vector<double>& coordinates,int period,int d,int seasonal_d,
    double mean=0,double rank_tolerance=1e-10,double log_jacobian=0,
    const InformationControl& control={},const Interrupt& interrupt={});
} // namespace persephone
#endif
