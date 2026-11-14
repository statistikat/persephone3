#ifndef PERSEPHONE_PREDICTION_MOMENTS_HPP
#define PERSEPHONE_PREDICTION_MOMENTS_HPP
#include "linear_algebra.hpp"
#include "transforms.hpp"
#include <optional>
#include <string>

namespace persephone {
struct PredictionMoments {
    std::string median_status="not_evaluated",mean_status="not_evaluated",covariance_status="not_evaluated",standard_error_status="not_evaluated";
    std::string median_message,mean_message,covariance_message,standard_error_message;
    std::vector<double> medians,means,standard_errors;
    std::optional<DenseMatrix> covariance;
};
// Moments of the Gaussian (identity) or lognormal predictive approximation.
// Input covariance must be positive definite; it is not reestimated or repaired.
// Separate range statuses preserve representable quantities when another fails.
PredictionMoments prediction_moments(const std::vector<double>& location,
    const DenseMatrix& covariance,Transform transform,const Interrupt& interrupt={});
} // namespace persephone
#endif
