#ifndef PERSEPHONE_FORECAST_INFORMATION_HPP
#define PERSEPHONE_FORECAST_INFORMATION_HPP
#include "arma_residuals.hpp"
#include "prediction_arma_information.hpp"
#include <optional>

namespace persephone {
struct ForecastInformation {
    std::string status="center_unavailable",point_status="center_unavailable",message;
    std::shared_ptr<const ArmaObjective> center;
    std::size_t effective_count=0,residual_df=0,free_regression=0,free_arma=0;
    double variance=0;
    // Full extended-domain positions, chronological backcasts then forecasts.
    std::vector<std::size_t> rows;
    std::vector<double> points,standard_errors;
    std::optional<DenseMatrix> covariance,known_regression_covariance,regression_covariance;
};
// `series` is only the estimation window. `context` includes exactly the
// requested backcast + estimation + forecast rows, with finite regressors.
// Unobserved endpoints are integrated, not filled into the fitted likelihood.
ForecastInformation forecast_information(const MaskedSeries& series,
    const RegressionDesign& context,std::size_t backcasts,std::size_t forecasts,
    const ArmaParameterMap& map,const std::vector<double>& coordinates,
    int period,int d,int seasonal_d,double mean=0,double rank_tolerance=1e-10,
    double log_jacobian=0,ResidualScale scale=ResidualScale::ML,const Interrupt& interrupt={});
// Optional first-order propagation through refitted endpoint points. The
// immutable conditional result must describe the same inputs and use ML scale.
PredictionArmaInformation forecast_arma_information(const ForecastInformation& conditional,
    const MaskedSeries& series,const RegressionDesign& context,std::size_t backcasts,std::size_t forecasts,
    const ArmaParameterMap& map,const std::vector<double>& coordinates,
    int period,int d,int seasonal_d,double mean=0,double rank_tolerance=1e-10,
    double log_jacobian=0,const InformationControl& control={},const Interrupt& interrupt={});
} // namespace persephone
#endif
