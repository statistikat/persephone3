#ifndef PERSEPHONE_ARMA_RESIDUALS_HPP
#define PERSEPHONE_ARMA_RESIDUALS_HPP
#include "arma_objective.hpp"
#include <memory>

namespace persephone {
enum class ResidualScale { ML, ResidualDF };
struct ArmaResiduals {
    std::string status = "center_unavailable", complete_message;
    std::shared_ptr<const ArmaObjective> center;
    std::vector<std::size_t> rows;
    std::vector<double> unit_innovations, standardized_innovations, scaled_complete_residuals;
    CompleteResiduals complete;
    std::size_t effective_count = 0, residual_df = 0, free_regression = 0, free_arma = 0;
    double variance = 0;
};
// Fitted finite innovations at a supplied admissible ARMA point, with GLS
// coefficients estimated from the whole selected window. No optimizer or
// information qualification, leverage adjustment or parameter propagation.
// Complete-span residuals/interpolations have a separate availability status.
ArmaResiduals arma_residuals(const MaskedSeries& series, const RegressionDesign& design,
    const ArmaParameterMap& map, const std::vector<double>& coordinates,
    int period, int d, int seasonal_d, double mean=0, double rank_tolerance=1e-10,
    double log_jacobian=0, ResidualScale scale=ResidualScale::ML, const Interrupt& interrupt={});
} // namespace persephone
#endif
