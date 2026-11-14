#ifndef PERSEPHONE_ARMA_RESIDUALS_HPP
#define PERSEPHONE_ARMA_RESIDUALS_HPP
#include "arma_objective.hpp"
#include "transforms.hpp"
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

struct LegacyArmaResiduals {
    std::string status="accurate_unavailable",message,basis;
    std::vector<double> preliminary,interpolated,residuals,scaled_residuals;
    // Calendar labels used by the public reporting contract, not an observation
    // mapping when compressed=true. Accurate observed rows remain separate.
    std::vector<std::size_t> calendar_rows;
    bool compressed=false;
    int correction_sign=1;
    double sum_squares=0;
};
// Explicit comparison only. `accurate` must describe the same inputs; no Java
// object, observed target residual vector or optimizer history is accepted.
LegacyArmaResiduals legacy_arma_residuals(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaResiduals& accurate, int period,
    int d, int seasonal_d, double mean=0, double rank_tolerance=1e-10,
    Transform transformation=Transform::none, const Interrupt& interrupt={});
} // namespace persephone
#endif
