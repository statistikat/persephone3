#include "core/arma_residuals.hpp"
#include "core/parallel.hpp"
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>

namespace persephone {
namespace {
std::vector<double> standardized(const std::vector<double>& values, double sd,
    const Interrupt& interrupt) {
    std::vector<double> result(values.size());
    parallel_for(values.size(),1,interrupt,[&](std::size_t i) {
        const auto value=values[i]/sd;
        if(!std::isfinite(value) || (values[i]!=0 && value==0)) {
            throw std::range_error("residual standardization exceeds the finite numeric range");
        }
        result[i]=value;
    });
    return result;
}
} // namespace
ArmaResiduals arma_residuals(const MaskedSeries& series, const RegressionDesign& design,
    const ArmaParameterMap& map, const std::vector<double>& coordinates,
    int period, int d, int seasonal_d, double mean, double rank_tolerance,
    double log_jacobian, ResidualScale scale, const Interrupt& interrupt) {
    ArmaResiduals out;out.variance=std::numeric_limits<double>::quiet_NaN();
    out.free_arma=map.initial_coordinates().size();
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt) return;
        try { interrupt(); }
        catch(...) { interrupted=std::current_exception();throw; }
    };
    out.center=std::make_shared<ArmaObjective>(evaluate_arma_objective(series,design,map,coordinates,
        period,d,seasonal_d,mean,rank_tolerance,log_jacobian,checked_interrupt));
    for(const auto fixed:design.fixed) if(!fixed) ++out.free_regression;
    if(out.center->likelihood) out.effective_count=out.center->likelihood->finite_count;
    const auto estimated=out.free_arma+out.free_regression;
    out.residual_df=out.effective_count>estimated ? out.effective_count-estimated : 0;
    if(out.center->status!="available") return out;
    const auto& fit=*out.center->likelihood;
    out.rows=fit.finite_rows;out.unit_innovations=fit.whitened_residuals;
    const auto denominator=scale==ResidualScale::ML ? out.effective_count : out.residual_df;
    if(!denominator) { out.status="nonpositive_residual_df";return out; }
    out.variance=fit.sum_squared_standardized/static_cast<double>(denominator);
    if(!(out.variance>0) || !std::isfinite(out.variance)) { out.status="residual_scale_range";return out; }
    const auto sd=std::sqrt(out.variance);
    try { out.standardized_innovations=standardized(out.unit_innovations,sd,checked_interrupt); }
    catch(const std::runtime_error&) {
        if(interrupted) std::rethrow_exception(interrupted);
        out.status="standardization_range";return out;
    }
    out.status="available";
    const auto& p=out.center->parameters.coefficients;
    try {
        out.complete=sarima_complete_residuals(series,design,p[0],p[1],p[2],p[3],period,d,seasonal_d,
            mean,rank_tolerance,checked_interrupt);
        if(out.complete.status=="available") {
            out.scaled_complete_residuals=standardized(out.complete.residuals,sd,checked_interrupt);
        }
    } catch(const std::runtime_error& error) {
        if(interrupted) std::rethrow_exception(interrupted);
        out.complete.status="numerical_failure";out.complete_message=error.what();
        out.scaled_complete_residuals.clear();
    }
    return out;
}
} // namespace persephone
