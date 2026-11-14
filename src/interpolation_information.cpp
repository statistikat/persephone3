#include "core/interpolation_information.hpp"
#include "core/parallel.hpp"
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
InterpolationInformation interpolation_information(const MaskedSeries& series,
    const RegressionDesign& design,const ArmaParameterMap& map,
    const std::vector<double>& coordinates,int period,int d,int seasonal_d,
    double mean,double rank_tolerance,double log_jacobian,
    ResidualScale scale,const Interrupt& interrupt) {
    InterpolationInformation out;out.variance=std::numeric_limits<double>::quiet_NaN();
    // Obtain points even if the optional residual-df scale has no denominator.
    out.fit=arma_residuals(series,design,map,coordinates,period,d,seasonal_d,
        mean,rank_tolerance,log_jacobian,ResidualScale::ML,interrupt,{false,false,true});
    const auto& complete=out.fit.complete;
    if(complete.status!="available")return out;
    if(complete.missing_rows.empty()) {out.status="no_internal_missing";return out;}
    const auto n=complete.missing_rows.size();
    out.standard_errors.assign(n,std::numeric_limits<double>::quiet_NaN());
    const auto denominator=scale==ResidualScale::ML ? out.fit.effective_count : out.fit.residual_df;
    if(!denominator) {out.status="nonpositive_residual_df";return out;}
    out.variance=out.fit.center->likelihood->sum_squared_standardized/static_cast<double>(denominator);
    if(!(out.variance>0) || !std::isfinite(out.variance)) {out.status="variance_range";return out;}
    if(complete.prediction_status!="available") {
        out.status=complete.prediction_status;out.message=complete.prediction_message;return out;
    }
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt)return;
        try {interrupt();}catch(...) {interrupted=std::current_exception();throw;}
    };
    try {
        const auto scaled=[&](const std::vector<double>& unit) {
            if(unit.size()!=n*n)throw std::logic_error("prediction covariance dimensions disagree");
            DenseMatrix result(n,n);
            parallel_for(unit.size(),1,checked_interrupt,[&](std::size_t i) {
                const auto value=static_cast<long double>(unit[i])*out.variance;
                result.values()[i]=static_cast<double>(value);
                if(!std::isfinite(result.values()[i]) || (unit[i]!=0 && result.values()[i]==0))
                    throw std::range_error("prediction covariance exceeds the finite numeric range");
            });
            return result;
        };
        auto known=scaled(complete.prediction_known_unit);
        auto regression=scaled(complete.prediction_regression_unit);
        auto covariance=scaled(complete.prediction_total_unit);
        std::vector<double> se(n);
        parallel_for(n,1,checked_interrupt,[&](std::size_t i) {
            if(!(covariance(i,i)>0) || !(known(i,i)>0) || regression(i,i)<0)
                throw std::range_error("prediction covariance has an invalid variance");
            se[i]=std::sqrt(covariance(i,i));
        });
        out.known_regression_covariance=std::move(known);out.regression_covariance=std::move(regression);
        out.covariance=std::move(covariance);out.standard_errors=std::move(se);out.status="available";
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.status="covariance_range";out.message=error.what();
    }
    return out;
}
} // namespace persephone
