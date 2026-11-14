#include "core/forecast_information.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
ForecastInformation forecast_information(const MaskedSeries& series,
    const RegressionDesign& context,std::size_t backcasts,std::size_t forecasts,
    const ArmaParameterMap& map,const std::vector<double>& coordinates,
    int period,int d,int seasonal_d,double mean,double rank_tolerance,
    double log_jacobian,ResidualScale scale,const Interrupt& interrupt) {
    const auto n=series.values.size();
    const auto limit=static_cast<std::size_t>(std::numeric_limits<int>::max());
    if(n>limit || backcasts>limit-n || forecasts>limit-n-backcasts)
        throw std::length_error("prediction domain exceeds the supported range");
    const auto total=n+backcasts+forecasts,m=backcasts+forecasts;
    MaskedSeries expanded{std::vector<double>(total,0),std::vector<std::uint8_t>(total,1)};
    if(series.missing.size()!=n)throw std::invalid_argument("prediction response mask dimensions disagree");
    std::copy(series.values.begin(),series.values.end(),expanded.values.begin()+static_cast<std::ptrdiff_t>(backcasts));
    std::copy(series.missing.begin(),series.missing.end(),expanded.missing.begin()+static_cast<std::ptrdiff_t>(backcasts));
    validate_regression_inputs(expanded,context,mean,1,rank_tolerance,interrupt);
    RegressionDesign estimation{context.columns,std::vector<double>(n*context.columns),context.coefficients,context.fixed};
    parallel_for(context.columns,n,interrupt,[&](std::size_t j) {
        for(std::size_t i=0;i<n;++i)estimation.values[i+n*j]=context.values[backcasts+i+total*j];
    });
    ForecastInformation out;out.variance=std::numeric_limits<double>::quiet_NaN();
    for(std::size_t i=0;i<backcasts;++i)out.rows.push_back(i);
    for(std::size_t i=0;i<forecasts;++i)out.rows.push_back(backcasts+n+i);
    out.points.assign(m,std::numeric_limits<double>::quiet_NaN());
    out.standard_errors=out.points;
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt)return;
        try {interrupt();}catch(...) {interrupted=std::current_exception();throw;}
    };
    out.center=std::make_shared<ArmaObjective>(evaluate_arma_objective(series,estimation,map,
        coordinates,period,d,seasonal_d,mean,rank_tolerance,log_jacobian,checked_interrupt));
    out.free_arma=map.initial_coordinates().size();
    for(const auto fixed:context.fixed)if(!fixed)++out.free_regression;
    if(out.center->likelihood)out.effective_count=out.center->likelihood->finite_count;
    const auto estimated=out.free_arma+out.free_regression;
    out.residual_df=out.effective_count>estimated ? out.effective_count-estimated : 0;
    if(out.center->status!="available")return out;
    if(!m) {
        out.status="no_requested_predictions";out.point_status="available";
        return out;
    }
    const auto& p=out.center->parameters.coefficients;
    CompleteResiduals projection;
    try {
        projection=sarima_complete_residuals(expanded,context,p[0],p[1],p[2],p[3],
            period,d,seasonal_d,mean,rank_tolerance,checked_interrupt,{false,false,true,true});
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.status=out.point_status="projection_numerical_failure";out.message=error.what();return out;
    }
    out.point_status=projection.status;
    if(out.point_status!="available") {out.status="point_unavailable";return out;}
    if(projection.finite_count!=out.effective_count)
        throw std::logic_error("endpoint projection changed the observed finite count");
    std::vector<std::size_t> selected;
    for(const auto row:out.rows) {
        checked_interrupt();
        const auto it=std::lower_bound(projection.missing_rows.begin(),projection.missing_rows.end(),row);
        if(it==projection.missing_rows.end() || *it!=row)throw std::logic_error("prediction row absent from endpoint projection");
        selected.push_back(static_cast<std::size_t>(it-projection.missing_rows.begin()));
    }
    for(std::size_t i=0;i<m;++i)out.points[i]=projection.interpolated[selected[i]];
    const auto denominator=scale==ResidualScale::ML ? out.effective_count : out.residual_df;
    if(!denominator) {out.status="nonpositive_residual_df";return out;}
    out.variance=out.center->likelihood->sum_squared_standardized/static_cast<double>(denominator);
    if(!(out.variance>0) || !std::isfinite(out.variance)) {out.status="variance_range";return out;}
    if(projection.prediction_status!="available") {
        out.status=projection.prediction_status;out.message=projection.prediction_message;return out;
    }
    try {
        const auto scaled=[&](const std::vector<double>& unit) {
            const auto count=projection.missing_rows.size();
            if(unit.size()!=count*count)throw std::logic_error("endpoint covariance dimensions disagree");
            DenseMatrix result(m,m);
            parallel_for(m,m,checked_interrupt,[&](std::size_t j) {
                for(std::size_t i=0;i<m;++i) {
                    const auto original=unit[selected[i]+count*selected[j]];
                    const auto value=static_cast<long double>(original)*out.variance;
                    result(i,j)=static_cast<double>(value);
                    if(!std::isfinite(result(i,j)) || (original!=0 && result(i,j)==0))
                        throw std::range_error("forecast covariance exceeds the finite numeric range");
                }
            });
            return result;
        };
        auto known=scaled(projection.prediction_known_unit);
        auto regression=scaled(projection.prediction_regression_unit);
        auto covariance=scaled(projection.prediction_total_unit);
        std::vector<double> se(m);
        parallel_for(m,1,checked_interrupt,[&](std::size_t i) {
            if(!(covariance(i,i)>0) || !(known(i,i)>0) || regression(i,i)<0)
                throw std::range_error("forecast covariance has an invalid variance");
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
