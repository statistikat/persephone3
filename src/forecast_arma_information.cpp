#include "core/forecast_information.hpp"
#include "core/prediction_arma_information.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <exception>
#include <limits>
#include <stdexcept>
#include <utility>
namespace persephone {
namespace {
double finite_prediction(long double value) {
    const auto result=static_cast<double>(value);
    if(!std::isfinite(result) || (value!=0 && result==0))
        throw std::range_error("ARMA prediction uncertainty exceeds the finite numeric range");
    return result;
}
}
PredictionArmaInformation forecast_arma_information(const ForecastInformation& conditional,
    const MaskedSeries& series,const RegressionDesign& context,std::size_t backcasts,std::size_t forecasts,
    const ArmaParameterMap& map,const std::vector<double>& coordinates,
    int period,int d,int seasonal_d,double mean,double rank_tolerance,double log_jacobian,
    const InformationControl& control,const Interrupt& interrupt) {
    control.validate();PredictionArmaInformation out;out.derivatives.status="not_evaluated";
    const auto m=conditional.rows.size(),n=series.values.size();
    out.standard_errors.assign(m,std::numeric_limits<double>::quiet_NaN());
    if(conditional.status=="no_requested_predictions") {
        out.status="no_requested_predictions";out.derivatives.status="not_required";return out;
    }
    if(conditional.status!="available")return out;
    if(conditional.variance!=conditional.center->likelihood->sum_squared_standardized/static_cast<double>(conditional.effective_count))
        throw std::invalid_argument("ARMA forecast propagation requires ML conditional scale");
    const auto k=map.initial_coordinates().size();
    if(!k) {
        out.covariance=conditional.covariance;out.increment=DenseMatrix(m,m);
        out.standard_errors=conditional.standard_errors;
        out.status="available";out.derivatives.status="not_required";return out;
    }
    std::exception_ptr interrupted;
    const Interrupt checked_interrupt=[&] {
        if(!interrupt)return;
        try {interrupt();}catch(...) {interrupted=std::current_exception();throw;}
    };
    const auto total=n+backcasts+forecasts;
    RegressionDesign design{context.columns,std::vector<double>(n*context.columns),context.coefficients,context.fixed};
    parallel_for(context.columns,n,checked_interrupt,[&](std::size_t j) {
        for(std::size_t i=0;i<n;++i)design.values[i+n*j]=context.values[backcasts+i+total*j];
    });
    out.arma=arma_information(series,design,map,coordinates,period,d,seasonal_d,
        mean,rank_tolerance,log_jacobian,control,checked_interrupt);
    out.evaluations=out.arma->information.evaluations;
    if(out.arma->information.status!="available") {out.status="arma_information_unavailable";return out;}
    const auto remaining=control.max_evaluations-out.evaluations;
    if(remaining<=0) {out.status=out.derivatives.status="evaluation_limit";return out;}
    auto differences=control;differences.max_evaluations=remaining;
    auto corrected=conditional.center->likelihood->residual_series;
    auto correction_design=design,correction_context=context;
    std::fill(correction_design.coefficients.begin(),correction_design.coefficients.end(),0);
    std::fill(correction_context.coefficients.begin(),correction_context.coefficients.end(),0);
    const auto parameters=conditional.center->parameters.coefficients;
    try {
        for(std::size_t i=0;i<n;++i) {
            if((i&255U)==0)checked_interrupt();
            if(!corrected.missing[i])corrected.values[i]=finite_prediction(
                static_cast<long double>(corrected.values[i])-mean);
        }
        if(d || seasonal_d) {
            const auto groups=static_cast<std::size_t>(seasonal_d ? period : 1);
            std::vector<long double> sum(groups);std::vector<std::size_t> count(groups);
            for(std::size_t i=0;i<n;++i) {
                if((i&255U)==0)checked_interrupt();
                if(!corrected.missing[i]) {sum[i%groups]+=corrected.values[i];++count[i%groups];}
            }
            for(std::size_t i=0;i<n;++i) {
                if((i&255U)==0)checked_interrupt();
                if(!corrected.missing[i])corrected.values[i]=finite_prediction(
                    static_cast<long double>(corrected.values[i])-sum[i%groups]/static_cast<long double>(count[i%groups]));
            }
        }
        MaskedSeries expanded{std::vector<double>(total,0),std::vector<std::uint8_t>(total,1)};
        std::copy(corrected.values.begin(),corrected.values.end(),expanded.values.begin()+static_cast<std::ptrdiff_t>(backcasts));
        std::copy(corrected.missing.begin(),corrected.missing.end(),expanded.missing.begin()+static_cast<std::ptrdiff_t>(backcasts));
        std::vector<std::size_t> missing,selected;
        for(std::size_t i=0;i<total;++i) {
            if((i&255U)==0)checked_interrupt();
            if(expanded.missing[i])missing.push_back(i);
        }
        for(const auto row:conditional.rows) {
            const auto it=std::lower_bound(missing.begin(),missing.end(),row);
            if(it==missing.end() || *it!=row)throw std::logic_error("forecast sensitivity rows disagree with conditional points");
            selected.push_back(static_cast<std::size_t>(it-missing.begin()));
        }
        const VectorFunction points=[&](const std::vector<double>& values) {
            auto p=parameters;std::size_t at=0;
            for(std::size_t b=0;b<4;++b)for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j)
                if(!map.blocks()[b].fixed[j])p[b][j]=values.at(at++);
            const auto candidate=evaluate_arma_physical_objective(corrected,correction_design,map,p,
                period,d,seasonal_d,0,rank_tolerance,0,checked_interrupt);
            if(candidate.status!="available")return VectorSample{candidate.status,{}};
            if(candidate.likelihood->finite_count!=conditional.effective_count)
                return VectorSample{"prediction_domain_changed",{}};
            CompleteResiduals projection;
            try {
                projection=sarima_complete_residuals(expanded,correction_context,p[0],p[1],p[2],p[3],
                    period,d,seasonal_d,0,rank_tolerance,checked_interrupt,{false,false,false,true});
            } catch(const std::runtime_error&) {
                if(interrupted)std::rethrow_exception(interrupted);
                return VectorSample{"prediction_projection_range",{}};
            }
            if(projection.status!="available")return VectorSample{projection.status,{}};
            if(projection.missing_rows!=missing || projection.finite_count!=conditional.effective_count)
                return VectorSample{"prediction_domain_changed",{}};
            std::vector<double> result(m);
            for(std::size_t i=0;i<m;++i) {
                if((i&255U)==0)checked_interrupt();
                const auto point=projection.interpolated[selected[i]];
                result[i]=point/conditional.standard_errors[i];
                if(!std::isfinite(result[i]) || (point!=0 && result[i]==0))
                    return VectorSample{"prediction_sensitivity_range",{}};
            }
            return VectorSample{"available",std::move(result)};
        };
        out.derivatives=vector_jacobian(points,out.arma->coefficients,differences,checked_interrupt);
        out.evaluations+=out.derivatives.evaluations;
        if(out.derivatives.status!="available") {out.status=out.derivatives.status;return out;}
        assemble_prediction_delta(out,*conditional.covariance,conditional.standard_errors,checked_interrupt);
    } catch(const std::runtime_error& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.status="prediction_uncertainty_range";out.message=error.what();
    } catch(const std::invalid_argument& error) {
        if(interrupted)std::rethrow_exception(interrupted);
        out.status="prediction_covariance_not_positive_definite";out.message=error.what();
    }
    return out;
}
} // namespace persephone
