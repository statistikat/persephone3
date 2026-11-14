#include "core/interpolation_information.hpp"
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
InterpolationArmaInformation interpolation_arma_information(
    const InterpolationInformation& conditional,const MaskedSeries& series,
    const RegressionDesign& design,const ArmaParameterMap& map,
    const std::vector<double>& coordinates,int period,int d,int seasonal_d,
    double mean,double rank_tolerance,double log_jacobian,
    const InformationControl& control,const Interrupt& interrupt) {
    control.validate();InterpolationArmaInformation out;out.derivatives.status="not_evaluated";
    const auto m=conditional.fit.complete.status=="available" ? conditional.fit.complete.missing_rows.size() : 0;
    out.standard_errors.assign(m,std::numeric_limits<double>::quiet_NaN());
    if(conditional.status=="no_internal_missing") {
        out.status="no_internal_missing";out.derivatives.status="not_required";return out;
    }
    if(conditional.status!="available")return out;
    if(conditional.variance!=conditional.fit.variance)
        throw std::invalid_argument("ARMA interpolation propagation requires ML conditional scale");
    const auto& fit=conditional.fit;
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
    out.arma=arma_information(series,design,map,coordinates,period,d,seasonal_d,
        mean,rank_tolerance,log_jacobian,control,checked_interrupt);
    out.evaluations=out.arma->information.evaluations;
    if(out.arma->information.status!="available") {out.status="arma_information_unavailable";return out;}
    const auto remaining=control.max_evaluations-out.evaluations;
    if(remaining<=0) {out.status=out.derivatives.status="evaluation_limit";return out;}
    auto differences=control;differences.max_evaluations=remaining;
    auto corrected=fit.center->likelihood->residual_series;
    auto correction_design=design;
    std::fill(correction_design.coefficients.begin(),correction_design.coefficients.end(),0);
    const auto parameters=fit.center->parameters.coefficients;
    try {
        // Remove known mean and the final fitted regression effect before
        // perturbation. The profiled correction includes beta's dependence on
        // theta without subtracting two large fitted point estimates.
        for(std::size_t i=0;i<corrected.values.size();++i) {
            if((i&255U)==0)checked_interrupt();
            if(!corrected.missing[i])corrected.values[i]=finite_prediction(
                static_cast<long double>(corrected.values[i])-mean);
        }
        // A constant (or a periodic level when D>0) is a homogeneous diffuse
        // direction. Removing a fixed observed level changes each prediction
        // by a theta-independent amount, so it leaves its derivative unchanged.
        if(d || seasonal_d) {
            const auto groups=static_cast<std::size_t>(seasonal_d ? period : 1);
            std::vector<long double> sum(groups);std::vector<std::size_t> count(groups);
            for(std::size_t i=0;i<corrected.values.size();++i) {
                if((i&255U)==0)checked_interrupt();
                if(!corrected.missing[i]) {sum[i%groups]+=corrected.values[i];++count[i%groups];}
            }
            for(std::size_t i=0;i<corrected.values.size();++i) {
                if((i&255U)==0)checked_interrupt();
                if(!corrected.missing[i])corrected.values[i]=finite_prediction(
                    static_cast<long double>(corrected.values[i])-sum[i%groups]/static_cast<long double>(count[i%groups]));
            }
        }
        const VectorFunction points=[&](const std::vector<double>& values) {
            auto p=parameters;std::size_t at=0;
            for(std::size_t b=0;b<4;++b)for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j)
                if(!map.blocks()[b].fixed[j])p[b][j]=values.at(at++);
            const auto candidate=evaluate_arma_physical_objective(corrected,correction_design,map,p,
                period,d,seasonal_d,0,rank_tolerance,0,checked_interrupt);
            if(candidate.status!="available")return VectorSample{candidate.status,{}};
            if(candidate.likelihood->finite_count!=fit.effective_count)
                return VectorSample{"prediction_domain_changed",{}};
            CompleteResiduals projection;
            try {
                projection=sarima_complete_residuals(corrected,correction_design,p[0],p[1],p[2],p[3],
                    period,d,seasonal_d,0,rank_tolerance,checked_interrupt);
            } catch(const std::runtime_error&) {
                if(interrupted)std::rethrow_exception(interrupted);
                return VectorSample{"prediction_projection_range",{}};
            }
            if(projection.status!="available")return VectorSample{projection.status,{}};
            if(projection.missing_rows!=fit.complete.missing_rows || projection.finite_count!=fit.effective_count)
                return VectorSample{"prediction_domain_changed",{}};
            std::vector<double> result(m);
            for(std::size_t i=0;i<m;++i) {
                if((i&255U)==0)checked_interrupt();
                result[i]=projection.interpolated[i]/conditional.standard_errors[i];
                if(!std::isfinite(result[i]) || (projection.interpolated[i]!=0 && result[i]==0))
                    return VectorSample{"prediction_sensitivity_range",{}};
            }
            return VectorSample{"available",std::move(result)};
        };
        out.derivatives=vector_jacobian(points,out.arma->coefficients,differences,checked_interrupt);
        out.evaluations+=out.derivatives.evaluations;
        if(out.derivatives.status!="available") {out.status=out.derivatives.status;return out;}
        DenseMatrix sensitivity(m,k),error(m,k);
        parallel_for(m*k,1,checked_interrupt,[&](std::size_t at) {
            const auto i=at%m,j=at/m;
            sensitivity(i,j)=finite_prediction(static_cast<long double>((*out.derivatives.jacobian)(i,j))*conditional.standard_errors[i]);
            error(i,j)=finite_prediction(static_cast<long double>((*out.derivatives.error)(i,j))*conditional.standard_errors[i]);
        });
        out.sensitivity=std::move(sensitivity);out.sensitivity_error=std::move(error);
        const auto factor=cholesky_factor(*out.arma->information.covariance,checked_interrupt);
        DenseMatrix loading(m,k);
        parallel_for(m*k,k,checked_interrupt,[&](std::size_t at) {
            const auto i=at%m,a=at/m;long double value=0;
            for(auto j=a;j<k;++j)value+=static_cast<long double>((*out.derivatives.jacobian)(i,j))*factor.upper(a,j);
            loading(i,a)=finite_prediction(value);
        });
        DenseMatrix increment(m,m),covariance(m,m);
        parallel_for(m,m*k,checked_interrupt,[&](std::size_t j) {
            for(std::size_t i=0;i<=j;++i) {
                long double value=0;bool nonzero=false;
                for(std::size_t a=0;a<k;++a) {
                    value+=static_cast<long double>(loading(i,a))*loading(j,a);
                    nonzero=nonzero || loading(i,a)!=0;
                }
                value*=static_cast<long double>(conditional.standard_errors[i])*conditional.standard_errors[j];
                const auto addition=finite_prediction(value);
                if(i==j && nonzero && addition==0)throw std::underflow_error("ARMA prediction variance underflows");
                increment(i,j)=increment(j,i)=addition;
                covariance(i,j)=covariance(j,i)=finite_prediction(
                    static_cast<long double>((*conditional.covariance)(i,j))+addition);
            }
        });
        std::vector<double> se(m);
        parallel_for(m,1,checked_interrupt,[&](std::size_t i) {
            if(!(covariance(i,i)>0))throw std::range_error("ARMA prediction covariance has an invalid variance");
            se[i]=std::sqrt(covariance(i,i));
        });
        out.covariance=std::move(covariance);out.increment=std::move(increment);
        out.standard_errors=std::move(se);out.status="available";
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
