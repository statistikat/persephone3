#include "core/arma_information.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
ConditionalRegressionInformation conditional_regression_information(
    const ArmaInformation& result, const Interrupt& interrupt) {
    ConditionalRegressionInformation out;
    out.free_arma=result.coefficients.size();
    out.residual_variance=std::numeric_limits<double>::quiet_NaN();
    if(!result.estimation_center || !result.estimation_center->likelihood) return out;
    const auto& fit=*result.estimation_center->likelihood;
    out.free_columns=fit.free_columns; out.effective_count=fit.finite_count;
    for(const auto j:out.free_columns) out.coefficients.push_back(fit.coefficients.at(j));
    const auto r=out.free_columns.size(),estimated=r+out.free_arma;
    out.residual_df=out.effective_count>estimated ? out.effective_count-estimated : 0;
    if(result.information.status!="available" && result.information.status!="no_free_parameters") return out;
    if(!out.residual_df) { out.status="nonpositive_residual_df"; return out; }
    out.residual_variance=fit.sum_squared_standardized/static_cast<double>(out.residual_df);
    if(!(out.residual_variance>0) || !std::isfinite(out.residual_variance)) {
        out.status="regression_covariance_range"; return out;
    }
    if(!r) { out.status="no_free_coefficients"; return out; }
    if(fit.rank!=r || fit.unscaled_covariance.size()!=r*r) {
        out.status="regression_rank_unavailable"; return out;
    }
    DenseMatrix covariance(r,r);
    const auto scale=static_cast<long double>(fit.sum_squared_standardized)/static_cast<long double>(out.residual_df);
    for(std::size_t j=0;j<r;++j) {
        if(interrupt) interrupt();
        for(std::size_t i=0;i<r;++i) {
            const auto unit=fit.unscaled_covariance[i+r*j];
            const auto value=static_cast<double>(static_cast<long double>(unit)*scale);
            if(!std::isfinite(value) || (unit!=0 && value==0) || (i==j && value<=0)) {
                out.status="regression_covariance_range"; return out;
            }
            covariance(i,j)=value;
        }
    }
    out.covariance=std::move(covariance);out.status="available";
    return out;
}

ArmaInformation arma_information(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean, double rank_tolerance, double log_jacobian,
    const InformationControl& control, const Interrupt& interrupt, ArmaInformationMode mode,
    ArmaCovarianceMode covariance_mode, const std::optional<LegacyCovarianceState>& legacy_state) {
    control.validate();
    validate_arma_objective_inputs(series,design,map,period,d,seasonal_d,mean,rank_tolerance,log_jacobian,interrupt);
    ArmaInformation result;
    result.covariance_mode=covariance_mode;
    const bool legacy=covariance_mode==ArmaCovarianceMode::Legacy;
    if(legacy != legacy_state.has_value() || (legacy && mode!=ArmaInformationMode::TramoResidual)) {
        throw std::invalid_argument("legacy covariance requires TRAMO residual information and an explicit legacy state");
    }
    if(legacy) {
        const auto& s=*legacy_state; const auto k=coordinates.size();
        if(s.final_coefficients.size()!=k || s.working_coefficients.size()!=k || s.difference_steps.size()!=k ||
            !std::isfinite(s.damping) || s.damping<0 || !std::isfinite(s.normalized_sum_squares) || s.normalized_sum_squares<=0 || !s.effective_count) {
            throw std::invalid_argument("invalid legacy covariance state dimensions, damping or scale");
        }
        for(std::size_t j=0;j<k;++j) if(!std::isfinite(s.final_coefficients[j]) || !std::isfinite(s.working_coefficients[j]) ||
            !std::isfinite(s.difference_steps[j]) || s.difference_steps[j]==0 || std::abs(s.difference_steps[j])>.1) {
            throw std::invalid_argument("legacy coefficients and nonzero signed difference steps must be finite");
        }
        result.legacy_state=legacy_state;
    }
    result.structural=arma_identification(map,coordinates,period,1e-10,1e-4,interrupt);
    const auto parameters=map.decode(coordinates);
    if(parameters.status!="admissible") {
        result.information.status="parameters_unavailable"; return result;
    }
    for(std::size_t b=0;b<4;++b) for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j) {
        if(!map.blocks()[b].fixed[j]) result.coefficients.push_back(parameters.coefficients[b][j]);
    }
    if(legacy) for(std::size_t j=0;j<result.coefficients.size();++j) {
        if(std::abs(result.coefficients[j]-legacy_state->final_coefficients[j])>1e-10*(1+std::abs(result.coefficients[j]))) {
            throw std::invalid_argument("legacy covariance state does not match the final physical coefficients");
        }
    }
    auto active_design=design;
    auto differences=control;
    const bool conditional=mode==ArmaInformationMode::ConditionalResidualDF;
    for(const auto fixed:design.fixed) if(!fixed) ++result.free_regression;
    if(mode==ArmaInformationMode::TramoResidual) {
        result.estimation_center=std::make_shared<ArmaObjective>(evaluate_arma_physical_objective(series,design,map,
            parameters.coefficients,period,d,seasonal_d,mean,rank_tolerance,0,interrupt));
        result.information.evaluations=1;
        if(result.estimation_center->status!="available") { result.information.status="center_unavailable"; return result; }
        if(control.max_evaluations==1) { result.information.status="evaluation_limit"; return result; }
        --differences.max_evaluations;
        const auto n=result.estimation_center->likelihood->finite_count;
        const auto estimated=result.free_regression+result.coefficients.size();
        result.residual_df=n>estimated ? n-estimated : 0;
        result.covariance_multiplier=result.residual_df ? static_cast<double>(n)/static_cast<double>(result.residual_df) :
            std::numeric_limits<double>::quiet_NaN();
        const ResidualFunction residuals=[&](const std::vector<double>& values) {
            auto coefficients=parameters.coefficients; std::size_t index=0;
            for(std::size_t b=0;b<4;++b) {
                for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j) if(!map.blocks()[b].fixed[j]) coefficients[b][j]=values.at(index++);
                const auto check=arma_admissibility(coefficients[b],b==1 || b==3,map.margin());
                if(check.status!="admissible") return ResidualSample{check.status,{},0,{}};
            }
            auto detail=std::make_shared<CompleteResiduals>(sarima_complete_residuals(series,design,
                coefficients[0],coefficients[1],coefficients[2],coefficients[3],period,d,seasonal_d,mean,rank_tolerance,interrupt));
            if(detail->status!="available") return ResidualSample{detail->status,{},0,detail};
            if(!result.residual_center) result.residual_center=detail;
            if(detail->finite_count!=n) return ResidualSample{"inconsistent_residual_domain",{},0,detail};
            // A common, parameter-independent scaling leaves GN covariance and
            // score unchanged. Centering log determinants and normalizing by
            // sqrt(Q0) avoids needless overflow and response-unit sensitivity.
            const auto log_scale=(detail->log_determinant-result.residual_center->log_determinant)/(2*static_cast<double>(n));
            const auto scale=std::exp(log_scale)/std::sqrt(result.residual_center->sum_squares);
            if(!(scale>0) || !std::isfinite(scale)) return ResidualSample{"residual_normalization_range",{},0,detail};
            auto z=detail->residuals;
            for(auto& value:z) {
                const auto before=value; value*=scale;
                if(!std::isfinite(value) || (before!=0 && value==0)) return ResidualSample{"residual_normalization_range",{},0,detail};
            }
            return ResidualSample{"available",std::move(z),detail->finite_count,detail};
        };
        result.residual_information=persephone::residual_information(residuals,result.coefficients,differences,interrupt);
        result.information=result.residual_information->diagnostics;
        ++result.information.evaluations;
        if(result.information.status=="available" || result.information.status=="no_free_parameters") {
            if(!result.residual_df) { result.information.status="nonpositive_residual_df"; result.information.covariance.reset(); }
            else if(result.information.covariance) {
                for(auto& value:result.information.covariance->values()) value*=result.covariance_multiplier;
                result.information.covariance->validate(interrupt);
            }
        }
        if(result.structural.status=="rank_deficient_transfer" || result.structural.status=="near_redundant_transfer") {
            result.information.covariance.reset();
            result.information.status=result.structural.status=="rank_deficient_transfer" ? "structural_redundancy" : "structural_near_redundancy";
        }
        if(legacy) {
            result.accurate_covariance=result.information.covariance;
            // A working/damped matrix must never rescue an unqualified final
            // fit, a structurally redundant model or nonpositive residual df.
            if(result.information.status!="available" && result.information.status!="no_free_parameters") return result;
            const auto& state=*legacy_state; const auto& center=*result.residual_center;
            const auto log_q=std::log(center.sum_squares)+center.log_determinant/static_cast<double>(n);
            if(state.effective_count!=n || std::abs(std::log(state.normalized_sum_squares)-log_q)>1e-8) {
                throw std::invalid_argument("legacy covariance state does not match the effective domain or normalized residual scale");
            }
            result.normalized_legacy_damping=state.damping==0 ? 0 : std::exp(std::log(state.damping)-log_q);
            if(!std::isfinite(result.normalized_legacy_damping) || (state.damping>0 && result.normalized_legacy_damping==0)) {
                throw std::overflow_error("legacy damping normalization exceeds the numeric range");
            }
            const auto k=result.coefficients.size();
            if(!k) return result;
            const auto evaluate=[&](const std::vector<double>& point) {
                if(result.information.evaluations>=control.max_evaluations) return ResidualSample{"evaluation_limit",{},0,{}};
                ++result.information.evaluations; return residuals(point);
            };
            result.information.covariance.reset();
            const auto working=evaluate(state.working_coefficients);
            if(working.status!="available") { result.information.status="legacy_"+working.status; return result; }
            DenseMatrix J(working.residuals.size(),k);
            for(std::size_t j=0;j<k;++j) {
                auto point=state.working_coefficients; point[j]+=state.difference_steps[j];
                if(point[j]==state.working_coefficients[j]) { result.information.status="legacy_step_underflow"; return result; }
                const auto next=evaluate(point);
                if(next.status!="available") { result.information.status="legacy_"+next.status; return result; }
                if(next.residuals.size()!=working.residuals.size()) throw std::logic_error("legacy residual domain changed");
                for(std::size_t i=0;i<J.rows();++i) J(i,j)=(next.residuals[i]-working.residuals[i])/state.difference_steps[j];
            }
            J.validate(interrupt); result.legacy_jacobian=J;
            DenseMatrix H(k,k),E(k,k);
            // H=n*(J'J+mu/Qz0 I) because the common residual normalization
            // makes center z'z=1. Final-fit Q/df supplies the covariance scale.
            for(std::size_t j=0;j<k;++j) for(std::size_t i=0;i<=j;++i) {
                long double sum=i==j ? result.normalized_legacy_damping : 0;
                for(std::size_t t=0;t<J.rows();++t) sum+=static_cast<long double>(J(t,i))*J(t,j);
                H(i,j)=H(j,i)=static_cast<double>(static_cast<long double>(n)*sum);
            }
            H.validate(interrupt); result.legacy_information=H;
            ObservedInformation legacy_info; legacy_info.hessian=H; legacy_info.hessian_error=E; legacy_info.stationary=true;
            qualify_information(legacy_info,control,interrupt);
            if(legacy_info.status!="available") { result.information.status="legacy_"+legacy_info.status; return result; }
            result.information.covariance=legacy_info.covariance;
            for(auto& value:result.information.covariance->values()) value*=result.covariance_multiplier;
            result.information.covariance->validate(interrupt);
        }
        return result;
    }
    if(conditional) {
        result.estimation_center=std::make_shared<ArmaObjective>(evaluate_arma_physical_objective(series,design,map,
            parameters.coefficients,period,d,seasonal_d,mean,rank_tolerance,0,interrupt));
        result.information.evaluations=1;
        if(result.estimation_center->status!="available") {
            result.information.status="center_unavailable"; return result;
        }
        if(control.max_evaluations==1) {
            result.information.status="evaluation_limit"; return result;
        }
        --differences.max_evaluations;
        active_design.coefficients=result.estimation_center->likelihood->coefficients;
        std::fill(active_design.fixed.begin(),active_design.fixed.end(),1);
    }
    const ObjectiveFunction objective=[&](const std::vector<double>& values) {
        auto coefficients=parameters.coefficients;
        std::size_t index=0;
        for(std::size_t b=0;b<4;++b) for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j) {
            if(!map.blocks()[b].fixed[j]) coefficients[b][j]=values.at(index++);
        }
        // The observation Jacobian is constant in these coefficients. Excluding
        // it avoids needless cancellation without changing any derivative.
        auto detail=std::make_shared<ArmaObjective>(evaluate_arma_physical_objective(series,active_design,map,
            coefficients,period,d,seasonal_d,mean,rank_tolerance,0,interrupt));
        return ObjectiveSample{detail->value,detail->status,detail};
    };
    result.information=observed_information(objective,result.coefficients,differences,interrupt);
    if(conditional) ++result.information.evaluations;
    else result.estimation_center=std::static_pointer_cast<const ArmaObjective>(result.information.center.detail);
    if(result.estimation_center && result.estimation_center->status=="available") {
        const auto n=result.estimation_center->likelihood->finite_count;
        const auto estimated=result.free_regression+result.coefficients.size();
        result.residual_df=n>estimated ? n-estimated : 0;
        if(conditional) {
            result.covariance_multiplier=result.residual_df ? static_cast<double>(n)/static_cast<double>(result.residual_df) :
                std::numeric_limits<double>::quiet_NaN();
            if(result.information.status=="available" || result.information.status=="no_free_parameters") {
                if(!result.residual_df) {
                    result.information.status="nonpositive_residual_df";
                    result.information.covariance.reset();
                } else if(result.information.covariance) {
                    for(auto& value:result.information.covariance->values()) value*=result.covariance_multiplier;
                    result.information.covariance->validate(interrupt);
                }
            }
        }
    }
    if(result.structural.status=="rank_deficient_transfer" || result.structural.status=="near_redundant_transfer") {
        result.information.covariance.reset();
        result.information.status=result.structural.status=="rank_deficient_transfer" ?
            "structural_redundancy" : "structural_near_redundancy";
    }
    return result;
}
} // namespace persephone
