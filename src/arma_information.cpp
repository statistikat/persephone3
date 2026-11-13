#include "core/arma_information.hpp"
#include <algorithm>
#include <limits>

namespace persephone {
ArmaInformation arma_information(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean, double rank_tolerance, double log_jacobian,
    const InformationControl& control, const Interrupt& interrupt, ArmaInformationMode mode) {
    control.validate();
    validate_arma_objective_inputs(series,design,map,period,d,seasonal_d,mean,rank_tolerance,log_jacobian,interrupt);
    ArmaInformation result;
    result.structural=arma_identification(map,coordinates,period,1e-10,1e-4,interrupt);
    const auto parameters=map.decode(coordinates);
    if(parameters.status!="admissible") {
        result.information.status="parameters_unavailable"; return result;
    }
    for(std::size_t b=0;b<4;++b) for(std::size_t j=0;j<map.blocks()[b].fixed.size();++j) {
        if(!map.blocks()[b].fixed[j]) result.coefficients.push_back(parameters.coefficients[b][j]);
    }
    auto active_design=design;
    auto differences=control;
    const bool conditional=mode==ArmaInformationMode::ConditionalResidualDF;
    for(const auto fixed:design.fixed) if(!fixed) ++result.free_regression;
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
