#include "core/arma_optimizer.hpp"

#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
OptimizationResult optimize_arma_objective(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& start, int period, int d, int seasonal_d,
    double mean, double rank_tolerance, double log_jacobian,
    const OptimizerControl& control, const Interrupt& interrupt) {
    control.validate();
    validate_arma_objective_inputs(series,design,map,period,d,seasonal_d,mean,rank_tolerance,log_jacobian,interrupt);
    if(start.size()!=map.initial_coordinates().size()) throw std::invalid_argument("optimizer start must match the ARMA free parameter count");
    std::vector<bool> reflection;
    for(std::size_t b=0;b<4;++b) for(const auto flag:map.blocks()[b].fixed) {
        if(!flag) reflection.push_back(map.modes()[b]=="reflection");
    }
    auto working=start;
    for(std::size_t j=0;j<working.size();++j) {
        if(!std::isfinite(start[j])) throw std::invalid_argument("optimizer start coordinates must be finite");
        if(reflection[j]) working[j]=std::tanh(start[j]);
    }
    bool first=true;
    const ObjectiveFunction function=[&](const std::vector<double>& candidate) {
        auto coordinates=candidate;
        if(first) { coordinates=start; first=false; }
        else for(std::size_t j=0;j<candidate.size();++j) {
            if(reflection[j]) {
                if(!std::isfinite(candidate[j]) || std::abs(candidate[j])>=1) {
                    return ObjectiveSample{std::numeric_limits<double>::quiet_NaN(),"parameter_boundary",{}};
                }
                coordinates[j]=std::atanh(candidate[j]);
            }
        }
        auto detail=std::make_shared<ArmaOptimizationPoint>();
        detail->coordinates=std::move(coordinates);
        detail->objective=evaluate_arma_objective(series,design,map,detail->coordinates,
            period,d,seasonal_d,mean,rank_tolerance,log_jacobian,interrupt);
        return ObjectiveSample{detail->objective.value,detail->objective.status,detail};
    };
    return minimize_bfgs(function,working,control,interrupt);
}
} // namespace persephone
