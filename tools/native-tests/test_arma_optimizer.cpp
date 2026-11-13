#include "core/arma_optimizer.hpp"

#include <cassert>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const MaskedSeries series{{.2,.6,-.3,.1,-.7,-.3,.1,.7,.6,-.2,-.5,.3},std::vector<std::uint8_t>(12,0)};
    const RegressionDesign empty{0,{},{},{}};
    std::array<ArmaParameterBlock,4> blocks{}; blocks[0]={{0},{0}};
    const ArmaParameterMap map(blocks);
    const auto fit=optimize_arma_objective(series,empty,map,{0},4,0,0);
    assert(fit.converged);
    const auto detail=std::static_pointer_cast<const ArmaOptimizationPoint>(fit.final.detail);
    assert(detail && detail->objective.status=="available" && detail->objective.value==fit.final.value);
    const auto phi=detail->objective.parameters.coefficients[0][0];
    // Closed AR(1) likelihood under stationary initialization, no filtering.
    const auto analytic=[&](double p) {
        double q=(1-p*p)*series.values[0]*series.values[0];
        for(std::size_t i=1;i<series.values.size();++i) q+=std::pow(series.values[i]-p*series.values[i-1],2);
        return .5*(12*(std::log(2*std::acos(-1.0))+1+std::log(q/12))-std::log(1-p*p));
    };
    // Independent golden-section scalar search, separate from BFGS/differences.
    double low=-.95,high=.95;
    const double ratio=(std::sqrt(5.0)-1)/2;
    for(int i=0;i<100;++i) {
        const auto left=high-ratio*(high-low),right=low+ratio*(high-low);
        if(analytic(left)<analytic(right)) high=right; else low=left;
    }
    assert(std::abs(phi-(low+high)/2)<2e-6);
    assert(std::abs(fit.final.value-analytic(phi))<1e-10);
    assert(evaluate_arma_objective(series,empty,map,detail->coordinates,4,0,0).value==fit.final.value);
    OptimizerControl limited; limited.max_evaluations=2;
    const auto limit=optimize_arma_objective(series,empty,map,{0},4,0,0,0,1e-10,0,limited);
    assert(limit.status=="evaluation_limit" && limit.evaluations==2 && !limit.converged);
    const auto extreme=optimize_arma_objective(series,empty,map,{1000},4,0,0);
    assert(extreme.status=="initial_unavailable" && extreme.final.status=="parameter_boundary");
    const auto near=optimize_arma_objective(series,empty,map,{10},4,0,0);
    assert(!near.converged); // Saturating coordinates must not yield a false small gradient.
    const auto thread=std::this_thread::get_id();
    int callbacks=0;
    optimize_arma_objective(series,empty,map,{0},4,0,0,0,1e-10,0,{},[&]{
        assert(std::this_thread::get_id()==thread); ++callbacks;
    });
    for(int stop=1;stop<=callbacks;++stop) {
        int count=0; bool interrupted=false;
        try { optimize_arma_objective(series,empty,map,{0},4,0,0,0,1e-10,0,{},[&]{
            if(++count==stop) throw std::runtime_error("optimizer cancellation");
        }); } catch(const std::runtime_error& e) { interrupted=std::string(e.what())=="optimizer cancellation"; }
        assert(interrupted);
    }
    const auto again=optimize_arma_objective(series,empty,map,{0},4,0,0);
    assert(again.coordinates==fit.coordinates && again.final.value==fit.final.value && again.evaluations==fit.evaluations);
    std::cout<<"PASS: ARMA optimizer, independent AR(1) optimum, limits and "<<callbacks<<" main-thread cancellation boundaries\n";
}
