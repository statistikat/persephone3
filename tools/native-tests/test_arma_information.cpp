#include "core/arma_information.hpp"
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
    const auto result=arma_information(series,empty,map,detail->coordinates,4,0,0);
    assert(result.information.status=="available");
    const auto phi=result.coefficients[0];
    double A=0,B=0,C=0;
    for(std::size_t i=0;i<12;++i) {
        A+=series.values[i]*series.values[i];
        if(i>0) B+=series.values[i]*series.values[i-1];
        if(i>0 && i<11) C+=series.values[i]*series.values[i];
    }
    const auto q=A-2*B*phi+C*phi*phi,q1=-2*B+2*C*phi,q2=2*C;
    const auto expected=.5*(12*(q2/q-q1*q1/q/q)+2*(1+phi*phi)/std::pow(1-phi*phi,2));
    assert(std::abs((*result.information.hessian)(0,0)-expected)<1e-6);
    assert(std::abs((*result.information.covariance)(0,0)-1/expected)<1e-8);
    assert(result.information.evaluations==7);
    const auto conditional=arma_information(series,empty,map,detail->coordinates,4,0,0,0,1e-10,0,{}, {},
        ArmaInformationMode::ConditionalResidualDF);
    assert(conditional.information.status=="available" && conditional.information.evaluations==8);
    assert(conditional.residual_df==11 && conditional.free_regression==0);
    assert(std::abs((*conditional.information.covariance)(0,0)-12./11/expected)<1e-8);
    const auto physical=evaluate_arma_physical_objective(series,empty,map,detail->objective.parameters.coefficients,4,0,0);
    assert(physical.value==fit.final.value);
    auto boundary=detail->objective.parameters.coefficients; boundary[0][0]=1;
    assert(evaluate_arma_physical_objective(series,empty,map,boundary,4,0,0).status=="inadmissible_parameters");
    blocks[0]={{.3},{1}}; const ArmaParameterMap fixed(blocks);
    bool rejected=false;
    try { evaluate_arma_physical_objective(series,empty,fixed,boundary,4,0,0); }
    catch(const std::invalid_argument&) { rejected=true; }
    assert(rejected);
    blocks[0]={{.3},{0}};blocks[1]={{-.3},{0}};const ArmaParameterMap redundant(blocks);
    const auto common=arma_information(series,empty,redundant,redundant.initial_coordinates(),4,0,0);
    assert(common.information.status=="structural_redundancy" && !common.information.covariance);
    const auto thread=std::this_thread::get_id();int callbacks=0,total_callbacks=0;
    for(const auto mode:{ArmaInformationMode::ProfileML,ArmaInformationMode::ConditionalResidualDF,ArmaInformationMode::TramoResidual}) {
    callbacks=0;
    arma_information(series,empty,map,detail->coordinates,4,0,0,0,1e-10,0,{},[&] {
        assert(std::this_thread::get_id()==thread);++callbacks;
    },mode);
    total_callbacks+=callbacks;
    for(int stop=1;stop<=callbacks;++stop) {
        int count=0;bool interrupted=false;
        try { arma_information(series,empty,map,detail->coordinates,4,0,0,0,1e-10,0,{},[&] {
            if(++count==stop) throw std::runtime_error("cancel ARMA information");
        },mode); } catch(const std::runtime_error& e) { interrupted=std::string(e.what())=="cancel ARMA information"; }
        assert(interrupted);
    }
    }
    const auto gn=arma_information(series,empty,map,detail->coordinates,4,0,0,0,1e-10,0,{}, {},ArmaInformationMode::TramoResidual);
    assert(gn.information.status=="available");
    const auto& r=*gn.residual_center;
    const LegacyCovarianceState state{gn.coefficients,gn.coefficients,{1e-6},.01,
        r.sum_squares*std::exp(r.log_determinant/12),12};
    callbacks=0;
    arma_information(series,empty,map,detail->coordinates,4,0,0,0,1e-10,0,{},[&]{
        assert(std::this_thread::get_id()==thread);++callbacks;
    },ArmaInformationMode::TramoResidual,ArmaCovarianceMode::Legacy,state);
    total_callbacks+=callbacks;
    for(int stop=1;stop<=callbacks;++stop) {
        int at=0;bool cancelled=false;
        try {arma_information(series,empty,map,detail->coordinates,4,0,0,0,1e-10,0,{},[&]{
            if(++at==stop)throw std::runtime_error("cancel legacy information");
        },ArmaInformationMode::TramoResidual,ArmaCovarianceMode::Legacy,state);}
        catch(const std::runtime_error& e){cancelled=std::string(e.what())=="cancel legacy information";}
        assert(cancelled);
    }
    const auto again=arma_information(series,empty,map,detail->coordinates,4,0,0);
    assert(again.information.covariance->values()==result.information.covariance->values());
    std::cout<<"PASS: ARMA information, analytic AR(1), physical/fixed coefficients and "<<total_callbacks<<" cancellation boundaries\n";
}
