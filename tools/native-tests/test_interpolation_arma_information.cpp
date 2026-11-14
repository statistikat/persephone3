#include "core/interpolation_information.hpp"
#include "core/arma_optimizer.hpp"
#include "core/parallel.hpp"
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <thread>

namespace {
void threads(const char* value) {
#ifdef _WIN32
    assert(_putenv_s("PERSEPHONE_NUM_THREADS",value)==0);
#else
    assert(setenv("PERSEPHONE_NUM_THREADS",value,1)==0);
#endif
}
}
int main() {
    using namespace persephone;
    std::array<ArmaParameterBlock,4> blocks{};blocks[0]={{0},{0}};
    const ArmaParameterMap map(blocks);const RegressionDesign none{};
    MaskedSeries y{std::vector<double>(80),std::vector<std::uint8_t>(80,0)};
    for(std::size_t i=0;i<80;++i)y.values[i]=std::sin(static_cast<double>(i+1)*1.7)+.3*std::cos(static_cast<double>(i+1)*.6);
    y.missing[30]=1;y.values[30]=0;
    const auto optimized=optimize_arma_objective(y,none,map,map.initial_coordinates(),4,0,0);
    assert(optimized.converged);
    const auto point=std::static_pointer_cast<const ArmaOptimizationPoint>(optimized.final.detail);
    const auto conditional=interpolation_information(y,none,map,point->coordinates,4,0,0);
    const auto result=interpolation_arma_information(conditional,y,none,map,point->coordinates,4,0,0);
    assert(result.status=="available" && result.arma && result.derivatives.status=="available");
    const auto phi=result.arma->coefficients[0];
    const auto expected=(1-phi*phi)/std::pow(1+phi*phi,2)*(y.values[29]+y.values[31]);
    assert(std::abs((*result.sensitivity)(0,0)-expected)<2e-7);
    assert((*result.covariance)(0,0)>=(*conditional.covariance)(0,0));
    assert(std::abs((*result.increment)(0,0)-expected*expected*(*result.arma->information.covariance)(0,0))<1e-9);
    InformationControl budget;budget.max_evaluations=result.arma->information.evaluations;
    const auto limited=interpolation_arma_information(conditional,y,none,map,point->coordinates,4,0,0,0,1e-10,0,budget);
    assert(limited.status=="evaluation_limit" && !limited.covariance && std::isnan(limited.standard_errors[0]));
    assert(conditional.status=="available" && conditional.covariance);
    const auto thread=std::this_thread::get_id();int calls=0;
    interpolation_arma_information(conditional,y,none,map,point->coordinates,4,0,0,0,1e-10,0,{},[&] {
        assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {interpolation_arma_information(conditional,y,none,map,point->coordinates,4,0,0,0,1e-10,0,{},[&] {
            if(++at==stop)throw std::runtime_error("cancel ARMA interpolation");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel ARMA interpolation";}
        assert(caught);
    }
    assert(interpolation_arma_information(conditional,y,none,map,point->coordinates,4,0,0).covariance->values()==result.covariance->values());
    MaskedSeries large{std::vector<double>(600),std::vector<std::uint8_t>(600,0)};
    for(std::size_t i=0;i<600;++i) {
        const auto t=i+1;
        if(t%2==0)large.values[i]=(t/2%2 ? -1. : 1.)*(1+.1*static_cast<double>(t%7));
    }
    for(std::size_t i=40;i<560;i+=2){large.missing[i]=1;large.values[i]=0;}
    threads("1");const auto base=interpolation_information(large,none,map,map.initial_coordinates(),12,0,0);
    const auto serial=interpolation_arma_information(base,large,none,map,map.initial_coordinates(),12,0,0);
    threads("2");const auto parallel=interpolation_arma_information(base,large,none,map,map.initial_coordinates(),12,0,0);
    assert(serial.status=="available" && parallel.status=="available");
    assert(serial.covariance->values()==parallel.covariance->values());
    assert(serial.increment->values()==parallel.increment->values());
    for(std::size_t i=0;i<260;++i) {
        const auto row=base.fit.complete.missing_rows[i];
        assert(std::abs((*serial.sensitivity)(i,0)-large.values[row-1]-large.values[row+1])<2e-7);
    }
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    std::cout<<"PASS: closed AR(1) delta covariance, budget/conditional retention, 260-point serial/OpenMP and "
        <<calls<<" cancellation boundaries\n";
}
