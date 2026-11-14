#include "core/information.hpp"
#include <cassert>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const ResidualFunction function=[](const std::vector<double>& x) {
        return ResidualSample{"available",{1,x[0]-2,2*(x[1]+1)},10,{}};
    };
    const auto out=residual_information(function,{2,-1});
    assert(out.diagnostics.status=="available" && out.diagnostics.evaluations==13);
    assert(std::abs((*out.diagnostics.covariance)(0,0)-.1)<1e-10);
    assert(std::abs((*out.diagnostics.covariance)(1,1)-.025)<1e-10);
    assert((*out.diagnostics.covariance)(0,1)==0);
    assert(residual_information(function,{0,0}).diagnostics.status=="not_stationary");
    InformationControl budget;budget.max_evaluations=12;
    const auto limited=residual_information(function,{2,-1},budget);
    assert(limited.diagnostics.status=="evaluation_limit" && limited.diagnostics.evaluations==12);
    const auto redundant=residual_information([](const auto& x) {
        return ResidualSample{"available",{1,x[0]+x[1]},10,{}};
    },{0,0});
    assert(!redundant.diagnostics.covariance && redundant.diagnostics.rank==1);
    const auto zero=residual_information([](const auto&) {return ResidualSample{"available",{0,0},10,{}};},{0});
    assert(zero.diagnostics.status=="invalid_residual_scale");
    const auto boundary=residual_information([](const auto& x) {
        return x[0]>0 ? ResidualSample{"unavailable",{},0,{}} : ResidualSample{"available",{1,x[0]},10,{}};
    },{0});
    assert(boundary.diagnostics.status=="derivatives_unresolved");
    bool changed=false;
    try { (void)residual_information([](const auto& x) {
        return ResidualSample{"available",{1,x[0]},x[0]>0 ? 9U : 10U,{}};
    },{0}); } catch(const std::logic_error&) {changed=true;}
    assert(changed);
    const auto thread=std::this_thread::get_id();int count=0;
    residual_information(function,{2,-1},{},[&]{assert(std::this_thread::get_id()==thread);++count;});
    for(int stop=1;stop<=count;++stop) {
        int at=0;bool cancelled=false;
        try {residual_information(function,{2,-1},{},[&]{if(++at==stop)throw std::runtime_error("cancel residual information");});}
        catch(const std::runtime_error& e){cancelled=std::string(e.what())=="cancel residual information";}
        assert(cancelled);
    }
    assert(residual_information(function,{2,-1}).diagnostics.covariance->values()==out.diagnostics.covariance->values());
    std::cout<<"PASS: residual information closed forms, rank/budget/domain gates and "<<count<<" cancellation boundaries\n";
}
