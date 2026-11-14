#include "core/information.hpp"
#include <cassert>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const VectorFunction function=[](const std::vector<double>& x) {
        return VectorSample{"available",{std::sin(x[0])+x[1]*x[1],std::exp(x[0]-x[1]),0}};
    };
    const auto result=vector_jacobian(function,{.3,-.2});
    assert(result.status=="available" && result.evaluations==13);
    assert(std::abs((*result.jacobian)(0,0)-std::cos(.3))<1e-10);
    assert(std::abs((*result.jacobian)(0,1)+.4)<1e-10);
    assert(std::abs((*result.jacobian)(1,0)-std::exp(.5))<1e-10);
    assert(std::abs((*result.jacobian)(1,1)+std::exp(.5))<1e-10);
    assert((*result.jacobian)(2,0)==0 && (*result.error)(2,1)==0);
    const auto zero=vector_jacobian([](const auto& x){return VectorSample{"available",{x[0],0}};},{0});
    assert(zero.status=="available" && std::abs((*zero.jacobian)(0,0)-1)<1e-12);
    InformationControl control;control.max_evaluations=12;
    const auto limited=vector_jacobian(function,{.3,-.2},control);
    assert(limited.status=="evaluation_limit" && limited.evaluations==12);
    const auto boundary=vector_jacobian([](const auto& x) {
        return x[0]>0 ? VectorSample{"outside",{}} : VectorSample{"available",{x[0]}};
    },{0});
    assert(boundary.status=="derivatives_unresolved" && boundary.rejected.at("outside")>0);
    control=InformationControl{};control.difference_refinements=0;
    const auto noisy=vector_jacobian([](const auto& x){return VectorSample{"available",{std::sin(1e7*x[0])}};},{0},control);
    assert(noisy.status=="derivatives_unresolved");
    const auto bad=vector_jacobian([](const auto&){return VectorSample{"available",{INFINITY}};},{0});
    assert(bad.status=="center_unavailable" && bad.center.status=="nonfinite_vector");
    bool changed=false;
    try { vector_jacobian([](const auto& x){return VectorSample{"available",std::vector<double>(x[0]>0 ? 2U : 1U,0)};},{0}); }
    catch(const std::logic_error&) {changed=true;}
    assert(changed);
    const auto thread=std::this_thread::get_id();int calls=0;
    vector_jacobian(function,{.3,-.2},{},[&]{assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {vector_jacobian(function,{.3,-.2},{},[&]{if(++at==stop)throw std::runtime_error("cancel Jacobian");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel Jacobian";}
        assert(caught);
    }
    assert(vector_jacobian(function,{.3,-.2}).jacobian->values()==result.jacobian->values());
    std::cout<<"PASS: vector Jacobian analytic/zero centers, domain, accuracy and budget gates; "<<calls<<" cancellation boundaries\n";
}
