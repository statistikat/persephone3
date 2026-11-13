#include "core/information.hpp"
#include <cassert>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const ObjectiveFunction gaussian=[](const std::vector<double>& x) {
        const auto a=x[0]-2,b=x[1]+1;
        return ObjectiveSample{10+.5*(4*a*a+2*a*b+2*b*b),"available",{}};
    };
    const auto result=observed_information(gaussian,{2,-1});
    assert(result.status=="available" && result.stationary && result.evaluations==25);
    assert(std::abs((*result.hessian)(0,0)-4)<1e-7);
    assert(std::abs((*result.hessian)(0,1)-1)<1e-7);
    assert(std::abs((*result.hessian)(1,1)-2)<1e-7);
    assert(std::abs((*result.covariance)(0,0)-2./7)<1e-7);
    assert(std::abs((*result.covariance)(0,1)+1./7)<1e-7);
    assert(observed_information(gaussian,{0,0}).status=="not_stationary");
    const auto scalar=[](double curvature) {
        return [curvature](const std::vector<double>& x) {
            return ObjectiveSample{.5*curvature*x[0]*x[0],"available",{}};
        };
    };
    assert(observed_information(scalar(-1),{0}).status=="indefinite_information");
    assert(observed_information(scalar(0),{0}).status=="rank_deficient_information");
    const ObjectiveFunction ridge=[](const std::vector<double>& x) {
        return ObjectiveSample{.5*(x[0]*x[0]+2*(1-1e-7)*x[0]*x[1]+x[1]*x[1]),"available",{}};
    };
    assert(observed_information(ridge,{0,0}).status=="weak_information");
    const auto empty=observed_information([](const std::vector<double>&) { return ObjectiveSample{0,"available",{}}; },{});
    assert(empty.status=="no_free_parameters" && empty.evaluations==1);
    const auto rejected=observed_information([](const std::vector<double>&) { return ObjectiveSample{0,"bad",{}}; },{0});
    assert(rejected.status=="center_unavailable" && rejected.rejected.at("bad")==1);
    const auto overflow=observed_information([](const std::vector<double>& x) {
        return ObjectiveSample{x[0]==0 ? 1e308 : -1e308,"available",{}};
    },{0});
    assert(overflow.status=="derivatives_unresolved" && !overflow.covariance);
    const auto isolated=observed_information([](const std::vector<double>& x) {
        return ObjectiveSample{0,x[0]==0 ? "available" : "boundary",{}};
    },{0});
    assert(isolated.status=="derivatives_unresolved" && !isolated.covariance);
    InformationControl limited; limited.max_evaluations=2;
    const auto exhausted=observed_information(gaussian,{2,-1},limited);
    assert(exhausted.status=="evaluation_limit" && exhausted.evaluations==2 && !exhausted.covariance);
    // The coordinate Hessian transforms with D'H D at a stationary point.
    const auto transformed=observed_information([&](const std::vector<double>& x) {
        return gaussian({2+3*x[0]+x[1],-1+2*x[1]});
    },{0,0});
    assert(transformed.status=="available");
    assert(std::abs((*transformed.hessian)(0,0)-36)<1e-6);
    assert(std::abs((*transformed.hessian)(0,1)-18)<1e-6);
    assert(std::abs((*transformed.hessian)(1,1)-16)<1e-6);
    DenseMatrix indefinite(2,2,{1,2,2,1});
    const auto values=symmetric_eigenvalues(indefinite);
    assert(std::abs(values[0]+1)<1e-14 && std::abs(values[1]-3)<1e-14);
    bool invalid=false;
    try { symmetric_eigenvalues(DenseMatrix(2,2,{1,2,3,1})); } catch(const std::invalid_argument&) { invalid=true; }
    assert(invalid);
    const auto thread=std::this_thread::get_id(); int callbacks=0;
    observed_information(gaussian,{2,-1},{},[&] { assert(std::this_thread::get_id()==thread); ++callbacks; });
    for(int stop=1;stop<=callbacks;++stop) {
        int count=0;bool interrupted=false;
        try { observed_information(gaussian,{2,-1},{},[&] {
            if(++count==stop) throw std::runtime_error("cancel information");
        }); } catch(const std::runtime_error& e) { interrupted=std::string(e.what())=="cancel information"; }
        assert(interrupted);
    }
    const auto again=observed_information(gaussian,{2,-1});
    assert(again.covariance->values()==result.covariance->values());
    std::cout<<"PASS: independent Gaussian curvature, coordinate transform, rank/weak/indefinite gates and "<<callbacks<<" cancellation boundaries\n";
}
