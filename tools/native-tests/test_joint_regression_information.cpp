#include "core/arma_information.hpp"
#include "core/arma_optimizer.hpp"
#include <cassert>
#include <cmath>
#include <iostream>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const MaskedSeries y{{.2,.6,-.3,.1,-.7,-.3,.1,.7,.6,-.2,-.5,.3},std::vector<std::uint8_t>(12,0)};
    const RegressionDesign design{1,std::vector<double>(12,1),{0},{0}};
    std::array<ArmaParameterBlock,4> blocks{};blocks[0]={{.2},{0}};
    const ArmaParameterMap map(blocks);
    const auto optimized=optimize_arma_objective(y,design,map,map.initial_coordinates(),4,0,0);
    const auto final=std::static_pointer_cast<const ArmaOptimizationPoint>(optimized.final.detail);
    const auto out=joint_regression_information(y,design,map,final->coordinates,4,0,0);
    assert(out.status=="available" && out.derivatives.status=="available");
    const auto phi=out.arma.coefficients[0],n=static_cast<double>(y.values.size());
    double middle=0;for(std::size_t i=1;i+1<y.values.size();++i) middle+=y.values[i];
    const auto ends=y.values.front()+y.values.back(),denominator=2+(n-2)*(1-phi);
    const auto beta=(ends+(1-phi)*middle)/denominator;
    const auto B=((n-2)*ends-2*middle)/(denominator*denominator);
    const auto A=out.ml_variance/((1-phi*phi)+(n-1)*(1-phi)*(1-phi));
    const auto V=(*out.arma.information.covariance)(0,0);
    assert(std::abs(out.coefficients[0]-beta)<1e-12);
    assert(std::abs((*out.sensitivity)(0,0)-B)<1e-9);
    assert(std::abs((*out.conditional_ml_covariance)(0,0)-A)<1e-12);
    assert(std::abs((*out.covariance)(0,0)-(A+B*B*V))<1e-10);
    assert(std::abs((*out.cross_covariance)(0,0)-B*V)<1e-10);
    assert(std::abs((*out.joint_covariance)(1,1)-V)<1e-14);
    // Independent full two-parameter AR(1) Gaussian density, profiling only
    // innovation variance. Its inverse Hessian must equal the joint result.
    const auto density=[&](double b,double p) {
        double Q=(1-p*p)*std::pow(y.values.front()-b,2);
        for(std::size_t i=1;i<y.values.size();++i) Q+=std::pow((y.values[i]-b)-p*(y.values[i-1]-b),2);
        return .5*(n*std::log(Q/n)-std::log(1-p*p));
    };
    const double h=1e-4,f=density(beta,phi);
    const auto Hbb=(density(beta+h,phi)+density(beta-h,phi)-2*f)/(h*h);
    const auto Hpp=(density(beta,phi+h)+density(beta,phi-h)-2*f)/(h*h);
    const auto Hbp=(density(beta+h,phi+h)-density(beta+h,phi-h)-density(beta-h,phi+h)+density(beta-h,phi-h))/(4*h*h);
    const auto determinant=Hbb*Hpp-Hbp*Hbp;
    assert(std::abs((*out.joint_covariance)(0,0)-Hpp/determinant)<1e-7);
    assert(std::abs((*out.joint_covariance)(0,1)+Hbp/determinant)<1e-7);
    assert(std::abs((*out.joint_covariance)(1,1)-Hbb/determinant)<1e-7);
    auto shifted=y;for(auto& value:shifted.values) value+=3;
    const auto known_mean=joint_regression_information(shifted,design,map,final->coordinates,4,0,0,3);
    assert(known_mean.status=="available");
    assert(std::abs((*known_mean.sensitivity)(0,0)-B)<1e-9);
    assert(std::abs((*known_mean.covariance)(0,0)-(*out.covariance)(0,0))<1e-8);
    InformationControl control;control.max_evaluations=out.arma.information.evaluations;
    const auto limited=joint_regression_information(y,design,map,final->coordinates,4,0,0,0,1e-10,0,control);
    assert(limited.status=="evaluation_limit" && !limited.covariance && limited.conditional_ml_covariance);
    assert(limited.evaluations==control.max_evaluations);
    control=InformationControl{};control.weak_tolerance=.99;
    assert(joint_regression_information(y,design,map,final->coordinates,4,0,0,0,1e-10,0,control).status=="weak_joint_covariance");
    const ArmaParameterMap white(std::array<ArmaParameterBlock,4>{});
    const auto ols=joint_regression_information(y,design,white,{},4,0,0);
    assert(ols.status=="available" && ols.derivatives.status=="not_required");
    assert(std::abs((*ols.covariance)(0,0)-ols.ml_variance/n)<1e-12);
    const auto empty=joint_regression_information(y,RegressionDesign{},white,{},4,0,0);
    assert(empty.status=="no_free_parameters" && !empty.joint_covariance);
    const auto thread=std::this_thread::get_id();int calls=0;
    const auto callback=[&]{assert(std::this_thread::get_id()==thread);++calls;};
    joint_regression_information(y,design,map,final->coordinates,4,0,0,0,1e-10,0,{},callback);
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {joint_regression_information(y,design,map,final->coordinates,4,0,0,0,1e-10,0,{},[&]{
            if(++at==stop)throw std::runtime_error("cancel joint inference");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel joint inference";}
        assert(caught);
    }
    assert(joint_regression_information(y,design,map,final->coordinates,4,0,0).joint_covariance->values()==out.joint_covariance->values());
    std::cout<<"PASS: independent AR(1) mean derivative/full Gaussian Hessian, ML/empty/budget/weak gates; "<<calls<<" cancellation boundaries\n";
}
