#include "core/arma_information.hpp"
#include <cassert>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const MaskedSeries y{{.2,.6,-.3,.1,-.7,-.3,.1,.7,.6,-.2,-.5,.3},std::vector<std::uint8_t>(12,0)};
    RegressionDesign design{2,std::vector<double>(24,1),{0,0},{0,0}};
    double sx=0,sxx=0,sy=0,sxy=0;
    for(std::size_t i=0;i<12;++i) {
        const auto x=static_cast<double>(i+1);design.values[i+12]=x;
        sx+=x;sxx+=x*x;sy+=y.values[i];sxy+=x*y.values[i];
    }
    const ArmaParameterMap map(std::array<ArmaParameterBlock,4>{});
    auto information=arma_information(y,design,map,{},4,0,0);
    const auto fitted=conditional_regression_information(information);
    assert(fitted.status=="available" && fitted.residual_df==10 && fitted.free_arma==0);
    const auto determinant=12*sxx-sx*sx;
    const auto intercept=(sxx*sy-sx*sxy)/determinant, slope=(12*sxy-sx*sy)/determinant;
    double squares=0;
    for(std::size_t i=0;i<12;++i) squares+=std::pow(y.values[i]-intercept-slope*static_cast<double>(i+1),2);
    assert(std::abs(fitted.coefficients[0]-intercept)<1e-12);
    assert(std::abs(fitted.coefficients[1]-slope)<1e-12);
    assert(std::abs((*fitted.covariance)(0,0)-squares/10*sxx/determinant)<1e-12);
    assert(std::abs((*fitted.covariance)(0,1)+squares/10*sx/determinant)<1e-12);
    assert(std::abs((*fitted.covariance)(1,1)-squares/10*12/determinant)<1e-12);
    // Isolate the public inference record's denominator contract: free ARMA
    // directions count, while the scale parameter is not subtracted again.
    information.coefficients={.1,.2};
    const auto estimated=conditional_regression_information(information);
    assert(estimated.residual_df==8 && estimated.free_arma==2);
    assert(std::abs((*estimated.covariance)(0,0)-squares/8*sxx/determinant)<1e-12);
    information.coefficients.assign(10,.1);
    assert(conditional_regression_information(information).status=="nonpositive_residual_df");
    information.coefficients.clear();information.information.status="not_stationary";
    const auto unavailable=conditional_regression_information(information);
    assert(unavailable.status=="arma_information_unavailable" && !unavailable.covariance);
    assert(unavailable.coefficients==fitted.coefficients);
    information.information.status="no_free_parameters";
    const auto thread=std::this_thread::get_id();int calls=0;
    conditional_regression_information(information,[&]{assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {conditional_regression_information(information,[&]{if(++at==stop)throw std::runtime_error("cancel GLS covariance");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel GLS covariance";}
        assert(caught);
    }
    auto overflow=std::make_shared<ArmaObjective>(*information.estimation_center);
    overflow->likelihood->unscaled_covariance.assign(4,std::numeric_limits<double>::max());
    overflow->likelihood->sum_squared_standardized=100;
    information.estimation_center=overflow;
    const auto range=conditional_regression_information(information);
    assert(range.status=="regression_covariance_range" && !range.covariance);
    overflow->likelihood->unscaled_covariance.assign(4,std::numeric_limits<double>::denorm_min());
    overflow->likelihood->sum_squared_standardized=1e-100;
    const auto underflow=conditional_regression_information(information);
    assert(underflow.status=="regression_covariance_range" && !underflow.covariance);
    std::cout<<"PASS: conditional GLS covariance, independent OLS, estimated-ARMA df, rank/range gates and "<<calls<<" cancellation boundaries\n";
}
