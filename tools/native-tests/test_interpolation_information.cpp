#include "core/interpolation_information.hpp"
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
    const RegressionDesign none{};
    std::array<ArmaParameterBlock,4> blocks{};blocks[0]={{.5},{1}};
    const ArmaParameterMap ar1(blocks),white(std::array<ArmaParameterBlock,4>{});
    const MaskedSeries y{{0,1,0,3,4,0},{1,0,1,0,0,1}};
    const auto ar=interpolation_information(y,none,ar1,{},4,0,0);
    assert(ar.status=="available" && ar.fit.complete.missing_rows==std::vector<std::size_t>{2});
    assert(std::abs(ar.fit.complete.interpolated[0]-1.6)<1e-12);
    assert(std::abs((*ar.covariance)(0,0)/ar.variance-.8)<1e-12);
    assert((*ar.regression_covariance)(0,0)==0);
    const MaskedSeries yy{{1,0,3,0,4,5},{0,1,0,1,0,0}};
    const RegressionDesign mean{1,std::vector<double>(6,1),{0},{0}};
    const auto ols=interpolation_information(yy,mean,white,{},4,0,0);
    assert(ols.status=="available" && std::abs(ols.variance-8.75/4)<1e-12);
    for(std::size_t i=0;i<2;++i)for(std::size_t j=0;j<2;++j) {
        assert(std::abs((*ols.known_regression_covariance)(i,j)-(i==j ? ols.variance : 0))<1e-12);
        assert(std::abs((*ols.regression_covariance)(i,j)-ols.variance/4)<1e-12);
        assert(std::abs((*ols.covariance)(i,j)-ols.variance*((i==j ? 1 : 0)+.25))<1e-12);
    }
    const auto thread=std::this_thread::get_id();int calls=0;
    interpolation_information(yy,mean,white,{},4,0,0,0,1e-10,0,ResidualScale::ML,[&] {
        assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {interpolation_information(yy,mean,white,{},4,0,0,0,1e-10,0,ResidualScale::ML,[&] {
            if(++at==stop)throw std::runtime_error("cancel interpolation");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel interpolation";}
        assert(caught);
    }
    assert(interpolation_information(yy,mean,white,{},4,0,0).covariance->values()==ols.covariance->values());
    blocks[0]={{0,0},{0,0}};const ArmaParameterMap many_parameters(blocks);
    const auto no_df=interpolation_information({{1,0,2},{0,1,0}},none,many_parameters,
        many_parameters.initial_coordinates(),4,0,0,0,1e-10,0,ResidualScale::ResidualDF);
    assert(no_df.status=="nonpositive_residual_df" && no_df.fit.complete.status=="available");
    assert(!no_df.covariance && std::isnan(no_df.standard_errors[0]));
    const RegressionDesign tiny{1,{0,1e-200,1,0,0,0},{0},{0}};
    const MaskedSeries one_missing{{1,0,0,4,5,6},{0,1,0,0,0,0}};
    const auto underflow=interpolation_information(one_missing,tiny,white,{},4,0,0);
    assert(underflow.status=="numerical_failure" && underflow.fit.complete.status=="available");
    assert(!underflow.covariance && std::isnan(underflow.standard_errors[0]));
    MaskedSeries large{std::vector<double>(600),std::vector<std::uint8_t>(600,0)};
    for(std::size_t i=0;i<600;++i)large.values[i]=3+std::sin(static_cast<double>(i+1)*1.7);
    for(std::size_t i=40;i<560;i+=2){large.missing[i]=1;large.values[i]=0;}
    const RegressionDesign intercept{1,std::vector<double>(600,1),{0},{0}};
    threads("1");const auto serial=interpolation_information(large,intercept,white,{},12,0,0);
    threads("2");const auto parallel=interpolation_information(large,intercept,white,{},12,0,0);
    assert(serial.status=="available" && parallel.status=="available");
    assert(serial.covariance->values()==parallel.covariance->values());
    assert(serial.regression_covariance->values()==parallel.regression_covariance->values());
    assert(serial.standard_errors==parallel.standard_errors);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    std::cout<<"PASS: conditional AR(1)/OLS covariance, retained points without df, serial/OpenMP and "
        <<calls<<" cancellation boundaries\n";
}
