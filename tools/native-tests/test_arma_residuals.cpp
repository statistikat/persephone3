#include "core/arma_residuals.hpp"
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
    const MaskedSeries y{{0,1,0,3,4,0},{1,0,1,0,0,1}};
    const RegressionDesign none{};
    std::array<ArmaParameterBlock,4> blocks{};blocks[0]={{.5},{1}};
    const ArmaParameterMap fixed(blocks);
    const auto result=arma_residuals(y,none,fixed,{},4,0,0);
    assert(result.status=="available" && result.complete.status=="available");
    assert(result.rows==std::vector<std::size_t>({1,3,4}));
    assert(result.complete.rows==std::vector<std::size_t>({1,2,3,4}));
    assert(result.complete.missing_rows==std::vector<std::size_t>({2}));
    assert(std::abs(result.complete.interpolated[0]-1.6)<1e-12);
    const std::vector<double> innovations{std::sqrt(.75),2.75/std::sqrt(1.25),2.5};
    double Q=0;
    for(std::size_t i=0;i<3;++i) {
        assert(std::abs(result.unit_innovations[i]-innovations[i])<1e-12);
        Q+=innovations[i]*innovations[i];
    }
    assert(std::abs(result.variance-Q/3)<1e-12);
    assert(std::abs(result.complete.sum_squares-Q)<1e-12);
    for(std::size_t i=0;i<3;++i) assert(std::abs(result.standardized_innovations[i]-innovations[i]/std::sqrt(Q/3))<1e-12);
    blocks[0].fixed[0]=0;const ArmaParameterMap free(blocks);
    const auto df=arma_residuals(y,none,free,free.initial_coordinates(),4,0,0,0,1e-10,0,ResidualScale::ResidualDF);
    assert(df.status=="available" && df.free_arma==1 && df.residual_df==2);
    assert(std::abs(df.variance-Q/2)<1e-12);
    const ArmaParameterMap white(std::array<ArmaParameterBlock,4>{});
    const MaskedSeries complete{{1,2,4,3},{0,0,0,0}};
    const RegressionDesign mean{1,std::vector<double>(4,1),{0},{0}};
    const auto ols=arma_residuals(complete,mean,white,{},4,0,0,0,1e-10,0,ResidualScale::ResidualDF);
    assert(ols.status=="available" && ols.free_regression==1 && ols.residual_df==3);
    assert(std::abs(ols.variance-5./3)<1e-12);
    const MaskedSeries seasonal{{7,21,3,13,9,0,8,16,11,27,6,18,14,31,12,19},
        {0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0}};
    const auto walk=arma_residuals(seasonal,none,white,{},4,0,1);
    assert(walk.status=="available" && walk.complete.status=="available");
    assert(std::abs(walk.complete.interpolated[0]-24)<1e-12);
    assert(std::abs(walk.complete.sum_squares-130)<1e-12);
    const auto thread=std::this_thread::get_id();int calls=0;
    arma_residuals(y,none,fixed,{},4,0,0,0,1e-10,0,ResidualScale::ML,[&]{
        assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {arma_residuals(y,none,fixed,{},4,0,0,0,1e-10,0,ResidualScale::ML,[&]{
            if(++at==stop)throw std::runtime_error("cancel residuals");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel residuals";}
        assert(caught);
    }
    assert(arma_residuals(y,none,fixed,{},4,0,0).standardized_innovations==result.standardized_innovations);
    MaskedSeries large{std::vector<double>(70000),std::vector<std::uint8_t>(70000,0)};
    for(std::size_t i=0;i<large.values.size();++i)large.values[i]=std::sin(1.7*static_cast<double>(i+1));
    threads("1");const auto serial=arma_residuals(large,none,white,{},12,0,0);
    threads("2");const auto parallel=arma_residuals(large,none,white,{},12,0,0);
    assert(serial.status=="available" && parallel.complete.status=="available");
    assert(serial.standardized_innovations==parallel.standardized_innovations);
    assert(serial.scaled_complete_residuals==parallel.scaled_complete_residuals);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    std::cout<<"PASS: closed AR(1)/seasonal-walk innovations/interpolation, df/OLS, serial/parallel scaling and "<<calls<<" cancellation boundaries\n";
}
