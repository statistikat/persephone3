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
    const RegressionDesign none{};
    const ArmaParameterMap white(std::array<ArmaParameterBlock,4>{});
    const MaskedSeries y{{7,21,3,13,9,0,8,16,11,27,6,18,14,31,12,19},
        {0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0}};
    const auto accurate=arma_residuals(y,none,white,{},4,0,1);
    const auto legacy=legacy_arma_residuals(y,none,accurate,4,0,1);
    assert(legacy.status=="available" && legacy.compressed && legacy.correction_sign==-1);
    assert(std::abs(legacy.preliminary[0]-8.5)<1e-12);
    assert(std::abs(legacy.interpolated[0]+7)<1e-12);
    assert(std::abs(accurate.complete.interpolated[0]-24)<1e-12);
    const std::vector<double> expected{3+std::sqrt(2.),5,3,2,3-std::sqrt(2.),-2,2,3,4,6,1};
    assert(legacy.residuals.size()==expected.size());
    for(std::size_t i=0;i<expected.size();++i)assert(std::abs(legacy.residuals[i]-expected[i])<1e-12);
    assert(std::abs(legacy.sum_squares-130)<1e-12 && legacy.calendar_rows.front()==5 && legacy.calendar_rows.back()==15);
    const auto thread=std::this_thread::get_id();int calls=0;
    legacy_arma_residuals(y,none,accurate,4,0,1,0,1e-10,Transform::none,[&] {
        assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {legacy_arma_residuals(y,none,accurate,4,0,1,0,1e-10,Transform::none,[&] {
            if(++at==stop)throw std::runtime_error("cancel legacy residuals");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel legacy residuals";}
        assert(caught);
    }
    assert(legacy_arma_residuals(y,none,accurate,4,0,1).residuals==legacy.residuals);
    // Exercise the fast AR(1) reporting basis and every interruption point in
    // that branch. A missing stationary observation has a closed Gaussian point.
    std::array<ArmaParameterBlock,4> blocks{};blocks[0]={{.2},{1}};
    const ArmaParameterMap ar1(blocks);
    const auto ar_fit=arma_residuals(y,none,ar1,{},4,0,0);
    const auto ar_legacy=legacy_arma_residuals(y,none,ar_fit,4,0,0);
    assert(ar_legacy.status=="available" && ar_legacy.compressed);
    assert(std::abs(ar_legacy.interpolated[0]-.2*(9+8)/1.04)<1e-12);
    assert(std::abs(ar_legacy.sum_squares-ar_fit.complete.sum_squares)<1e-10);
    int ar_calls=0;
    legacy_arma_residuals(y,none,ar_fit,4,0,0,0,1e-10,Transform::none,[&] {
        assert(std::this_thread::get_id()==thread);++ar_calls;});
    for(int stop=1;stop<=ar_calls;++stop) {
        int at=0;bool caught=false;
        try {legacy_arma_residuals(y,none,ar_fit,4,0,0,0,1e-10,Transform::none,[&] {
            if(++at==stop)throw std::runtime_error("cancel AR(1) reporting");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel AR(1) reporting";}
        assert(caught);
    }
    assert(legacy_arma_residuals(y,none,ar_fit,4,0,0).residuals==ar_legacy.residuals);
    const MaskedSeries stationary{{1,0,3,4,5},{0,1,0,0,0}};
    const RegressionDesign mean{1,std::vector<double>(5,1),{0},{0}};
    const auto fitted=arma_residuals(stationary,mean,white,{},4,0,0);
    const auto ols=legacy_arma_residuals(stationary,mean,fitted,4,0,0);
    assert(ols.status=="available" && !ols.compressed && ols.correction_sign==1);
    assert(std::abs(ols.interpolated[0]-3.25)<1e-12 && std::abs(ols.sum_squares-8.75)<1e-12);
    assert(ols.residuals.size()==5 && std::abs(ols.residuals[1])<1e-12);
    auto log_y=y;log_y.values[4]=680;log_y.values[6]=679;
    const auto log_accurate=arma_residuals(log_y,none,white,{},4,0,1);
    const auto logged=legacy_arma_residuals(log_y,none,log_accurate,4,0,1,0,1e-10,Transform::logarithm);
    const auto log_midpoint=680+std::log1p(std::exp(-1.))-std::log(2.);
    assert(logged.status=="available" && std::abs(logged.preliminary[0]-log_midpoint)<1e-12);
    // Inversion/range qualification is separate; log-space reporting stays
    // finite even when the deliberately erroneous legacy original point does not.
    assert(logged.interpolated[0]>1000);
    MaskedSeries large{std::vector<double>(70000),std::vector<std::uint8_t>(70000,0)};
    for(std::size_t i=0;i<large.values.size();++i)large.values[i]=2+std::sin(1.7*static_cast<double>(i+1));
    large.missing[99]=1;large.values[99]=0;
    const RegressionDesign intercept{1,std::vector<double>(large.values.size(),1),{0},{0}};
    threads("1");const auto large_accurate=arma_residuals(large,intercept,white,{},12,0,0);
    const auto serial=legacy_arma_residuals(large,intercept,large_accurate,12,0,0);
    threads("2");const auto parallel=legacy_arma_residuals(large,intercept,large_accurate,12,0,0);
    assert(serial.status=="available" && parallel.status=="available");
    assert(serial.residuals==parallel.residuals && serial.scaled_residuals==parallel.scaled_residuals);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    std::cout<<"PASS: legacy closed reporting, retained accurate points, log range, serial/parallel and "
        <<calls<<" seasonal / "<<ar_calls<<" AR(1) cancellation boundaries\n";
}
