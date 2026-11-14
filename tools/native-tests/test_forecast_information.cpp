#include "core/forecast_information.hpp"
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
    const MaskedSeries y{{1,3,2,4,5,2,3,5,4,2,3,6,2,4,5,3},std::vector<std::uint8_t>(16,0)};
    const RegressionDesign none{};
    std::array<ArmaParameterBlock,4> blocks{};blocks[0]={{.5},{1}};
    const ArmaParameterMap ar1(blocks),white(std::array<ArmaParameterBlock,4>{});
    const auto ar=forecast_information(y,none,3,4,ar1,{},4,0,0,2);
    assert(ar.status=="available" && ar.rows==std::vector<std::size_t>({0,1,2,19,20,21,22}));
    const std::vector<int> horizons{3,2,1,1,2,3,4};
    for(std::size_t i=0;i<7;++i) {
        const auto h=horizons[i];
        const auto endpoint=i<3 ? y.values.front() : y.values.back();
        assert(std::abs(ar.points[i]-(2+std::pow(.5,h)*(endpoint-2)))<1e-12);
        for(std::size_t j=0;j<7;++j) {
            double target=0;
            if((i<3)==(j<3))for(int k=0;k<std::min(h,horizons[j]);++k)
                target+=std::pow(.5,std::abs(h-horizons[j])+2*k);
            assert(std::abs((*ar.covariance)(i,j)/ar.variance-target)<1e-12);
        }
    }
    RegressionDesign drift{1,std::vector<double>(23),{0},{0}};
    for(std::size_t i=0;i<23;++i)drift.values[i]=static_cast<double>(i)-2;
    const auto rw=forecast_information(y,drift,3,4,white,{},4,1,0);
    assert(rw.status=="available");
    const double beta=2./15.;
    for(std::size_t i=0;i<7;++i) {
        const auto h=(i<3 ? -1 : 1)*horizons[i];
        assert(std::abs(rw.points[i]-((i<3 ? 1 : 3)+h*beta))<1e-11);
        for(std::size_t j=0;j<7;++j) {
            const auto g=(j<3 ? -1 : 1)*horizons[j];
            assert(std::abs((*rw.regression_covariance)(i,j)/rw.variance-h*g/15.)<1e-11);
        }
    }
    const auto thread=std::this_thread::get_id();int calls=0;
    forecast_information(y,drift,3,4,white,{},4,1,0,0,1e-10,0,ResidualScale::ML,[&] {
        assert(std::this_thread::get_id()==thread);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {forecast_information(y,drift,3,4,white,{},4,1,0,0,1e-10,0,ResidualScale::ML,[&] {
            if(++at==stop)throw std::runtime_error("cancel forecast");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel forecast";}
        assert(caught);
    }
    assert(forecast_information(y,drift,3,4,white,{},4,1,0).covariance->values()==rw.covariance->values());
    blocks[0]={{0,0},{0,0}};const ArmaParameterMap free(blocks);
    const auto nodf=forecast_information({{1,2},{0,0}},none,0,2,free,free.initial_coordinates(),4,0,0,0,1e-10,0,ResidualScale::ResidualDF);
    assert(nodf.status=="nonpositive_residual_df" && nodf.point_status=="available");
    assert(!nodf.covariance && std::isnan(nodf.standard_errors[0]));
    const auto empty=forecast_information(y,none,0,0,white,{},4,0,0);
    assert(empty.status=="no_requested_predictions" && empty.point_status=="available" && empty.rows.empty());
    const RegressionDesign intercept{1,std::vector<double>(276,1),{0},{0}};
    threads("1");const auto serial=forecast_information(y,intercept,130,130,white,{},4,0,0);
    threads("2");const auto parallel=forecast_information(y,intercept,130,130,white,{},4,0,0);
    assert(serial.status=="available" && parallel.status=="available");
    assert(serial.points==parallel.points && serial.covariance->values()==parallel.covariance->values());
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    std::cout<<"PASS: closed AR(1)/drift endpoint covariance, retained points without df, 260-point serial/OpenMP and "<<calls<<" cancellation boundaries\n";
}
