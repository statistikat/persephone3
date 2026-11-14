#include "core/x11_mean_endpoints.hpp"
#include "core/parallel.hpp"
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <limits>
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
    const auto closed=x11_mean_endpoints({{1,2,3,4,5},{0,0,0,0,0}},4);
    assert(closed.values.values==std::vector<double>({27./16,35./16,3,61./16,69./16}));
    assert(closed.left_mean==1.5 && closed.right_mean==4.5);
    for(const int f:{4,12}) {
        const auto h=static_cast<std::size_t>(f/2);
        for(std::size_t n=0;n<h;++n) {
            bool invalid=false;
            try {x11_mean_endpoints({std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)},f);}
            catch(const std::invalid_argument&){invalid=true;}assert(invalid);
        }
        for(std::size_t n=h;n<=20;++n) {
            MaskedSeries constant{std::vector<double>(n,3.5),std::vector<std::uint8_t>(n,0)};
            const auto filtered=x11_mean_endpoints(constant,f);
            for(std::size_t i=0;i<n;++i)assert(std::abs(filtered.values.values[i]-3.5)<1e-14 && !filtered.values.missing[i]);
            assert(filtered.left_mean==3.5 && filtered.right_mean==3.5);
            constant.missing.assign(n,1);const auto missing=x11_mean_endpoints(constant,f);
            assert(missing.left_missing && missing.right_missing && missing.values.missing==constant.missing);
        }
        MaskedSeries extreme{std::vector<double>(h),std::vector<std::uint8_t>(h,0)};
        for(std::size_t i=0;i<h;++i)extreme.values[i]=std::copysign(std::numeric_limits<double>::max(),i%2==0 ? 1. : -1.);
        const auto balanced=x11_mean_endpoints(extreme,f);
        assert(balanced.left_mean==0 && balanced.right_mean==0);
        for(const auto value:balanced.values.values)assert(std::abs(value)/std::numeric_limits<double>::max()<1e-15);
    }
    MaskedSeries y{std::vector<double>(40000),std::vector<std::uint8_t>(40000,0)};
    for(std::size_t i=0;i<y.values.size();++i)y.values[i]=2+std::sin(static_cast<double>(i)*.7)+static_cast<double>(i)/31;
    y.missing[1]=y.missing[19001]=1;y.values[1]=y.values[19001]=0;
    threads("1");const auto serial=x11_mean_endpoints(y,12);
    threads("2");const auto parallel=x11_mean_endpoints(y,12);
    assert(serial.values.values==parallel.values.values && serial.values.missing==parallel.values.missing);
    assert(serial.left_missing && serial.left_missing==parallel.left_missing && serial.right_mean==parallel.right_mean);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    const auto calling=std::this_thread::get_id();int calls=0;
    x11_mean_endpoints(y,12,[&] {assert(std::this_thread::get_id()==calling);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {x11_mean_endpoints(y,12,[&] {if(++at==stop)throw std::runtime_error("cancel mean endpoints");});}
        catch(const std::runtime_error& e) {caught=std::string(e.what())=="cancel mean endpoints";}
        assert(caught);
    }
    assert(x11_mean_endpoints(y,12).values.values==serial.values.values);
    bool invalid=false;try {x11_mean_endpoints(y,52);}catch(const std::invalid_argument&){invalid=true;}assert(invalid);
    invalid=false;try {x11_mean_endpoints({{1,2},{0}},4);}catch(const std::invalid_argument&){invalid=true;}assert(invalid);
    std::cout<<"PASS: mean endpoint closed/constant/short/mask/range checks, actual OpenMP and "<<calls<<" cancellation boundaries\n";
}
