#include "core/x11_filters.hpp"
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
    for(const int frequency:{4,12})for(int length=1;length<=101;length+=2) {
        const auto f=x11_filter_definition(X11FilterKind::henderson,frequency,length);
        assert(f.radius==(length<=3 ? 0 : length/2));
        long double sum=0,first=0,second=0,third=0;
        for(std::size_t i=0;i<f.weights.size();++i) {
            assert(f.weights[i]==f.weights[f.weights.size()-1-i]);
            const auto j=static_cast<long double>(f.offsets[i])/static_cast<long double>(std::max<std::int64_t>(1,f.radius));
            sum+=f.weights[i];first+=f.weights[i]*j;second+=f.weights[i]*j*j;third+=f.weights[i]*j*j*j;
        }
        assert(std::abs(sum-1)<2e-15 && std::abs(first)<2e-15 && std::abs(second)<2e-15 && std::abs(third)<2e-15);
    }
    const auto trend=x11_filter_definition(X11FilterKind::trend,4,0);
    assert(trend.weights==std::vector<double>({.125,.25,.25,.25,.125}));
    const auto seasonal=x11_filter_definition(X11FilterKind::seasonal,12,5);
    assert(seasonal.offsets==std::vector<std::int64_t>({-36,-24,-12,0,12,24,36}));
    MaskedSeries y{std::vector<double>(40000),std::vector<std::uint8_t>(40000,0)};
    for(std::size_t i=0;i<y.values.size();++i)y.values[i]=2+std::sin(static_cast<double>(i)*.7)+static_cast<double>(i)/31;
    y.missing[103]=y.missing[19001]=1;y.values[103]=y.values[19001]=0;
    threads("1");const auto serial=x11_symmetric_filter(y,X11FilterKind::henderson,12,101);
    threads("2");const auto parallel=x11_symmetric_filter(y,X11FilterKind::henderson,12,101);
    assert(serial.values==parallel.values && serial.missing==parallel.missing);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    const auto calling=std::this_thread::get_id();int calls=0;
    x11_symmetric_filter(y,X11FilterKind::henderson,12,101,[&] {assert(std::this_thread::get_id()==calling);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {x11_symmetric_filter(y,X11FilterKind::henderson,12,101,[&] {if(++at==stop)throw std::runtime_error("cancel X11");});}
        catch(const std::runtime_error& e) {caught=std::string(e.what())=="cancel X11";}
        assert(caught);
    }
    assert(x11_symmetric_filter(y,X11FilterKind::henderson,12,101).values==serial.values);
    MaskedSeries short_series{{1,2,3,4},{0,0,0,0}};
    const auto no_support=x11_symmetric_filter(short_series,X11FilterKind::seasonal,12,15);
    assert(no_support.missing==std::vector<std::uint8_t>({1,1,1,1}));
    assert(x11_symmetric_filter(short_series,X11FilterKind::henderson,4,3).values==short_series.values);
    assert(x11_symmetric_filter({},X11FilterKind::trend,4,0).values.empty());
    bool invalid=false;
    try {x11_filter_definition(X11FilterKind::henderson,12,2);}catch(const std::invalid_argument&){invalid=true;}
    assert(invalid);invalid=false;
    try {x11_filter_definition(X11FilterKind::seasonal,12,7);}catch(const std::invalid_argument&){invalid=true;}
    assert(invalid);invalid=false;
    try {x11_filter_definition(X11FilterKind::trend,52,0);}catch(const std::invalid_argument&){invalid=true;}
    assert(invalid);
    const auto f=x11_filter_definition(X11FilterKind::henderson,12,13);
    MaskedSeries extreme{std::vector<double>(13),std::vector<std::uint8_t>(13,0)};
    for(std::size_t i=0;i<13;++i)extreme.values[i]=f.weights[i]==0 ? 0 : std::copysign(std::numeric_limits<double>::max(),f.weights[i]);
    bool overflow=false;try {x11_symmetric_filter(extreme,X11FilterKind::henderson,12,13);}
    catch(const std::overflow_error&) {overflow=true;}assert(overflow);
    std::cout<<"PASS: symmetric filter identities, invalid/range/missing support, actual OpenMP and "<<calls<<" cancellation boundaries\n";
}
