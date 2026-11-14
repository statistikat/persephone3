#include "core/x11_initial.hpp"
#include "core/parallel.hpp"
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <string>
#include <thread>
namespace {
void threads(const char* value) {
#ifdef _WIN32
    assert(_putenv_s("PERSEPHONE_NUM_THREADS",value)==0);
#else
    assert(setenv("PERSEPHONE_NUM_THREADS",value,1)==0);
#endif
}
template<class F>void invalid(F run) {bool caught=false;try{run();}catch(const std::invalid_argument&){caught=true;}assert(caught);}
persephone::MaskedSeries input(std::size_t n) {return {std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};}
void equal(const persephone::MaskedSeries& a,const persephone::MaskedSeries& b) {
    assert(a.values==b.values && a.missing==b.missing);
}
void equal(const persephone::X11InitialAdditive& a,const persephone::X11InitialAdditive& b) {
    equal(a.input,b.input);equal(a.trend,b.trend);equal(a.seasonal_irregular,b.seasonal_irregular);
    equal(a.seasonal.values,b.seasonal.values);equal(a.normalization.values,b.normalization.values);
    equal(a.normalization.normalization,b.normalization.normalization);equal(a.irregular,b.irregular);
    assert(a.normalization.normalization_sources==b.normalization.normalization_sources &&
        a.normalization.output_sources==b.normalization.output_sources && a.normalization.rules==b.normalization.rules);
    assert(a.drop==b.drop && a.first_period==b.first_period);
}
}
int main() {
    using namespace persephone;
    for(const int frequency:{4,12})for(const int years:{3,5,6,20,21})for(const int filter:{0,1,3,5,9,15}) {
        const auto f=static_cast<std::size_t>(frequency),n=f*static_cast<std::size_t>(years)+1;
        auto y=input(n);
        for(std::size_t j=0;j<n;++j)y.values[j]=100+.125*static_cast<double>(j)+(j%f==0 ? 1 : 0)-1./frequency;
        const auto a=x11_initial_additive(y,frequency,2,std::vector<int>(f,filter));
        equal(a.input,y);assert(a.drop==f/2 && a.first_period==2+frequency/2 && a.trend.values.size()==n-f);
        for(std::size_t j=0;j<n-f;++j) {
            assert(std::abs(a.trend.values[j]-(100+.125*static_cast<double>(j+a.drop)))<1e-12);
            assert(std::abs(a.irregular.values[j])<1e-12);
        }
    }
    for(const int frequency:{4,12}) {
        const auto f=static_cast<std::size_t>(frequency);std::vector<int> filters(f,3);
        auto y=input(10*f+3);y.missing[0]=1;const auto a=x11_initial_additive(y,frequency,frequency,filters);
        assert(a.trend.missing[0] && a.seasonal_irregular.missing[0]);
        for(std::size_t j=1;j<a.trend.missing.size();++j)assert(!a.trend.missing[j]);
        for(auto& m:y.missing)m=1;
        const auto b=x11_initial_additive(y,frequency,1,filters);
        for(auto m:b.irregular.missing)assert(m==1);
        invalid([&]{x11_initial_additive(input(3*f-1),frequency,1,filters);});
        invalid([&]{x11_initial_additive(y,frequency,frequency+1,filters);});
        invalid([&]{x11_initial_additive(y,frequency,0,filters);});
        invalid([&]{x11_initial_additive(y,frequency,1,std::vector<int>(f,7));});
        invalid([&]{x11_initial_additive(y,frequency,1,std::vector<int>(f-1,3));});
        y.missing[0]=2;invalid([&]{x11_initial_additive(y,frequency,1,filters);});
    }
    invalid([&]{x11_initial_additive(input(80),2,1,std::vector<int>(2,3));});
    auto constant=input(80);for(auto& v:constant.values)v=std::numeric_limits<double>::max();
    const auto safe_pairs=x11_initial_additive(constant,4,1,std::vector<int>(4,0));
    for(auto v:safe_pairs.trend.values)assert(v==std::numeric_limits<double>::max());
    for(auto v:safe_pairs.irregular.values)assert(v==0);
    auto huge=input(100003);for(auto& v:huge.values)v=std::numeric_limits<double>::max();huge.values[13]=-huge.values[13];
    threads("2");bool range=false;
    try{x11_initial_additive(huge,12,1,std::vector<int>(12,3));}catch(const std::overflow_error&){range=true;}assert(range);
    auto large=input(32779);
    for(std::size_t j=0;j<large.values.size();++j)large.values[j]=10+std::sin(static_cast<double>(j)*.37);
    std::vector<int> filters(12,3);filters[0]=0;filters[5]=15;
    threads("1");const auto a=x11_initial_additive(large,12,11,filters);
    threads("2");equal(a,x11_initial_additive(large,12,11,filters));
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    const auto caller=std::this_thread::get_id();int calls=0;
    x11_initial_additive(large,12,11,filters,[&]{assert(std::this_thread::get_id()==caller);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try{x11_initial_additive(large,12,11,filters,[&]{assert(std::this_thread::get_id()==caller);
            if(++at==stop)throw std::runtime_error("cancel initial tables");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel initial tables";}assert(caught);
    }
    equal(a,x11_initial_additive(large,12,11,filters));
    std::cout<<"PASS: closed additive initial tables, short/long spans, masks/ranges, workers and "<<calls<<" interruption boundaries\n";
}
