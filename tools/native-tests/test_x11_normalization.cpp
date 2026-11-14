#include "core/x11_normalization.hpp"
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
}
int main() {
    using namespace persephone;using Rule=X11NormalizationRule;
    for(const int frequency:{4,12}) {
        const auto f=static_cast<std::size_t>(frequency),h=f/2;auto y=input(10*f+3);const auto n=y.values.size();
        for(std::size_t j=0;j<n;++j)y.values[j]=static_cast<double>(j+1);
        std::vector<int> fixed(f,3),stable(f,0),mixed(f,3);mixed[0]=0;
        const auto a=x11_seasonal_normalize(y,frequency,1,fixed);
        const auto b=x11_seasonal_normalize(y,frequency,1,stable);
        const auto c=x11_seasonal_normalize(y,frequency,1,mixed,f+3);
        for(std::size_t j=0;j<n;++j) {
            const auto source=j<h ? h : (j>=n-h ? n-h-1 : j);
            assert(a.normalization_sources[j]==source);
            assert(std::abs(a.normalization.values[j]-y.values[source])<1e-12);
            assert(std::abs(a.values.values[j]-(y.values[j]-y.values[source]))<1e-12);
            const auto periodic=j<h ? j+f : (j>=n-h ? j-f : j);
            assert(b.normalization_sources[j]==periodic && b.rules[j%f]==Rule::requested_stable);
            const auto chosen=j%f==0 ? periodic : source;
            assert(c.normalization_sources[j]==chosen);
        }
        for(std::size_t j=0;j<c.values.values.size();++j)assert(c.values.values[j]==c.values.values[f+3+c.output_sources[j]]);
        auto short_input=input(5*f-1);
        assert(x11_seasonal_normalize(short_input,frequency,1,fixed).rules[0]==Rule::short_span);
        std::vector<int> long_filter(f,15);
        assert(x11_seasonal_normalize(input(19*f),frequency,1,long_filter).rules[0]==Rule::short_3x15_span);
        assert(x11_seasonal_normalize(input(19*f+1),frequency,1,long_filter).rules[0]==Rule::nearest);
        const auto minimum=x11_seasonal_normalize(input(2*f),frequency,frequency,stable,3*f);
        for(const auto source:minimum.normalization_sources)assert(source>=h && source<2*f-h);
        y.values[0]=0;y.missing[0]=1;
        const auto missing=x11_seasonal_normalize(y,frequency,1,fixed,f);
        assert(missing.values.missing[f] && missing.values.missing[0]);
        assert(!missing.values.missing[f+n-1]);
        invalid([&]{x11_seasonal_normalize(input(2*f-1),frequency,1,fixed);});
        invalid([&]{x11_seasonal_normalize(y,frequency,frequency+1,fixed);});
        invalid([&]{x11_seasonal_normalize(y,frequency,1,std::vector<int>(f,7));});
        invalid([&]{x11_seasonal_normalize(y,frequency,1,std::vector<int>(f-1,3));});
        invalid([&]{x11_seasonal_normalize(y,frequency,1,fixed,std::numeric_limits<std::size_t>::max());});
        y.missing[0]=2;invalid([&]{x11_seasonal_normalize(y,frequency,1,fixed);});
    }
    auto huge=input(80);for(auto& value:huge.values)value=std::numeric_limits<double>::max();huge.values[0]=-huge.values[0];
    bool range=false;try{x11_seasonal_normalize(huge,4,1,std::vector<int>(4,3));}catch(const std::overflow_error&){range=true;}assert(range);
    auto large=input(32779);for(std::size_t j=0;j<large.values.size();++j)large.values[j]=10+std::sin(static_cast<double>(j)*.37);
    std::vector<int> filters(12,3);filters[0]=0;filters[5]=15;
    threads("1");const auto a=x11_seasonal_normalize(large,12,11,filters,20000);
    threads("2");const auto b=x11_seasonal_normalize(large,12,11,filters,20000);
    assert(a.values.values==b.values.values && a.normalization.values==b.normalization.values && a.output_sources==b.output_sources);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    const auto caller=std::this_thread::get_id();int calls=0;
    x11_seasonal_normalize(large,12,11,filters,20000,[&]{assert(std::this_thread::get_id()==caller);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try{x11_seasonal_normalize(large,12,11,filters,20000,[&]{if(++at==stop)throw std::runtime_error("cancel normalization");});}
        catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel normalization";}assert(caught);
    }
    assert(x11_seasonal_normalize(large,12,11,filters,20000).values.values==a.values.values);
    std::cout<<"PASS: additive normalization, endpoint policies, restoration, masks/ranges, workers and "<<calls<<" interruption boundaries\n";
}
