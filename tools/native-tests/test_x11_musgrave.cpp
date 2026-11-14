#include "core/x11_musgrave.hpp"
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
template<class F> void invalid(F run) {
    bool caught=false;try {run();}catch(const std::invalid_argument&){caught=true;}assert(caught);
}
}
int main() {
    using namespace persephone;
    for(int length=5;length<=101;length+=2)for(int q=0;q<length/2;++q)
    for(const double ratio:{0.,.001,3.5,100.,std::numeric_limits<double>::max()}) {
        const auto right=x11_musgrave_definition(12,length,q,X11EndpointSide::right,ratio);
        const auto left=x11_musgrave_definition(12,length,q,X11EndpointSide::left,ratio);
        long double sum=0,slope=0;
        for(std::size_t j=0;j<right.weights.size();++j) {
            const auto mirror=right.weights.size()-1-j;
            assert(right.weights[j]==left.weights[mirror]);
            assert(right.offsets[j]==-left.offsets[mirror]);
            sum+=right.weights[j];slope+=right.weights[j]*static_cast<long double>(right.offsets[j]);
        }
        assert(std::abs(sum-1)<2e-15);
        if(ratio==0)assert(std::abs(slope)<1e-13);
    }
    const auto closed=x11_musgrave_definition(4,5,0,X11EndpointSide::right,0);
    assert(std::abs(closed.weights[0]+105./572)<1e-15);
    MaskedSeries y{std::vector<double>(40000),std::vector<std::uint8_t>(40000,0)};
    for(std::size_t i=0;i<y.values.size();++i)y.values[i]=2+std::sin(static_cast<double>(i)*.7)+static_cast<double>(i)/31;
    y.missing[1]=y.missing[19001]=1;y.values[1]=y.values[19001]=0;
    threads("1");const auto serial=x11_musgrave_filter(y,12,101,3.5);
    threads("2");const auto parallel=x11_musgrave_filter(y,12,101,3.5);
    assert(serial.values==parallel.values && serial.missing==parallel.missing);
    if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
    const auto calling=std::this_thread::get_id();int calls=0;
    x11_musgrave_filter(y,12,101,3.5,[&] {assert(std::this_thread::get_id()==calling);++calls;});
    for(int stop=1;stop<=calls;++stop) {
        int at=0;bool caught=false;
        try {x11_musgrave_filter(y,12,101,3.5,[&] {if(++at==stop)throw std::runtime_error("cancel Musgrave");});}
        catch(const std::runtime_error& e) {caught=std::string(e.what())=="cancel Musgrave";}
        assert(caught);
    }
    assert(x11_musgrave_filter(y,12,101,3.5).values==serial.values);
    invalid([] {x11_musgrave_filter({},4,5,1);});
    invalid([] {x11_musgrave_definition(4,3,0,X11EndpointSide::right,1);});
    invalid([] {x11_musgrave_definition(4,13,-1,X11EndpointSide::right,1);});
    invalid([] {x11_musgrave_definition(4,13,6,X11EndpointSide::right,1);});
    invalid([] {x11_musgrave_definition(4,13,0,static_cast<X11EndpointSide>(4),1);});
    invalid([] {x11_musgrave_definition(4,13,0,X11EndpointSide::right,-1);});
    invalid([] {x11_musgrave_definition(4,13,0,X11EndpointSide::right,std::numeric_limits<double>::infinity());});
    invalid([] {x11_musgrave_filter({{1,2,3,4,5},{0}},4,5,1);});
    MaskedSeries line{{1,2,3,4,5},{0,0,0,0,0}};
    const auto linear=x11_musgrave_filter(line,4,5,0);
    for(std::size_t i=0;i<5;++i)assert(std::abs(linear.values[i]-line.values[i])<1e-14 && !linear.missing[i]);
    assert(x11_musgrave_filter(line,4,3,0).values==line.values);
    MaskedSeries extreme{std::vector<double>(13,std::numeric_limits<double>::max()),std::vector<std::uint8_t>(13,0)};
    bool overflow=false;try {x11_musgrave_filter(extreme,12,13,1);}
    catch(const std::overflow_error&){overflow=true;}assert(overflow);
    extreme.missing.assign(13,1);
    assert(x11_musgrave_filter(extreme,12,13,1).missing==extreme.missing);
    std::cout<<"PASS: endpoint moments, mirrors, limits, invalid/range/masks, actual OpenMP and "<<calls<<" cancellation boundaries\n";
}
