#include "core/x11_seasonal_processing.hpp"
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
template<class F> void invalid(F run) {
    bool caught=false;try{run();}catch(const std::invalid_argument&){caught=true;}assert(caught);
}
persephone::MaskedSeries input(std::size_t n) {
    persephone::MaskedSeries y{std::vector<double>(n),std::vector<std::uint8_t>(n,0)};
    for(std::size_t i=0;i<n;++i)y.values[i]=2+std::sin(static_cast<double>(i)*.7)+static_cast<double>(i)/31;
    return y;
}
}
int main() {
    using namespace persephone;using Policy=X11ShortSeriesPolicy;using Method=X11SeasonalRowMethod;
    for(const int f:{4,12})for(const int N:{1,3,5,9,15}) {
        auto y=input(static_cast<std::size_t>(20*f+1));y.values[1]=0;y.missing[1]=1;
        const std::vector<int> choices(static_cast<std::size_t>(f),N);
        const auto original=x11_seasonal_endpoints(y,f,N);
        const auto selected=x11_seasonal_process(y,f,3,choices,Policy::reject);
        assert(selected.values.values==original.values && selected.values.missing==original.missing);
        assert(selected.phase_lengths[2]==21 && selected.selected_filters==choices);
        const auto automatic=x11_seasonal_process(y,f,3,choices,Policy::x11);
        assert(automatic.values.values==original.values && automatic.values.missing==original.missing);
    }
    auto five=input(20);for(std::size_t i=0;i<20;++i)five.values[i]=static_cast<double>(i+1);
    const auto middle=x11_seasonal_process(five,4,1,{5,5,5,5},Policy::x11);
    for(std::size_t row=8;row<12;++row) {
        assert(middle.row_methods[row]==Method::short_mean && middle.available[row]==-1);
        assert(std::abs(middle.values.values[row]-five.values[row])<1e-13);
    }
    const auto partial=x11_seasonal_process(input(21),4,3,{9,9,9,9},Policy::x11);
    assert(partial.selected_filters==std::vector<int>({0,0,9,0}));
    assert(partial.row_methods[0]==Method::left && partial.row_methods[20]==Method::right);
    invalid([&]{x11_seasonal_process(five,4,1,{5,5,5,5},Policy::reject);});
    auto extreme=input(16);
    for(std::size_t i=0;i<16;++i)extreme.values[i]=(i/4)%2 ? -std::numeric_limits<double>::max() : std::numeric_limits<double>::max();
    const auto balanced=x11_seasonal_process(extreme,4,1,{0,0,0,0},Policy::x11);
    for(const double value:balanced.values.values)assert(value==0);
    for(auto& value:extreme.values)value=std::numeric_limits<double>::max();
    const auto large=x11_seasonal_process(extreme,4,1,{0,0,0,0},Policy::x11);
    for(const double value:large.values.values)assert(value==std::numeric_limits<double>::max());
    auto y=input(100003);y.values[1]=0;y.missing[1]=1;
    const auto calling=std::this_thread::get_id();int total=0;
    for(const std::vector<int>& choices:{std::vector<int>(12,0),std::vector<int>{1,3,5,9,15,0,1,3,5,9,15,0}}) {
        threads("1");const auto serial=x11_seasonal_process(y,12,3,choices,Policy::x11);
        threads("2");const auto parallel=x11_seasonal_process(y,12,3,choices,Policy::x11);
        assert(serial.values.values==parallel.values.values && serial.values.missing==parallel.values.missing);
        assert(serial.row_methods==parallel.row_methods && serial.available==parallel.available);
        if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
        int calls=0;
        x11_seasonal_process(y,12,3,choices,Policy::x11,[&]{assert(std::this_thread::get_id()==calling);++calls;});
        for(int stop=1;stop<=calls;++stop) {
            int at=0;bool caught=false;
            try{x11_seasonal_process(y,12,3,choices,Policy::x11,[&]{if(++at==stop)throw std::runtime_error("cancel seasonal processing");});}
            catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel seasonal processing";}
            assert(caught);
        }
        assert(x11_seasonal_process(y,12,3,choices,Policy::x11).values.values==serial.values.values);
        total+=calls;std::cout<<(choices[0]==0 ? "stable" : "mixed")<<": "<<calls<<" cancellation boundaries\n";
    }
    invalid([&]{x11_seasonal_process(y,52,1,std::vector<int>(52,0),Policy::x11);});
    invalid([&]{x11_seasonal_process(y,4,0,{0,0,0,0},Policy::x11);});
    invalid([&]{x11_seasonal_process(y,4,5,{0,0,0,0},Policy::x11);});
    invalid([&]{x11_seasonal_process(y,4,1,{0},Policy::x11);});
    invalid([&]{x11_seasonal_process(y,4,1,{0,0,0,std::numeric_limits<int>::max()},Policy::x11);});
    invalid([&]{x11_seasonal_process(y,4,1,{0,0,0,0},static_cast<Policy>(9));});
    invalid([]{x11_seasonal_process(input(3),4,1,{0,0,0,0},Policy::x11);});
    invalid([]{x11_seasonal_process({{1},{0,0}},4,1,{0,0,0,0},Policy::x11);});
    y.values[1]=std::numeric_limits<double>::infinity();
    invalid([&]{x11_seasonal_process(y,4,1,{0,0,0,0},Policy::x11);});
    std::cout<<"PASS: explicit equality, short/mixed/calendar/mean/range checks, actual workers and "<<total<<" cancellation boundaries\n";
}
