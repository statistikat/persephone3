#include "core/x11_trend_selection.hpp"
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
template<class F>void invalid(F run) {
    bool caught=false;try{run();}catch(const std::invalid_argument&){caught=true;}assert(caught);
}
persephone::MaskedSeries input(std::size_t n) {
    persephone::MaskedSeries y{std::vector<double>(n),std::vector<std::uint8_t>(n,0)};
    for(std::size_t j=0;j<n;++j)y.values[j]=100+static_cast<double>(j)/1000+std::sin(static_cast<double>(j)*.7);
    return y;
}
}
int main() {
    using namespace persephone;using Mode=X11ChangeMode;using Status=X11TrendSelectionStatus;
    for(const int f:{4,12}) {
        assert(x11_henderson_select(f,std::nextafter(3.5,0.))==(f==4 ? 5 : 13));
        assert(x11_henderson_select(f,3.5)==(f==4 ? 7 : 23));
        assert(x11_henderson_select(f,std::numeric_limits<double>::infinity())==(f==4 ? 7 : 23));
        auto y=input(121);for(std::size_t j=0;j<121;++j)y.values[j]=j%2 ? 11 : 9;
        const auto closed=x11_trend_select(y,f,Mode::additive);
        const auto target=f==4 ? 504./25 : 4232./33;
        assert(std::abs(closed.comparison_ic_ratio-target)<1e-10);
        assert(closed.endpoint_ic_ratio==4.5);
        for(auto& value:y.values)value=3;
        assert(x11_trend_select(y,f,Mode::additive).status==Status::undefined_ic_ratio);
        assert(x11_trend_select(input(static_cast<std::size_t>(f+1)),f,Mode::additive).status==Status::insufficient_observations);
        y=input(121);y.values[60]=0;y.missing[60]=1;
        assert(x11_trend_select(y,f,Mode::additive).status==Status::missing_observations);
        assert(x11_trend_select(y,f,Mode::additive,61).status==Status::available);
        y=input(121);y.values[60]=0;
        assert(x11_trend_select(y,f,Mode::relative).status==Status::nonpositive_relative_input);
        for(auto& value:y.values)value=1;y.values[60]=100;
        assert(x11_trend_select(y,f,Mode::relative).status==Status::nonpositive_relative_trend);
    }
    assert(x11_henderson_select(12,std::nextafter(1.,0.))==9 && x11_henderson_select(12,1)==13);
    auto y=input(100003);const auto calling=std::this_thread::get_id();int total=0;
    for(const auto mode:{Mode::additive,Mode::relative}) {
        threads("1");const auto serial=x11_trend_select(y,12,mode,2,3);
        threads("2");const auto parallel=x11_trend_select(y,12,mode,2,3);
        assert(serial.comparison_ic_ratio==parallel.comparison_ic_ratio && serial.trend_mean_change==parallel.trend_mean_change);
        assert(serial.irregular_mean_change==parallel.irregular_mean_change && serial.selected_length==parallel.selected_length);
        if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
        int calls=0;x11_trend_select(y,12,mode,2,3,[&]{assert(std::this_thread::get_id()==calling);++calls;});
        for(int stop=1;stop<=calls;++stop) {
            int at=0;bool caught=false;
            try{x11_trend_select(y,12,mode,2,3,[&]{if(++at==stop)throw std::runtime_error("cancel trend selection");});}
            catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel trend selection";}
            assert(caught);
        }
        assert(x11_trend_select(y,12,mode,2,3).comparison_ic_ratio==serial.comparison_ic_ratio);
        total+=calls;std::cout<<(mode==Mode::additive ? "absolute" : "relative")<<": "<<calls<<" cancellation boundaries\n";
    }
    for(const double ratio:{-1.,-std::numeric_limits<double>::infinity(),std::numeric_limits<double>::quiet_NaN()})invalid([&]{x11_henderson_select(12,ratio);});
    invalid([]{x11_henderson_select(52,1);});invalid([]{x11_henderson_default_ratio(0,5);});
    for(const int length:{0,2,102,std::numeric_limits<int>::max()})invalid([&]{x11_henderson_default_ratio(12,length);});
    invalid([&]{x11_trend_select(y,52,Mode::additive);});
    invalid([&]{x11_trend_select(y,12,static_cast<Mode>(2));});
    invalid([&]{x11_trend_select(y,12,Mode::additive,std::numeric_limits<std::size_t>::max(),1);});
    invalid([&]{x11_trend_select(y,12,Mode::additive,1,std::numeric_limits<std::size_t>::max());});
    invalid([]{x11_trend_select({{1},{0,0}},12,Mode::additive);});
    y.missing[0]=2;invalid([&]{x11_trend_select(y,12,Mode::additive);});
    y.missing[0]=1;y.values[0]=std::numeric_limits<double>::infinity();invalid([&]{x11_trend_select(y,12,Mode::additive);});
    y=input(20);for(auto& value:y.values)value=std::numeric_limits<double>::max();y.values[10]=std::numeric_limits<double>::min();
    assert(x11_trend_select(y,12,Mode::additive).status==Status::numerical_range);
    for(std::size_t j=0;j<20;++j)y.values[j]=(j%2 ? 1 : -1)*std::numeric_limits<double>::max();
    assert(x11_trend_select(y,12,Mode::additive).status==Status::numerical_range);
    for(std::size_t j=0;j<20;++j)y.values[j]=j%2 ? std::numeric_limits<double>::denorm_min() : 0;
    assert(x11_trend_select(y,12,Mode::additive).status==Status::numerical_range);
    std::cout<<"PASS: thresholds, defaults, closed ratios, invalid/storage/range cases, workers and "<<total<<" cancellation boundaries\n";
}
