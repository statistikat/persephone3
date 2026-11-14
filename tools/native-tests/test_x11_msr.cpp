#include "core/x11_msr.hpp"
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
persephone::MaskedSeries input(std::size_t n) {
    persephone::MaskedSeries y{std::vector<double>(n),std::vector<std::uint8_t>(n,0)};
    for(std::size_t j=0;j<n;++j)y.values[j]=100+static_cast<double>(j)/1000+std::sin(static_cast<double>(j)*.7);
    return y;
}
}
int main() {
    using namespace persephone;using Mode=X11MsrMode;using Status=X11MsrStatus;
    for(const int f:{4,12}) {
        auto y=input(static_cast<std::size_t>(f*5));auto z=y;
        for(std::size_t j=0;j<y.values.size();++j) {y.values[j]=static_cast<double>(j);z.values[j]=2*y.values[j];}
        const auto table=x11_msr_table(y,z,f,f,Mode::absolute);
        assert(table.status==Status::available);
        assert(std::abs(table.global_msr-2*1.01779/1.55291)<1e-14);
        for(const auto mean:table.seasonal.raw_mean)assert(std::abs(mean-f)<1e-14);
        for(auto& value:y.values)value=1;
        assert(x11_msr_table(y,y,f,1,Mode::absolute).status==Status::undefined_msr);
        assert(std::isinf(x11_msr_table(y,z,f,1,Mode::absolute).global_msr));
        y.values[2]=0;y.missing[2]=1;const auto gap=x11_msr_table(y,z,f,1,Mode::absolute);
        assert(gap.status==Status::incomplete_phases && gap.seasonal.status[2]==Status::missing_observations);
        assert(gap.irregular.status[2]==Status::available);
        y.missing[2]=0;assert(x11_msr_table(y,z,f,1,Mode::relative).seasonal.status[2]==Status::nonpositive_relative_component);
        const auto short_table=x11_msr_table(input(static_cast<std::size_t>(f*2-1)),input(static_cast<std::size_t>(f*2-1)),f,1,Mode::absolute);
        assert(short_table.phase_status.back()==Status::insufficient_observations);
        y=input(static_cast<std::size_t>(3*f-1));
        for(std::size_t j=0;j<y.values.size();++j)y.values[j]=(j/static_cast<std::size_t>(f))%2 ? std::numeric_limits<double>::max() : 0;
        const auto large=x11_msr_table(y,y,f,f,Mode::absolute);
        assert(large.status==Status::available && large.global_msr==1);
    }
    auto y=input(100003);auto z=y;for(auto& value:z.values)value=std::cos(value)*.3+1;
    int total=0;const auto caller=std::this_thread::get_id();
    for(const auto mode:{Mode::absolute,Mode::relative}) {
        threads("1");const auto serial=x11_msr_table(y,z,12,11,mode);
        threads("2");const auto parallel=x11_msr_table(y,z,12,11,mode);
        assert(serial.global_msr==parallel.global_msr && serial.seasonal.corrected_mean==parallel.seasonal.corrected_mean);
        assert(serial.irregular.corrected_mean==parallel.irregular.corrected_mean && serial.ratios==parallel.ratios);
        if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
        int calls=0;x11_msr_table(y,z,12,11,mode,[&]{assert(std::this_thread::get_id()==caller);++calls;});
        for(int stop=1;stop<=calls;++stop) {
            int at=0;bool caught=false;
            try{x11_msr_table(y,z,12,11,mode,[&]{if(++at==stop)throw std::runtime_error("cancel MSR");});}
            catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel MSR";}
            assert(caught);
        }
        assert(x11_msr_table(y,z,12,11,mode).global_msr==serial.global_msr);
        total+=calls;std::cout<<"Mode "<<static_cast<int>(mode)<<": "<<calls<<" cancellation boundaries\n";
    }
    invalid([&]{x11_msr_table(y,z,52,1,Mode::absolute);});
    for(const int first:{0,13})invalid([&]{x11_msr_table(y,z,12,first,Mode::absolute);});
    invalid([&]{x11_msr_table(y,z,12,1,static_cast<Mode>(2));});
    invalid([&]{x11_msr_table(y,{},12,1,Mode::absolute);});
    y.missing[0]=2;invalid([&]{x11_msr_table(y,z,12,1,Mode::absolute);});
    y.missing[0]=1;y.values[0]=std::numeric_limits<double>::infinity();invalid([&]{x11_msr_table(y,z,12,1,Mode::absolute);});
    y=input(12);z=y;for(std::size_t j=0;j<12;++j)y.values[j]=j<8 ? 0 : std::numeric_limits<double>::denorm_min();
    assert(x11_msr_table(y,y,4,1,Mode::absolute).status==Status::incomplete_phases);
    y=input(8);for(std::size_t j=0;j<8;++j)y.values[j]=j<4 ? -std::numeric_limits<double>::max() : std::numeric_limits<double>::max();
    assert(x11_msr_table(y,y,4,1,Mode::absolute).status==Status::incomplete_phases);
    std::cout<<"PASS: closed means, invalid/range cases, workers and "<<total<<" interruption boundaries\n";
}
