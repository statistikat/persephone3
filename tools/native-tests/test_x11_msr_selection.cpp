#include "core/x11_msr_selection.hpp"
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
persephone::MaskedSeries input(std::size_t n) {return {std::vector<double>(n),std::vector<std::uint8_t>(n,0)};}
}
int main() {
    using namespace persephone;using Mode=X11MsrMode;using Policy=X11MsrIterationPolicy;using Status=X11MsrSelectionStatus;using Band=X11MsrBand;
    const double infinity=std::numeric_limits<double>::infinity();
    assert(x11_msr_band(0)==Band::s3x3 && x11_msr_band(infinity)==Band::s3x9);
    for(const auto cutoff:{2.5,3.5,5.5,6.5}) {
        const auto before=cutoff==2.5 ? Band::s3x3 : (cutoff==3.5 ? Band::lower_gap : (cutoff==5.5 ? Band::s3x5 : Band::upper_gap));
        const auto after=cutoff==2.5 ? Band::lower_gap : (cutoff==3.5 ? Band::s3x5 : (cutoff==5.5 ? Band::upper_gap : Band::s3x9));
        assert(x11_msr_band(std::nextafter(cutoff,0.))==before);
        assert(x11_msr_band(cutoff)==after && x11_msr_band(std::nextafter(cutoff,infinity))==after);
    }
    for(const auto ratio:{-1.,-infinity,std::numeric_limits<double>::quiet_NaN()})invalid([&]{x11_msr_band(ratio);});
    for(const int frequency:{4,12}) {
        const auto f=static_cast<std::size_t>(frequency);auto y=input(3*f);
        for(std::size_t j=0;j<3*f;++j)y.values[j]=static_cast<double>(j/f)*3;
        const auto pilot=x11_msr_pilot(y,frequency,Mode::absolute);assert(pilot.status==X11MsrPilotStatus::available);
        for(std::size_t j=0;j<3*f;++j) {assert(pilot.seasonal.values[j]==3);assert(pilot.irregular.values[j]==y.values[j]-3);}
        assert(x11_msr_pilot(input(3*f-1),frequency,Mode::absolute).status==X11MsrPilotStatus::insufficient_observations);
        const auto short_result=x11_msr_select(y,frequency,1,Mode::absolute);
        assert(short_result.status==Status::available && short_result.using_default && short_result.selected_filter==5 && short_result.attempts.empty());
        y=input(20*f);
        for(std::size_t j=0;j<20*f;++j) {const auto year=static_cast<double>(j/f+1);y.values[j]=year+2.6133797380008934*std::sin(year*1.37);}
        const auto legacy=x11_msr_select(y,frequency,1,Mode::absolute);
        const auto capped=x11_msr_select(y,frequency,1,Mode::absolute,Policy::x11_five_year_cap);
        assert(legacy.status==Status::available && legacy.selected_filter==9 && legacy.attempts.size()==16 && !legacy.using_default);
        assert(capped.status==Status::available && capped.selected_filter==5 && capped.attempts.size()==6 && capped.using_default);
        assert(capped.reason==X11MsrSelectionReason::five_year_cap && capped.final_count==15*f);
        assert(std::abs(legacy.table.global_msr-7.2118625547008)<2e-12);
        const auto budget=x11_msr_select(y,frequency,1,Mode::absolute,Policy::jdplus,0,0,1);
        assert(budget.status==Status::budget_exhausted && budget.selected_filter==0 && budget.has_table && budget.attempts.size()==1);
        y.values[5*f]=0;y.missing[5*f]=1;
        assert(x11_msr_select(y,frequency,1,Mode::absolute).status==Status::missing_observations);
        y.missing[5*f]=0;
        assert(x11_msr_select(y,frequency,1,Mode::relative).status==Status::nonpositive_relative_input);
        for(std::size_t j=0;j<20*f;++j)y.values[j]=static_cast<double>(j%f+1);
        assert(x11_msr_select(y,frequency,1,Mode::absolute).status==Status::undefined_msr);
        // Trim explicit extensions, then the calendar tail, preserving the start.
        y=input(21*f+3);for(std::size_t j=0;j<y.values.size();++j)y.values[j]=2+std::sin(static_cast<double>(j)*.17);
        const auto a=x11_msr_select(y,frequency,frequency-1,Mode::absolute,Policy::jdplus,2,3);
        assert(a.first_period==1 && a.calendar_count==20*f && a.partial_year_trim==f-2);
        auto cropped=input(20*f);for(std::size_t j=0;j<20*f;++j)cropped.values[j]=y.values[j+2];
        const auto b=x11_msr_select(cropped,frequency,1,Mode::absolute);
        assert(a.selected_filter==b.selected_filter && a.table.global_msr==b.table.global_msr);
        y.values[0]=0;y.missing[0]=1;y.values.back()=0;y.missing.back()=1;
        assert(x11_msr_select(y,frequency,frequency-1,Mode::absolute,Policy::jdplus,2,3).table.global_msr==a.table.global_msr);
    }
    // Preserve tiny represented differences without subtracting normalized levels.
    auto fine=input(80);for(std::size_t j=0;j<80;++j)fine.values[j]=1+1e-8*std::sin(static_cast<double>(j)*.31);
    for(const auto mode:{Mode::absolute,Mode::relative}) {
        const auto t=x11_msr_table(fine,fine,4,1,mode);
        for(std::size_t p=0;p<4;++p) {
            double sum=0;for(std::size_t j=p+4;j<80;j+=4)sum+=std::abs(fine.values[j]-fine.values[j-4])/(mode==Mode::relative ? fine.values[j-4] : 1);
            assert(std::abs(t.seasonal.raw_mean[p]-sum/19)<1e-23);
        }
    }
    auto huge=input(14*4);for(std::size_t j=0;j<huge.values.size();++j)huge.values[j]=(j<4 ? -1 : 1)*std::numeric_limits<double>::max();
    const auto range=x11_msr_table(huge,huge,4,1,Mode::absolute);
    assert(range.status==X11MsrStatus::available);
    assert(std::abs(range.seasonal.raw_mean[0]/std::numeric_limits<double>::max()-2./13)<1e-16);
    huge=input(10*4);for(auto& v:huge.values)v=-std::numeric_limits<double>::max();huge.values[5*4]=std::numeric_limits<double>::max();
    assert(x11_msr_pilot(huge,4,Mode::absolute).status==X11MsrPilotStatus::numerical_range);
    // Two full attempts exercise bounded copying, filtering and table reductions.
    auto large=input(32779);for(std::size_t j=0;j<large.values.size();++j) {
        const auto year=j/12;large.values[j]=100+static_cast<double>(year)+(year%2 ? -1.3 : 1.3);
    }
    const auto caller=std::this_thread::get_id();int total=0;
    for(const auto mode:{Mode::absolute,Mode::relative}) {
        threads("1");const auto a=x11_msr_select(large,12,5,mode,Policy::jdplus,0,0,2);
        threads("2");const auto b=x11_msr_select(large,12,5,mode,Policy::jdplus,0,0,2);
        assert(a.status==Status::budget_exhausted && b.status==a.status && a.attempts.size()==2);
        assert(a.pilot.seasonal.values==b.pilot.seasonal.values && a.pilot.irregular.values==b.pilot.irregular.values && a.table.global_msr==b.table.global_msr);
        if(parallel_info().compiled)assert(parallel_probe_threads(parallel_info())>1);
        int calls=0;x11_msr_select(large,12,5,mode,Policy::jdplus,0,0,2,[&]{assert(std::this_thread::get_id()==caller);++calls;});
        for(int stop=1;stop<=calls;++stop) {
            int at=0;bool caught=false;
            try{x11_msr_select(large,12,5,mode,Policy::jdplus,0,0,2,[&]{if(++at==stop)throw std::runtime_error("cancel MSR selection");});}
            catch(const std::runtime_error& e){caught=std::string(e.what())=="cancel MSR selection";}assert(caught);
        }
        assert(x11_msr_select(large,12,5,mode,Policy::jdplus,0,0,2).table.global_msr==a.table.global_msr);
        total+=calls;std::cout<<"Mode "<<static_cast<int>(mode)<<": "<<calls<<" cancellation boundaries\n";
    }
    invalid([&]{x11_msr_select(large,12,1,Mode::absolute,static_cast<Policy>(2));});
    invalid([&]{x11_msr_select(large,12,1,Mode::absolute,Policy::jdplus,0,0,0);});
    invalid([&]{x11_msr_select(large,12,13,Mode::absolute);});
    invalid([&]{x11_msr_select(large,52,1,Mode::absolute);});
    invalid([&]{x11_msr_select(large,12,1,static_cast<Mode>(2));});
    invalid([&]{x11_msr_select(large,12,1,Mode::absolute,Policy::jdplus,std::numeric_limits<std::size_t>::max());});
    large.missing[0]=2;invalid([&]{x11_msr_pilot(large,12,Mode::absolute);});
    std::cout<<"PASS: pilot, exact thresholds, both policies, calendar/extensions, ranges and "<<total<<" interruption boundaries\n";
}
