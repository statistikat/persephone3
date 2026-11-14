#include "core/x11_msr_selection.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <utility>
namespace persephone {
namespace {
void validate(int frequency,X11MsrMode mode) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(mode!=X11MsrMode::absolute && mode!=X11MsrMode::relative)throw std::invalid_argument("unsupported MSR change measure");
}
void poll(std::size_t i,const Interrupt& interrupt) {if((i&4095U)==0 && interrupt)interrupt();}
X11MsrPilot failure(X11MsrPilotStatus status) {X11MsrPilot out;out.status=status;return out;}
X11MsrSelectionStatus pilot_status(X11MsrPilotStatus status) {
    using P=X11MsrPilotStatus;using S=X11MsrSelectionStatus;
    switch(status) {
    case P::available:return S::available;
    case P::missing_observations:return S::missing_observations;
    case P::nonpositive_relative_input:return S::nonpositive_relative_input;
    case P::numerical_range:return S::numerical_range;
    case P::insufficient_observations:return S::incomplete_table;
    }
    throw std::logic_error("unknown MSR pilot status");
}
int selected(X11MsrBand band) {
    if(band==X11MsrBand::s3x3)return 3;
    if(band==X11MsrBand::s3x5)return 5;
    if(band==X11MsrBand::s3x9)return 9;
    return 0;
}
void use_default(X11MsrSelection& out,X11MsrSelectionReason reason) {
    out.status=X11MsrSelectionStatus::available;out.reason=reason;
    out.using_default=true;out.selected_filter=5;
}
}
X11MsrPilot x11_msr_pilot(const MaskedSeries& series,int frequency,X11MsrMode mode,const Interrupt& interrupt) {
    validate(frequency,mode);series.validate();const auto n=series.values.size(),f=static_cast<std::size_t>(frequency);
    if(interrupt)interrupt();
    if(n<3*f)return failure(X11MsrPilotStatus::insufficient_observations);
    std::vector<double> scale(f,0),left(f),right(f),unit(n);
    std::vector<std::size_t> counts(f,n/f);std::vector<std::uint8_t> constant(f,1);
    for(std::size_t p=0;p<n%f;++p)++counts[p];
    for(std::size_t j=0;j<n;++j) {
        poll(j,interrupt);const auto p=j%f;const auto value=series.values[j];
        if(series.missing[j])return failure(X11MsrPilotStatus::missing_observations);
        if(mode==X11MsrMode::relative && value<=0)return failure(X11MsrPilotStatus::nonpositive_relative_input);
        scale[p]=std::max(scale[p],std::abs(value));
        if(value!=series.values[p])constant[p]=0;
    }
    for(std::size_t j=0;j<n;++j) {
        poll(j,interrupt);const auto p=j%f;
        unit[j]=scale[p]==0 ? 0 : series.values[j]/scale[p];
        if(series.values[j]!=0 && unit[j]==0)return failure(X11MsrPilotStatus::numerical_range);
    }
    for(std::size_t p=0;p<f;++p) {
        const auto last=p+(counts[p]-1)*f;
        left[p]=(unit[p]+unit[p+f]+unit[p+2*f])/3;
        right[p]=(unit[last-2*f]+unit[last-f]+unit[last])/3;
    }
    X11MsrPilot out;out.seasonal={std::vector<double>(n),std::vector<std::uint8_t>(n,0)};
    out.irregular={std::vector<double>(n),std::vector<std::uint8_t>(n,0)};
    std::vector<std::uint8_t> valid(n,1);
    parallel_for(n,21,interrupt,[&](std::size_t j) {
        const auto p=j%f,at=j/f;
        if(constant[p]) {
            out.seasonal.values[j]=series.values[j];out.irregular.values[j]=mode==X11MsrMode::relative ? 1 : 0;return;
        }
        double sum=0;
        for(int lag=-3;lag<=3;++lag) {
            if(lag<0 && at<static_cast<std::size_t>(-lag))sum+=left[p];
            else if(lag>0 && static_cast<std::size_t>(lag)>=counts[p]-at)sum+=right[p];
            else {
                const auto row=lag<0 ? j-static_cast<std::size_t>(-lag)*f : j+static_cast<std::size_t>(lag)*f;
                sum+=unit[row];
            }
        }
        // A positive-weight mean stays within the normalized input bounds.
        const auto s=std::clamp(sum/7,-1.,1.);
        const auto i=mode==X11MsrMode::relative ? unit[j]/s : unit[j]-s;
        out.seasonal.values[j]=s*scale[p];out.irregular.values[j]=mode==X11MsrMode::relative ? i : i*scale[p];
        if(!std::isfinite(out.seasonal.values[j]) || !std::isfinite(out.irregular.values[j]) ||
            (s!=0 && out.seasonal.values[j]==0) || (i!=0 && out.irregular.values[j]==0) ||
            (mode==X11MsrMode::relative && (s<=0 || i<=0)))valid[j]=0;
    });
    for(std::size_t j=0;j<n;++j) {poll(j,interrupt);if(!valid[j])return failure(X11MsrPilotStatus::numerical_range);}
    out.status=X11MsrPilotStatus::available;return out;
}
X11MsrBand x11_msr_band(double ratio) {
    if(std::isnan(ratio) || ratio<0)throw std::invalid_argument("MSR ratio must be nonnegative and not missing");
    if(ratio<2.5)return X11MsrBand::s3x3;
    if(ratio<3.5)return X11MsrBand::lower_gap;
    if(ratio<5.5)return X11MsrBand::s3x5;
    if(ratio<6.5)return X11MsrBand::upper_gap;
    return X11MsrBand::s3x9;
}
X11MsrSelection x11_msr_select(const MaskedSeries& series,int frequency,int first_period,
    X11MsrMode mode,X11MsrIterationPolicy policy,std::size_t backcasts,std::size_t forecasts,
    std::size_t max_iterations,const Interrupt& interrupt) {
    validate(frequency,mode);series.validate();const auto n=series.values.size(),f=static_cast<std::size_t>(frequency);
    if(first_period<1 || first_period>frequency)throw std::invalid_argument("first period must be within the annual frequency");
    if(policy!=X11MsrIterationPolicy::jdplus && policy!=X11MsrIterationPolicy::x11_five_year_cap)throw std::invalid_argument("unsupported MSR iteration policy");
    if(max_iterations==0)throw std::invalid_argument("max_iterations must be positive");
    if(backcasts>n || forecasts>n-backcasts)throw std::invalid_argument("extension lengths exceed the input span");
    if(interrupt)interrupt();
    X11MsrSelection out;out.estimation_count=n-backcasts-forecasts;
    const auto first=(static_cast<std::size_t>(first_period-1)+backcasts%f)%f;
    out.first_period=static_cast<int>(first+1);
    out.partial_year_trim=std::min(out.estimation_count,(first+out.estimation_count%f)%f);
    out.calendar_count=out.estimation_count-out.partial_year_trim;out.final_count=out.calendar_count;
    if(out.calendar_count<5*f) {use_default(out,X11MsrSelectionReason::insufficient_initial_history);return out;}
    auto count=out.calendar_count;std::size_t removed=0;
    for(;;) {
        if(out.attempts.size()==max_iterations) {
            out.status=X11MsrSelectionStatus::budget_exhausted;out.reason=X11MsrSelectionReason::iteration_budget;return out;
        }
        if(interrupt)interrupt();
        MaskedSeries input{std::vector<double>(count),std::vector<std::uint8_t>(count)};
        for(std::size_t j=0;j<count;++j) {poll(j,interrupt);input.values[j]=series.values[backcasts+j];input.missing[j]=series.missing[backcasts+j];}
        out.final_count=count;out.has_table=false;out.table=X11MsrTable{};
        out.pilot=x11_msr_pilot(input,frequency,mode,interrupt);
        X11MsrAttempt attempt;attempt.count=count;attempt.years_removed=removed;
        attempt.status=pilot_status(out.pilot.status);
        if(out.pilot.status!=X11MsrPilotStatus::available) {
            out.status=attempt.status;out.attempts.push_back(attempt);return out;
        }
        out.table=x11_msr_table(out.pilot.seasonal,out.pilot.irregular,frequency,out.first_period,mode,interrupt);out.has_table=true;
        attempt.global_msr=out.table.global_msr;
        if(out.table.status!=X11MsrStatus::available) {
            const auto range=[](X11MsrStatus s){return s==X11MsrStatus::numerical_range;};
            const bool component_range=std::any_of(out.table.seasonal.status.begin(),out.table.seasonal.status.end(),range) ||
                std::any_of(out.table.irregular.status.begin(),out.table.irregular.status.end(),range);
            attempt.status=out.table.status==X11MsrStatus::undefined_msr ? X11MsrSelectionStatus::undefined_msr :
                ((out.table.status==X11MsrStatus::numerical_range || component_range) ? X11MsrSelectionStatus::numerical_range : X11MsrSelectionStatus::incomplete_table);
            out.status=attempt.status;out.attempts.push_back(attempt);return out;
        }
        attempt.band=x11_msr_band(attempt.global_msr);attempt.status=X11MsrSelectionStatus::available;
        out.attempts.push_back(attempt);out.selected_filter=selected(attempt.band);
        if(out.selected_filter!=0) {out.status=X11MsrSelectionStatus::available;out.reason=X11MsrSelectionReason::decisive_ratio;return out;}
        if(policy==X11MsrIterationPolicy::x11_five_year_cap && removed==5) {
            use_default(out,X11MsrSelectionReason::five_year_cap);return out;
        }
        if(count-f<5*f) {use_default(out,X11MsrSelectionReason::insufficient_remaining_history);return out;}
        count-=f;++removed;
    }
}
} // namespace persephone
