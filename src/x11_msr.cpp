#include "core/x11_msr.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
namespace persephone {
namespace {
using Status=X11MsrStatus;
constexpr auto missing=std::numeric_limits<double>::quiet_NaN();
void poll(std::size_t i,const Interrupt& interrupt) {if((i&4095U)==0 && interrupt)interrupt();}
double decimal(double x,double scale) {return std::round(x*scale)/scale;}
double correction(std::size_t n,bool seasonal) {
    // Ladiray/Quenneville (2000), table 4.112. Small-sample factors are
    // rounded to five decimals. For n>=7 the four radical coefficients are
    // rounded separately to six decimals, using k=n-1 annual changes.
    if(n<4)return 1;
    if(n==4)return seasonal ? 3 : decimal(90/(2*std::sqrt(842.)+21*std::sqrt(2.)),1e5);
    if(n==5)return decimal(seasonal ? 3*std::sqrt(2.)/(1+std::sqrt(3.)) :
        60/(std::sqrt(894.)+2*std::sqrt(211.)),1e5);
    if(n==6)return decimal(seasonal ? 5*std::sqrt(6.)/(8+std::sqrt(2.)) :
        25*std::sqrt(3.)/(2*std::sqrt(298.)+std::sqrt(67.)),1e5);
    const auto a=decimal(seasonal ? std::sqrt(3.) : 5*std::sqrt(6.),1e6);
    const auto b=decimal(seasonal ? 6*std::sqrt(2.) : 6*std::sqrt(149.),1e6);
    const auto k=static_cast<double>(n-1);
    return a*k/(b+(k-6)*a);
}
Status ratio(double numerator,double denominator,double& out) {
    if(numerator==0 && denominator==0)return Status::undefined_msr;
    if(denominator==0) {out=std::numeric_limits<double>::infinity();return Status::available;}
    const auto value=numerator/denominator;
    if(!std::isfinite(value) || (numerator>0 && value==0))return Status::numerical_range;
    out=value;return Status::available;
}
X11MsrComponent component(const MaskedSeries& input,std::size_t f,std::size_t first,
    const std::vector<std::size_t>& observations,bool seasonal,X11MsrMode mode,const Interrupt& interrupt) {
    const auto n=input.values.size();X11MsrComponent out;
    out.raw_mean.assign(f,missing);out.corrected_mean.assign(f,missing);out.correction.resize(f);
    out.status.assign(f,Status::available);std::vector<double> scale(f,0),largest(f,0),sum(f,0),changes(n,0);
    for(std::size_t p=0;p<f;++p) {
        out.correction[p]=correction(observations[p],seasonal);
        if(observations[p]<2)out.status[p]=Status::insufficient_observations;
    }
    for(std::size_t j=0;j<n;++j) {
        poll(j,interrupt);const auto p=(j%f+first)%f;
        if(out.status[p]!=Status::available)continue;
        if(input.missing[j])out.status[p]=Status::missing_observations;
        else if(mode==X11MsrMode::relative && input.values[j]<=0)out.status[p]=Status::nonpositive_relative_component;
        else scale[p]=std::max(scale[p],std::abs(input.values[j]));
    }
    // Each worker owns one change. Status inspection/reductions and all polls
    // are on the caller; no shared phase accumulators are mutated in workers.
    parallel_for(n,8,interrupt,[&](std::size_t j) {
        const auto p=(j%f+first)%f;
        if(j<f || out.status[p]!=Status::available || scale[p]==0)return;
        const auto a=input.values[j]/scale[p],b=input.values[j-f]/scale[p];
        if((input.values[j]!=0 && a==0) || (input.values[j-f]!=0 && b==0)) {changes[j]=missing;return;}
        changes[j]=std::abs(a-b);
        if(mode==X11MsrMode::relative)changes[j]/=b;
    });
    for(std::size_t j=f;j<n;++j) {
        poll(j,interrupt);const auto p=(j%f+first)%f;
        if(out.status[p]!=Status::available)continue;
        if(!std::isfinite(changes[j]))out.status[p]=Status::numerical_range;
        else largest[p]=std::max(largest[p],changes[j]);
    }
    for(std::size_t j=f;j<n;++j) {
        poll(j,interrupt);const auto p=(j%f+first)%f;
        if(out.status[p]==Status::available && largest[p]>0)sum[p]+=changes[j]/largest[p];
    }
    for(std::size_t p=0;p<f;++p) {
        if(out.status[p]!=Status::available)continue;
        const auto unit=(sum[p]/static_cast<double>(observations[p]-1))*largest[p];
        const auto raw=mode==X11MsrMode::relative ? unit : unit*scale[p];
        const auto corrected=raw*out.correction[p];
        if(!std::isfinite(raw) || !std::isfinite(corrected) || (largest[p]>0 && (unit==0 || raw==0 || corrected==0))) {
            out.status[p]=Status::numerical_range;continue;
        }
        out.raw_mean[p]=raw;out.corrected_mean[p]=corrected;
    }
    return out;
}
bool weighted_mean(X11MsrComponent& value,const std::vector<std::size_t>& counts) {
    double scale=0,total=0,sum=0;
    for(std::size_t p=0;p<counts.size();++p) {
        if(value.status[p]!=Status::available)return false;
        scale=std::max(scale,value.corrected_mean[p]);total+=static_cast<double>(counts[p]);
    }
    if(scale==0) {value.global_mean=0;return true;}
    for(std::size_t p=0;p<counts.size();++p)sum+=(value.corrected_mean[p]/scale)*(static_cast<double>(counts[p])/total);
    // A convex mean cannot exceed its largest input; avoid overflow caused
    // solely by the final few ulps in the normalized positive sum.
    value.global_mean=std::min(1.,sum)*scale;
    return std::isfinite(value.global_mean) && value.global_mean>0;
}
}
X11MsrTable x11_msr_table(const MaskedSeries& seasonal,const MaskedSeries& irregular,
    int frequency,int first_period,X11MsrMode mode,const Interrupt& interrupt) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(first_period<1 || first_period>frequency)throw std::invalid_argument("first period must be within the annual frequency");
    if(mode!=X11MsrMode::absolute && mode!=X11MsrMode::relative)throw std::invalid_argument("unsupported MSR change measure");
    seasonal.validate();irregular.validate();const auto n=seasonal.values.size();
    if(n!=irregular.values.size())throw std::invalid_argument("MSR components must have identical lengths");
    if(interrupt)interrupt();
    const auto f=static_cast<std::size_t>(frequency),first=static_cast<std::size_t>(first_period-1);
    X11MsrTable out;out.observations.assign(f,n/f);out.changes.resize(f);
    for(std::size_t j=0;j<n%f;++j)++out.observations[(first+j)%f];
    for(std::size_t p=0;p<f;++p)out.changes[p]=out.observations[p]>0 ? out.observations[p]-1 : 0;
    out.seasonal=component(seasonal,f,first,out.observations,true,mode,interrupt);
    out.irregular=component(irregular,f,first,out.observations,false,mode,interrupt);
    out.ratios.assign(f,missing);out.phase_status.assign(f,Status::available);
    bool complete=true;
    for(std::size_t p=0;p<f;++p) {
        if(out.seasonal.status[p]!=Status::available)out.phase_status[p]=out.seasonal.status[p];
        else if(out.irregular.status[p]!=Status::available)out.phase_status[p]=out.irregular.status[p];
        else {out.phase_status[p]=ratio(out.irregular.corrected_mean[p],out.seasonal.corrected_mean[p],out.ratios[p]);continue;}
        complete=false;
    }
    const auto s_ok=weighted_mean(out.seasonal,out.changes),i_ok=weighted_mean(out.irregular,out.changes);
    if(!complete)out.status=Status::incomplete_phases;
    else if(!s_ok || !i_ok)out.status=Status::numerical_range;
    else out.status=ratio(out.irregular.global_mean,out.seasonal.global_mean,out.global_msr);
    if(interrupt)interrupt();
    return out;
}
} // namespace persephone
