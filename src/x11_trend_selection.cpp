#include "core/x11_trend_selection.hpp"
#include "core/parallel.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
namespace persephone {
namespace {
void frequency_check(int frequency) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
}
void poll(std::size_t row,const Interrupt& interrupt) {
    if((row&4095U)==0 && interrupt)interrupt();
}
// Nonnegative finite values. Scaling before summation avoids a large unscaled
// sum; the reduction order is fixed and stays on the calling thread.
double mean_change(const std::vector<double>& values,const Interrupt& interrupt) {
    double scale=0,sum=0;
    for(std::size_t i=0;i<values.size();++i) {poll(i,interrupt);scale=std::max(scale,values[i]);}
    if(scale==0)return 0;
    for(std::size_t i=0;i<values.size();++i) {poll(i,interrupt);sum+=values[i]/scale;}
    return (sum/static_cast<double>(values.size()))*scale;
}
}
int x11_henderson_select(int frequency,double ratio) {
    frequency_check(frequency);
    if(std::isnan(ratio) || ratio<0)throw std::invalid_argument("comparison I/C ratio must be nonnegative and not missing");
    if(frequency==4)return ratio<3.5 ? 5 : 7;
    return ratio<1 ? 9 : (ratio<3.5 ? 13 : 23);
}
double x11_henderson_default_ratio(int frequency,int length) {
    frequency_check(frequency);
    if(length<1 || length>101 || length%2==0)throw std::invalid_argument("Henderson length must be odd and between 1 and 101");
    if(frequency==4)return length<=5 ? .001 : 4.5;
    return length<=9 ? 1 : (length<=13 ? 3.5 : 4.5);
}
X11TrendSelection x11_trend_select(const MaskedSeries& series,int frequency,
    X11ChangeMode mode,std::size_t backcasts,std::size_t forecasts,const Interrupt& interrupt) {
    frequency_check(frequency);
    if(mode!=X11ChangeMode::additive && mode!=X11ChangeMode::relative)throw std::invalid_argument("unsupported change mode");
    series.validate();const auto n=series.values.size();
    if(backcasts>n || forecasts>n-backcasts)throw std::invalid_argument("extension lengths exceed the input span");
    const auto count=n-backcasts-forecasts;
    X11TrendSelection out;out.pilot_length=frequency+1;
    const auto h=static_cast<std::size_t>(frequency/2);
    out.pilot_count=count>2*h ? count-2*h : 0;
    out.change_count=out.pilot_count>0 ? out.pilot_count-1 : 0;
    if(interrupt)interrupt();
    if(out.change_count==0)return out;
    double scale=0;bool constant=true;
    for(std::size_t i=0;i<count;++i) {
        poll(i,interrupt);const auto row=backcasts+i;const auto value=series.values[row];
        if(series.missing[row]) {out.status=X11TrendSelectionStatus::missing_observations;return out;}
        if(mode==X11ChangeMode::relative && value<=0) {out.status=X11TrendSelectionStatus::nonpositive_relative_input;return out;}
        scale=std::max(scale,std::abs(value));constant=constant && value==series.values[backcasts];
    }
    if(constant) {
        out.trend_mean_change=out.irregular_mean_change=0;
        out.status=X11TrendSelectionStatus::undefined_ic_ratio;return out;
    }
    MaskedSeries input{std::vector<double>(count,0),std::vector<std::uint8_t>(count,0)};
    for(std::size_t i=0;i<count;++i) {
        poll(i,interrupt);const auto value=series.values[backcasts+i];
        input.values[i]=value/scale;
        if(value!=0 && input.values[i]==0) {out.status=X11TrendSelectionStatus::numerical_range;return out;}
    }
    const auto pilot=x11_symmetric_filter(input,X11FilterKind::henderson,frequency,out.pilot_length,interrupt);
    const auto m=out.pilot_count;std::vector<double> irregular(m);
    for(std::size_t j=0;j<m;++j) {
        poll(j,interrupt);
        if(mode==X11ChangeMode::relative && pilot.values[h+j]<=0) {
            out.status=X11TrendSelectionStatus::nonpositive_relative_trend;return out;
        }
    }
    parallel_for(m,3,interrupt,[&](std::size_t j) {
        irregular[j]=mode==X11ChangeMode::relative ? input.values[h+j]/pilot.values[h+j] : input.values[h+j]-pilot.values[h+j];
    });
    for(std::size_t j=0;j<m;++j) {
        poll(j,interrupt);
        if(!std::isfinite(irregular[j]) || (mode==X11ChangeMode::relative && irregular[j]==0)) {
            out.status=X11TrendSelectionStatus::numerical_range;return out;
        }
    }
    std::vector<double> c(out.change_count),i(out.change_count);
    parallel_for(out.change_count,6,interrupt,[&](std::size_t j) {
        c[j]=std::abs(pilot.values[h+j+1]-pilot.values[h+j]);
        i[j]=std::abs(irregular[j+1]-irregular[j]);
        if(mode==X11ChangeMode::relative) {c[j]/=pilot.values[h+j];i[j]/=irregular[j];}
    });
    for(std::size_t j=0;j<out.change_count;++j) {
        poll(j,interrupt);
        if(!std::isfinite(c[j]) || !std::isfinite(i[j])) {out.status=X11TrendSelectionStatus::numerical_range;return out;}
    }
    const auto cm=mean_change(c,interrupt),im=mean_change(i,interrupt);
    out.trend_mean_change=mode==X11ChangeMode::relative ? cm : cm*scale;
    out.irregular_mean_change=mode==X11ChangeMode::relative ? im : im*scale;
    if(!std::isfinite(out.trend_mean_change) || !std::isfinite(out.irregular_mean_change) ||
        (cm>0 && out.trend_mean_change==0) || (im>0 && out.irregular_mean_change==0)) {
        out.status=X11TrendSelectionStatus::numerical_range;return out;
    }
    if(cm==0 && im==0) {out.status=X11TrendSelectionStatus::undefined_ic_ratio;return out;}
    out.raw_ic_ratio=cm==0 ? std::numeric_limits<double>::infinity() : im/cm;
    if(im>0 && out.raw_ic_ratio==0) {out.status=X11TrendSelectionStatus::numerical_range;return out;}
    out.comparison_ic_ratio=out.raw_ic_ratio*(frequency==4 ? 3 : 1);
    out.selected_length=x11_henderson_select(frequency,out.comparison_ic_ratio);
    out.endpoint_ic_ratio=x11_henderson_default_ratio(frequency,out.selected_length);
    out.status=X11TrendSelectionStatus::available;
    return out;
}
} // namespace persephone
