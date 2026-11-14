#include "core/x11_mean_endpoints.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
namespace persephone {
namespace {
void boundary_mean(const MaskedSeries& input,std::size_t start,std::size_t count,
    double& mean,bool& missing) {
    double scale=0;
    for(std::size_t j=0;j<count;++j) {
        if(input.missing[start+j]) {missing=true;return;}
        scale=std::max(scale,std::abs(input.values[start+j]));
    }
    if(scale==0)return;
    long double normalized=0;
    for(std::size_t j=0;j<count;++j)normalized+=static_cast<long double>(input.values[start+j])/scale;
    mean=static_cast<double>(normalized/static_cast<long double>(count)*scale);
    if(!std::isfinite(mean))throw std::overflow_error("non-finite endpoint mean");
}
}
X11MeanEndpointResult x11_mean_endpoints(const MaskedSeries& series,int frequency,
    const Interrupt& interrupt) {
    const auto filter=x11_filter_definition(X11FilterKind::trend,frequency,0,interrupt);
    series.validate();
    const auto h=static_cast<std::size_t>(filter.radius),n=series.values.size();
    if(n<h)throw std::invalid_argument("mean endpoints require at least the centered-trend half-width");
    if(n>static_cast<std::size_t>(std::numeric_limits<std::int64_t>::max())-2*h)
        throw std::overflow_error("mean endpoint extended domain is too long");
    X11MeanEndpointResult out;
    boundary_mean(series,0,h,out.left_mean,out.left_missing);
    boundary_mean(series,n-h,h,out.right_mean,out.right_missing);
    MaskedSeries extended{std::vector<double>(n+2*h,0),std::vector<std::uint8_t>(n+2*h,0)};
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0 && interrupt)interrupt();
        extended.values[h+i]=series.values[i];extended.missing[h+i]=series.missing[i];
    }
    for(std::size_t i=0;i<h;++i) {
        extended.values[i]=out.left_mean;extended.missing[i]=out.left_missing;
        extended.values[h+n+i]=out.right_mean;extended.missing[h+n+i]=out.right_missing;
    }
    const auto applied=finite_filter(extended,filter.weights,filter.offsets,interrupt);
    out.values.values.resize(n);out.values.missing.resize(n);
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0 && interrupt)interrupt();
        out.values.values[i]=applied.values[h+i];out.values.missing[i]=applied.missing[h+i];
    }
    return out;
}
} // namespace persephone
