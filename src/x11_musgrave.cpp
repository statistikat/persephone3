#include "core/x11_musgrave.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
namespace persephone {
namespace {
void validate_ratio(double ratio) {
    if(!std::isfinite(ratio) || ratio<0)
        throw std::invalid_argument("Musgrave I/C ratio must be finite and nonnegative");
}
X11FilterDefinition endpoint(const X11FilterDefinition& full,int available,
    X11EndpointSide side,double ratio) {
    if(available<0 || available>=full.radius)
        throw std::invalid_argument("available boundary observations must be below the effective Henderson radius");
    if(side!=X11EndpointSide::left && side!=X11EndpointSide::right)
        throw std::invalid_argument("unsupported Musgrave endpoint side");
    const auto count=static_cast<std::size_t>(full.radius+available+1);
    const auto m=static_cast<long double>(count),center=(m+1)/2;
    long double omitted=0,moment=0;
    for(std::size_t j=count;j<full.weights.size();++j) {
        omitted+=full.weights[j];
        moment+=(static_cast<long double>(j+1)-center)*full.weights[j];
    }
    const auto spread=m*(m*m-1)/12;
    const auto pi_over_four=std::acos(-1.L)/4;
    const auto r=static_cast<long double>(ratio);
    // Scaling by 1/r avoids squaring large finite ratios. Underflow of this
    // vanishing correction gives the constant-only minimum-revision limit.
    const auto inverse=ratio>1 ? 1/r : 1.L;
    const auto slope=ratio>1 ? moment*inverse*inverse/(spread*inverse*inverse+pi_over_four)
        : moment/(spread+pi_over_four*r*r);
    X11FilterDefinition out;out.radius=full.radius;
    out.weights.resize(count);out.offsets.resize(count);
    for(std::size_t j=0;j<count;++j) {
        const auto weight=static_cast<double>(full.weights[j]+omitted/m+(static_cast<long double>(j+1)-center)*slope);
        if(!std::isfinite(weight))throw std::overflow_error("non-finite Musgrave endpoint weight");
        out.weights[j]=weight;out.offsets[j]=-full.radius+static_cast<std::int64_t>(j);
    }
    if(side==X11EndpointSide::left) {
        std::reverse(out.weights.begin(),out.weights.end());
        std::reverse(out.offsets.begin(),out.offsets.end());
        for(auto& offset:out.offsets)offset=-offset;
    }
    return out;
}
void apply_endpoint(const MaskedSeries& input,MaskedSeries& output,
    std::size_t row,const X11FilterDefinition& filter) {
    // Definition and minimum-length checks guarantee every source index exists.
    // Resolve missingness first, including the zero-tap exception.
    for(std::size_t j=0;j<filter.weights.size();++j) {
        const auto source=static_cast<std::size_t>(static_cast<std::int64_t>(row)+filter.offsets[j]);
        if(filter.weights[j]!=0 && input.missing[source]) {output.missing[row]=1;return;}
    }
    double value=0;
    for(std::size_t j=0;j<filter.weights.size();++j) {
        if(filter.weights[j]==0)continue;
        const auto source=static_cast<std::size_t>(static_cast<std::int64_t>(row)+filter.offsets[j]);
        value+=filter.weights[j]*input.values[source];
        if(!std::isfinite(value))throw std::overflow_error("non-finite result in Musgrave filter arithmetic");
    }
    output.values[row]=value;output.missing[row]=0;
}
}
X11FilterDefinition x11_musgrave_definition(int frequency,int length,int available,
    X11EndpointSide side,double ic_ratio,const Interrupt& interrupt) {
    validate_ratio(ic_ratio);
    const auto full=x11_filter_definition(X11FilterKind::henderson,frequency,length,interrupt);
    return endpoint(full,available,side,ic_ratio);
}
MaskedSeries x11_musgrave_filter(const MaskedSeries& series,int frequency,int length,
    double ic_ratio,const Interrupt& interrupt) {
    validate_ratio(ic_ratio);
    const auto full=x11_filter_definition(X11FilterKind::henderson,frequency,length,interrupt);
    if(series.values.size()<full.weights.size())
        throw std::invalid_argument("Musgrave endpoints require at least the effective Henderson length");
    auto out=finite_filter(series,full.weights,full.offsets,interrupt);
    // At most 100 endpoint outputs: keep these small operations and callbacks
    // on the caller. The independent central outputs already use owned OpenMP.
    for(int available=0;available<full.radius;++available) {
        if(interrupt)interrupt();
        apply_endpoint(series,out,static_cast<std::size_t>(available),
            endpoint(full,available,X11EndpointSide::left,ic_ratio));
        apply_endpoint(series,out,series.values.size()-1-static_cast<std::size_t>(available),
            endpoint(full,available,X11EndpointSide::right,ic_ratio));
    }
    return out;
}
} // namespace persephone
