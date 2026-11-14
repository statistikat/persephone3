#include "core/x11_seasonal_endpoints.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>
namespace persephone {
namespace {
void validate_length(int length) {
    if(length!=1 && length!=3 && length!=5 && length!=9 && length!=15)
        throw std::invalid_argument("seasonal endpoints currently support 3x1, 3x3, 3x5, 3x9 and 3x15");
}
X11FilterDefinition endpoint(int frequency,int length,int available,X11SeasonalSide side) {
    const int h=(length+1)/2;
    if(available<0 || available>=h)throw std::invalid_argument("available seasonal observations must be below the annual filter radius");
    if(side!=X11SeasonalSide::left && side!=X11SeasonalSide::right)throw std::invalid_argument("unsupported seasonal endpoint side");
    const int m=h+available+1;
    X11FilterDefinition out;out.radius=static_cast<std::int64_t>(frequency)*h;
    std::vector<int> numerators(static_cast<std::size_t>(m),0);int denominator=1;
    if(length==1) {numerators={61,39};denominator=100;}
    else if(length==9) {
        // Published X-11 method definitions: Census Technical Paper 15,
        // Appendix B, table 1D. These are not full-precision Musgrave weights.
        static const std::vector<std::vector<int>> table{
            {246,221,197,173,112,51},
            {208,192,176,160,144,92,28},
            {173,163,154,143,133,123,79,32},
            {141,137,132,128,123,117,113,75,34},
            {84,120,118,117,116,114,113,111,73,34}};
        numerators=table[static_cast<std::size_t>(available)];denominator=1000;
    } else {
        // Construct exact rational coefficients by convolving the 3xN kernel
        // with virtual values. For 3x3/3x5 the nearest missing annual values
        // use the first h+1 mean; the furthest uses observation h-1. For
        // 3x15 all eight virtual values use the first five mean, followed
        // by the finite-decimal convention characterized via public APIs.
        const int pool=length==15 ? 5 : h+1;denominator=3*length*pool;
        for(int j=0;j<length+2;++j) {
            const int base=std::min({j+1,3,length+2-j});
            const int index=available+j-h;
            if(index>=0)numerators[static_cast<std::size_t>(index)]+=base*pool;
            else if(length==15 || -index<h)for(int k=0;k<pool;++k)numerators[static_cast<std::size_t>(k)]+=base;
            else numerators[static_cast<std::size_t>(h-1)]+=base*pool;
        }
        if(length==15) {
            // Integer nearest rounding to five decimals (no ties at /225).
            // Close the farthest observed tap so the defined row sums to one.
            int remainder=100000;
            for(int j=0;j<m-1;++j) {
                auto& value=numerators[static_cast<std::size_t>(j)];
                value=(value*100000+denominator/2)/denominator;remainder-=value;
            }
            numerators.back()=remainder;denominator=100000;
        }
    }
    for(int j=0;j<m;++j) {
        out.weights.push_back(static_cast<double>(numerators[static_cast<std::size_t>(j)])/denominator);
        out.offsets.push_back(static_cast<std::int64_t>(j-available)*frequency);
    }
    if(side==X11SeasonalSide::right) {
        std::reverse(out.weights.begin(),out.weights.end());
        std::reverse(out.offsets.begin(),out.offsets.end());
        for(auto& offset:out.offsets)offset=-offset;
    }
    return out;
}
void apply(const MaskedSeries& input,MaskedSeries& output,std::size_t row,
    const X11FilterDefinition& filter) {
    for(std::size_t j=0;j<filter.weights.size();++j) {
        const auto at=static_cast<std::size_t>(static_cast<std::int64_t>(row)+filter.offsets[j]);
        if(filter.weights[j]!=0 && input.missing[at]) {output.missing[row]=1;return;}
    }
    double value=0;
    for(std::size_t j=0;j<filter.weights.size();++j) {
        if(filter.weights[j]==0)continue;
        const auto at=static_cast<std::size_t>(static_cast<std::int64_t>(row)+filter.offsets[j]);
        value+=filter.weights[j]*input.values[at];
        if(!std::isfinite(value))throw std::overflow_error("non-finite result in seasonal endpoint arithmetic");
    }
    output.values[row]=value;output.missing[row]=0;
}
}
int x11_seasonal_minimum_phase_length(int length) {
    validate_length(length);
    return length==15 ? 20 : std::max(5,length+2);
}
X11FilterDefinition x11_seasonal_endpoint_definition(int frequency,int length,
    int available,X11SeasonalSide side,const Interrupt& interrupt) {
    validate_length(length);
    x11_filter_definition(X11FilterKind::seasonal,frequency,length,interrupt);
    return endpoint(frequency,length,available,side);
}
MaskedSeries x11_seasonal_endpoints(const MaskedSeries& series,int frequency,
    int length,const Interrupt& interrupt) {
    validate_length(length);
    const auto full=x11_filter_definition(X11FilterKind::seasonal,frequency,length,interrupt);
    const auto f=static_cast<std::size_t>(frequency),n=series.values.size();
    const auto minimum=static_cast<std::size_t>(x11_seasonal_minimum_phase_length(length));
    if(n<f*minimum)throw std::invalid_argument("seasonal endpoints require at least "+std::to_string(minimum)+" observations in every phase");
    auto out=finite_filter(series,full.weights,full.offsets,interrupt);
    for(int q=0;q<(length+1)/2;++q) {
        const auto left=endpoint(frequency,length,q,X11SeasonalSide::left);
        const auto right=endpoint(frequency,length,q,X11SeasonalSide::right);
        for(std::size_t phase=0;phase<f;++phase) {
            if(interrupt)interrupt();
            const auto last=phase+((n-1-phase)/f)*f;
            apply(series,out,phase+static_cast<std::size_t>(q)*f,left);
            apply(series,out,last-static_cast<std::size_t>(q)*f,right);
        }
    }
    return out;
}
} // namespace persephone
