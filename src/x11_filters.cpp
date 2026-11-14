#include "core/x11_filters.hpp"
#include <cmath>
#include <stdexcept>
namespace persephone {
X11FilterDefinition x11_filter_definition(X11FilterKind kind,int frequency,int length,
    const Interrupt& interrupt) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("X-11 filters require quarterly or monthly frequency");
    if(interrupt)interrupt();
    X11FilterDefinition out;
    std::int64_t spacing=1;
    switch(kind) {
    case X11FilterKind::trend:
        if(length!=0)throw std::invalid_argument("centered trend length is determined by frequency");
        out.weights.assign(static_cast<std::size_t>(frequency+1),1./frequency);
        out.weights.front()/=2;out.weights.back()/=2;
        break;
    case X11FilterKind::seasonal:
        if(length!=1 && length!=3 && length!=5 && length!=9 && length!=15)
            throw std::invalid_argument("seasonal 3 x N requires N in 1,3,5,9,15");
        out.weights=convolve(std::vector<double>(3,1./3),
            std::vector<double>(static_cast<std::size_t>(length),1./length),interrupt);
        spacing=frequency;
        break;
    case X11FilterKind::henderson: {
        if(length<1 || length>101 || length%2==0)
            throw std::invalid_argument("Henderson length must be odd and between 1 and 101");
        // Cubic reproduction at lengths 1 and 3 gives the identity. Remove
        // exact zero outer taps so support and application remain canonical.
        if(length<=3) {out.weights={1};break;}
        const int half=length/2;
        const auto n=static_cast<long double>(half+2),n2=n*n;
        const auto denominator=8*n*(n2-1)*(4*n2-1)*(4*n2-9)*(4*n2-25);
        out.weights.resize(static_cast<std::size_t>(length));
        // Closed solution of minimum squared third differences of the
        // zero-extended weights, subject to reproducing a cubic polynomial.
        for(int j=0;j<=half;++j) {
            const auto j2=static_cast<long double>(j)*j;
            const auto value=315*((n-1)*(n-1)-j2)*(n2-j2)*((n+1)*(n+1)-j2)*(3*n2-16-11*j2)/denominator;
            const auto weight=static_cast<double>(value);
            if(!std::isfinite(weight))throw std::range_error("Henderson weight exceeds the finite range");
            out.weights[static_cast<std::size_t>(half+j)]=out.weights[static_cast<std::size_t>(half-j)]=weight;
        }
        break;
    }
    default:throw std::invalid_argument("unsupported X-11 filter family");
    }
    const auto half=static_cast<std::int64_t>(out.weights.size()/2);
    out.radius=spacing*half;out.offsets.reserve(out.weights.size());
    for(std::int64_t j=-half;j<=half;++j)out.offsets.push_back(spacing*j);
    return out;
}
MaskedSeries x11_symmetric_filter(const MaskedSeries& series,X11FilterKind kind,
    int frequency,int length,const Interrupt& interrupt) {
    const auto filter=x11_filter_definition(kind,frequency,length,interrupt);
    return finite_filter(series,filter.weights,filter.offsets,interrupt);
}
} // namespace persephone
