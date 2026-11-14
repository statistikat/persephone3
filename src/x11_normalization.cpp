#include "core/x11_normalization.hpp"
#include "core/parallel.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace persephone {
X11SeasonalNormalization x11_seasonal_normalize(const MaskedSeries& series,
    int frequency,int first_period,const std::vector<int>& filters,
    std::size_t extend,const Interrupt& interrupt) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(first_period<1 || first_period>frequency)throw std::invalid_argument("first period must be within the annual frequency");
    const auto f=static_cast<std::size_t>(frequency),h=f/2,n=series.values.size();
    if(filters.size()!=f)throw std::invalid_argument("provide one fixed seasonal request per calendar period");
    for(const auto filter:filters)if(filter!=0 && filter!=1 && filter!=3 && filter!=5 && filter!=9 && filter!=15)
        throw std::invalid_argument("unsupported fixed seasonal request");
    series.validate();
    if(n<2*f)throw std::invalid_argument("seasonal normalization requires at least two annual cycles");
    if(extend>(std::numeric_limits<std::size_t>::max()-n)/2)throw std::invalid_argument("normalization extension exceeds supported length");
    if(interrupt)interrupt();
    const auto central=x11_symmetric_filter(series,X11FilterKind::trend,frequency,0,interrupt);
    X11SeasonalNormalization out;const auto total=n+2*extend;
    out.values={std::vector<double>(total,0),std::vector<std::uint8_t>(total,0)};
    out.normalization={std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    out.rules.resize(f);out.normalization_sources.resize(n);out.output_sources.resize(total);
    for(std::size_t p=0;p<f;++p) {
        out.rules[p]=filters[p]==0 ? X11NormalizationRule::requested_stable :
            (n/f<5 ? X11NormalizationRule::short_span :
                (filters[p]==15 && n/f+(n%f!=0)<20 ? X11NormalizationRule::short_3x15_span : X11NormalizationRule::nearest));
    }
    // Read only the immutable central means: mixed endpoint policies cannot
    // introduce order-dependent reads from another just-filled boundary row.
    parallel_for(n,8,interrupt,[&](std::size_t j) {
        const auto p=(j%f+static_cast<std::size_t>(first_period-1))%f;
        const bool periodic=out.rules[p]!=X11NormalizationRule::nearest;
        const auto source=j<h ? (periodic ? j+f : h) : (j>=n-h ? (periodic ? j-f : n-h-1) : j);
        out.normalization_sources[j]=source;out.output_sources[extend+j]=j;
        out.normalization.values[j]=central.values[source];out.normalization.missing[j]=central.missing[source];
        if(series.missing[j] || central.missing[source])out.values.missing[extend+j]=1;
        else {
            const auto value=series.values[j]-central.values[source];
            if(!std::isfinite(value))throw std::overflow_error("non-finite additive seasonal normalization");
            out.values.values[extend+j]=value;
        }
    });
    // Extension reads only the completed original span, even across >1 year.
    parallel_for(2*extend,4,interrupt,[&](std::size_t j) {
        const auto row=j<extend ? j : n+j;
        const auto source=j<extend ? (f-extend%f+j%f)%f : n-f+(j-extend)%f;
        out.output_sources[row]=source;out.values.values[row]=out.values.values[extend+source];
        out.values.missing[row]=out.values.missing[extend+source];
    });
    return out;
}
} // namespace persephone
