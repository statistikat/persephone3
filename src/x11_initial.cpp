#include "core/x11_initial.hpp"
#include "core/parallel.hpp"
#include <cmath>
#include <stdexcept>
namespace persephone {
X11InitialAdditive x11_initial_additive(const MaskedSeries& series,int frequency,
    int first_period,const std::vector<int>& filters,const Interrupt& interrupt) {
    if(frequency!=4 && frequency!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(first_period<1 || first_period>frequency)throw std::invalid_argument("first period must be within the annual frequency");
    const auto f=static_cast<std::size_t>(frequency),h=f/2,n=series.values.size();
    if(filters.size()!=f)throw std::invalid_argument("provide one fixed seasonal request per calendar period");
    for(const auto choice:filters)if(choice!=0 && choice!=1 && choice!=3 && choice!=5 && choice!=9 && choice!=15)
        throw std::invalid_argument("unsupported fixed seasonal request");
    series.validate();
    if(n<3*f)throw std::invalid_argument("initial additive tables require at least three annual cycles");
    if(interrupt)interrupt();
    X11InitialAdditive out;out.drop=h;out.first_period=static_cast<int>((static_cast<std::size_t>(first_period-1)+h)%f+1);
    out.input={std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    parallel_for(n,1,interrupt,[&](std::size_t j){out.input.values[j]=series.values[j];out.input.missing[j]=series.missing[j];});
    const auto count=n-f;
    out.trend={std::vector<double>(count,0),std::vector<std::uint8_t>(count,0)};
    out.seasonal_irregular=out.trend;
    parallel_for(count,2*f+4,interrupt,[&](std::size_t j) {
        const auto row=j+h;
        for(std::size_t k=j;k<=j+f;++k)if(series.missing[k]) {
            out.trend.missing[j]=out.seasonal_irregular.missing[j]=1;return;
        }
        // The annual kernel is symmetric: accumulate its center, then equal
        // distance pairs. This limits rounding-order differences before the
        // subtraction of a large level to obtain a small SI component.
        const double unit=1./static_cast<double>(frequency);
        double mean=unit*series.values[row];
        for(std::size_t k=1;k<=h;++k) {
            const double weight=k==h ? .5*unit : unit;
            const double pair=series.values[row-k]+series.values[row+k];
            // A pair can overflow while its weighted contribution is finite.
            const double term=std::isfinite(pair) ? weight*pair :
                weight*series.values[row-k]+weight*series.values[row+k];
            mean+=term;
            if(!std::isfinite(mean))throw std::overflow_error("non-finite initial annual mean");
        }
        out.trend.values[j]=mean;
        const auto value=series.values[row]-mean;
        if(!std::isfinite(value))throw std::overflow_error("non-finite initial seasonal-irregular component");
        out.seasonal_irregular.values[j]=value;
    });
    out.seasonal=x11_seasonal_process(out.seasonal_irregular,frequency,out.first_period,filters,X11ShortSeriesPolicy::x11,interrupt);
    out.normalization=x11_seasonal_normalize(out.seasonal.values,frequency,out.first_period,filters,0,interrupt);
    out.irregular={std::vector<double>(count,0),std::vector<std::uint8_t>(count,0)};
    parallel_for(count,2,interrupt,[&](std::size_t j) {
        if(out.seasonal_irregular.missing[j] || out.normalization.values.missing[j])out.irregular.missing[j]=1;
        else {
            const auto value=out.seasonal_irregular.values[j]-out.normalization.values.values[j];
            if(!std::isfinite(value))throw std::overflow_error("non-finite initial irregular component");
            out.irregular.values[j]=value;
        }
    });
    return out;
}
} // namespace persephone
