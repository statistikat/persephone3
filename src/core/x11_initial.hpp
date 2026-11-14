#ifndef PERSEPHONE_X11_INITIAL_HPP
#define PERSEPHONE_X11_INITIAL_HPP
#include "x11_seasonal_processing.hpp"
#include "x11_normalization.hpp"
namespace persephone {
struct X11InitialAdditive {
    // B1 on the supplied span; all remaining tables on the central cropped
    // span. B4a and B4anorm are the values in seasonal and normalization.
    MaskedSeries input,trend,seasonal_irregular,irregular;
    X11SeasonalProcessing seasonal;
    X11SeasonalNormalization normalization;
    std::size_t drop=0;
    int first_period=1;
};
// Fixed additive initial B1/B2/B3/B4a/B4anorm/B4d only. There is no extreme
// correction, second trend estimate, complete B pass or final decomposition.
X11InitialAdditive x11_initial_additive(const MaskedSeries& series,int frequency,
    int first_period,const std::vector<int>& filters,const Interrupt& interrupt={});
} // namespace persephone
#endif
