#ifndef PERSEPHONE_X11_SEASONAL_ENDPOINTS_HPP
#define PERSEPHONE_X11_SEASONAL_ENDPOINTS_HPP
#include "x11_filters.hpp"
namespace persephone {
enum class X11SeasonalSide { left, right };
// N=1/3/5/9 only. `available` counts same-season observations towards the
// selected boundary; coefficients use signed original observation offsets.
X11FilterDefinition x11_seasonal_endpoint_definition(int frequency,int length,
    int available,X11SeasonalSide side,const Interrupt& interrupt={});
// Requires max(5,N+2) observations in every seasonal phase. Short-series selection,
// mixed phase choices and 3x15 endpoint weights are separate policies.
MaskedSeries x11_seasonal_endpoints(const MaskedSeries& series,int frequency,
    int length,const Interrupt& interrupt={});
} // namespace persephone
#endif
