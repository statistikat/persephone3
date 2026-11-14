#ifndef PERSEPHONE_X11_MEAN_ENDPOINTS_HPP
#define PERSEPHONE_X11_MEAN_ENDPOINTS_HPP
#include "x11_filters.hpp"
namespace persephone {
struct X11MeanEndpointResult {
    MaskedSeries values;
    double left_mean=0,right_mean=0;
    bool left_missing=false,right_missing=false;
};
// Explicit mean padding of the centered 2 x frequency trend filter. Each
// padding value is the mean of the first/last half-width observations.
// At least frequency/2 inputs required; no automatic endpoint selection.
X11MeanEndpointResult x11_mean_endpoints(const MaskedSeries& series,int frequency,
    const Interrupt& interrupt={});
} // namespace persephone
#endif
