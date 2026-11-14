#ifndef PERSEPHONE_X11_FILTERS_HPP
#define PERSEPHONE_X11_FILTERS_HPP
#include "numerics.hpp"
namespace persephone {
enum class X11FilterKind { trend, seasonal, henderson };
struct X11FilterDefinition {
    std::vector<double> weights;
    std::vector<std::int64_t> offsets;
    std::int64_t radius=0;
};
// Only monthly/quarterly central filters. `length` is 0 for the centered
// 2 x frequency trend, N for seasonal 3 x N, or the full odd Henderson length.
// Endpoints, automatic selection and decomposition passes are separate APIs.
X11FilterDefinition x11_filter_definition(X11FilterKind kind,int frequency,int length,
    const Interrupt& interrupt={});
// Full calendar, with unsupported endpoints missing. Henderson lengths 1 and 3
// are canonically the identity (zero radius). Zero-weight taps do not propagate
// internal missing values. No extrapolation or renormalization.
MaskedSeries x11_symmetric_filter(const MaskedSeries& series,X11FilterKind kind,
    int frequency,int length,const Interrupt& interrupt={});
} // namespace persephone
#endif
