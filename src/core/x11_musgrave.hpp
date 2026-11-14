#ifndef PERSEPHONE_X11_MUSGRAVE_HPP
#define PERSEPHONE_X11_MUSGRAVE_HPP
#include "x11_filters.hpp"
namespace persephone {
enum class X11EndpointSide { left, right };
// `available` is the number of observations remaining towards the selected
// boundary, from 0 through effective Henderson radius - 1. Left mirrors right.
// The finite nonnegative ratio is E|delta irregular| / |trend slope|.
X11FilterDefinition x11_musgrave_definition(int frequency,int length,int available,
    X11EndpointSide side,double ic_ratio,const Interrupt& interrupt={});
// Requires at least the effective symmetric length: no two-boundary truncation.
// Identity lengths 1/3 retain identity. No extrapolation or automatic I/C choice.
MaskedSeries x11_musgrave_filter(const MaskedSeries& series,int frequency,int length,
    double ic_ratio,const Interrupt& interrupt={});
} // namespace persephone
#endif
