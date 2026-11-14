#ifndef PERSEPHONE_X11_NORMALIZATION_HPP
#define PERSEPHONE_X11_NORMALIZATION_HPP
#include "x11_filters.hpp"
namespace persephone {
enum class X11NormalizationRule { nearest, requested_stable, short_span, short_3x15_span };
struct X11SeasonalNormalization {
    MaskedSeries values,normalization;
    std::vector<X11NormalizationRule> rules;
    // Original-input indices. Normalization sources identify the centered
    // annual mean used at each input row; output sources identify the original
    // normalized seasonal value repeated by explicit periodic restoration.
    std::vector<std::size_t> normalization_sources,output_sources;
};
// Additive working scale only. Fixed requested filters use 0 for stable and
// N for 3xN; these are not the fixed processor's length-adjusted choices.
// At least two annual cycles ensure all periodic mean sources are central.
// Extend both ends by repeating the nearest normalized annual cycle. This is
// restoration of a cropped seasonal domain, not ARIMA extrapolation.
X11SeasonalNormalization x11_seasonal_normalize(const MaskedSeries& series,
    int frequency,int first_period,const std::vector<int>& filters,
    std::size_t extend=0,const Interrupt& interrupt={});
} // namespace persephone
#endif
