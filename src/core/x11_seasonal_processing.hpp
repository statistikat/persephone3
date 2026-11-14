#ifndef PERSEPHONE_X11_SEASONAL_PROCESSING_HPP
#define PERSEPHONE_X11_SEASONAL_PROCESSING_HPP
#include "x11_seasonal_endpoints.hpp"
namespace persephone {
enum class X11ShortSeriesPolicy { reject, x11 };
enum class X11SeasonalRowMethod { stable, symmetric, left, right, short_mean };
enum class X11SeasonalSelectionReason {
    requested_filter, requested_stable, short_span, short_3x15_span, insufficient_endpoint_support
};
struct X11SeasonalProcessing {
    MaskedSeries values;
    std::vector<int> selected_filters;
    std::vector<std::size_t> phase_lengths;
    std::vector<X11SeasonalSelectionReason> reasons;
    std::vector<X11SeasonalRowMethod> row_methods;
    // -1 except for an asymmetric row's same-phase observations toward its end.
    std::vector<int> available;
};
// Filters are in calendar-period order, independent of input start. Each is
// 0 (stable) or N in 3xN. Requires at least one observation in every phase.
// `reject` retains explicit endpoint minimum lengths; `x11` implements the
// qualified fixed-choice short-series policy, not data-driven MSR selection.
X11SeasonalProcessing x11_seasonal_process(const MaskedSeries& series,int frequency,
    int start_period,const std::vector<int>& filters,X11ShortSeriesPolicy policy,
    const Interrupt& interrupt={});
} // namespace persephone
#endif
