#ifndef PERSEPHONE_X11_TREND_SELECTION_HPP
#define PERSEPHONE_X11_TREND_SELECTION_HPP
#include "x11_filters.hpp"
#include <limits>
namespace persephone {
enum class X11ChangeMode { additive, relative };
enum class X11TrendSelectionStatus {
    available, insufficient_observations, missing_observations,
    nonpositive_relative_input, nonpositive_relative_trend,
    undefined_ic_ratio, numerical_range
};
// The comparison ratio is the mean absolute irregular change divided by the
// corresponding trend change, multiplied by three for quarterly series.
// Positive infinity selects the longest filter; NaN/negative ratios are invalid.
int x11_henderson_select(int frequency,double comparison_ic_ratio);
// Conventional endpoint ratio, distinct from the estimated selection ratio.
double x11_henderson_default_ratio(int frequency,int length);
struct X11TrendSelection {
    X11TrendSelectionStatus status=X11TrendSelectionStatus::insufficient_observations;
    int pilot_length=0,selected_length=0;
    std::size_t pilot_count=0,change_count=0;
    double trend_mean_change=std::numeric_limits<double>::quiet_NaN();
    double irregular_mean_change=std::numeric_limits<double>::quiet_NaN();
    double raw_ic_ratio=std::numeric_limits<double>::quiet_NaN();
    double comparison_ic_ratio=std::numeric_limits<double>::quiet_NaN();
    double endpoint_ic_ratio=std::numeric_limits<double>::quiet_NaN();
};
// Trim explicitly supplied endpoint extensions before the symmetric pilot.
// Relative changes require positive input/trend; additive input is already in
// its requested working scale. No log transform, imputation or B/C/D pass.
X11TrendSelection x11_trend_select(const MaskedSeries& series,int frequency,
    X11ChangeMode mode,std::size_t backcasts=0,std::size_t forecasts=0,
    const Interrupt& interrupt={});
} // namespace persephone
#endif
