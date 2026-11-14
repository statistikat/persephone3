#ifndef PERSEPHONE_X11_MSR_SELECTION_HPP
#define PERSEPHONE_X11_MSR_SELECTION_HPP
#include "x11_msr.hpp"
namespace persephone {
enum class X11MsrPilotStatus {
    available, insufficient_observations, missing_observations,
    nonpositive_relative_input, numerical_range
};
struct X11MsrPilot {
    X11MsrPilotStatus status=X11MsrPilotStatus::insufficient_observations;
    // Working MSR components: difference or ratio. The difference is centered
    // at zero also for pseudo-additive MSR; no unit shift/decomposition pass.
    MaskedSeries seasonal,irregular;
};
X11MsrPilot x11_msr_pilot(const MaskedSeries& series,int frequency,
    X11MsrMode mode,const Interrupt& interrupt={});
enum class X11MsrBand { unavailable, s3x3, lower_gap, s3x5, upper_gap, s3x9 };
// Nonnegative scalar ratio; positive infinity is meaningful, NaN is invalid.
// Exact boundaries: [2.5,3.5) and [5.5,6.5) are ambiguous.
X11MsrBand x11_msr_band(double ratio);
enum class X11MsrIterationPolicy { jdplus, x11_five_year_cap };
enum class X11MsrSelectionStatus {
    available, missing_observations, nonpositive_relative_input,
    undefined_msr, numerical_range, incomplete_table, budget_exhausted
};
enum class X11MsrSelectionReason {
    unavailable, decisive_ratio, insufficient_initial_history,
    insufficient_remaining_history, five_year_cap, iteration_budget
};
struct X11MsrAttempt {
    std::size_t count=0,years_removed=0;
    double global_msr=std::numeric_limits<double>::quiet_NaN();
    X11MsrBand band=X11MsrBand::unavailable;
    X11MsrSelectionStatus status=X11MsrSelectionStatus::incomplete_table;
};
struct X11MsrSelection {
    X11MsrSelectionStatus status=X11MsrSelectionStatus::incomplete_table;
    X11MsrSelectionReason reason=X11MsrSelectionReason::unavailable;
    int selected_filter=0,first_period=1;
    bool using_default=false,has_table=false;
    std::size_t estimation_count=0,partial_year_trim=0,calendar_count=0,final_count=0;
    std::vector<X11MsrAttempt> attempts;
    X11MsrPilot pilot;
    X11MsrTable table;
};
// Trim explicit extensions and the trailing partial calendar year. Retain the
// initial partial year. At least 5*f rows are needed for each MSR evaluation.
// Defaults are selected only by method rules; a resource-budget stop is never
// a chosen filter. The final evaluated table/pilot and all attempts are retained.
X11MsrSelection x11_msr_select(const MaskedSeries& series,int frequency,int first_period,
    X11MsrMode mode,X11MsrIterationPolicy policy=X11MsrIterationPolicy::jdplus,
    std::size_t backcasts=0,std::size_t forecasts=0,std::size_t max_iterations=1000,
    const Interrupt& interrupt={});
} // namespace persephone
#endif
