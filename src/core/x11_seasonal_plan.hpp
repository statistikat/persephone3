#ifndef PERSEPHONE_X11_SEASONAL_PLAN_HPP
#define PERSEPHONE_X11_SEASONAL_PLAN_HPP
#include <vector>
namespace persephone {
enum class X11SeasonalRequest { stable=0,s3x1=1,s3x3=3,s3x5=5,s3x9=9,s3x15=15,x11default=-1,msr=-2 };
enum class X11SeasonalStage { b_initial,b_final,c_initial,c_final,d_initial,d_final };
enum class X11SeasonalResolution { fixed,x11default_initial,x11default_final,msr_initial,msr_preliminary,msr_mixed,requires_msr };
struct X11SeasonalPlan {
    std::vector<int> base_filters;
    std::vector<X11SeasonalResolution> resolutions;
    bool global_msr=false,requires_msr=false;
};
// Requests are in calendar-period order. This bounded policy does no numerical
// filtering or length-based fallback. A final all-MSR request stays unresolved;
// base_filters are provisional and must not be used as resolved final choices.
X11SeasonalPlan x11_seasonal_plan(int frequency,const std::vector<X11SeasonalRequest>& requests,
    X11SeasonalStage stage);
} // namespace persephone
#endif
