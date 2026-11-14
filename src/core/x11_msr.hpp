#ifndef PERSEPHONE_X11_MSR_HPP
#define PERSEPHONE_X11_MSR_HPP
#include "numerics.hpp"
#include <limits>
namespace persephone {
enum class X11MsrMode { absolute, relative };
enum class X11MsrStatus {
    available, insufficient_observations, missing_observations,
    nonpositive_relative_component, undefined_msr, numerical_range, incomplete_phases
};
struct X11MsrComponent {
    std::vector<double> raw_mean,corrected_mean,correction;
    std::vector<X11MsrStatus> status;
    double global_mean=std::numeric_limits<double>::quiet_NaN();
};
struct X11MsrTable {
    std::vector<std::size_t> observations,changes;
    X11MsrComponent seasonal,irregular;
    std::vector<double> ratios;
    std::vector<X11MsrStatus> phase_status;
    double global_msr=std::numeric_limits<double>::quiet_NaN();
    X11MsrStatus status=X11MsrStatus::incomplete_phases;
};
// Supplied components already share the same working domain. Period is 1-based.
// Published finite-length formulas with explicit X-11 decimal/change-count
// conventions. No pilot, trimming, transformation or automatic selection.
X11MsrTable x11_msr_table(const MaskedSeries& seasonal,const MaskedSeries& irregular,
    int frequency,int first_period,X11MsrMode mode,const Interrupt& interrupt={});
} // namespace persephone
#endif
