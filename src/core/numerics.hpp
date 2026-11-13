#ifndef PERSEPHONE_NUMERICS_HPP
#define PERSEPHONE_NUMERICS_HPP

// Independently authored numerical building blocks; no upstream Java code is
// translated or incorporated here. These are not a seasonal-adjustment engine.
#include <cstddef>
#include <cstdint>
#include <functional>
#include <utility>
#include <vector>

namespace persephone {

using Interrupt = std::function<void()>;

// A regular monthly/quarterly domain. Ordinals count periods from year zero;
// using integers avoids floating-point year/period drift and supports years < 0.
class TimeDomain {
public:
    TimeDomain(std::int64_t year, int period, int frequency, std::int64_t length);
    std::pair<std::int64_t, int> at(std::int64_t offset) const;
    std::int64_t length() const noexcept { return length_; }
    int frequency() const noexcept { return frequency_; }

private:
    std::int64_t start_;
    std::int64_t length_;
    int frequency_;
};

// Missing values have a separate mask and zero in the owned numeric storage.
// Algorithms must consult the mask; infinity is never a missing observation.
struct MaskedSeries {
    std::vector<double> values;
    std::vector<std::uint8_t> missing;
    void validate() const;
};

// Polynomial coefficients are ordered by increasing powers of the lag B.
std::vector<double> convolve(const std::vector<double>& left,
                             const std::vector<double>& right,
                             const Interrupt& interrupt = {});

// (1 - B)^d (1 - B^frequency)^seasonal_d. The explicit order bound is a
// resource/overflow guard for this internal primitive, not a fitted-model rule.
std::vector<double> difference_polynomial(int d, int seasonal_d, int frequency,
                                         const Interrupt& interrupt = {});

// result[i] = sum_j weights[j] * series[i + offsets[j]]. The original length
// is retained. A nonzero tap outside the input or on a missing value makes the
// result missing. Zero taps have no domain or missingness effect. Offsets must
// be unique. Missing endpoints are not imputed or extrapolated.
MaskedSeries finite_filter(const MaskedSeries& series,
                           const std::vector<double>& weights,
                           const std::vector<std::int64_t>& offsets,
                           const Interrupt& interrupt = {});

} // namespace persephone
#endif
