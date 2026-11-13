#ifndef PERSEPHONE_EXACT_HISTORY_HPP
#define PERSEPHONE_EXACT_HISTORY_HPP

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <stdexcept>

namespace persephone {
namespace detail {

// Exact observed-row rank for Delta=(1-B)^d(1-B^s)^D, d,D <= 2.
// A basis of its homogeneous solutions consists of seasonal polynomials of
// degree < D and the d global monomials t^D,...,t^(D+d-1). This basis is
// invertibly related to the original initial-history coordinates. It is used
// ONLY for rank certification, never to transform the filter's diffuse basis.
//
// Each phase contributes min(count,D) seasonal pivots. D-th divided differences
// annihilate those columns and map the global columns to [1] or [1,sum(knots)].
// Thus two global pivots exist exactly when qualifying phases have different
// first-(D+1)-time sums, or one phase supplies at least D+2 distinct times.
// See the divided-difference/Newton identities in NIST DLMF 3.3.35 and 3.3.38;
// the bounded-order rank formula and implementation are independently derived.
class CountsHistoryCertificate {
public:
    CountsHistoryCertificate(int d, int seasonal_d, int period)
        : d_(d), seasonal_d_(seasonal_d), period_(period) {
        if (d < 0 || d > 2 || seasonal_d < 0 || seasonal_d > 2 ||
            (period != 4 && period != 12)) {
            throw std::invalid_argument("exact history certification requires d,D in 0..2 and period 4 or 12");
        }
    }

    std::size_t dimension() const {
        return static_cast<std::size_t>(d_ + period_ * seasonal_d_);
    }

    std::size_t rank() const { return rank_; }

    void validate_length(std::uint64_t length) const {
        // No sum contains more than D+1 (at most three) time indices.
        if (length > std::numeric_limits<std::uint64_t>::max() /
                         static_cast<std::uint64_t>(seasonal_d_ + 1)) {
            throw std::overflow_error("exact history time-index sum exceeds uint64 range");
        }
    }

    // Missing rows are classified hypothetically and never enter observed rank.
    // Call once per time, followed by advance(); stop accumulating at full rank.
    bool classify_current(bool observed) {
        if (rank_ == dimension()) return false;
        auto counts = counts_;
        auto sums = sums_;
        const auto phase = static_cast<std::size_t>(time_ % static_cast<std::uint64_t>(period_));
        if (counts[phase] < seasonal_d_ + 1) {
            if (sums[phase] > std::numeric_limits<std::uint64_t>::max() - time_) {
                throw std::overflow_error("exact history time-index sum exceeds uint64 range");
            }
            sums[phase] += time_;
        }
        if (counts[phase] < seasonal_d_ + 2) ++counts[phase];
        const auto candidate = rank_of(counts, sums);
        if (candidate < rank_ || candidate > rank_ + 1 || candidate > dimension()) {
            throw std::logic_error("exact history certificate has inconsistent row rank");
        }
        const bool independent = candidate > rank_;
        if (observed) {
            counts_ = counts;
            sums_ = sums;
            rank_ = candidate;
        }
        return independent;
    }

    void advance(std::uint64_t periods = 1) {
        if (rank_ == dimension()) return;
        if (periods > std::numeric_limits<std::uint64_t>::max() - time_) {
            throw std::overflow_error("exact history time index exceeds uint64 range");
        }
        time_ += periods;
    }

private:
    std::size_t rank_of(const std::array<int, 12>& counts,
                        const std::array<std::uint64_t, 12>& sums) const {
        std::size_t base = 0;
        int extra = 0;
        std::uint64_t first_sum = 0;
        bool qualified = false;
        for (std::size_t phase = 0; phase < static_cast<std::size_t>(period_); ++phase) {
            base += static_cast<std::size_t>(std::min(counts[phase], seasonal_d_));
            if (d_ > 0 && counts[phase] >= seasonal_d_ + 1) {
                extra = std::max(extra, 1);
                if (d_ == 2 && (counts[phase] >= seasonal_d_ + 2 ||
                               (qualified && first_sum != sums[phase]))) extra = 2;
                if (!qualified) first_sum = sums[phase];
                qualified = true;
            }
        }
        return base + static_cast<std::size_t>(extra);
    }

    int d_, seasonal_d_, period_;
    std::array<int, 12> counts_{};
    std::array<std::uint64_t, 12> sums_{};
    std::uint64_t time_ = 1;
    std::size_t rank_ = 0;
};

} // namespace detail
} // namespace persephone
#endif
