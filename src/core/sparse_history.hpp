#ifndef PERSEPHONE_SPARSE_HISTORY_HPP
#define PERSEPHONE_SPARSE_HISTORY_HPP

#include "numerics.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace persephone {
namespace detail {

// Canonical SARIMA diffuse subspace, evaluated from factored homogeneous
// functions rather than recursively propagated floating history coordinates.
// The functions are normalized at the ORIGINAL history times 0,-1,...,1-k.
// Their current loading therefore preserves the original Euclidean diffuse
// coordinate measure; it is not a newly chosen flat prior at the current time.
class SparseHistoryBasis {
public:
    SparseHistoryBasis(int d, int seasonal_d, int period,
                       const Interrupt& interrupt = {});

    std::size_t dimension() const;
    std::size_t rank() const;
    std::size_t remaining_rank() const;
    std::size_t rebuild_count() const { return rebuild_count_; }
    double normalization_residual() const { return maximum_normalization_residual_; }

    // Observe each time at most once, in strictly increasing positive order.
    // Missing times must never be supplied to observe().
    void observe(std::uint64_t time, const Interrupt& interrupt = {});

    // Column-major k x remaining_rank loading for y_(t-1),...,y_(t-k).
    // Empty storage denotes an exhausted diffuse subspace.
    std::vector<double> loading(std::uint64_t prediction_time,
                                const Interrupt& interrupt = {}) const;

private:
    std::size_t identified_rank() const;
    int global_constraints() const;
    std::int64_t global_knot_sum() const;
    std::vector<double> functions(std::int64_t time) const;
    std::vector<double> evaluate(std::int64_t prediction_time,
                                 const Interrupt& interrupt) const;
    void rebuild(const Interrupt& interrupt);

    int d_, seasonal_d_, period_;
    std::array<int, 12> counts_{};
    std::array<std::array<std::int64_t, 2>, 12> knots_{};
    std::array<std::int64_t, 12> sums_{};
    std::uint64_t last_observed_ = 0;
    std::vector<double> column_scales_;
    std::vector<double> upper_;
    std::size_t rebuild_count_ = 0;
    double maximum_normalization_residual_ = 0;
};

} // namespace detail
} // namespace persephone
#endif
