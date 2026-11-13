#include "core/diffuse_state_space.hpp"
#include "core/exact_history.hpp"

#include <cassert>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <string>

template<class Function>
bool rejects(Function function) {
    try { function(); } catch (const std::exception&) { return true; }
    return false;
}

int main() {
    using namespace persephone;
    // Equal phase sums can leave a global direction unidentified even when
    // each of two seasons has two observations: {1,9}, {3,7} both sum to 10.
    // A subsequent observation at 11 supplies a new independent direction.
    detail::CountsHistoryCertificate equal_sums(2, 1, 4);
    for (std::uint64_t t = 1; t <= 11; ++t) {
        const bool observed = t == 1 || t == 3 || t == 7 || t == 9 || t == 11;
        const bool independent = equal_sums.classify_current(observed);
        if (t == 9) {
            assert(!independent);
            assert(equal_sums.rank() == 3);
        }
        if (t == 11) {
            assert(independent);
            assert(equal_sums.rank() == 4);
        }
        equal_sums.advance();
    }
    // A hypothetical gap row contributes no observed rank.
    detail::CountsHistoryCertificate missing_history(1, 0, 4);
    assert(missing_history.classify_current(false));
    assert(missing_history.rank() == 0);
    missing_history.advance(50);
    assert(missing_history.classify_current(true));
    assert(missing_history.rank() == 1);
    missing_history.advance(std::numeric_limits<std::uint64_t>::max()); // Full rank stops growth.
    assert(!missing_history.classify_current(true));
    // Checked integer failures are exercised without allocating enormous data.
    detail::CountsHistoryCertificate large_history(2, 2, 12);
    const auto uint_max = std::numeric_limits<std::uint64_t>::max();
    assert(rejects([&] { large_history.validate_length(uint_max / 3 + 1); }));
    assert(rejects([&] { large_history.advance(uint_max); }));
    large_history.advance(uint_max / 3 + 10);
    assert(large_history.classify_current(true));
    large_history.advance(12);
    assert(large_history.classify_current(true));
    large_history.advance(12);
    assert(rejects([&] { large_history.classify_current(true); }));
    assert(large_history.rank() == 2); // Rejected addition leaves rank intact.

    const MaskedSeries values{{1, 3, 0, 2, 6}, {0, 0, 1, 0, 0}};
    const auto rw = sarima_innovations(values, {}, {}, {}, {}, 4, 1, 0);
    const auto& result = rw.likelihood;
    assert(result.initial_diffuse_rank == 1 && result.remaining_diffuse_rank == 0);
    assert(result.diffuse_count == 1 && result.finite_count == 3 && result.observed_count == 4);
    assert(result.update_kind == std::vector<int>({1, 2, 0, 2, 2}));
    assert(result.standardized_innovations.missing == std::vector<std::uint8_t>({1, 0, 1, 0, 0}));
    assert(std::abs(result.finite_variances[3] - 2) < 1e-12);
    assert(std::abs(result.sum_squared_standardized - 20.5) < 1e-12);
    assert(std::abs(result.sum_log_finite_variances - std::log(2.0)) < 1e-12);
    assert(result.sum_log_diffuse_variances == 0);
    assert(result.numerics.exact_history_certification);
    assert(result.numerics.exact_history_pivot_count == 1);
    assert(result.numerics.exact_history_zero_count == 0);
    assert(std::abs(result.fixed_log_likelihood + .5 * (3 * std::log(2 * std::acos(-1.0)) + std::log(2.0) + 20.5)) < 1e-12);

    // A finite observation can be informative while a different initial
    // coordinate remains diffuse and unobserved.
    const DiffuseStateSpace mixed{DenseMatrix(2, 2, {1, 0, 0, .5}), {0, 1},
                                  DenseMatrix(2, 1, {0, 1}), .2};
    const DiffuseInitialState start{{0, 0}, DenseMatrix(2, 1, {0, 1}), {1, 0}, 1};
    const auto unresolved = diffuse_innovations(values, mixed, start);
    assert(unresolved.remaining_diffuse_rank == 1 && unresolved.diffuse_count == 0);
    assert(unresolved.finite_count == 4 && std::isnan(unresolved.fixed_log_likelihood));
    assert(unresolved.likelihood_status == "unresolved_diffuse");
    assert(unresolved.profile_status == "unresolved_diffuse");
    assert(!unresolved.numerics.exact_history_certification);
    assert(unresolved.numerics.exact_history_zero_count == 0);
    assert(unresolved.numerics.exact_history_pivot_count == 0);

    // Exact T=cI preserves observation-null subspaces even when Householder
    // arithmetic leaves roundoff in a mixed-coordinate basis.
    const DiffuseStateSpace static_mixed{DenseMatrix(2, 2, {1, 0, 0, 1}), {1, 2},
                                         DenseMatrix(2, 2, {1, 0, 0, 1}), .2};
    const DiffuseInitialState nonorthogonal{{0, 0}, DenseMatrix(2, 2, {1, 0, 0, 1}),
                                           {1, .2, .3, 1}, 2};
    const auto static_result = diffuse_innovations(values, static_mixed, nonorthogonal);
    assert(static_result.diffuse_count == 1 && static_result.finite_count == 3);
    assert(static_result.remaining_diffuse_rank == 1);
    assert(std::isnan(static_result.fixed_log_likelihood));
    assert(static_result.diffuse_variances[1] == 0);

    // Scaling the flat-prior coordinate changes only its explicit density
    // normalization, not finite filtering once that coordinate is identified.
    const DiffuseStateSpace scalar{DenseMatrix(1, 1, {1}), {1}, DenseMatrix(1, 1, {1}), 0};
    const DiffuseInitialState scaled{{0}, DenseMatrix(1, 1, {0}), {3}, 1};
    const auto scaled_result = diffuse_innovations(values, scalar, scaled);
    assert(std::abs(scaled_result.fixed_log_likelihood - result.fixed_log_likelihood + std::log(3.0)) < 1e-12);
    assert(scaled_result.standardized_innovations.values == result.standardized_innovations.values);
    const auto no_finite = diffuse_innovations({{1}, {0}}, scalar, scaled);
    assert(no_finite.profile_status == "no_finite_innovations");
    assert(std::isnan(no_finite.profiled_variance));
    assert(std::abs(no_finite.fixed_log_likelihood + std::log(3.0)) < 1e-12);
    const auto zero = diffuse_innovations({{1, 1, 1}, {0, 0, 0}}, scalar, scaled);
    assert(zero.profile_status == "zero_scale_boundary");
    assert(zero.profiled_log_likelihood == std::numeric_limits<double>::infinity());

    auto killed = scalar;
    killed.transition(0, 0) = 0;
    assert(rejects([&] { diffuse_innovations({{0, 1}, {1, 0}}, killed, scaled); }));
    assert(rejects([&] { diffuse_innovations(values, mixed,
        DiffuseInitialState{{0, 0}, DenseMatrix(2, 1, {0, 0}), {1, 0, 2, 0}, 2}); }));
    assert(rejects([&] { sarima_innovations(values, {}, {}, {}, {}, 4, 1, 0, 1); }));
    assert(rejects([&] { sarima_innovations(values, {}, {}, {}, {}, 12, 3, 0); }));
    assert(rejects([&] { sarima_innovations(values, {}, {}, {}, {}, 1, 0, 0); }));

    // Genuinely weak observability is rejected, never silently reclassified as
    // finite just because its diffuse variance is numerically small.
    auto weak = mixed;
    weak.observation = {1e-15, 1};
    assert(rejects([&] { diffuse_innovations(values, weak, start); }));

    MaskedSeries combined{std::vector<double>(12), std::vector<std::uint8_t>(12, 0)};
    for (std::size_t i = 0; i < combined.values.size(); ++i) combined.values[i] = static_cast<double>(i * i + 1);
    const auto seasonal = sarima_innovations(combined, {}, {}, {}, {}, 4, 1, 1);
    assert(seasonal.likelihood.diffuse_count == 5);
    assert(seasonal.likelihood.finite_count == 7);
    assert(std::abs(seasonal.likelihood.sum_log_diffuse_variances) < 1e-12);

    // SARIMA's next-history row is exactly Z: an observed history entry has
    // exactly zero diffuse loading, regardless of finite uncertainty/noise.
    MaskedSeries second_order{std::vector<double>(90), std::vector<std::uint8_t>(90, 0)};
    for (std::size_t i = 0; i < second_order.values.size(); ++i) second_order.values[i] = std::sin(static_cast<double>(i) / 3);
    for (const std::size_t i : {0U, 2U, 4U, 5U, 15U, 20U, 22U, 85U}) {
        second_order.values[i] = 0;
        second_order.missing[i] = 1;
    }
    for (const int period : {4, 12}) {
        const auto second = sarima_innovations(second_order, {}, {}, {}, {}, period, 0, 2);
        assert(second.likelihood.remaining_diffuse_rank == 0);
        assert(second.likelihood.diffuse_count == static_cast<std::size_t>(2 * period));
        assert(second.likelihood.finite_count + second.likelihood.diffuse_count == 82);
        assert(second.likelihood.profile_status == "available");
    }
    // Registered combined-order gap cases previously encountered cancellation
    // in a truly dependent diffuse observation. Every case must now succeed.
    for (const auto orders : {std::array<int, 3>{4, 2, 1}, {12, 1, 2}, {12, 2, 2}}) {
        const auto gap = sarima_innovations(second_order, {}, {}, {}, {}, orders[0], orders[1], orders[2]);
        const auto& fit = gap.likelihood;
        const auto dimension = static_cast<std::size_t>(orders[1] + orders[0] * orders[2]);
        assert(fit.remaining_diffuse_rank == 0);
        assert(fit.diffuse_count == dimension);
        assert(fit.finite_count + fit.diffuse_count == 82);
        assert(fit.profile_status == "available");
        assert(fit.numerics.exact_history_certification);
        assert(fit.numerics.exact_history_pivot_count == dimension);
        assert(fit.numerics.exact_history_zero_count > 0);
    }

    int polls = 0;
    sarima_innovations(combined, {}, {}, {}, {}, 4, 1, 1, 0, 1, [&] { ++polls; });
    assert(polls > 5);
    for (int cancel_at = 1; cancel_at <= polls; ++cancel_at) {
        int count = 0;
        bool cancelled = false;
        try {
            sarima_innovations(combined, {}, {}, {}, {}, 4, 1, 1, 0, 1, [&] {
                if (++count == cancel_at) throw std::runtime_error("cancelled");
            });
        } catch (const std::runtime_error& error) {
            cancelled = std::string(error.what()) == "cancelled";
        }
        assert(cancelled);
    }
    assert(sarima_innovations(values, {}, {}, {}, {}, 4, 1, 0).likelihood.fixed_log_likelihood == result.fixed_log_likelihood);
}
