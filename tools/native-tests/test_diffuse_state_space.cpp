#include "core/diffuse_state_space.hpp"
#include "core/exact_history.hpp"
#include "core/finite_state_factor.hpp"

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
    // A singular proper covariance must retain its nonzero direction through
    // conditioning and prediction. Expectations below are direct 2x2 Gaussian
    // identities for P = [2,1]'[2,1], z=[1,.5], and H=.25.
    detail::FiniteStateFactor finite(DenseMatrix(2, 2, {4, 2, 2, 1}));
    const auto finite_observation = finite.observation({1, .5}, .25);
    assert(std::abs(finite_observation.variance - 6.5) < 1e-12);
    assert(std::abs(finite_observation.pz[0] - 5) < 1e-12);
    assert(std::abs(finite_observation.pz[1] - 2.5) < 1e-12);
    finite.update({10.0 / 13, 5.0 / 13}, {1, .5}, .25);
    auto finite_covariance = finite.covariance();
    assert(std::abs(finite_covariance(0, 0) - 2.0 / 13) < 1e-12);
    assert(std::abs(finite_covariance(0, 1) - 1.0 / 13) < 1e-12);
    assert(std::abs(finite_covariance(1, 1) - 1.0 / 26) < 1e-12);
    finite.predict(DenseMatrix(2, 2, {1, 0, .2, .5}),
                   DenseMatrix(2, 2, {.3, .1, 0, .4}));
    finite_covariance = finite.covariance();
    assert(std::abs(finite_covariance(0, 0) - (4.84 / 26 + .09)) < 1e-12);
    assert(std::abs(finite_covariance(0, 1) - (1.1 / 26 + .03)) < 1e-12);
    assert(std::abs(finite_covariance(1, 1) - (.25 / 26 + .17)) < 1e-12);

    // Small positive covariance directions are not removed by a rank cutoff,
    // including after rectangular QR compression during prediction.
    detail::FiniteStateFactor tiny_finite(DenseMatrix(2, 2, {1, 0, 0, 1e-24}));
    assert(std::abs(tiny_finite.observation({0, 1}, 0).variance / 1e-24 - 1) < 1e-12);
    tiny_finite.predict(DenseMatrix(2, 2, {1, 0, 0, 1}), DenseMatrix(2, 1, {0, 0}));
    assert(std::abs(tiny_finite.observation({0, 1}, 0).variance / 1e-24 - 1) < 1e-12);
    assert(rejects([&] { detail::FiniteStateFactor bad(DenseMatrix(2, 2, {1, 0, 0, -.1})); }));
    assert(rejects([&] { detail::FiniteStateFactor bad(DenseMatrix(2, 2, {1, 1, 0, 1})); }));
    assert(rejects([&] { detail::FiniteStateFactor bad(DenseMatrix(1, 1,
        {std::numeric_limits<double>::infinity()})); }));
    assert(rejects([&] { finite.observation({1}, 0); }));
    assert(rejects([&] { finite.observation({1, std::numeric_limits<double>::quiet_NaN()}, 0); }));
    assert(rejects([&] { finite.observation({1, 0}, -.1); }));
    assert(rejects([&] { finite.update({1}, {1, 0}, 0); }));
    assert(rejects([&] { finite.predict(DenseMatrix(1, 1, {1}), DenseMatrix(2, 1, {0, 0})); }));
    assert(rejects([&] { finite.predict(DenseMatrix(2, 2, {1, 0, 0, 1}), DenseMatrix(1, 1, {0})); }));

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

    // A sparse same-phase history formerly accumulated recurrence error before
    // time 90. It has only three identified coordinates and no full density.
    MaskedSeries sparse{std::vector<double>(90, 0), std::vector<std::uint8_t>(90, 1)};
    for (const std::size_t index : {0U, 20U, 40U, 60U, 80U}) {
        const auto time = static_cast<double>(index + 1);
        sparse.values[index] = std::sin(time * std::sqrt(2.0)) + std::cos(time * std::sqrt(3.0)) / 2;
        sparse.missing[index] = 0;
    }
    const auto sparse_fit = sarima_innovations(sparse, {}, {}, {}, {}, 4, 2, 1).likelihood;
    assert(sparse_fit.remaining_diffuse_rank == 3);
    assert(sparse_fit.diffuse_count == 3 && sparse_fit.finite_count == 2);
    assert(sparse_fit.likelihood_status == "unresolved_diffuse");
    assert(std::isnan(sparse_fit.fixed_log_likelihood));
    assert(sparse_fit.numerics.stable_history_propagation);
    assert(sparse_fit.numerics.history_reconstructions > 0);

    // The final four times identify every original history coordinate. Constants
    // below come from independent base-R differencing of I followed by Gaussian
    // QR integration over missing coordinates (test-native-diffuse.R), not a
    // native filter replay or a changed flat-prior normalization.
    for (std::size_t index = 86; index < 90; ++index) {
        const auto time = static_cast<double>(index + 1);
        sparse.values[index] = std::sin(time * std::sqrt(2.0)) + std::cos(time * std::sqrt(3.0)) / 2;
        sparse.missing[index] = 0;
    }
    const auto late = sarima_innovations(sparse, {}, {}, {}, {}, 4, 2, 1).likelihood;
    assert(late.remaining_diffuse_rank == 0 && late.diffuse_count == 6 && late.finite_count == 3);
    assert(late.likelihood_status == "available" && late.profile_status == "available");
    assert(std::abs(late.fixed_log_likelihood + 24.788587348806054) < 1e-7);
    assert(std::abs(late.profiled_log_likelihood + 16.303203434824791) < 1e-7);
    assert(std::abs(late.sum_squared_standardized - .0038602490727738231) < 1e-10);
    assert(std::abs(late.sum_log_finite_variances + late.sum_log_diffuse_variances -
                    44.059683249311298) < 1e-7);

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
