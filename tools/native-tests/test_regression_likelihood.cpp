#include "core/regression_likelihood.hpp"
#include "core/parallel.hpp"

#include <cassert>
#include <cmath>
#include <limits>
#include <iostream>
#include <stdexcept>
#include <string>

namespace {
bool close(double first, double second, double tolerance = 1e-10) {
    return std::abs(first - second) <= tolerance * (1 + std::abs(second));
}
template<class Function> void rejects(const Function& function) {
    bool rejected = false;
    try { function(); } catch (const std::exception&) { rejected = true; }
    assert(rejected);
}
template<class Function> void covariance_underflows(const Function& function) {
    bool rejected = false;
    try { function(); } catch (const std::underflow_error& error) {
        rejected = std::string(error.what()).find("covariance") != std::string::npos;
    }
    assert(rejected);
}
void thread_setting(const char* value) {
#ifdef _WIN32
    assert(_putenv_s("PERSEPHONE_NUM_THREADS", value ? value : "") == 0);
#else
    if (value) assert(setenv("PERSEPHONE_NUM_THREADS", value, 1) == 0);
    else assert(unsetenv("PERSEPHONE_NUM_THREADS") == 0);
#endif
}
}

int main() {
    using namespace persephone;
    // Orthogonal white-noise GLS: errors have zero sum and zero linear moment.
    // Observed x=-2..2, errors=(1,-2,2,-2,1), hence beta=(2,3), Q=14.
    MaskedSeries values{{0, -3, -3, 4, 3, 9, 0}, {1, 0, 0, 0, 0, 0, 1}};
    RegressionDesign design{2, {1, 1, 1, 1, 1, 1, 1, -3, -2, -1, 0, 1, 2, 3}, {0, 0}, {0, 0}};
    const auto fit = sarima_regression_likelihood(values, design, {}, {}, {}, {}, 4, 0, 0, 0, 2);
    assert(fit.status == "available" && fit.rank == 2 && fit.residual_df == 3);
    assert(close(fit.coefficients[0], 2) && close(fit.coefficients[1], 3));
    assert(close(fit.sum_squared_standardized, 14));
    assert(close(fit.profiled_variance, 14.0 / 5));
    assert(close(fit.unscaled_covariance[0], .2) && close(fit.unscaled_covariance[3], .1));
    assert(close(fit.unscaled_covariance[1], 0) && close(fit.unscaled_covariance[2], 0));
    assert(close(fit.fixed_covariance[0], .4));
    assert(close(fit.ml_covariance[0], 14.0 / 25));
    assert(close(fit.residual_covariance[0], 14.0 / 15));
    assert(close(fit.fixed_log_likelihood, -.5 * (5 * std::log(4 * std::acos(-1.0)) + 7)));
    assert(fit.finite_rows == std::vector<std::size_t>({1, 2, 3, 4, 5}));
    assert(fit.residual_series.missing == values.missing);

    design.fixed[0] = 1;
    design.coefficients[0] = 2;
    const auto fixed = sarima_regression_likelihood(values, design, {}, {}, {}, {}, 4, 0, 0);
    assert(fixed.rank == 1 && fixed.residual_df == 4 && fixed.free_columns[0] == 1);
    assert(close(fixed.coefficients[0], 2) && close(fixed.coefficients[1], 3));
    assert(close(fixed.sum_squared_standardized, 14) && close(fixed.unscaled_covariance[0], .1));

    // A random-walk drift is identified from seven first differences. The
    // initial level is diffuse and contributes no regression observation.
    MaskedSeries walk{{52, 55, 56, 60, 60, 63, 64, 66}, std::vector<std::uint8_t>(8, 0)};
    RegressionDesign trend{1, {1, 2, 3, 4, 5, 6, 7, 8}, {0}, {0}};
    const auto drift = sarima_regression_likelihood(walk, trend, {}, {}, {}, {}, 4, 1, 0);
    assert(drift.diffuse_count == 1 && drift.finite_count == 7 && drift.rank == 1 && drift.residual_df == 6);
    assert(close(drift.coefficients[0], 2) && close(drift.sum_squared_standardized, 12));
    assert(close(drift.unscaled_covariance[0], 1.0 / 7));
    trend.values.assign(8, 1);
    const auto alias = sarima_regression_likelihood(walk, trend, {}, {}, {}, {}, 4, 1, 0);
    assert(alias.status == "rank_deficient" && alias.rank == 0);
    assert(std::isnan(alias.coefficients[0]) && std::isnan(alias.fixed_log_likelihood));
    assert(alias.numerically_zero_columns[0] == 1);
    trend.fixed[0] = 1;
    trend.coefficients[0] = 100;
    const auto translated = sarima_regression_likelihood(walk, trend, {}, {}, {}, {}, 4, 1, 0);
    RegressionDesign none{0, {}, {}, {}};
    const auto plain = sarima_regression_likelihood(walk, none, {}, {}, {}, {}, 4, 1, 0);
    assert(close(translated.fixed_log_likelihood, plain.fixed_log_likelihood));

    RegressionDesign saturated{2, {1, 0, 0, 1}, {0, 0}, {0, 0}};
    const auto boundary = sarima_regression_likelihood({{3, 4}, {0, 0}}, saturated, {}, {}, {}, {}, 4, 0, 0);
    assert(boundary.rank == 2 && boundary.residual_df == 0 && boundary.sum_squared_standardized == 0);
    assert(boundary.profile_status == "zero_scale_boundary" && boundary.profiled_variance == 0);
    assert(std::isinf(boundary.profiled_log_likelihood) && boundary.profiled_log_likelihood > 0);
    assert(std::isnan(boundary.residual_covariance[0]));
    saturated.values = {1, 1, 1, 1};
    const auto duplicate = sarima_regression_likelihood({{3, 4}, {0, 0}}, saturated, {}, {}, {}, {}, 4, 0, 0);
    assert(duplicate.rank == 1 && duplicate.status == "rank_deficient");
    assert(std::isnan(duplicate.coefficients[0]) && std::isnan(duplicate.coefficients[1]));

    RegressionDesign one{1, {1, 2}, {0}, {0}};
    const auto empty = sarima_regression_likelihood({{0, 0}, {1, 1}}, one, {}, {}, {}, {}, 4, 0, 0);
    assert(empty.status == "no_finite_innovations" && std::isnan(empty.fixed_log_likelihood));
    const auto unresolved = sarima_regression_likelihood({{1, 2}, {0, 0}}, one, {}, {}, {}, {}, 12, 0, 1);
    assert(unresolved.status == "unresolved_diffuse" && unresolved.remaining_diffuse_rank == 10);
    one.fixed[0] = 1; one.coefficients[0] = .5;
    const auto fixed_unresolved = sarima_regression_likelihood({{1, 2}, {0, 0}}, one, {}, {}, {}, {}, 12, 0, 1);
    assert(fixed_unresolved.status == "unresolved_diffuse");
    assert(fixed_unresolved.fitted_effect == std::vector<double>({.5, 1}));
    assert(fixed_unresolved.residual_series.values == std::vector<double>({.5, 1}));
    assert(fixed_unresolved.residual_series.missing == std::vector<std::uint8_t>({0, 0}));
    one.fixed[0] = 0; one.coefficients[0] = 0;
    one.values[0] = std::numeric_limits<double>::quiet_NaN();
    rejects([&] { sarima_regression_likelihood({{0, 2}, {1, 0}}, one, {}, {}, {}, {}, 4, 0, 0); });
    one.values = {1, 2};
    rejects([&] { sarima_regression_likelihood({{1, 2}, {0, 0}}, one, {}, {}, {}, {}, 4, 0, 0, 0, 1, 0); });

    // Positive coefficient uncertainty must not silently underflow to zero.
    RegressionDesign huge{1, {1e200}, {0}, {0}};
    covariance_underflows([&] { sarima_regression_likelihood({{1}, {0}}, huge, {}, {}, {}, {}, 4, 0, 0); });
    huge.values[0] = 1e154;
    covariance_underflows([&] { sarima_regression_likelihood({{1}, {0}}, huge, {}, {}, {}, {}, 4, 0, 0, 0, 1e-308); });
    huge.values[0] = 1e152;
    const auto tiny_covariance = sarima_regression_likelihood({{1}, {0}}, huge, {}, {}, {}, {}, 4, 0, 0);
    assert(tiny_covariance.unscaled_covariance[0] > 0);
    assert(std::abs(tiny_covariance.unscaled_covariance[0] / 1e-304 - 1) < 1e-12);

    int polls = 0;
    sarima_regression_likelihood(values, design, {}, {}, {}, {}, 4, 0, 0, 0, 1, 1e-10, [&] {
        assert(!in_parallel_region()); ++polls;
    });
    assert(polls > 5);
    for (int cancel_at = 1; cancel_at <= polls; ++cancel_at) {
        int count = 0;
        bool cancelled = false;
        try {
            sarima_regression_likelihood(values, design, {}, {}, {}, {}, 4, 0, 0, 0, 1, 1e-10, [&] {
                assert(!in_parallel_region());
                if (++count == cancel_at) throw std::runtime_error("cancelled");
            });
        } catch (const std::runtime_error& error) { cancelled = std::string(error.what()) == "cancelled"; }
        assert(cancelled);
    }
    assert(close(sarima_regression_likelihood(values, design, {}, {}, {}, {}, 4, 0, 0).coefficients[1], 3));

    // This monthly d=2,D=2 workload exceeds the channel-parallel threshold.
    // Scalar time order is unchanged within each response/design channel.
    const auto old_setting = std::getenv("PERSEPHONE_NUM_THREADS");
    const bool had_setting = old_setting != nullptr;
    const std::string saved_setting = old_setting ? old_setting : "";
    constexpr std::size_t n = 180;
    MaskedSeries large{std::vector<double>(n, 0), std::vector<std::uint8_t>(n, 0)};
    RegressionDesign many{3, std::vector<double>(3 * n, 0), {0, 0, 0}, {0, 0, 0}};
    for (std::size_t i = 0; i < n; ++i) {
        const auto time = static_cast<double>(i + 1);
        many.values[i] = std::sin(.37 * time);
        many.values[i + n] = std::cos(.17 * time);
        many.values[i + 2 * n] = std::sin(.73 * time) + .1 * std::cos(.11 * time);
        large.values[i] = 3 * many.values[i] - 2 * many.values[i + n] + .25 * many.values[i + 2 * n] + .1 * std::sin(1.31 * time);
        if (i % 17 == 0) { large.values[i] = 0; large.missing[i] = 1; }
    }
    thread_setting("1");
    const auto serial = sarima_regression_likelihood(large, many, {}, {}, {}, {}, 12, 2, 2);
    thread_setting("4");
    const auto info = parallel_info();
    const auto actual = parallel_probe_threads(info);
    const auto parallel = sarima_regression_likelihood(large, many, {}, {}, {}, {}, 12, 2, 2, 0, 1, 1e-10, [] {
        assert(!in_parallel_region());
    });
    assert(serial.status == "available" && parallel.status == "available");
    assert(serial.coefficients == parallel.coefficients);
    assert(serial.whitened_response == parallel.whitened_response);
    assert(serial.whitened_design == parallel.whitened_design);
    assert(serial.fixed_log_likelihood == parallel.fixed_log_likelihood);
    assert(serial.profiled_log_likelihood == parallel.profiled_log_likelihood);
    assert(serial.unscaled_covariance == parallel.unscaled_covariance);
    std::cout << "PASS: regression whitening serial/threaded equality; actual team " << actual << "\n";
    thread_setting(had_setting ? saved_setting.c_str() : nullptr);
}
