#ifndef PERSEPHONE_REGRESSION_LIKELIHOOD_HPP
#define PERSEPHONE_REGRESSION_LIKELIHOOD_HPP

#include "diffuse_state_space.hpp"

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace persephone {

// Design values are column-major, with the response's full calendar domain.
// Unlike response values, regressors must be finite even at missing responses.
// Zero columns are supported; at most 32 coefficients are accepted in this pilot.
struct RegressionDesign {
    std::size_t columns;
    std::vector<double> values;
    std::vector<double> coefficients; // Finite fixed values; zero placeholders for free values.
    std::vector<std::uint8_t> fixed;
};

// Shared preflight for fitting and optimization candidates. Invalid data and
// controls remain errors even when a candidate's ARMA parameters are rejected.
void validate_regression_inputs(const MaskedSeries& series, const RegressionDesign& design,
    double mean, double variance, double rank_tolerance, const Interrupt& interrupt = {});

struct RegressionLikelihood {
    std::vector<double> coefficients;
    std::vector<std::uint8_t> fixed;
    std::vector<std::size_t> free_columns; // Zero-based original column indices.
    std::size_t rank;
    std::size_t residual_df;
    std::string status;
    std::string covariance_status;
    // Covariance arrays are free_count x free_count, column-major. They are
    // unavailable (NaN) for nonunique coefficients. Fixed coefficients are omitted.
    std::vector<double> unscaled_covariance;
    std::vector<double> fixed_covariance;
    std::vector<double> ml_covariance;
    std::vector<double> residual_covariance;
    std::vector<std::size_t> finite_rows; // Zero-based calendar positions.
    std::vector<double> whitened_response;
    std::vector<double> whitened_design; // finite_count x free_count.
    std::vector<double> whitened_residuals;
    std::vector<double> whitening_norm_ratio;
    std::vector<std::uint8_t> numerically_zero_columns; // Free-column order.
    double whitening_zero_tolerance;
    std::vector<double> fixed_effect;
    std::vector<double> fitted_effect;
    MaskedSeries adjusted_response;
    MaskedSeries residual_series;
    std::size_t observed_count;
    std::size_t diffuse_count;
    std::size_t finite_count;
    std::size_t initial_diffuse_rank;
    std::size_t remaining_diffuse_rank;
    double sum_log_finite_variances;
    double sum_log_diffuse_variances;
    double sum_squared_standardized;
    double fixed_log_likelihood;
    double profiled_variance;
    double profiled_log_likelihood;
    std::string likelihood_status;
    std::string profile_status;
    std::vector<double> difference_polynomial;
};

// Fixed ARMA coefficients; finite innovations of response and design use the
// SAME state model, missing mask and stored gains. Covariance filtering runs
// once; independent owned-data whitening channels may use optional OpenMP.
// No R, BLAS or LAPACK operation runs on worker threads.
// Free coefficients maximize the existing fixed-initial-history likelihood.
// ML variance is Q/n_finite, not Q/(n_finite-rank); no REML determinant is added.
RegressionLikelihood sarima_regression_likelihood(
    const MaskedSeries& series, const RegressionDesign& design,
    const std::vector<double>& ar, const std::vector<double>& ma,
    const std::vector<double>& seasonal_ar, const std::vector<double>& seasonal_ma,
    int period, int d, int seasonal_d, double mean = 0, double variance = 1,
    double rank_tolerance = 1e-10, const Interrupt& interrupt = {});

// Complete-span residual basis for residual-Jacobian information. End gaps are
// trimmed; internal missing coordinates are profiled as extra dummy columns.
// These columns are integration variables, not estimated regression parameters.
struct CompleteResiduals {
    std::string status = "unavailable";
    std::vector<double> residuals, regression_coefficients, interpolated;
    std::vector<std::size_t> rows, missing_rows;
    std::size_t finite_count = 0;
    double log_determinant = 0, sum_squares = 0;
    // Retained only on explicit request; finite complete rows x missing rows.
    std::vector<double> missing_whitened_design;
};
struct CompleteResidualOptions {
    bool retain_missing_design = false;
    // Reporting comparisons only: apply exact finite differencing before
    // stationary whitening, rather than the default integrated-state route.
    bool difference_first = false;
};
CompleteResiduals sarima_complete_residuals(const MaskedSeries& series,
    const RegressionDesign& design, const std::vector<double>& ar,
    const std::vector<double>& ma, const std::vector<double>& seasonal_ar,
    const std::vector<double>& seasonal_ma, int period, int d, int seasonal_d,
    double mean = 0, double rank_tolerance = 1e-10, const Interrupt& interrupt = {},
    const CompleteResidualOptions& options = {});

} // namespace persephone
#endif
