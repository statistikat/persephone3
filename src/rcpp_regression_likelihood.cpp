#include <Rcpp.h>
#include "core/regression_likelihood.hpp"

#include <cmath>
#include <limits>
#include <stdexcept>
#include <string>

namespace {

void numeric_type(SEXP value, const char* name) {
    if ((TYPEOF(value) != REALSXP && TYPEOF(value) != INTSXP) || Rf_isFactor(value)) {
        throw std::invalid_argument(std::string(name) + " must be real numeric data");
    }
}

Rcpp::NumericVector vector(SEXP value, const char* name, R_xlen_t maximum = R_XLEN_T_MAX) {
    numeric_type(value, name);
    if (!Rf_isNull(Rf_getAttrib(value, R_DimSymbol))) throw std::invalid_argument(std::string(name) + " must be a vector without dimensions");
    if (Rf_xlength(value) > maximum) throw std::invalid_argument(std::string(name) + " exceeds the supported length");
    return Rcpp::as<Rcpp::NumericVector>(value);
}

double scalar(SEXP value, const char* name, double fallback) {
    if (Rf_isNull(value)) return fallback;
    const auto values = vector(value, name, 1);
    if (values.size() != 1) throw std::invalid_argument(std::string(name) + " must be one real numeric scalar");
    return values[0];
}

int order(SEXP value, const char* name) {
    const auto result = scalar(value, name, 0);
    if (!std::isfinite(result) || result < 0 || result > 2 || result != std::floor(result)) {
        throw std::invalid_argument(std::string(name) + " must be an integer between zero and two");
    }
    return static_cast<int>(result);
}

double optional(double value) { return std::isnan(value) ? NA_REAL : value; }

Rcpp::NumericVector numeric(const std::vector<double>& values) {
    Rcpp::NumericVector result(values.size());
    for (std::size_t i = 0; i < values.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = optional(values[i]);
    }
    return result;
}

Rcpp::NumericMatrix matrix(const std::vector<double>& values, std::size_t rows,
                            std::size_t columns) {
    if (values.size() != rows * columns) throw std::logic_error("regression output matrix has inconsistent dimensions");
    Rcpp::NumericMatrix result(static_cast<int>(rows), static_cast<int>(columns));
    for (std::size_t i = 0; i < values.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = optional(values[i]);
    }
    return result;
}

Rcpp::LogicalVector logical(const std::vector<std::uint8_t>& values) {
    Rcpp::LogicalVector result(values.size());
    for (std::size_t i = 0; i < values.size(); ++i) result[static_cast<R_xlen_t>(i)] = values[i] != 0;
    return result;
}

Rcpp::IntegerVector indices(const std::vector<std::size_t>& values) {
    Rcpp::IntegerVector result(values.size());
    for (std::size_t i = 0; i < values.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = static_cast<int>(values[i] + 1);
    }
    return result;
}

Rcpp::NumericVector series(const persephone::MaskedSeries& values) {
    auto result = numeric(values.values);
    for (std::size_t i = 0; i < values.values.size(); ++i) if (values.missing[i]) result[static_cast<R_xlen_t>(i)] = NA_REAL;
    return result;
}

} // namespace

// [[Rcpp::export(name = ".native_regression_likelihood")]]
Rcpp::List native_regression_likelihood(SEXP values, SEXP design, SEXP coefficients,
    SEXP ar, SEXP ma, SEXP seasonal_ar, SEXP seasonal_ma, SEXP period,
    SEXP d = R_NilValue, SEXP seasonal_d = R_NilValue,
    SEXP mean = R_NilValue, SEXP variance = R_NilValue,
    SEXP rank_tolerance = R_NilValue) {
    const auto response = vector(values, "values", std::numeric_limits<int>::max());
    persephone::MaskedSeries input{std::vector<double>(response.size(), 0),
                                  std::vector<std::uint8_t>(response.size(), 0)};
    for (R_xlen_t i = 0; i < response.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        if (std::isnan(response[i])) input.missing[static_cast<std::size_t>(i)] = 1;
        else if (!std::isfinite(response[i])) throw std::invalid_argument("observations must be finite or missing");
        else input.values[static_cast<std::size_t>(i)] = response[i];
    }
    numeric_type(design, "design");
    const auto dims = Rf_getAttrib(design, R_DimSymbol);
    if (Rf_isNull(dims) || TYPEOF(dims) != INTSXP || Rf_xlength(dims) != 2) throw std::invalid_argument("design must be a numeric matrix");
    const Rcpp::IntegerVector shape(dims);
    if (shape[0] != response.size() || shape[1] < 0 || shape[1] > 32 ||
        Rf_xlength(design) != static_cast<R_xlen_t>(shape[0]) * shape[1]) {
        throw std::invalid_argument("design must match the response rows and have at most 32 columns");
    }
    const auto coefficient_values = vector(coefficients, "coefficients", 32);
    if (coefficient_values.size() != shape[1]) throw std::invalid_argument("coefficients must match every design column; NA denotes a free coefficient");
    const auto design_values = Rcpp::as<Rcpp::NumericVector>(design);
    persephone::RegressionDesign specification{static_cast<std::size_t>(shape[1]),
        std::vector<double>(design_values.begin(), design_values.end()),
        std::vector<double>(static_cast<std::size_t>(shape[1]), 0),
        std::vector<std::uint8_t>(static_cast<std::size_t>(shape[1]), 0)};
    for (int j = 0; j < shape[1]; ++j) {
        if (R_IsNA(coefficient_values[j])) continue;
        if (!std::isfinite(coefficient_values[j])) throw std::invalid_argument("coefficients must be finite or NA for free values");
        specification.coefficients[static_cast<std::size_t>(j)] = coefficient_values[j];
        specification.fixed[static_cast<std::size_t>(j)] = 1;
    }
    const auto season = scalar(period, "period", 0);
    if (season != 4 && season != 12) throw std::invalid_argument("period must be 4 or 12");
    const auto regular = order(d, "d"), seasonal = order(seasonal_d, "seasonal_d");
    const auto level = scalar(mean, "mean", 0), scale = scalar(variance, "variance", 1);
    const auto tolerance = scalar(rank_tolerance, "rank_tolerance", 1e-10);
    const auto fit = persephone::sarima_regression_likelihood(input, specification,
        Rcpp::as<std::vector<double>>(vector(ar, "AR coefficients", 16)),
        Rcpp::as<std::vector<double>>(vector(ma, "MA coefficients", 16)),
        Rcpp::as<std::vector<double>>(vector(seasonal_ar, "seasonal AR coefficients", 16)),
        Rcpp::as<std::vector<double>>(vector(seasonal_ma, "seasonal MA coefficients", 16)),
        static_cast<int>(season), regular, seasonal, level, scale, tolerance,
        [] { Rcpp::checkUserInterrupt(); });
    const auto free = fit.free_columns.size();
    // Populate in groups to avoid old-Rcpp fixed-arity List::create limits.
    Rcpp::List output;
    output["schema_version"] = 1;
    output["status"] = fit.status;
    output["coefficients"] = numeric(fit.coefficients);
    output["fixed"] = logical(fit.fixed);
    output["free_columns"] = indices(fit.free_columns);
    output["rank"] = static_cast<int>(fit.rank);
    output["free_count"] = static_cast<int>(free);
    output["residual_df"] = static_cast<double>(fit.residual_df);
    output["covariance_status"] = fit.covariance_status;
    output["unscaled_covariance"] = matrix(fit.unscaled_covariance, free, free);
    output["fixed_covariance"] = matrix(fit.fixed_covariance, free, free);
    output["ml_covariance"] = matrix(fit.ml_covariance, free, free);
    output["residual_covariance"] = matrix(fit.residual_covariance, free, free);
    output["finite_rows"] = indices(fit.finite_rows);
    output["whitened_response"] = numeric(fit.whitened_response);
    output["whitened_design"] = matrix(fit.whitened_design, fit.finite_count, free);
    output["whitened_residuals"] = numeric(fit.whitened_residuals);
    output["whitening_norm_ratio"] = numeric(fit.whitening_norm_ratio);
    output["numerically_zero_columns"] = logical(fit.numerically_zero_columns);
    output["whitening_zero_tolerance"] = fit.whitening_zero_tolerance;
    output["fixed_effect"] = numeric(fit.fixed_effect);
    output["fitted_effect"] = numeric(fit.fitted_effect);
    output["adjusted_response"] = series(fit.adjusted_response);
    output["residual_series"] = series(fit.residual_series);
    output["observed_count"] = static_cast<double>(fit.observed_count);
    output["diffuse_count"] = static_cast<double>(fit.diffuse_count);
    output["finite_count"] = static_cast<double>(fit.finite_count);
    output["initial_diffuse_rank"] = static_cast<int>(fit.initial_diffuse_rank);
    output["remaining_diffuse_rank"] = static_cast<int>(fit.remaining_diffuse_rank);
    output["sum_log_finite_variances"] = fit.sum_log_finite_variances;
    output["sum_log_diffuse_variances"] = fit.sum_log_diffuse_variances;
    output["sum_squared_standardized"] = optional(fit.sum_squared_standardized);
    output["fixed_log_likelihood"] = optional(fit.fixed_log_likelihood);
    output["profiled_log_likelihood"] = optional(fit.profiled_log_likelihood);
    output["profiled_variance"] = optional(fit.profiled_variance);
    output["likelihood_status"] = fit.likelihood_status;
    output["profile_status"] = fit.profile_status;
    output["likelihood_normalization"] = "fixed_initial_diffuse_coordinates";
    output["variance_estimator"] = "concentrated_ML_Q_over_nfinite";
    output["difference_polynomial"] = numeric(fit.difference_polynomial);
    output["period"] = static_cast<int>(season);
    output["d"] = regular;
    output["seasonal_d"] = seasonal;
    output["mean"] = level;
    output["variance"] = scale;
    output["rank_tolerance"] = tolerance;
    return output;
}
