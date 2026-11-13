#include <Rcpp.h>
#include "core/diffuse_state_space.hpp"

#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>

// Internal, serial, owned-data boundaries. Validate R types and shapes before
// conversion so complex values, factors and array dimensions are never lost.
namespace {

persephone::Interrupt diffuse_interrupt() { return [] { Rcpp::checkUserInterrupt(); }; }

void numeric_type(SEXP input, const char* name) {
    if ((TYPEOF(input) != REALSXP && TYPEOF(input) != INTSXP) || Rf_isFactor(input)) {
        throw std::invalid_argument(std::string(name) + " must be real numeric data");
    }
}

Rcpp::NumericVector vector(SEXP input, const char* name, R_xlen_t maximum = R_XLEN_T_MAX) {
    numeric_type(input, name);
    if (!Rf_isNull(Rf_getAttrib(input, R_DimSymbol))) {
        throw std::invalid_argument(std::string(name) + " must be a vector without dimensions");
    }
    if (Rf_xlength(input) > maximum) {
        throw std::invalid_argument(std::string(name) + " exceeds the supported dimension bound");
    }
    return Rcpp::as<Rcpp::NumericVector>(input);
}

double scalar(SEXP input, const char* name) {
    if (Rf_xlength(input) != 1) {
        throw std::invalid_argument(std::string(name) + " must be one real numeric scalar");
    }
    return vector(input, name, 1)[0];
}

double scalar(SEXP input, const char* name, double default_value) {
    return Rf_isNull(input) ? default_value : scalar(input, name);
}

int difference_order(SEXP input, const char* name) {
    const double value = scalar(input, name, 0);
    if (!std::isfinite(value) || value < 0 || value > 2 || value != std::floor(value)) {
        throw std::invalid_argument(std::string(name) + " must be an integer between 0 and 2");
    }
    return static_cast<int>(value);
}

Rcpp::IntegerVector dimensions(SEXP input, const char* name, bool empty_columns = false) {
    numeric_type(input, name);
    SEXP dim = Rf_getAttrib(input, R_DimSymbol);
    if (Rf_isNull(dim) || TYPEOF(dim) != INTSXP || Rf_xlength(dim) != 2) {
        throw std::invalid_argument(std::string(name) + " must be a numeric matrix");
    }
    Rcpp::IntegerVector result(dim);
    if (result[0] < 1 || result[0] > 32 || result[1] < (empty_columns ? 0 : 1) || result[1] > 32) {
        throw std::invalid_argument(std::string(name) + " dimensions exceed the 32-state pilot bound");
    }
    if (Rf_xlength(input) != static_cast<R_xlen_t>(result[0]) * result[1]) {
        throw std::invalid_argument(std::string(name) + " storage does not match its dimensions");
    }
    return result;
}

persephone::DenseMatrix matrix(SEXP input, const char* name) {
    const auto dim = dimensions(input, name);
    const auto values = Rcpp::as<Rcpp::NumericVector>(input);
    return persephone::DenseMatrix(static_cast<std::size_t>(dim[0]), static_cast<std::size_t>(dim[1]),
                                   std::vector<double>(values.begin(), values.end()));
}

Rcpp::NumericMatrix matrix(const persephone::DenseMatrix& input) {
    Rcpp::NumericMatrix result(static_cast<int>(input.rows()), static_cast<int>(input.columns()));
    std::copy(input.values().begin(), input.values().end(), result.begin());
    return result;
}

persephone::MaskedSeries series(SEXP input) {
    const auto values = vector(input, "values");
    persephone::MaskedSeries result{std::vector<double>(values.size(), 0),
                                    std::vector<std::uint8_t>(values.size(), 0)};
    for (R_xlen_t i = 0; i < values.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        if (std::isnan(values[i])) result.missing[static_cast<std::size_t>(i)] = 1;
        else if (!std::isfinite(values[i])) throw std::invalid_argument("observations must be finite or missing; infinity is invalid");
        else result.values[static_cast<std::size_t>(i)] = values[i];
    }
    return result;
}

Rcpp::NumericVector series(const persephone::MaskedSeries& input) {
    Rcpp::NumericVector result(input.values.size());
    for (std::size_t i = 0; i < input.values.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = input.missing[i] ? NA_REAL : input.values[i];
    }
    return result;
}

double optional_number(double value) { return std::isnan(value) ? NA_REAL : value; }

Rcpp::IntegerVector ranks(const std::vector<std::size_t>& input) {
    Rcpp::IntegerVector result(input.size());
    for (std::size_t i = 0; i < input.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = static_cast<int>(input[i]);
    }
    return result;
}

Rcpp::List result(const persephone::DiffuseLikelihood& value,
                   const persephone::DiffuseStateSpace& model,
                   double mean, double variance) {
    return Rcpp::List::create(
        Rcpp::Named("schema_version") = 1,
        Rcpp::Named("initialization") = "exact_diffuse",
        Rcpp::Named("likelihood_normalization") = "fixed_initial_diffuse_coordinates",
        Rcpp::Named("predictions") = Rcpp::wrap(value.predictions),
        Rcpp::Named("innovations") = series(value.innovations),
        Rcpp::Named("standardized_innovations") = series(value.standardized_innovations),
        Rcpp::Named("finite_variances") = Rcpp::wrap(value.finite_variances),
        Rcpp::Named("diffuse_variances") = Rcpp::wrap(value.diffuse_variances),
        Rcpp::Named("update_kind") = Rcpp::wrap(value.update_kind),
        Rcpp::Named("update_codes") = Rcpp::IntegerVector::create(
            Rcpp::Named("missing") = 0, Rcpp::Named("diffuse") = 1, Rcpp::Named("finite") = 2),
        Rcpp::Named("rank_before") = ranks(value.rank_before),
        Rcpp::Named("rank_after") = ranks(value.rank_after),
        Rcpp::Named("observed_count") = static_cast<double>(value.observed_count),
        Rcpp::Named("diffuse_count") = static_cast<double>(value.diffuse_count),
        Rcpp::Named("finite_count") = static_cast<double>(value.finite_count),
        Rcpp::Named("initial_diffuse_rank") = static_cast<int>(value.initial_diffuse_rank),
        Rcpp::Named("remaining_diffuse_rank") = static_cast<int>(value.remaining_diffuse_rank),
        Rcpp::Named("last_diffuse_index") = static_cast<double>(value.last_diffuse_index),
        Rcpp::Named("sum_squared_standardized") = value.sum_squared_standardized,
        Rcpp::Named("sum_log_finite_variances") = value.sum_log_finite_variances,
        Rcpp::Named("sum_log_diffuse_variances") = value.sum_log_diffuse_variances,
        Rcpp::Named("fixed_log_likelihood") = optional_number(value.fixed_log_likelihood),
        Rcpp::Named("profiled_variance") = optional_number(value.profiled_variance),
        Rcpp::Named("profiled_log_likelihood") = optional_number(value.profiled_log_likelihood),
        Rcpp::Named("likelihood_status") = value.likelihood_status,
        Rcpp::Named("profile_status") = value.profile_status,
        Rcpp::Named("initial_finite_covariance") = matrix(value.initial_finite_covariance),
        Rcpp::Named("initial_diffuse_covariance") = matrix(value.initial_diffuse_covariance),
        Rcpp::Named("final_state") = Rcpp::wrap(value.final_state),
        Rcpp::Named("final_finite_covariance") = matrix(value.final_finite_covariance),
        Rcpp::Named("final_diffuse_covariance") = matrix(value.final_diffuse_covariance),
        Rcpp::Named("numerics") = Rcpp::List::create(
            Rcpp::Named("rank_tolerance") = value.numerics.rank_tolerance,
            Rcpp::Named("projection_tolerance") = value.numerics.projection_tolerance,
            Rcpp::Named("propagation_rank_checks") = static_cast<double>(value.numerics.propagation_rank_checks),
            Rcpp::Named("maximum_projection_residual") = value.numerics.maximum_projection_residual,
            Rcpp::Named("exact_history_certification") = value.numerics.exact_history_certification,
            Rcpp::Named("exact_history_zero_count") = static_cast<double>(value.numerics.exact_history_zero_count),
            Rcpp::Named("exact_history_pivot_count") = static_cast<double>(value.numerics.exact_history_pivot_count),
            Rcpp::Named("max_states") = 32),
        Rcpp::Named("model") = Rcpp::List::create(
            Rcpp::Named("transition") = matrix(model.transition),
            Rcpp::Named("observation") = Rcpp::wrap(model.observation),
            Rcpp::Named("noise_loading") = matrix(model.noise_loading),
            Rcpp::Named("observation_variance") = model.observation_variance,
            Rcpp::Named("mean") = mean,
            Rcpp::Named("variance") = variance));
}

} // namespace

// [[Rcpp::export(name = ".native_diffuse_likelihood")]]
Rcpp::List native_diffuse_likelihood(SEXP values, SEXP transition, SEXP observation,
                                    SEXP noise_loading, SEXP initial_state,
                                    SEXP finite_loading, SEXP diffuse_loading,
                                    SEXP observation_variance = R_NilValue,
                                    SEXP mean = R_NilValue, SEXP variance = R_NilValue) {
    const double mean_value = scalar(mean, "mean", 0);
    const double variance_value = scalar(variance, "variance", 1);
    const persephone::DiffuseStateSpace model{
        matrix(transition, "transition"),
        Rcpp::as<std::vector<double>>(vector(observation, "observation", 32)),
        matrix(noise_loading, "noise loading"),
        scalar(observation_variance, "observation variance", 0)};
    const auto dim = dimensions(diffuse_loading, "initial diffuse loading", true);
    if (static_cast<std::size_t>(dim[0]) != model.transition.rows()) {
        throw std::invalid_argument("initial diffuse loading rows must match the state dimension");
    }
    const auto diffuse = Rcpp::as<Rcpp::NumericVector>(diffuse_loading);
    const persephone::DiffuseInitialState initial{
        Rcpp::as<std::vector<double>>(vector(initial_state, "initial state", 32)),
        matrix(finite_loading, "initial finite loading"),
        std::vector<double>(diffuse.begin(), diffuse.end()), static_cast<std::size_t>(dim[1])};
    const auto likelihood = persephone::diffuse_innovations(series(values), model, initial,
        mean_value, variance_value, diffuse_interrupt());
    auto output = result(likelihood, model, mean_value, variance_value);
    output["initial_state"] = Rcpp::wrap(initial.state);
    return output;
}

// [[Rcpp::export(name = ".native_sarima_likelihood")]]
Rcpp::List native_sarima_likelihood(SEXP values, SEXP ar, SEXP ma,
                                   SEXP seasonal_ar, SEXP seasonal_ma, SEXP period,
                                   SEXP d = R_NilValue, SEXP seasonal_d = R_NilValue,
                                   SEXP mean = R_NilValue, SEXP variance = R_NilValue) {
    const double period_value = scalar(period, "seasonal period");
    if (!std::isfinite(period_value) || (period_value != 4 && period_value != 12)) {
        throw std::invalid_argument("seasonal period must be 4 or 12");
    }
    const int regular_order = difference_order(d, "d");
    const int seasonal_order = difference_order(seasonal_d, "seasonal_d");
    const double mean_value = scalar(mean, "mean", 0);
    const double variance_value = scalar(variance, "variance", 1);
    const auto fit = persephone::sarima_innovations(series(values),
        Rcpp::as<std::vector<double>>(vector(ar, "AR coefficients", 16)),
        Rcpp::as<std::vector<double>>(vector(ma, "MA coefficients", 16)),
        Rcpp::as<std::vector<double>>(vector(seasonal_ar, "seasonal AR coefficients", 16)),
        Rcpp::as<std::vector<double>>(vector(seasonal_ma, "seasonal MA coefficients", 16)),
        static_cast<int>(period_value), regular_order, seasonal_order,
        mean_value, variance_value, diffuse_interrupt());
    auto output = result(fit.likelihood, fit.model, mean_value, variance_value);
    output["initial_state"] = Rcpp::NumericVector(static_cast<R_xlen_t>(fit.model.transition.rows()), 0.0);
    output["expanded_ar"] = Rcpp::wrap(fit.expanded_ar);
    output["expanded_ma"] = Rcpp::wrap(fit.expanded_ma);
    output["difference_polynomial"] = Rcpp::wrap(fit.difference);
    output["d"] = regular_order;
    output["seasonal_d"] = seasonal_order;
    output["period"] = static_cast<int>(period_value);
    return output;
}
