#include <Rcpp.h>
#include "core/state_space.hpp"

#include <cmath>
#include <limits>
#include <stdexcept>
#include <string>

namespace {

persephone::Interrupt interrupt() { return [] { Rcpp::checkUserInterrupt(); }; }

void numeric_type(SEXP input, const char* name) {
    if ((TYPEOF(input) != REALSXP && TYPEOF(input) != INTSXP) || Rf_isFactor(input)) {
        throw std::invalid_argument(std::string(name) + " must be real numeric data");
    }
}

Rcpp::NumericVector vector(SEXP input, const char* name, bool bounded = false) {
    numeric_type(input, name);
    if (!Rf_isNull(Rf_getAttrib(input, R_DimSymbol))) {
        throw std::invalid_argument(std::string(name) + " must be a vector without matrix or array dimensions");
    }
    if (bounded && Rf_xlength(input) > 16) {
        throw std::invalid_argument(std::string(name) + " exceeds the 16-state pilot bound");
    }
    return Rcpp::as<Rcpp::NumericVector>(input);
}

double scalar(SEXP input, const char* name) {
    if (Rf_xlength(input) != 1) {
        throw std::invalid_argument(std::string(name) + " must be one real numeric scalar");
    }
    const auto values = vector(input, name);
    return values[0];
}

double scalar(SEXP input, const char* name, double default_value) {
    return Rf_isNull(input) ? default_value : scalar(input, name);
}

persephone::DenseMatrix matrix(SEXP input, const char* name) {
    numeric_type(input, name);
    SEXP dimensions = Rf_getAttrib(input, R_DimSymbol);
    if (Rf_isNull(dimensions) || TYPEOF(dimensions) != INTSXP || Rf_xlength(dimensions) != 2) {
        throw std::invalid_argument(std::string(name) + " must be a numeric matrix");
    }
    Rcpp::IntegerVector dims(dimensions);
    if (dims[0] < 1 || dims[1] < 1 || dims[0] > 16 || dims[1] > 16) {
        throw std::invalid_argument(std::string(name) + " must have positive dimensions of at most 16");
    }
    Rcpp::NumericVector values = Rcpp::as<Rcpp::NumericVector>(input);
    return persephone::DenseMatrix(static_cast<std::size_t>(dims[0]), static_cast<std::size_t>(dims[1]),
                                   std::vector<double>(values.begin(), values.end()));
}

Rcpp::NumericMatrix matrix(const persephone::DenseMatrix& input) {
    Rcpp::NumericMatrix result(input.rows(), input.columns());
    std::copy(input.values().begin(), input.values().end(), result.begin());
    return result;
}

persephone::MaskedSeries series(const Rcpp::NumericVector& input) {
    persephone::MaskedSeries result{std::vector<double>(input.size(), 0),
                                    std::vector<std::uint8_t>(input.size(), 0)};
    for (R_xlen_t i = 0; i < input.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        if (std::isnan(input[i])) result.missing[static_cast<std::size_t>(i)] = 1;
        else if (!std::isfinite(input[i])) throw std::invalid_argument("observations must be finite or missing; infinity is invalid");
        else result.values[static_cast<std::size_t>(i)] = input[i];
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

Rcpp::List result(const persephone::InnovationsLikelihood& value,
                   const persephone::StationaryStateSpace& model,
                   double mean, double variance) {
    return Rcpp::List::create(
        Rcpp::Named("schema_version") = 1,
        Rcpp::Named("initialization") = "stationary_lyapunov",
        Rcpp::Named("predictions") = Rcpp::wrap(value.predictions),
        Rcpp::Named("innovations") = series(value.innovations),
        Rcpp::Named("standardized_innovations") = series(value.standardized_innovations),
        Rcpp::Named("innovation_variances") = Rcpp::wrap(value.innovation_variances),
        Rcpp::Named("observed_count") = static_cast<double>(value.observed_count),
        Rcpp::Named("sum_squared_standardized") = value.sum_squared_standardized,
        Rcpp::Named("sum_log_variances") = value.sum_log_variances,
        Rcpp::Named("fixed_log_likelihood") = value.fixed_log_likelihood,
        Rcpp::Named("profiled_variance") = std::isnan(value.profiled_variance) ? NA_REAL : value.profiled_variance,
        Rcpp::Named("profiled_log_likelihood") = std::isnan(value.profiled_log_likelihood) ? NA_REAL : value.profiled_log_likelihood,
        Rcpp::Named("profile_status") = value.profile_status,
        Rcpp::Named("initial_covariance") = matrix(value.initialization.covariance),
        Rcpp::Named("final_state") = Rcpp::wrap(value.final_state),
        Rcpp::Named("final_covariance") = matrix(value.final_covariance),
        Rcpp::Named("numerics") = Rcpp::List::create(
            Rcpp::Named("lyapunov_scaled_residual") = value.initialization.scaled_residual,
            Rcpp::Named("stability_certificate_rcond") = value.initialization.certificate_reciprocal_condition,
            Rcpp::Named("rank_tolerance") = 1e-12,
            Rcpp::Named("max_states") = 16),
        Rcpp::Named("model") = Rcpp::List::create(
            Rcpp::Named("transition") = matrix(model.transition),
            Rcpp::Named("observation") = Rcpp::wrap(model.observation),
            Rcpp::Named("noise_loading") = matrix(model.noise_loading),
            Rcpp::Named("process_covariance") = matrix(value.initialization.process_covariance),
            Rcpp::Named("observation_variance") = model.observation_variance,
            Rcpp::Named("mean") = mean,
            Rcpp::Named("variance") = variance));
}

} // namespace

// [[Rcpp::export(name = ".native_stationary_likelihood")]]
Rcpp::List native_stationary_likelihood(SEXP values, SEXP transition,
                                       SEXP observation, SEXP noise_loading,
                                       SEXP observation_variance = R_NilValue,
                                       SEXP mean = R_NilValue, SEXP variance = R_NilValue) {
    const double mean_value = scalar(mean, "mean", 0);
    const double variance_value = scalar(variance, "variance", 1);
    const persephone::StationaryStateSpace model{matrix(transition, "transition"),
        Rcpp::as<std::vector<double>>(vector(observation, "observation", true)),
        matrix(noise_loading, "noise loading"), scalar(observation_variance, "observation variance", 0)};
    const auto likelihood = persephone::stationary_innovations(series(vector(values, "values")), model,
                                                               mean_value, variance_value, interrupt());
    return result(likelihood, model, mean_value, variance_value);
}

// [[Rcpp::export(name = ".native_arma_likelihood")]]
Rcpp::List native_arma_likelihood(SEXP values, SEXP ar, SEXP ma,
                                 SEXP seasonal_ar, SEXP seasonal_ma,
                                 SEXP period, SEXP mean = R_NilValue, SEXP variance = R_NilValue) {
    const double period_value = scalar(period, "seasonal period");
    const double mean_value = scalar(mean, "mean", 0);
    const double variance_value = scalar(variance, "variance", 1);
    if (!std::isfinite(period_value) || period_value < 1 || std::floor(period_value) != period_value ||
        period_value > std::numeric_limits<int>::max()) {
        throw std::invalid_argument("seasonal period must be a positive supported integer");
    }
    const auto model = persephone::stationary_arma(
        Rcpp::as<std::vector<double>>(vector(ar, "AR coefficients", true)),
        Rcpp::as<std::vector<double>>(vector(ma, "MA coefficients", true)),
        Rcpp::as<std::vector<double>>(vector(seasonal_ar, "seasonal AR coefficients", true)),
        Rcpp::as<std::vector<double>>(vector(seasonal_ma, "seasonal MA coefficients", true)),
        static_cast<int>(period_value), interrupt());
    const auto likelihood = persephone::stationary_innovations(series(vector(values, "values")), model.state_space,
                                                               mean_value, variance_value, interrupt());
    auto output = result(likelihood, model.state_space, mean_value, variance_value);
    output["expanded_ar"] = Rcpp::wrap(model.ar);
    output["expanded_ma"] = Rcpp::wrap(model.ma);
    return output;
}
