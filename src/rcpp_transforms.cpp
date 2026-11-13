#include <Rcpp.h>
#include "core/transforms.hpp"

#include <cmath>
#include <stdexcept>

namespace {
persephone::Transform transform_kind(SEXP input) {
    if (Rf_isNull(input)) return persephone::Transform::none;
    if (TYPEOF(input) != STRSXP || Rf_xlength(input) != 1 || STRING_ELT(input, 0) == NA_STRING) {
        throw std::invalid_argument("transformation must be 'none' or 'log'");
    }
    const auto transformation = Rcpp::as<std::string>(input);
    if (transformation == "none") return persephone::Transform::none;
    if (transformation == "log") return persephone::Transform::logarithm;
    throw std::invalid_argument("transformation must be 'none' or 'log'");
}

persephone::MaskedSeries transform_input(SEXP input) {
    if ((TYPEOF(input) != REALSXP && TYPEOF(input) != INTSXP) ||
        !Rf_isNull(Rf_getAttrib(input, R_DimSymbol))) {
        throw std::invalid_argument("series must be a real numeric vector without dimensions");
    }
    const auto values = Rcpp::as<Rcpp::NumericVector>(input);
    persephone::MaskedSeries result{std::vector<double>(values.size(), 0.0),
                                    std::vector<std::uint8_t>(values.size(), 0)};
    for (R_xlen_t i = 0; i < values.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        if (std::isnan(values[i])) result.missing[static_cast<std::size_t>(i)] = 1;
        else if (!std::isfinite(values[i])) {
            throw std::invalid_argument("series values must be finite or missing");
        } else result.values[static_cast<std::size_t>(i)] = values[i];
    }
    return result;
}

Rcpp::NumericVector transform_output(const persephone::MaskedSeries& series) {
    Rcpp::NumericVector result(series.values.size());
    for (std::size_t i = 0; i < series.values.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = series.missing[i] ? NA_REAL : series.values[i];
    }
    return result;
}
}

// [[Rcpp::export(name = ".native_transform")]]
Rcpp::List native_transform(SEXP values, SEXP transformation = R_NilValue) {
    const auto result = persephone::transform_series(transform_input(values),
        transform_kind(transformation), [] { Rcpp::checkUserInterrupt(); });
    Rcpp::LogicalVector missing(Rf_xlength(values));
    for (R_xlen_t i = 0; i < Rf_xlength(values); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        missing[i] = result.series.missing[static_cast<std::size_t>(i)] != 0;
    }
    return Rcpp::List::create(Rcpp::Named("values") = transform_output(result.series),
        Rcpp::Named("missing") = missing,
        Rcpp::Named("log_jacobian") = transform_output(result.log_jacobian));
}

// [[Rcpp::export(name = ".native_inverse_transform")]]
Rcpp::NumericVector native_inverse_transform(SEXP values,
                                           SEXP transformation = R_NilValue) {
    return transform_output(persephone::inverse_transform(transform_input(values),
        transform_kind(transformation), [] { Rcpp::checkUserInterrupt(); }));
}
