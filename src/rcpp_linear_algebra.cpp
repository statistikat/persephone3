#include <Rcpp.h>
#include "core/linear_algebra.hpp"

#include <limits>
#include <stdexcept>
#include <string>

// Plain-data test boundaries. These primitives fit no seasonal-adjustment
// model, do not infer coefficient covariance, and retain no external pointers.
namespace {

persephone::Interrupt linear_interrupt() {
    // All LAPACK calls and callbacks run synchronously on R's main thread.
    return [] { Rcpp::checkUserInterrupt(); };
}

double rank_threshold(SEXP input) {
    if (Rf_isNull(input)) return 1e-10;
    if ((TYPEOF(input) != REALSXP && TYPEOF(input) != INTSXP) ||
        Rf_isFactor(input) || Rf_xlength(input) != 1 ||
        !Rf_isNull(Rf_getAttrib(input, R_DimSymbol))) {
        throw std::invalid_argument("rank tolerance must be one real numeric scalar");
    }
    return Rf_asReal(input);
}

persephone::DenseMatrix owned_matrix(SEXP input, const char* name, bool allow_vector) {
    if ((TYPEOF(input) != REALSXP && TYPEOF(input) != INTSXP) || Rf_isFactor(input)) {
        throw std::invalid_argument(std::string(name) + " must be a real numeric matrix or vector");
    }
    std::size_t rows = 0;
    std::size_t columns = 1;
    SEXP dimensions = Rf_getAttrib(input, R_DimSymbol);
    if (Rf_isNull(dimensions)) {
        if (!allow_vector) throw std::invalid_argument(std::string(name) + " must be a matrix");
        rows = static_cast<std::size_t>(Rf_xlength(input));
    } else {
        Rcpp::IntegerVector dims(dimensions);
        if (dims.size() != 2 || dims[0] <= 0 || dims[1] <= 0) {
            throw std::invalid_argument(std::string(name) + " must have two positive matrix dimensions");
        }
        rows = static_cast<std::size_t>(dims[0]);
        columns = static_cast<std::size_t>(dims[1]);
    }
    // Validate dimensions before allocating or copying input values.
    persephone::DenseMatrix result(rows, columns);
    if (static_cast<std::size_t>(Rf_xlength(input)) != result.values().size()) {
        throw std::invalid_argument(std::string(name) + " storage does not match its dimensions");
    }
    Rcpp::NumericVector values = Rcpp::as<Rcpp::NumericVector>(input);
    for (R_xlen_t i = 0; i < values.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        result.values()[static_cast<std::size_t>(i)] = values[i];
    }
    result.validate(linear_interrupt());
    return result;
}

Rcpp::NumericMatrix as_matrix(const persephone::DenseMatrix& matrix) {
    Rcpp::NumericMatrix result(static_cast<int>(matrix.rows()), static_cast<int>(matrix.columns()));
    for (std::size_t i = 0; i < matrix.values().size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        result[static_cast<R_xlen_t>(i)] = matrix.values()[i];
    }
    return result;
}

} // namespace

// [[Rcpp::export(name = ".native_cholesky_solve")]]
Rcpp::List native_cholesky_solve(SEXP matrix, SEXP rhs) {
    const auto a = owned_matrix(matrix, "matrix", false);
    const auto b = owned_matrix(rhs, "right-hand side", true);
    const auto factor = persephone::cholesky_factor(a, linear_interrupt());
    const auto solution = persephone::cholesky_solve(factor, b, linear_interrupt());
    return Rcpp::List::create(
        Rcpp::Named("solution") = as_matrix(solution),
        Rcpp::Named("upper") = as_matrix(factor.upper),
        Rcpp::Named("log_determinant") = factor.log_determinant,
        Rcpp::Named("reciprocal_condition") = factor.reciprocal_condition,
        Rcpp::Named("condition_norm") = "1");
}

// [[Rcpp::export(name = ".native_qr_solve")]]
Rcpp::List native_qr_solve(SEXP matrix, SEXP rhs, SEXP rank_tolerance = R_NilValue) {
    const double tolerance = rank_threshold(rank_tolerance);
    const auto a = owned_matrix(matrix, "matrix", false);
    const auto b = owned_matrix(rhs, "right-hand side", true);
    const auto result = persephone::qr_least_squares(a, b, tolerance, linear_interrupt());
    return Rcpp::List::create(
        Rcpp::Named("coefficients") = as_matrix(result.coefficients),
        Rcpp::Named("residuals") = as_matrix(result.residuals),
        Rcpp::Named("rank") = result.rank,
        Rcpp::Named("pivots") = Rcpp::wrap(result.pivots),
        Rcpp::Named("rank_tolerance") = result.rank_tolerance,
        Rcpp::Named("solution_kind") = "minimum_norm",
        Rcpp::Named("condition") = Rcpp::List::create(
            Rcpp::Named("available") = false,
            Rcpp::Named("reason") = "DGELSY determines effective rank but does not return a condition estimate for the complete matrix"));
}
