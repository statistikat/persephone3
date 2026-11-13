#include <Rcpp.h>
#include "core/numerics.hpp"

#include <cmath>
#include <limits>
#include <stdexcept>

// Thin boundary around independently authored, R-independent C++ primitives.
// Native model fitting is deliberately not implemented by these routines.
namespace {

std::int64_t scalar_integer(double value, const char* name) {
    // Stay in the exact integer range of R doubles. Exclude its endpoints so
    // year-to-period conversion remains exact when returned through R doubles.
    constexpr double limit = 9007199254740991.0;
    if (!std::isfinite(value) || std::floor(value) != value ||
        value < -limit || value > limit) {
        throw std::invalid_argument(std::string(name) + " must be a finite exact integer");
    }
    return static_cast<std::int64_t>(value);
}

int small_integer(double value, const char* name) {
    const auto integer = scalar_integer(value, name);
    if (integer < std::numeric_limits<int>::min() ||
        integer > std::numeric_limits<int>::max()) {
        throw std::invalid_argument(std::string(name) + " is outside the supported integer range");
    }
    return static_cast<int>(integer);
}

persephone::Interrupt interrupt() {
    // Called synchronously on the R thread, never from background workers.
    return [] { Rcpp::checkUserInterrupt(); };
}

Rcpp::NumericVector period(const std::pair<std::int64_t, int>& value) {
    return Rcpp::NumericVector::create(static_cast<double>(value.first), value.second);
}

persephone::MaskedSeries masked_series(const Rcpp::NumericVector& values) {
    const auto size = static_cast<std::size_t>(values.size());
    persephone::MaskedSeries result{std::vector<double>(size, 0.0),
                                    std::vector<std::uint8_t>(size, 0)};
    for (R_xlen_t i = 0; i < values.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        if (std::isnan(values[i])) {
            result.missing[static_cast<std::size_t>(i)] = 1;
        } else if (!std::isfinite(values[i])) {
            throw std::invalid_argument("series values must be finite or missing; infinity is invalid");
        } else {
            result.values[static_cast<std::size_t>(i)] = values[i];
        }
    }
    return result;
}

} // namespace

// [[Rcpp::export(name = ".native_time_domain")]]
Rcpp::List native_time_domain(double year, double start_period, double frequency,
                             double length) {
    const auto y = scalar_integer(year, "year");
    const auto n = scalar_integer(length, "length");
    const auto f = small_integer(frequency, "frequency");
    // Keep all period ordinals exact when represented by R's numeric type.
    constexpr double exact_limit = 9007199254740991.0;
    if (f > 0 && (std::abs(year) > exact_limit / f ||
                  std::abs(year) * f + std::abs(length) + f > exact_limit)) {
        throw std::invalid_argument("time-domain ordinals exceed R's exact integer range");
    }
    const persephone::TimeDomain domain(y, small_integer(start_period, "start_period"), f, n);
    Rcpp::List result = Rcpp::List::create(
        Rcpp::Named("start") = period(domain.at(0)),
        Rcpp::Named("end") = R_NilValue,
        Rcpp::Named("next") = period(domain.at(n)),
        Rcpp::Named("frequency") = f,
        Rcpp::Named("length") = length);
    if (n > 0) result["end"] = period(domain.at(n - 1));
    return result;
}

// [[Rcpp::export(name = ".native_difference_polynomial")]]
Rcpp::NumericVector native_difference_polynomial(double d = 1, double seasonal_d = 0,
                                               double frequency = 12) {
    return Rcpp::wrap(persephone::difference_polynomial(
        small_integer(d, "d"), small_integer(seasonal_d, "seasonal_d"),
        small_integer(frequency, "frequency"), interrupt()));
}

// [[Rcpp::export(name = ".native_convolve")]]
Rcpp::NumericVector native_convolve(Rcpp::NumericVector left, Rcpp::NumericVector right) {
    return Rcpp::wrap(persephone::convolve(Rcpp::as<std::vector<double>>(left),
                                          Rcpp::as<std::vector<double>>(right), interrupt()));
}

// [[Rcpp::export(name = ".native_filter")]]
Rcpp::List native_filter(Rcpp::NumericVector values, Rcpp::NumericVector weights,
                        Rcpp::NumericVector offsets) {
    std::vector<std::int64_t> positions;
    positions.reserve(static_cast<std::size_t>(offsets.size()));
    for (R_xlen_t i = 0; i < offsets.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        positions.push_back(scalar_integer(offsets[i], "offsets"));
    }
    const auto result = persephone::finite_filter(masked_series(values),
        Rcpp::as<std::vector<double>>(weights), positions, interrupt());
    Rcpp::NumericVector output(result.values.size());
    Rcpp::LogicalVector missing(result.values.size());
    for (std::size_t i = 0; i < result.values.size(); ++i) {
        if ((i & 4095U) == 0) Rcpp::checkUserInterrupt();
        missing[static_cast<R_xlen_t>(i)] = result.missing[i] != 0;
        output[static_cast<R_xlen_t>(i)] = result.missing[i] ? NA_REAL : result.values[i];
    }
    return Rcpp::List::create(Rcpp::Named("values") = output,
                              Rcpp::Named("missing") = missing);
}
