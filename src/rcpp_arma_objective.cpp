#include <Rcpp.h>
#include "core/arma_objective.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace {
const std::array<std::string, 4> block_names{{"ar", "ma", "seasonal_ar", "seasonal_ma"}};

Rcpp::NumericVector vector(SEXP value, const char* name, R_xlen_t limit = 16) {
    if ((TYPEOF(value) != REALSXP && TYPEOF(value) != INTSXP) || Rf_isFactor(value) ||
        !Rf_isNull(Rf_getAttrib(value, R_DimSymbol)) || Rf_xlength(value) > limit) {
        throw std::invalid_argument(std::string(name) + " must be a real numeric vector within the supported length");
    }
    return Rcpp::as<Rcpp::NumericVector>(value);
}
double scalar(SEXP value, const char* name, double fallback) {
    if (Rf_isNull(value)) return fallback;
    const auto v = vector(value, name, 1);
    if (v.size() != 1) throw std::invalid_argument(std::string(name) + " must be one numeric scalar");
    return v[0];
}
int integer(SEXP value, const char* name, int fallback, int maximum) {
    const auto v = scalar(value, name, fallback);
    if (!std::isfinite(v) || v < 0 || v > maximum || v != std::floor(v)) {
        throw std::invalid_argument(std::string(name) + " is outside its supported integer range");
    }
    return static_cast<int>(v);
}
double optional(double value) { return std::isnan(value) ? NA_REAL : value; }
Rcpp::NumericVector numeric(const std::vector<double>& values) {
    Rcpp::NumericVector result(values.size());
    for (std::size_t i = 0; i < values.size(); ++i) result[static_cast<R_xlen_t>(i)] = optional(values[i]);
    return result;
}
Rcpp::List named_blocks(const std::array<std::vector<double>, 4>& blocks) {
    Rcpp::List result;
    for (std::size_t b = 0; b < blocks.size(); ++b) result[block_names[b]] = numeric(blocks[b]);
    return result;
}
Rcpp::List named_list(SEXP value, const char* name, bool required) {
    if (!required && Rf_isNull(value)) return Rcpp::List();
    if (TYPEOF(value) != VECSXP || !Rf_isNull(Rf_getAttrib(value, R_DimSymbol))) {
        throw std::invalid_argument(std::string(name) + " must be a named list of ARMA factors");
    }
    const Rcpp::List result(value);
    if (result.size() == 0 && !required) return result;
    const auto names = Rf_getAttrib(value, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || Rf_xlength(names) != result.size()) throw std::invalid_argument("ARMA factor lists require unique names");
    std::array<bool, 4> seen{};
    for (R_xlen_t i = 0; i < result.size(); ++i) {
        if (STRING_ELT(names, i) == NA_STRING) throw std::invalid_argument("ARMA factor names cannot be missing");
        const std::string label = Rcpp::as<std::string>(STRING_ELT(names, i));
        const auto found = std::find(block_names.begin(), block_names.end(), label);
        if (found == block_names.end() || seen[static_cast<std::size_t>(found - block_names.begin())]) {
            throw std::invalid_argument("ARMA factor names must be unique and known");
        }
        seen[static_cast<std::size_t>(found - block_names.begin())] = true;
    }
    if (required && result.size() != 4) throw std::invalid_argument("ARMA specification must contain all four factors");
    return result;
}
persephone::ArmaParameterMap parameter_map(SEXP specification, SEXP initial, SEXP margin) {
    const auto specs = named_list(specification, "specification", true);
    const auto starts = named_list(initial, "initial", false);
    std::array<persephone::ArmaParameterBlock, 4> blocks;
    for (std::size_t b = 0; b < blocks.size(); ++b) {
        const auto& name = block_names[b];
        const auto spec = vector(specs[name], "ARMA specification");
        auto& block = blocks[b];
        block.initial.resize(static_cast<std::size_t>(spec.size()));
        block.fixed.resize(static_cast<std::size_t>(spec.size()));
        const bool provided = starts.containsElementNamed(name.c_str());
        const auto start = provided ? vector(starts[name], "ARMA initial values") : Rcpp::NumericVector(spec.size());
        if (start.size() != spec.size()) throw std::invalid_argument("ARMA initial values must match their factor length");
        for (R_xlen_t j = 0; j < spec.size(); ++j) {
            const bool free = R_IsNA(spec[j]);
            if ((!free && !std::isfinite(spec[j])) || (provided && !std::isfinite(start[j]))) {
                throw std::invalid_argument("ARMA specifications require finite fixed values or NA; initial values must be finite");
            }
            if (!free && provided && start[j] != spec[j]) throw std::invalid_argument("ARMA initial values must preserve fixed coefficients exactly");
            block.fixed[static_cast<std::size_t>(j)] = free ? 0 : 1;
            block.initial[static_cast<std::size_t>(j)] = free ? start[j] : spec[j];
        }
    }
    return persephone::ArmaParameterMap(std::move(blocks), scalar(margin, "reflection margin", 1e-8));
}
Rcpp::List decoded(const persephone::ArmaDecoded& result) {
    return Rcpp::List::create(Rcpp::_["status"] = result.status,
        Rcpp::_["admissible"] = result.status == "admissible",
        Rcpp::_["failed_block"] = result.failed_block < 0 ? Rcpp::CharacterVector::create(NA_STRING) :
            Rcpp::CharacterVector::create(block_names[static_cast<std::size_t>(result.failed_block)]),
        Rcpp::_["failed_order"] = static_cast<int>(result.failed_order),
        Rcpp::_["coefficients"] = named_blocks(result.coefficients),
        Rcpp::_["reflections"] = named_blocks(result.reflections));
}
} // namespace

// [[Rcpp::export(name = ".native_arma_parameter_map")]]
Rcpp::List native_arma_parameter_map(SEXP specification, SEXP initial = R_NilValue, SEXP margin = R_NilValue) {
    const auto map = parameter_map(specification, initial, margin);
    Rcpp::List starts, fixed, modes;
    Rcpp::CharacterVector labels;
    for (std::size_t b = 0; b < 4; ++b) {
        const auto& name = block_names[b];
        const auto& block = map.blocks()[b];
        starts[name] = numeric(block.initial);
        Rcpp::LogicalVector flags(block.fixed.size());
        for (std::size_t j = 0; j < block.fixed.size(); ++j) {
            flags[static_cast<R_xlen_t>(j)] = block.fixed[j] != 0;
            if (!block.fixed[j]) labels.push_back(name + (map.modes()[b] == "reflection" ? ".reflection" : ".coefficient") + std::to_string(j + 1));
        }
        fixed[name] = flags;
        modes[name] = map.modes()[b];
    }
    auto coordinates = numeric(map.initial_coordinates());
    coordinates.attr("names") = labels;
    return Rcpp::List::create(Rcpp::_["schema_version"] = 1,
        Rcpp::_["specification"] = specification, Rcpp::_["initial"] = starts,
        Rcpp::_["fixed"] = fixed, Rcpp::_["modes"] = modes,
        Rcpp::_["initial_coordinates"] = coordinates,
        Rcpp::_["reflection_margin"] = map.margin(),
        Rcpp::_["constraint"] = "strict_stationarity_and_invertibility");
}

// [[Rcpp::export(name = ".native_arma_decode")]]
Rcpp::List native_arma_decode(SEXP specification, SEXP initial, SEXP coordinates, SEXP margin = R_NilValue) {
    const auto map = parameter_map(specification, initial, margin);
    return decoded(map.decode(Rcpp::as<std::vector<double>>(vector(coordinates, "ARMA coordinates", 64))));
}

// [[Rcpp::export(name = ".native_arma_admissibility")]]
Rcpp::List native_arma_admissibility(SEXP coefficients, SEXP moving_average, SEXP margin = R_NilValue) {
    if (TYPEOF(moving_average) != LGLSXP || Rf_xlength(moving_average) != 1 ||
        !Rf_isNull(Rf_getAttrib(moving_average, R_DimSymbol)) || LOGICAL(moving_average)[0] == NA_LOGICAL) {
        throw std::invalid_argument("moving_average must be one nonmissing logical value");
    }
    const auto result = persephone::arma_admissibility(Rcpp::as<std::vector<double>>(vector(coefficients, "coefficients")),
        LOGICAL(moving_average)[0] != 0, scalar(margin, "reflection margin", 1e-8));
    return Rcpp::List::create(Rcpp::_["status"] = result.status,
        Rcpp::_["reflections"] = numeric(result.reflections), Rcpp::_["failed_order"] = static_cast<int>(result.failed_order));
}

// [[Rcpp::export(name = ".native_arma_objective")]]
Rcpp::List native_arma_objective(SEXP values, SEXP design, SEXP coefficients,
    SEXP specification, SEXP initial, SEXP coordinates, SEXP period,
    SEXP d = R_NilValue, SEXP seasonal_d = R_NilValue, SEXP mean = R_NilValue,
    SEXP rank_tolerance = R_NilValue, SEXP margin = R_NilValue, SEXP log_jacobian = R_NilValue) {
    const auto map = parameter_map(specification, initial, margin);
    const auto response = vector(values, "values", std::numeric_limits<int>::max());
    persephone::MaskedSeries input{std::vector<double>(response.size(), 0), std::vector<std::uint8_t>(response.size(), 0)};
    for (R_xlen_t i = 0; i < response.size(); ++i) {
        if ((i & 4095) == 0) Rcpp::checkUserInterrupt();
        if (std::isnan(response[i])) input.missing[static_cast<std::size_t>(i)] = 1;
        else input.values[static_cast<std::size_t>(i)] = response[i];
    }
    const auto dims = Rf_getAttrib(design, R_DimSymbol);
    if ((TYPEOF(design) != REALSXP && TYPEOF(design) != INTSXP) || Rf_isFactor(design) ||
        TYPEOF(dims) != INTSXP || Rf_xlength(dims) != 2) throw std::invalid_argument("design must be a real numeric matrix");
    const Rcpp::IntegerVector shape(dims);
    if (shape[0] != response.size() || shape[1] < 0 || shape[1] > 32 ||
        Rf_xlength(design) != static_cast<R_xlen_t>(shape[0]) * shape[1]) throw std::invalid_argument("design dimensions must match response rows and at most 32 columns");
    const auto coeff = vector(coefficients, "regression coefficients", 32);
    if (coeff.size() != shape[1]) throw std::invalid_argument("regression coefficients must match design columns");
    const auto x = Rcpp::as<Rcpp::NumericVector>(design);
    persephone::RegressionDesign regression{static_cast<std::size_t>(shape[1]),
        std::vector<double>(x.begin(), x.end()), std::vector<double>(coeff.size(), 0), std::vector<std::uint8_t>(coeff.size(), 0)};
    for (R_xlen_t i = 0; i < coeff.size(); ++i) {
        if (R_IsNA(coeff[i])) continue;
        if (!std::isfinite(coeff[i])) throw std::invalid_argument("regression coefficients must be finite or NA");
        regression.coefficients[static_cast<std::size_t>(i)] = coeff[i]; regression.fixed[static_cast<std::size_t>(i)] = 1;
    }
    const auto result = persephone::evaluate_arma_objective(input, regression, map,
        Rcpp::as<std::vector<double>>(vector(coordinates, "ARMA coordinates", 64)),
        integer(period, "period", 0, 12), integer(d, "d", 0, 2), integer(seasonal_d, "seasonal_d", 0, 2),
        scalar(mean, "mean", 0), scalar(rank_tolerance, "rank tolerance", 1e-10), scalar(log_jacobian, "log Jacobian", 0),
        [] { Rcpp::checkUserInterrupt(); });
    Rcpp::List output = Rcpp::List::create(Rcpp::_["schema_version"] = 1,
        Rcpp::_["status"] = result.status, Rcpp::_["message"] = result.message,
        Rcpp::_["available"] = result.status == "available", Rcpp::_["value"] = optional(result.value),
        Rcpp::_["adjusted_log_likelihood"] = optional(result.adjusted_log_likelihood),
        Rcpp::_["parameters"] = decoded(result.parameters));
    if (result.likelihood) {
        const auto& fit = *result.likelihood;
        output["likelihood"] = Rcpp::List::create(
            Rcpp::_["profiled_log_likelihood"] = optional(fit.profiled_log_likelihood),
            Rcpp::_["profiled_variance"] = optional(fit.profiled_variance),
            Rcpp::_["profile_status"] = fit.profile_status, Rcpp::_["finite_count"] = static_cast<double>(fit.finite_count),
            Rcpp::_["regression_rank"] = static_cast<int>(fit.rank), Rcpp::_["regression_coefficients"] = numeric(fit.coefficients),
            Rcpp::_["nparams_profiled"] = result.status == "available" ?
                static_cast<double>(fit.free_columns.size() + map.initial_coordinates().size() + 1) : NA_REAL);
    } else output["likelihood"] = R_NilValue;
    return output;
}
