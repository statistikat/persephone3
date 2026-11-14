#include <Rcpp.h>
#include "core/arma_objective.hpp"
#include "core/arma_optimizer.hpp"
#include "core/arma_identification.hpp"
#include "core/arma_information.hpp"
#include "core/arma_residuals.hpp"
#include "core/interpolation_information.hpp"

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
struct ObjectiveData {
    persephone::MaskedSeries input;
    persephone::RegressionDesign regression;
};
ObjectiveData objective_data(SEXP values, SEXP design, SEXP coefficients) {
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
    return {std::move(input),std::move(regression)};
}
Rcpp::List objective_output(const persephone::ArmaObjective& result, std::size_t free_parameters) {
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
                static_cast<double>(fit.free_columns.size() + free_parameters + 1) : NA_REAL);
    } else output["likelihood"] = R_NilValue;
    return output;
}
Rcpp::List identification_output(const persephone::ArmaIdentification& result) {
    Rcpp::List out = Rcpp::List::create(Rcpp::_["schema_version"] = 1,
        Rcpp::_["status"] = result.status, Rcpp::_["parameter_status"] = result.parameter_status,
        Rcpp::_["scope"] = "local_transfer_sensitivity", Rcpp::_["statistical_identification"] = "not_assessed",
        Rcpp::_["coordinates"] = "free_polynomial_coefficients",
        Rcpp::_["free_parameters"] = static_cast<int>(result.free_parameters),
        Rcpp::_["rank"] = result.parameter_status == "admissible" ? result.rank : NA_INTEGER,
        Rcpp::_["singular_ratio"] = optional(result.singular_ratio),
        Rcpp::_["minimum_reflection_slack"] = optional(result.minimum_reflection_slack),
        Rcpp::_["singular_values"] = numeric(result.singular_values),
        Rcpp::_["column_norms"] = numeric(result.column_norms),
        Rcpp::_["rank_tolerance"] = result.rank_tolerance, Rcpp::_["near_tolerance"] = result.near_tolerance);
    if (result.numerator_jacobian) {
        const auto& a = *result.numerator_jacobian;
        out["numerator_jacobian"] = Rcpp::NumericMatrix(static_cast<int>(a.rows()),
            static_cast<int>(a.columns()), a.values().begin());
    } else out["numerator_jacobian"] = R_NilValue;
    return out;
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

// [[Rcpp::export(name = ".native_arma_identification")]]
Rcpp::List native_arma_identification(SEXP specification, SEXP initial, SEXP coordinates, SEXP period,
    SEXP margin = R_NilValue, SEXP rank_tolerance = R_NilValue, SEXP near_tolerance = R_NilValue) {
    const auto map = parameter_map(specification, initial, margin);
    return identification_output(persephone::arma_identification(map,
        Rcpp::as<std::vector<double>>(vector(coordinates, "ARMA coordinates", 64)),
        integer(period, "period", 0, 12), scalar(rank_tolerance, "identification rank tolerance", 1e-10),
        scalar(near_tolerance, "identification near tolerance", 1e-4), [] { Rcpp::checkUserInterrupt(); }));
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
    auto data = objective_data(values, design, coefficients);
    const auto& input = data.input;
    const auto& regression = data.regression;
    const auto result = persephone::evaluate_arma_objective(input, regression, map,
        Rcpp::as<std::vector<double>>(vector(coordinates, "ARMA coordinates", 64)),
        integer(period, "period", 0, 12), integer(d, "d", 0, 2), integer(seasonal_d, "seasonal_d", 0, 2),
        scalar(mean, "mean", 0), scalar(rank_tolerance, "rank tolerance", 1e-10), scalar(log_jacobian, "log Jacobian", 0),
        [] { Rcpp::checkUserInterrupt(); });
    return objective_output(result, map.initial_coordinates().size());
}

namespace {
persephone::OptimizerControl optimizer_control(SEXP values) {
    persephone::OptimizerControl c;
    if(Rf_isNull(values)) return c;
    if(TYPEOF(values)!=VECSXP || !Rf_isNull(Rf_getAttrib(values,R_DimSymbol))) throw std::invalid_argument("optimizer control must be a named list");
    const Rcpp::List list(values);
    if(list.size()==0) return c;
    const auto names=Rf_getAttrib(values,R_NamesSymbol);
    if(TYPEOF(names)!=STRSXP || Rf_xlength(names)!=list.size()) throw std::invalid_argument("optimizer controls need unique names");
    std::vector<std::string> seen;
    for(R_xlen_t i=0;i<list.size();++i) {
        if(STRING_ELT(names,i)==NA_STRING) throw std::invalid_argument("optimizer control names cannot be missing");
        const auto name=Rcpp::as<std::string>(STRING_ELT(names,i));
        if(std::find(seen.begin(),seen.end(),name)!=seen.end() || Rf_isNull(list[i])) throw std::invalid_argument("optimizer controls need distinct names and non-NULL values");
        seen.push_back(name);
        if(name=="max_iterations") c.max_iterations=integer(list[i],name.c_str(),100,10000);
        else if(name=="max_evaluations") c.max_evaluations=integer(list[i],name.c_str(),2000,1000000);
        else if(name=="max_line_search") c.max_line_search=integer(list[i],name.c_str(),30,100);
        else if(name=="difference_refinements") c.difference_refinements=integer(list[i],name.c_str(),10,20);
        else if(name=="gradient_tolerance") c.gradient_tolerance=scalar(list[i],name.c_str(),1e-5);
        else if(name=="step_tolerance") c.step_tolerance=scalar(list[i],name.c_str(),1e-10);
        else if(name=="function_tolerance") c.function_tolerance=scalar(list[i],name.c_str(),1e-10);
        else if(name=="difference_step") c.difference_step=scalar(list[i],name.c_str(),1e-4);
        else if(name=="armijo") c.armijo=scalar(list[i],name.c_str(),1e-4);
        else if(name=="max_step") c.max_step=scalar(list[i],name.c_str(),1);
        else throw std::invalid_argument("unknown optimizer control: "+name);
    }
    c.validate(); return c;
}
Rcpp::List control_output(const persephone::OptimizerControl& c) {
    return Rcpp::List::create(Rcpp::_["max_iterations"]=c.max_iterations,Rcpp::_["max_evaluations"]=c.max_evaluations,
        Rcpp::_["max_line_search"]=c.max_line_search,Rcpp::_["difference_refinements"]=c.difference_refinements,
        Rcpp::_["gradient_tolerance"]=c.gradient_tolerance,Rcpp::_["step_tolerance"]=c.step_tolerance,
        Rcpp::_["function_tolerance"]=c.function_tolerance,Rcpp::_["difference_step"]=c.difference_step,
        Rcpp::_["armijo"]=c.armijo,Rcpp::_["max_step"]=c.max_step);
}
persephone::InformationControl information_control(SEXP values) {
    persephone::InformationControl c;
    if(Rf_isNull(values)) return c;
    if(TYPEOF(values)!=VECSXP || !Rf_isNull(Rf_getAttrib(values,R_DimSymbol))) throw std::invalid_argument("information control must be a named list");
    const Rcpp::List list(values);
    if(list.size()==0) return c;
    const auto names=Rf_getAttrib(values,R_NamesSymbol);
    if(TYPEOF(names)!=STRSXP || Rf_xlength(names)!=list.size()) throw std::invalid_argument("information controls need unique names");
    std::vector<std::string> seen;
    for(R_xlen_t i=0;i<list.size();++i) {
        if(STRING_ELT(names,i)==NA_STRING) throw std::invalid_argument("information control names cannot be missing");
        const auto name=Rcpp::as<std::string>(STRING_ELT(names,i));
        if(std::find(seen.begin(),seen.end(),name)!=seen.end() || Rf_isNull(list[i])) throw std::invalid_argument("information controls need distinct names and non-NULL values");
        seen.push_back(name);
        if(name=="difference_step") c.difference_step=scalar(list[i],name.c_str(),1e-3);
        else if(name=="difference_refinements") c.difference_refinements=integer(list[i],name.c_str(),8,16);
        else if(name=="max_evaluations") c.max_evaluations=integer(list[i],name.c_str(),20000,1000000);
        else if(name=="relative_tolerance") c.relative_tolerance=scalar(list[i],name.c_str(),1e-4);
        else if(name=="absolute_tolerance") c.absolute_tolerance=scalar(list[i],name.c_str(),1e-6);
        else if(name=="gradient_tolerance") c.gradient_tolerance=scalar(list[i],name.c_str(),1e-4);
        else if(name=="rank_tolerance") c.rank_tolerance=scalar(list[i],name.c_str(),1e-10);
        else if(name=="weak_tolerance") c.weak_tolerance=scalar(list[i],name.c_str(),1e-6);
        else throw std::invalid_argument("unknown information control: "+name);
    }
    c.validate(); return c;
}
Rcpp::RObject matrix_or_null(const std::optional<persephone::DenseMatrix>& a) {
    if(!a) return R_NilValue;
    return Rcpp::NumericMatrix(static_cast<int>(a->rows()),static_cast<int>(a->columns()),a->values().begin());
}
std::string information_option(SEXP value, const char* name, const std::string& fallback) {
    if(Rf_isNull(value)) return fallback;
    if(TYPEOF(value)!=STRSXP || Rf_xlength(value)!=1 || STRING_ELT(value,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(value,R_DimSymbol))) throw std::invalid_argument(std::string(name)+" must be one nonmissing string");
    return Rcpp::as<std::string>(value);
}
std::optional<persephone::LegacyCovarianceState> legacy_covariance_state(SEXP input) {
    if(Rf_isNull(input)) return std::nullopt;
    const std::vector<std::string> required{"final_coefficients","working_coefficients","difference_steps",
        "damping","normalized_sum_squares","effective_count"};
    if(TYPEOF(input)!=VECSXP || !Rf_isNull(Rf_getAttrib(input,R_DimSymbol))) throw std::invalid_argument("legacy state must be a named list");
    const Rcpp::List list(input); const auto names=Rf_getAttrib(input,R_NamesSymbol);
    if(TYPEOF(names)!=STRSXP || Rf_xlength(names)!=6 || list.size()!=6) throw std::invalid_argument("legacy state requires all six fields");
    std::vector<std::string> seen;
    for(R_xlen_t i=0;i<6;++i) {
        if(STRING_ELT(names,i)==NA_STRING || Rf_isNull(list[i])) throw std::invalid_argument("legacy state fields cannot be missing or NULL");
        const auto name=Rcpp::as<std::string>(STRING_ELT(names,i));
        if(std::find(required.begin(),required.end(),name)==required.end() || std::find(seen.begin(),seen.end(),name)!=seen.end()) {
            throw std::invalid_argument("legacy state requires unique known fields");
        }
        seen.push_back(name);
    }
    persephone::LegacyCovarianceState state;
    state.final_coefficients=Rcpp::as<std::vector<double>>(vector(list["final_coefficients"],"legacy final coefficients",64));
    state.working_coefficients=Rcpp::as<std::vector<double>>(vector(list["working_coefficients"],"legacy working coefficients",64));
    state.difference_steps=Rcpp::as<std::vector<double>>(vector(list["difference_steps"],"legacy difference steps",64));
    state.damping=scalar(list["damping"],"legacy damping",0);
    state.normalized_sum_squares=scalar(list["normalized_sum_squares"],"legacy residual scale",0);
    state.effective_count=static_cast<std::size_t>(integer(list["effective_count"],"legacy effective count",0,std::numeric_limits<int>::max()));
    return state;
}
Rcpp::List arma_information_output(const persephone::ArmaInformation& result,
    const persephone::InformationControl& c, const std::string& selected, SEXP legacy_state) {
    const bool conditional=selected=="conditional_residual_df",residual=selected=="tramo_residual";
    const std::string covariance_mode=result.covariance_mode==persephone::ArmaCovarianceMode::Legacy ? "legacy" : "accurate";
    const auto& info=result.information;
    Rcpp::List out;
    out["schema_version"]=1; out["status"]=info.status;
    out["available"]=info.status=="available" || info.status=="no_free_parameters";
    out["method"]=selected;
    out["information_type"]=residual ? "Gauss_Newton_profiled_normalized_residuals" : "observed";
    out["covariance_mode"]=covariance_mode;
    out["comparison_only"]=covariance_mode=="legacy";
    out["coordinates"]="free_polynomial_coefficients";
    out["scale_convention"]=conditional || residual ? "residual_df_n_minus_free_regression_minus_free_ARMA" : "profiled_ML_Q_over_n_finite";
    out["regression_convention"]=conditional ? "held_at_fitted_GLS_coefficients" : "profiled_at_each_candidate";
    out["inference"]="local_asymptotic_normal_approximation";
    out["reference_parity"]="not_qualified";
    out["free_regression"]=static_cast<int>(result.free_regression);
    out["residual_df"]=result.estimation_center && result.estimation_center->status=="available" ? static_cast<double>(result.residual_df) : NA_REAL;
    out["covariance_multiplier"]=optional(result.covariance_multiplier);
    out["coefficients"]=numeric(result.coefficients);
    out["gradient"]=numeric(info.gradient); out["gradient_error"]=numeric(info.gradient_error);
    if(residual) {
        out["normal_information"]=matrix_or_null(info.hessian); out["normal_information_error"]=matrix_or_null(info.hessian_error);
        out["hessian"]=R_NilValue; out["hessian_error"]=R_NilValue;
        out["residual_jacobian"]=result.residual_information ? matrix_or_null(result.residual_information->jacobian) : Rcpp::RObject(R_NilValue);
        out["residual_jacobian_error"]=result.residual_information ? matrix_or_null(result.residual_information->jacobian_error) : Rcpp::RObject(R_NilValue);
        out["residual_basis"]="complete_span_missing_coordinate_projection";
        out["residual_normalization"]="determinant_normalized_relative_to_center_divided_by_sqrt_center_Q";
        if(result.residual_center) {
            const auto& basis=*result.residual_center;
            Rcpp::IntegerVector rows,missing;
            for(const auto i:basis.rows) rows.push_back(static_cast<int>(i+1));
            for(const auto i:basis.missing_rows) missing.push_back(static_cast<int>(i+1));
            out["residual_center"]=Rcpp::List::create(Rcpp::_["status"]=basis.status,
                Rcpp::_["residuals"]=numeric(basis.residuals),Rcpp::_["rows"]=rows,Rcpp::_["missing_rows"]=missing,
                Rcpp::_["interpolated"]=numeric(basis.interpolated),Rcpp::_["regression_coefficients"]=numeric(basis.regression_coefficients),
                Rcpp::_["effective_count"]=static_cast<double>(basis.finite_count),Rcpp::_["sum_squares"]=basis.sum_squares,
                Rcpp::_["log_determinant"]=basis.log_determinant);
        } else out["residual_center"]=R_NilValue;
    } else { out["hessian"]=matrix_or_null(info.hessian); out["hessian_error"]=matrix_or_null(info.hessian_error); }
    out["covariance"]=matrix_or_null(info.covariance);
    if(info.status=="no_free_parameters") out["covariance"]=Rcpp::NumericMatrix(0,0);
    if(covariance_mode=="legacy") {
        out["accurate_covariance"]=matrix_or_null(result.accurate_covariance);
        if(info.status=="no_free_parameters") out["accurate_covariance"]=Rcpp::NumericMatrix(0,0);
        out["legacy_state"]=legacy_state;
        out["legacy_jacobian"]=matrix_or_null(result.legacy_jacobian);
        out["legacy_information"]=matrix_or_null(result.legacy_information);
        out["normalized_legacy_damping"]=result.normalized_legacy_damping;
        out["legacy_derivative_convention"]="recorded_working_point_signed_forward_differences";
    }
    out["derivative_status"]=info.derivative_status; out["curvature_status"]=info.curvature_status;
    out["stationary"]=info.stationary; out["difference_steps"]=numeric(info.steps);
    out["evaluations"]=info.evaluations; out["refinements"]=info.refinements;
    out["rank"]=info.normalized_eigenvalues.empty() ? NA_INTEGER : info.rank;
    out["normalized_eigenvalues"]=numeric(info.normalized_eigenvalues);
    out["normalized_error_estimate"]=info.normalized_eigenvalues.empty() ? NA_REAL : info.normalized_error;
    out["eigenvalue_ratio"]=info.normalized_eigenvalues.empty() ? NA_REAL : info.eigenvalue_ratio;
    Rcpp::IntegerVector rejected; Rcpp::CharacterVector reasons;
    for(const auto& entry:info.rejected) { reasons.push_back(entry.first); rejected.push_back(entry.second); }
    rejected.attr("names")=reasons; out["rejected"]=rejected;
    out["parameter_diagnostics"]=identification_output(result.structural);
    const auto center=result.estimation_center;
    out["center"]=center ? static_cast<SEXP>(objective_output(*center,result.coefficients.size())) : R_NilValue;
    const auto regression=persephone::conditional_regression_information(result,[] { Rcpp::checkUserInterrupt(); });
    Rcpp::IntegerVector free_columns;
    for(const auto j:regression.free_columns) free_columns.push_back(static_cast<int>(j+1));
    Rcpp::List regression_output=Rcpp::List::create(Rcpp::_["schema_version"]=1,
        Rcpp::_["status"]=regression.status,
        Rcpp::_["available"]=regression.status=="available" || regression.status=="no_free_coefficients",
        Rcpp::_["method"]="conditional_residual_df",Rcpp::_["conditioned_on"]="supplied_ARMA_coefficients",
        Rcpp::_["propagates_ARMA_uncertainty"]=false,
        Rcpp::_["scale_convention"]="Q_over_n_minus_free_regression_minus_free_ARMA",
        Rcpp::_["free_columns"]=free_columns,Rcpp::_["coefficients"]=numeric(regression.coefficients),
        Rcpp::_["covariance"]=matrix_or_null(regression.covariance),
        Rcpp::_["effective_count"]=center && center->likelihood ? static_cast<double>(regression.effective_count) : NA_REAL,
        Rcpp::_["residual_df"]=center && center->likelihood ? static_cast<double>(regression.residual_df) : NA_REAL,
        Rcpp::_["residual_variance"]=optional(regression.residual_variance),
        Rcpp::_["free_ARMA"]=static_cast<int>(regression.free_arma),
        Rcpp::_["arma_information_status"]=info.status);
    if(regression.status=="no_free_coefficients") regression_output["covariance"]=Rcpp::NumericMatrix(0,0);
    out["regression_information"]=regression_output;
    out["control"]=Rcpp::List::create(Rcpp::_["difference_step"]=c.difference_step,
        Rcpp::_["difference_refinements"]=c.difference_refinements,Rcpp::_["max_evaluations"]=c.max_evaluations,
        Rcpp::_["relative_tolerance"]=c.relative_tolerance,Rcpp::_["absolute_tolerance"]=c.absolute_tolerance,
        Rcpp::_["gradient_tolerance"]=c.gradient_tolerance,Rcpp::_["rank_tolerance"]=c.rank_tolerance,
        Rcpp::_["weak_tolerance"]=c.weak_tolerance);
    return out;
}
} // namespace

// [[Rcpp::export(name = ".native_arma_information")]]
Rcpp::List native_arma_information(SEXP values, SEXP design, SEXP coefficients,
    SEXP specification, SEXP initial, SEXP coordinates, SEXP period,
    SEXP d=R_NilValue, SEXP seasonal_d=R_NilValue, SEXP mean=R_NilValue,
    SEXP rank_tolerance=R_NilValue, SEXP margin=R_NilValue, SEXP log_jacobian=R_NilValue,
    SEXP control=R_NilValue, SEXP method=R_NilValue, SEXP covariance=R_NilValue, SEXP legacy_state=R_NilValue) {
    const auto c=information_control(control);
    const auto map=parameter_map(specification,initial,margin);
    const auto data=objective_data(values,design,coefficients);
    const auto selected=information_option(method,"information method","profile_ml");
    const auto covariance_mode=information_option(covariance,"covariance mode","accurate");
    if(covariance_mode!="accurate" && covariance_mode!="legacy") throw std::invalid_argument("unknown covariance mode");
    if(selected!="profile_ml" && selected!="conditional_residual_df" && selected!="tramo_residual") throw std::invalid_argument("unknown information method");
    const bool conditional=selected=="conditional_residual_df";
    const bool residual=selected=="tramo_residual";
    const auto result=persephone::arma_information(data.input,data.regression,map,
        Rcpp::as<std::vector<double>>(vector(coordinates,"ARMA coordinates",64)),
        integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
        scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),scalar(log_jacobian,"log Jacobian",0),
        c,[] { Rcpp::checkUserInterrupt(); }, residual ? persephone::ArmaInformationMode::TramoResidual :
            conditional ? persephone::ArmaInformationMode::ConditionalResidualDF :
            persephone::ArmaInformationMode::ProfileML,
        covariance_mode=="legacy" ? persephone::ArmaCovarianceMode::Legacy : persephone::ArmaCovarianceMode::Accurate,
        legacy_covariance_state(legacy_state));
    return arma_information_output(result,c,selected,legacy_state);
}

// [[Rcpp::export(name = ".native_joint_regression_information")]]
Rcpp::List native_joint_regression_information(SEXP values, SEXP design, SEXP coefficients,
    SEXP specification, SEXP initial, SEXP coordinates, SEXP period,
    SEXP d=R_NilValue, SEXP seasonal_d=R_NilValue, SEXP mean=R_NilValue,
    SEXP rank_tolerance=R_NilValue, SEXP margin=R_NilValue, SEXP log_jacobian=R_NilValue,
    SEXP control=R_NilValue) {
    const auto c=information_control(control);
    const auto map=parameter_map(specification,initial,margin);
    const auto data=objective_data(values,design,coefficients);
    const auto result=persephone::joint_regression_information(data.input,data.regression,map,
        Rcpp::as<std::vector<double>>(vector(coordinates,"ARMA coordinates",64)),
        integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
        scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),scalar(log_jacobian,"log Jacobian",0),
        c,[] { Rcpp::checkUserInterrupt(); });
    const bool available=result.status=="available" || result.status=="no_free_parameters";
    const auto r=static_cast<int>(result.free_columns.size()),k=static_cast<int>(map.initial_coordinates().size());
    Rcpp::List out;
    out["schema_version"]=1;out["status"]=result.status;out["available"]=available;
    out["method"]="joint_profile_ml";out["conditioned_on"]="fixed_model_and_fixed_coefficients";
    out["propagates_ARMA_uncertainty"]=true;out["scale_convention"]="profiled_ML_Q_over_n_finite";
    out["inference"]="local_asymptotic_normal_approximation";
    out["arma_information"]=arma_information_output(result.arma,c,"profile_ml",R_NilValue);
    Rcpp::IntegerVector columns;
    for(const auto j:result.free_columns) columns.push_back(static_cast<int>(j+1));
    out["free_columns"]=columns;out["coefficients"]=numeric(result.coefficients);
    out["covariance"]=matrix_or_null(result.covariance);
    out["conditional_ml_covariance"]=matrix_or_null(result.conditional_ml_covariance);
    out["joint_covariance"]=matrix_or_null(result.joint_covariance);
    out["cross_covariance"]=matrix_or_null(result.cross_covariance);
    out["sensitivity"]=matrix_or_null(result.sensitivity);
    out["sensitivity_error"]=matrix_or_null(result.sensitivity_error);
    if(available) {
        if(!r) { out["covariance"]=Rcpp::NumericMatrix(0,0);out["conditional_ml_covariance"]=Rcpp::NumericMatrix(0,0); }
        if(!r && !k) out["joint_covariance"]=Rcpp::NumericMatrix(0,0);
        if(!r || !k) {
            out["cross_covariance"]=Rcpp::NumericMatrix(r,k);
            out["sensitivity"]=Rcpp::NumericMatrix(r,k);out["sensitivity_error"]=Rcpp::NumericMatrix(r,k);
        }
    }
    const bool has_center=result.arma.estimation_center && result.arma.estimation_center->likelihood;
    out["effective_count"]=has_center ? static_cast<double>(result.effective_count) : NA_REAL;
    out["residual_df"]=has_center ? static_cast<double>(result.residual_df) : NA_REAL;
    out["residual_variance"]=optional(result.ml_variance);
    out["evaluations"]=result.evaluations;
    out["sensitivity_status"]=result.derivatives.status;
    out["sensitivity_evaluations"]=result.derivatives.evaluations;
    out["sensitivity_refinements"]=result.derivatives.refinements;
    out["sensitivity_steps"]=numeric(result.derivatives.steps);
    out["sensitivity_scale"]="conditional_ML_coefficient_standard_deviations";
    Rcpp::IntegerVector rejected;Rcpp::CharacterVector reasons;
    for(const auto& entry:result.derivatives.rejected) { reasons.push_back(entry.first);rejected.push_back(entry.second); }
    rejected.attr("names")=reasons;out["sensitivity_rejected"]=rejected;
    out["joint_correlation_eigenvalues"]=numeric(result.normalized_eigenvalues);
    out["joint_eigenvalue_ratio"]=result.normalized_eigenvalues.empty() ? NA_REAL : result.eigenvalue_ratio;
    return out;
}

// [[Rcpp::export(name = ".native_arma_residuals")]]
Rcpp::List native_arma_residuals(SEXP values, SEXP design, SEXP coefficients,
    SEXP specification, SEXP initial, SEXP coordinates, SEXP period,
    SEXP d=R_NilValue, SEXP seasonal_d=R_NilValue, SEXP mean=R_NilValue,
    SEXP rank_tolerance=R_NilValue, SEXP margin=R_NilValue, SEXP log_jacobian=R_NilValue,
    SEXP scale=R_NilValue, SEXP reporting=R_NilValue, SEXP transformation=R_NilValue) {
    const auto selected=information_option(scale,"residual scale","ml");
    if(selected!="ml" && selected!="residual_df") throw std::invalid_argument("residual scale must be ml or residual_df");
    const auto mode=information_option(reporting,"residual reporting","accurate");
    if(mode!="accurate" && mode!="legacy")throw std::invalid_argument("residual reporting must be accurate or legacy");
    const auto transform=information_option(transformation,"transformation","none");
    if(transform!="none" && transform!="log")throw std::invalid_argument("transformation must be none or log");
    const auto map=parameter_map(specification,initial,margin);
    const auto data=objective_data(values,design,coefficients);
    const auto result=persephone::arma_residuals(data.input,data.regression,map,
        Rcpp::as<std::vector<double>>(vector(coordinates,"ARMA coordinates",64)),
        integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
        scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),scalar(log_jacobian,"log Jacobian",0),
        selected=="ml" ? persephone::ResidualScale::ML : persephone::ResidualScale::ResidualDF,
        [] { Rcpp::checkUserInterrupt(); });
    Rcpp::IntegerVector rows,basis_rows,missing;
    for(const auto i:result.rows) rows.push_back(static_cast<int>(i+1));
    for(const auto i:result.complete.rows) basis_rows.push_back(static_cast<int>(i+1));
    for(const auto i:result.complete.missing_rows) missing.push_back(static_cast<int>(i+1));
    const auto& fit=result.center->likelihood;
    Rcpp::List out=Rcpp::List::create(Rcpp::_["schema_version"]=1,Rcpp::_["status"]=result.status,
        Rcpp::_["available"]=result.status=="available",Rcpp::_["scale"]=selected,
        Rcpp::_["scale_variance"]=optional(result.variance),
        Rcpp::_["effective_count"]=fit ? static_cast<double>(result.effective_count) : NA_REAL,
        Rcpp::_["residual_df"]=fit ? static_cast<double>(result.residual_df) : NA_REAL,
        Rcpp::_["free_regression"]=static_cast<int>(result.free_regression),
        Rcpp::_["free_ARMA"]=static_cast<int>(result.free_arma),
        Rcpp::_["sum_squares"]=fit ? optional(fit->sum_squared_standardized) : NA_REAL,
        Rcpp::_["point_scope"]="supplied_ARMA_with_profiled_GLS",
        Rcpp::_["studentized"]=false,Rcpp::_["propagates_parameter_uncertainty"]=false,
        Rcpp::_["center"]=objective_output(*result.center,result.free_arma));
    out["observed"]=Rcpp::List::create(Rcpp::_["rows"]=rows,
        Rcpp::_["unit_innovations"]=numeric(result.unit_innovations),
        Rcpp::_["standardized_innovations"]=numeric(result.standardized_innovations));
    out["complete"]=Rcpp::List::create(Rcpp::_["status"]=result.complete.status,
        Rcpp::_["available"]=result.complete.status=="available",Rcpp::_["message"]=result.complete_message,
        Rcpp::_["rows"]=basis_rows,Rcpp::_["missing_rows"]=missing,
        Rcpp::_["unit_residuals"]=numeric(result.complete.residuals),
        Rcpp::_["scaled_residuals"]=numeric(result.scaled_complete_residuals),
        Rcpp::_["interpolated"]=numeric(result.complete.interpolated),
        Rcpp::_["regression_coefficients"]=numeric(result.complete.regression_coefficients),
        Rcpp::_["effective_count"]=result.complete.status=="available" ? static_cast<double>(result.complete.finite_count) : NA_REAL,
        Rcpp::_["sum_squares"]=result.complete.status=="available" ? result.complete.sum_squares : NA_REAL,
        Rcpp::_["basis"]="complete_span_missing_coordinate_projection");
    out["reporting"]=mode;
    if(mode=="legacy") {
        const auto legacy=persephone::legacy_arma_residuals(data.input,data.regression,result,
            integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
            scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),
            transform=="log" ? persephone::Transform::logarithm : persephone::Transform::none,
            [] {Rcpp::checkUserInterrupt();});
        Rcpp::IntegerVector calendar;
        for(const auto i:legacy.calendar_rows)calendar.push_back(static_cast<int>(i+1));
        out["legacy"]=Rcpp::List::create(Rcpp::_["status"]=legacy.status,
            Rcpp::_["available"]=legacy.status=="available",Rcpp::_["message"]=legacy.message,
            Rcpp::_["basis"]=legacy.basis,Rcpp::_["compressed"]=legacy.compressed,
            Rcpp::_["calendar_rows"]=calendar,Rcpp::_["preliminary_transformed"]=numeric(legacy.preliminary),
            Rcpp::_["interpolated_transformed"]=numeric(legacy.interpolated),
            Rcpp::_["correction_sign"]=legacy.correction_sign,
            Rcpp::_["unit_residuals"]=numeric(legacy.residuals),Rcpp::_["scaled_residuals"]=numeric(legacy.scaled_residuals),
            Rcpp::_["sum_squares"]=legacy.status=="available" ? legacy.sum_squares : NA_REAL);
    }
    return out;
}

// [[Rcpp::export(name = ".native_interpolation_information")]]
Rcpp::List native_interpolation_information(SEXP values,SEXP design,SEXP coefficients,
    SEXP specification,SEXP initial,SEXP coordinates,SEXP period,
    SEXP d=R_NilValue,SEXP seasonal_d=R_NilValue,SEXP mean=R_NilValue,
    SEXP rank_tolerance=R_NilValue,SEXP margin=R_NilValue,SEXP log_jacobian=R_NilValue,
    SEXP scale=R_NilValue,SEXP method=R_NilValue,SEXP control=R_NilValue) {
    const auto selected=information_option(scale,"prediction scale","ml");
    if(selected!="ml" && selected!="residual_df")throw std::invalid_argument("prediction scale must be ml or residual_df");
    const auto mode=information_option(method,"prediction uncertainty method","conditional");
    if(mode!="conditional" && mode!="delta_profile_ml")throw std::invalid_argument("prediction method must be conditional or delta_profile_ml");
    if(mode=="delta_profile_ml" && selected!="ml")throw std::invalid_argument("ARMA propagation requires ML scale");
    const auto c=information_control(control);
    const auto map=parameter_map(specification,initial,margin);
    const auto data=objective_data(values,design,coefficients);
    const auto result=persephone::interpolation_information(data.input,data.regression,map,
        Rcpp::as<std::vector<double>>(vector(coordinates,"ARMA coordinates",64)),
        integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
        scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),scalar(log_jacobian,"log Jacobian",0),
        selected=="ml" ? persephone::ResidualScale::ML : persephone::ResidualScale::ResidualDF,
        [] {Rcpp::checkUserInterrupt();});
    const auto& fit=result.fit;
    const bool points=fit.complete.status=="available";
    Rcpp::IntegerVector rows;
    if(points)for(const auto i:fit.complete.missing_rows)rows.push_back(static_cast<int>(i+1));
    Rcpp::List out=Rcpp::List::create(Rcpp::_["schema_version"]=1,Rcpp::_["status"]=result.status,
        Rcpp::_["available"]=result.status=="available" || result.status=="no_internal_missing",
        Rcpp::_["message"]=result.message,Rcpp::_["point_status"]=fit.complete.status,
        Rcpp::_["point_available"]=points,Rcpp::_["rows"]=rows,
        Rcpp::_["points_transformed"]=points ? numeric(fit.complete.interpolated) : Rcpp::NumericVector(),
        Rcpp::_["standard_errors"]=numeric(result.standard_errors),
        Rcpp::_["scale"]=selected,Rcpp::_["scale_variance"]=optional(result.variance),
        Rcpp::_["effective_count"]=static_cast<double>(fit.effective_count),
        Rcpp::_["residual_df"]=static_cast<double>(fit.residual_df),
        Rcpp::_["free_regression"]=static_cast<int>(fit.free_regression),Rcpp::_["free_ARMA"]=static_cast<int>(fit.free_arma),
        Rcpp::_["covariance"]=matrix_or_null(result.covariance),
        Rcpp::_["known_regression_covariance"]=matrix_or_null(result.known_regression_covariance),
        Rcpp::_["regression_covariance"]=matrix_or_null(result.regression_covariance));
    out["sum_squares"]=fit.center->likelihood ? optional(fit.center->likelihood->sum_squared_standardized) : NA_REAL;
    out["center"]=objective_output(*fit.center,fit.free_arma);
    out["method"]="conditional_ARMA_with_estimated_regression";
    out["propagates_ARMA_uncertainty"]=false;
    if(mode=="delta_profile_ml") {
        const auto propagated=persephone::interpolation_arma_information(result,data.input,data.regression,map,
            Rcpp::as<std::vector<double>>(vector(coordinates,"ARMA coordinates",64)),
            integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
            scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),scalar(log_jacobian,"log Jacobian",0),
            c,[] {Rcpp::checkUserInterrupt();});
        out["conditional_status"]=result.status;
        out["conditional_available"]=result.status=="available" || result.status=="no_internal_missing";
        out["conditional_covariance"]=matrix_or_null(result.covariance);
        out["conditional_standard_errors"]=numeric(result.standard_errors);
        out["status"]=propagated.status;out["message"]=propagated.message;
        out["available"]=propagated.status=="available" || propagated.status=="no_internal_missing";
        out["covariance"]=matrix_or_null(propagated.covariance);
        out["arma_increment"]=matrix_or_null(propagated.increment);
        out["standard_errors"]=numeric(propagated.standard_errors);
        out["sensitivity"]=matrix_or_null(propagated.sensitivity);
        out["sensitivity_error"]=matrix_or_null(propagated.sensitivity_error);
        if(propagated.derivatives.status=="not_required") {
            out["sensitivity"]=Rcpp::NumericMatrix(static_cast<int>(rows.size()),static_cast<int>(fit.free_arma));
            out["sensitivity_error"]=Rcpp::NumericMatrix(static_cast<int>(rows.size()),static_cast<int>(fit.free_arma));
        }
        out["sensitivity_status"]=propagated.derivatives.status;
        out["sensitivity_steps"]=numeric(propagated.derivatives.steps);
        out["sensitivity_evaluations"]=propagated.derivatives.evaluations;
        out["sensitivity_refinements"]=propagated.derivatives.refinements;
        Rcpp::IntegerVector rejected;Rcpp::CharacterVector reasons;
        for(const auto& entry:propagated.derivatives.rejected) {reasons.push_back(entry.first);rejected.push_back(entry.second);}
        rejected.attr("names")=reasons;out["sensitivity_rejected"]=rejected;
        out["arma_information"]=propagated.arma ? static_cast<SEXP>(arma_information_output(*propagated.arma,c,"profile_ml",R_NilValue)) : R_NilValue;
        out["evaluations"]=propagated.evaluations;out["evaluation_budget"]=c.max_evaluations;
        out["evaluation_scope"]="ARMA_information_plus_point_sensitivity_samples_excludes_conditional_diagnostic";
        out["method"]="delta_profile_ml";out["propagates_ARMA_uncertainty"]=true;
        out["inference"]="first_order_delta_method_with_accurate_profile_ML_ARMA";
        out["sensitivity_coordinates"]="free_physical_ARMA_coefficients";
        out["sensitivity_centering"]="fitted_regression_known_mean_and_integrated_homogeneous_levels";
    }
    return out;
}

// [[Rcpp::export(name = ".native_arma_optimize")]]
Rcpp::List native_arma_optimize(SEXP values, SEXP design, SEXP coefficients,
    SEXP specification, SEXP initial, SEXP coordinates, SEXP period,
    SEXP d=R_NilValue, SEXP seasonal_d=R_NilValue, SEXP mean=R_NilValue,
    SEXP rank_tolerance=R_NilValue, SEXP margin=R_NilValue, SEXP log_jacobian=R_NilValue,
    SEXP control=R_NilValue) {
    const auto c=optimizer_control(control);
    const auto map=parameter_map(specification,initial,margin);
    const auto data=objective_data(values,design,coefficients);
    const auto result=persephone::optimize_arma_objective(data.input,data.regression,map,
        Rcpp::as<std::vector<double>>(vector(coordinates,"ARMA coordinates",64)),
        integer(period,"period",0,12),integer(d,"d",0,2),integer(seasonal_d,"seasonal_d",0,2),
        scalar(mean,"mean",0),scalar(rank_tolerance,"rank tolerance",1e-10),scalar(log_jacobian,"log Jacobian",0),
        c,[] { Rcpp::checkUserInterrupt(); });
    const auto first=std::static_pointer_cast<const persephone::ArmaOptimizationPoint>(result.initial.detail);
    const auto last=std::static_pointer_cast<const persephone::ArmaOptimizationPoint>(result.final.detail);
    if(!first || !last) throw std::logic_error("optimizer lost its accepted ARMA point");
    Rcpp::List out;
    out["schema_version"]=1; out["status"]=result.status; out["converged"]=result.converged;
    out["success"]=result.converged || result.status=="no_free_parameters";
    out["identification"]="not_assessed"; out["optimality"]="first_order_only";
    out["initial"]=objective_output(first->objective,map.initial_coordinates().size());
    out["final"]=objective_output(last->objective,map.initial_coordinates().size());
    out["initial_coordinates"]=numeric(first->coordinates); out["coordinates"]=numeric(last->coordinates);
    out["working_coordinates"]=numeric(result.coordinates);
    out["gradient"]=numeric(result.gradient); out["gradient_error"]=numeric(result.gradient_error);
    out["difference_steps"]=numeric(result.difference_steps); out["difference_schemes"]=result.difference_schemes;
    out["iterations"]=result.iterations; out["evaluations"]=result.evaluations;
    out["gradient_evaluations"]=result.gradient_evaluations; out["bfgs_resets"]=result.bfgs_resets;
    Rcpp::IntegerVector rejected; Rcpp::CharacterVector reasons;
    for(const auto& entry:result.rejected) { reasons.push_back(entry.first); rejected.push_back(entry.second); }
    rejected.attr("names")=reasons; out["rejected"]=rejected;
    Rcpp::NumericMatrix history(static_cast<int>(result.trace.size()),9);
    for(std::size_t i=0;i<result.trace.size();++i) {
        const auto& t=result.trace[i]; const int row=static_cast<int>(i);
        history(row,0)=t.iteration; history(row,1)=t.evaluations; history(row,2)=t.value;
        history(row,3)=optional(t.gradient_norm); history(row,4)=optional(t.gradient_error);
        history(row,5)=t.step_norm; history(row,6)=t.function_change; history(row,7)=t.step_length;
        history(row,8)=optional(t.gradient_bound);
    }
    Rcpp::colnames(history)=Rcpp::CharacterVector::create("iteration","evaluations","value","gradient_norm",
        "gradient_error","step_norm","function_change","step_length","gradient_bound");
    out["trace"]=history; out["control"]=control_output(c);
    out["method"]="bfgs_feasible_armijo";
    out["gradient_coordinates"]="unit_reflection_or_free_coefficient";
    out["parameter_diagnostics"]=identification_output(persephone::arma_identification(map,
        last->coordinates, integer(period,"period",0,12), 1e-10, 1e-4,
        [] { Rcpp::checkUserInterrupt(); }));
    return out;
}
