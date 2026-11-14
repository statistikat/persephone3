#include <Rcpp.h>
#include "core/x11_trend_selection.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
double scalar(SEXP value,const char* label,bool infinity=false) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) ||
        Rf_xlength(value)!=1 || !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))
        throw std::invalid_argument(std::string(label)+" must be one real number");
    const auto x=Rcpp::as<double>(value);
    if(std::isnan(x) || x<0 || (!infinity && !std::isfinite(x)))
        throw std::invalid_argument(std::string(label)+" must be nonnegative and not missing");
    return x;
}
int integer(SEXP value,const char* label) {
    const auto x=scalar(value,label);
    if(x!=std::floor(x) || x>std::numeric_limits<int>::max())throw std::invalid_argument(std::string(label)+" must be a supported integer");
    return static_cast<int>(x);
}
const char* status(persephone::X11TrendSelectionStatus value) {
    using S=persephone::X11TrendSelectionStatus;
    switch(value) {
    case S::available:return "available";
    case S::insufficient_observations:return "insufficient_observations";
    case S::missing_observations:return "missing_observations";
    case S::nonpositive_relative_input:return "nonpositive_relative_input";
    case S::nonpositive_relative_trend:return "nonpositive_relative_trend";
    case S::undefined_ic_ratio:return "undefined_ic_ratio";
    case S::numerical_range:return "numerical_range";
    }
    throw std::logic_error("unknown trend selection status");
}
double optional(double value) {return std::isnan(value) ? NA_REAL : value;}
}
// [[Rcpp::export(name = ".native_x11_trend_policy")]]
Rcpp::List native_x11_trend_policy(SEXP frequency,SEXP ic_ratio,SEXP length) {
    const auto f=integer(frequency,"frequency");
    if(Rf_isNull(ic_ratio)==Rf_isNull(length))throw std::invalid_argument("supply exactly one of comparison I/C ratio and Henderson length");
    const bool automatic=Rf_isNull(length);
    const double ratio=automatic ? scalar(ic_ratio,"comparison I/C ratio",true) : NA_REAL;
    const auto span=automatic ? persephone::x11_henderson_select(f,ratio) : integer(length,"Henderson length");
    const auto endpoint=persephone::x11_henderson_default_ratio(f,span);
    return Rcpp::List::create(Rcpp::_["frequency"]=f,Rcpp::_["selection"]=automatic ? "automatic_ic" : "fixed_length",
        Rcpp::_["comparison_ic_ratio"]=ratio,Rcpp::_["selected_length"]=span,
        Rcpp::_["endpoint_ic_ratio"]=endpoint,Rcpp::_["pilot_length"]=f+1);
}
// [[Rcpp::export(name = ".native_x11_trend_select")]]
Rcpp::List native_x11_trend_select(SEXP values,SEXP frequency,SEXP mode,SEXP backcasts,SEXP forecasts) {
    const auto f=integer(frequency,"frequency"),back=integer(backcasts,"backcasts"),fore=integer(forecasts,"forecasts");
    if(TYPEOF(mode)!=STRSXP || Rf_xlength(mode)!=1 || STRING_ELT(mode,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(mode,R_DimSymbol)))throw std::invalid_argument("provide one supported X-11 mode");
    const auto name=Rcpp::as<std::string>(mode);
    const bool relative=name=="multiplicative" || name=="pseudo_additive";
    if(!relative && name!="additive" && name!="log_additive")throw std::invalid_argument("unsupported X-11 mode");
    if((TYPEOF(values)!=REALSXP && TYPEOF(values)!=INTSXP) || Rf_isFactor(values) ||
        !Rf_isNull(Rf_getAttrib(values,R_DimSymbol)) || Rf_xlength(values)>std::numeric_limits<int>::max())
        throw std::invalid_argument("X-11 response must be a real numeric vector within the supported length");
    const auto y=Rcpp::as<Rcpp::NumericVector>(values);const auto n=static_cast<std::size_t>(y.size());
    persephone::MaskedSeries input{std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    for(std::size_t j=0;j<n;++j) {
        if((j&4095U)==0)Rcpp::checkUserInterrupt();const auto value=y[static_cast<R_xlen_t>(j)];
        if(std::isnan(value))input.missing[j]=1;
        else if(!std::isfinite(value))throw std::invalid_argument("X-11 response must be finite or missing");
        else input.values[j]=value;
    }
    const auto result=persephone::x11_trend_select(input,f,
        relative ? persephone::X11ChangeMode::relative : persephone::X11ChangeMode::additive,
        static_cast<std::size_t>(back),static_cast<std::size_t>(fore),[] {Rcpp::checkUserInterrupt();});
    return Rcpp::List::create(Rcpp::_["status"]=status(result.status),
        Rcpp::_["selected_length"]=result.selected_length ? result.selected_length : NA_INTEGER,
        Rcpp::_["pilot_length"]=result.pilot_length,Rcpp::_["pilot_count"]=static_cast<int>(result.pilot_count),
        Rcpp::_["change_count"]=static_cast<int>(result.change_count),
        Rcpp::_["trend_mean_change"]=optional(result.trend_mean_change),
        Rcpp::_["irregular_mean_change"]=optional(result.irregular_mean_change),
        Rcpp::_["raw_ic_ratio"]=optional(result.raw_ic_ratio),
        Rcpp::_["comparison_ic_ratio"]=optional(result.comparison_ic_ratio),
        Rcpp::_["endpoint_ic_ratio"]=optional(result.endpoint_ic_ratio),
        Rcpp::_["frequency"]=f,Rcpp::_["frequency_factor"]=f==4 ? 3 : 1,
        Rcpp::_["change_measure"]=relative ? "relative_absolute_change" : "absolute_change",
        Rcpp::_["backcasts"]=back,Rcpp::_["forecasts"]=fore);
}
