#include "rcpp_x11_msr_support.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace persephone_msr_rcpp {
int integer(SEXP value,const char* label) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) || Rf_xlength(value)!=1 ||
        !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))throw std::invalid_argument(std::string(label)+" must be one integer");
    const auto x=Rcpp::as<double>(value);
    if(!std::isfinite(x) || x<0 || x!=std::floor(x) || x>std::numeric_limits<int>::max())
        throw std::invalid_argument(std::string(label)+" must be one supported integer");
    return static_cast<int>(x);
}
persephone::MaskedSeries series(SEXP value) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) ||
        !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)) || Rf_xlength(value)>std::numeric_limits<int>::max())
        throw std::invalid_argument("MSR component must be a real numeric vector within the supported length");
    const auto y=Rcpp::as<Rcpp::NumericVector>(value);const auto n=static_cast<std::size_t>(y.size());
    persephone::MaskedSeries out{std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    for(std::size_t j=0;j<n;++j) {
        if((j&4095U)==0)Rcpp::checkUserInterrupt();const auto v=y[static_cast<R_xlen_t>(j)];
        if(std::isnan(v))out.missing[j]=1;
        else if(!std::isfinite(v))throw std::invalid_argument("MSR component must be finite or missing");
        else out.values[j]=v;
    }
    return out;
}
const char* status(persephone::X11MsrStatus value) {
    using S=persephone::X11MsrStatus;
    switch(value) {
    case S::available:return "available";
    case S::insufficient_observations:return "insufficient_observations";
    case S::missing_observations:return "missing_observations";
    case S::nonpositive_relative_component:return "nonpositive_relative_component";
    case S::undefined_msr:return "undefined_msr";
    case S::numerical_range:return "numerical_range";
    case S::incomplete_phases:return "incomplete_phases";
    }
    throw std::logic_error("unknown MSR status");
}
double optional(double value) {return std::isnan(value) ? NA_REAL : value;}
Rcpp::NumericVector numbers(const std::vector<double>& values) {
    Rcpp::NumericVector out(values.size());
    for(std::size_t j=0;j<values.size();++j)out[static_cast<R_xlen_t>(j)]=optional(values[j]);
    return out;
}
Rcpp::CharacterVector statuses(const std::vector<persephone::X11MsrStatus>& values) {
    Rcpp::CharacterVector out(values.size());
    for(std::size_t j=0;j<values.size();++j)out[static_cast<R_xlen_t>(j)]=status(values[j]);
    return out;
}
persephone::X11MsrMode mode(SEXP value) {
    if(TYPEOF(value)!=STRSXP || Rf_xlength(value)!=1 || STRING_ELT(value,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))throw std::invalid_argument("provide one supported X-11 mode");
    const auto name=Rcpp::as<std::string>(value);const bool relative=name=="multiplicative";
    if(!relative && name!="additive" && name!="log_additive" && name!="pseudo_additive")throw std::invalid_argument("unsupported X-11 mode");
    return relative ? persephone::X11MsrMode::relative : persephone::X11MsrMode::absolute;
}
Rcpp::List table_result(const persephone::X11MsrTable& result,int f,persephone::X11MsrMode mode) {
    const bool relative=mode==persephone::X11MsrMode::relative;
    Rcpp::IntegerVector counts(f),changes(f);
    for(int p=0;p<f;++p) {counts[p]=static_cast<int>(result.observations[static_cast<std::size_t>(p)]);
        changes[p]=static_cast<int>(result.changes[static_cast<std::size_t>(p)]);}
    const auto phases=Rcpp::DataFrame::create(Rcpp::_["period"]=Rcpp::seq_len(f),Rcpp::_["observations"]=counts,Rcpp::_["changes"]=changes,
        Rcpp::_["seasonal_raw_mean"]=numbers(result.seasonal.raw_mean),Rcpp::_["irregular_raw_mean"]=numbers(result.irregular.raw_mean),
        Rcpp::_["seasonal_correction"]=numbers(result.seasonal.correction),Rcpp::_["irregular_correction"]=numbers(result.irregular.correction),
        Rcpp::_["seasonal_mean"]=numbers(result.seasonal.corrected_mean),Rcpp::_["irregular_mean"]=numbers(result.irregular.corrected_mean),
        Rcpp::_["seasonal_status"]=statuses(result.seasonal.status),Rcpp::_["irregular_status"]=statuses(result.irregular.status),
        Rcpp::_["msr"]=numbers(result.ratios),Rcpp::_["status"]=statuses(result.phase_status));
    return Rcpp::List::create(Rcpp::_["frequency"]=f,Rcpp::_["status"]=status(result.status),Rcpp::_["global_msr"]=optional(result.global_msr),
        Rcpp::_["global_seasonal_mean"]=optional(result.seasonal.global_mean),Rcpp::_["global_irregular_mean"]=optional(result.irregular.global_mean),
        Rcpp::_["change_measure"]=relative ? "relative_absolute_change" : "absolute_change",Rcpp::_["phases"]=phases);
}
} // namespace persephone_msr_rcpp
// [[Rcpp::export(name = ".native_x11_msr_table")]]
Rcpp::List native_x11_msr_table(SEXP seasonal,SEXP irregular,SEXP frequency,SEXP first_period,SEXP mode) {
    using namespace persephone_msr_rcpp;
    const auto f=integer(frequency,"frequency"),first=integer(first_period,"first period");
    const auto measure=persephone_msr_rcpp::mode(mode);const auto s=series(seasonal),i=series(irregular);
    return table_result(persephone::x11_msr_table(s,i,f,first,measure,[]{Rcpp::checkUserInterrupt();}),f,measure);
}
