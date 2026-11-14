#include <Rcpp.h>
#include "core/x11_filters.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
int integer(SEXP value,const char* label) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) ||
        Rf_xlength(value)!=1 || !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))
        throw std::invalid_argument(std::string(label)+" must be one integer");
    const auto x=Rcpp::as<double>(value);
    if(!std::isfinite(x) || x!=std::floor(x) || x<0 || x>101)
        throw std::invalid_argument(std::string(label)+" is outside its supported range");
    return static_cast<int>(x);
}
struct Settings {persephone::X11FilterKind kind;int frequency,length;std::string type;};
Settings settings(SEXP type,SEXP frequency,SEXP length) {
    if(TYPEOF(type)!=STRSXP || Rf_xlength(type)!=1 || STRING_ELT(type,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(type,R_DimSymbol)))throw std::invalid_argument("X-11 filter type must be one string");
    const auto name=Rcpp::as<std::string>(type);const auto period=integer(frequency,"frequency");
    if(name=="henderson")return {persephone::X11FilterKind::henderson,period,integer(length,"Henderson length"),name};
    if(!Rf_isNull(length))throw std::invalid_argument("length is only supplied for Henderson filters");
    if(name=="trend")return {persephone::X11FilterKind::trend,period,0,name};
    const std::vector<std::string> options{"s3x1","s3x3","s3x5","s3x9","s3x15"};
    const int lengths[]{1,3,5,9,15};
    for(std::size_t i=0;i<options.size();++i)if(name==options[i])
        return {persephone::X11FilterKind::seasonal,period,lengths[i],name};
    throw std::invalid_argument("unsupported symmetric X-11 filter type");
}
Rcpp::List specification(const Settings& s) {
    const auto filter=persephone::x11_filter_definition(s.kind,s.frequency,s.length,[] {Rcpp::checkUserInterrupt();});
    Rcpp::IntegerVector offsets;for(const auto offset:filter.offsets)offsets.push_back(static_cast<int>(offset));
    return Rcpp::List::create(Rcpp::_["schema_version"]=1,Rcpp::_["type"]=s.type,
        Rcpp::_["frequency"]=s.frequency,Rcpp::_["length_parameter"]=s.length,
        Rcpp::_["terms"]=static_cast<int>(filter.weights.size()),Rcpp::_["radius"]=static_cast<int>(filter.radius),
        Rcpp::_["weights"]=filter.weights,Rcpp::_["offsets"]=offsets,
        Rcpp::_["endpoint_policy"]="unavailable_outside_filter_support",
        Rcpp::_["scope"]="symmetric_filter_only",Rcpp::_["backend"]="native");
}
}
// [[Rcpp::export(name = ".native_x11_filter_spec")]]
Rcpp::List native_x11_filter_spec(SEXP type,SEXP frequency,SEXP length=R_NilValue) {
    return specification(settings(type,frequency,length));
}
// [[Rcpp::export(name = ".native_x11_filter")]]
Rcpp::List native_x11_filter(SEXP values,SEXP type,SEXP frequency,SEXP length=R_NilValue) {
    const auto s=settings(type,frequency,length);
    if((TYPEOF(values)!=REALSXP && TYPEOF(values)!=INTSXP) || Rf_isFactor(values) ||
        !Rf_isNull(Rf_getAttrib(values,R_DimSymbol)) || Rf_xlength(values)>std::numeric_limits<int>::max())
        throw std::invalid_argument("X-11 response must be a real numeric vector within the supported length");
    const auto y=Rcpp::as<Rcpp::NumericVector>(values);const auto n=static_cast<std::size_t>(y.size());
    persephone::MaskedSeries input{std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        const auto value=y[static_cast<R_xlen_t>(i)];
        if(std::isnan(value))input.missing[i]=1;
        else if(!std::isfinite(value))throw std::invalid_argument("X-11 response must be finite or missing");
        else input.values[i]=value;
    }
    const auto filtered=persephone::x11_symmetric_filter(input,s.kind,s.frequency,s.length,[] {Rcpp::checkUserInterrupt();});
    Rcpp::NumericVector output(n);Rcpp::LogicalVector missing(n);
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        output[static_cast<R_xlen_t>(i)]=filtered.missing[i] ? NA_REAL : filtered.values[i];
        missing[static_cast<R_xlen_t>(i)]=filtered.missing[i]!=0;
    }
    auto out=specification(s);out["values"]=output;out["missing"]=missing;
    return out;
}
