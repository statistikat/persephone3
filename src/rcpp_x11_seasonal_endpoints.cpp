#include <Rcpp.h>
#include "core/x11_seasonal_endpoints.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
std::string string(SEXP value,const char* label) {
    if(TYPEOF(value)!=STRSXP || Rf_xlength(value)!=1 || STRING_ELT(value,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))throw std::invalid_argument(std::string(label)+" must be one string");
    return Rcpp::as<std::string>(value);
}
int integer(SEXP value,const char* label) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) ||
        Rf_xlength(value)!=1 || !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))
        throw std::invalid_argument(std::string(label)+" must be one integer");
    const auto x=Rcpp::as<double>(value);
    if(!std::isfinite(x) || x!=std::floor(x) || x<0 || x>101)
        throw std::invalid_argument(std::string(label)+" is outside its supported range");
    return static_cast<int>(x);
}
int length_for(const std::string& type) {
    if(type=="s3x1")return 1;
    if(type=="s3x3")return 3;
    if(type=="s3x5")return 5;
    if(type=="s3x9")return 9;
    throw std::invalid_argument("seasonal endpoints currently support s3x1, s3x3, s3x5 and s3x9");
}
}
// [[Rcpp::export(name = ".native_x11_seasonal_spec")]]
Rcpp::List native_x11_seasonal_spec(SEXP type,SEXP frequency,SEXP available,SEXP side) {
    const auto name=string(type,"seasonal filter type"),direction=string(side,"seasonal endpoint side");
    if(direction!="left" && direction!="right")throw std::invalid_argument("seasonal endpoint side must be left or right");
    const auto period=integer(frequency,"frequency"),q=integer(available,"available seasonal observations"),length=length_for(name);
    const auto filter=persephone::x11_seasonal_endpoint_definition(period,length,q,
        direction=="left" ? persephone::X11SeasonalSide::left : persephone::X11SeasonalSide::right,
        [] {Rcpp::checkUserInterrupt();});
    Rcpp::IntegerVector offsets;for(const auto offset:filter.offsets)offsets.push_back(static_cast<int>(offset));
    return Rcpp::List::create(Rcpp::_["schema_version"]=1,Rcpp::_["type"]=name,
        Rcpp::_["frequency"]=period,Rcpp::_["length_parameter"]=length,
        Rcpp::_["terms"]=static_cast<int>(filter.weights.size()),Rcpp::_["radius"]=static_cast<int>(filter.radius),
        Rcpp::_["minimum_phase_length"]=std::max(5,length+2),Rcpp::_["annual_radius"]=(length+1)/2,Rcpp::_["weights"]=filter.weights,Rcpp::_["offsets"]=offsets,
        Rcpp::_["side"]=direction,Rcpp::_["available"]=q,Rcpp::_["endpoint_policy"]="seasonal_x11",
        Rcpp::_["scope"]="seasonal_endpoint_filter_only",Rcpp::_["backend"]="native");
}
// [[Rcpp::export(name = ".native_x11_seasonal_filter")]]
Rcpp::List native_x11_seasonal_filter(SEXP values,SEXP type,SEXP frequency) {
    const auto length=length_for(string(type,"seasonal filter type")),period=integer(frequency,"frequency");
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
    const auto filtered=persephone::x11_seasonal_endpoints(input,period,length,[] {Rcpp::checkUserInterrupt();});
    Rcpp::NumericVector output(n);Rcpp::LogicalVector missing(n);
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        output[static_cast<R_xlen_t>(i)]=filtered.missing[i] ? NA_REAL : filtered.values[i];
        missing[static_cast<R_xlen_t>(i)]=filtered.missing[i]!=0;
    }
    return Rcpp::List::create(Rcpp::_["values"]=output,Rcpp::_["missing"]=missing);
}
