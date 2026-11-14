#include <Rcpp.h>
#include "core/x11_musgrave.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
double scalar(SEXP value,const char* label) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) ||
        Rf_xlength(value)!=1 || !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))
        throw std::invalid_argument(std::string(label)+" must be one real number");
    const auto out=Rcpp::as<double>(value);
    if(!std::isfinite(out) || out<0)throw std::invalid_argument(std::string(label)+" must be finite and nonnegative");
    return out;
}
int integer(SEXP value,const char* label) {
    const auto out=scalar(value,label);
    if(out!=std::floor(out) || out>101)throw std::invalid_argument(std::string(label)+" must be a supported integer");
    return static_cast<int>(out);
}
}
// [[Rcpp::export(name = ".native_x11_musgrave_spec")]]
Rcpp::List native_x11_musgrave_spec(SEXP frequency,SEXP length,SEXP available,SEXP ic_ratio,SEXP side) {
    if(TYPEOF(side)!=STRSXP || Rf_xlength(side)!=1 || STRING_ELT(side,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(side,R_DimSymbol)))throw std::invalid_argument("Musgrave side must be left or right");
    const auto name=Rcpp::as<std::string>(side);
    if(name!="left" && name!="right")throw std::invalid_argument("Musgrave side must be left or right");
    const auto period=integer(frequency,"frequency"),span=integer(length,"Henderson length"),q=integer(available,"available observations");
    const auto ratio=scalar(ic_ratio,"I/C ratio");
    const auto filter=persephone::x11_musgrave_definition(period,span,q,
        name=="left" ? persephone::X11EndpointSide::left : persephone::X11EndpointSide::right,
        ratio,[] {Rcpp::checkUserInterrupt();});
    Rcpp::IntegerVector offsets;for(const auto offset:filter.offsets)offsets.push_back(static_cast<int>(offset));
    return Rcpp::List::create(Rcpp::_["schema_version"]=1,Rcpp::_["type"]="henderson",
        Rcpp::_["frequency"]=period,Rcpp::_["length_parameter"]=span,
        Rcpp::_["terms"]=static_cast<int>(filter.weights.size()),Rcpp::_["radius"]=static_cast<int>(filter.radius),
        Rcpp::_["weights"]=filter.weights,Rcpp::_["offsets"]=offsets,Rcpp::_["side"]=name,
        Rcpp::_["available"]=q,Rcpp::_["ic_ratio"]=ratio,Rcpp::_["endpoint_policy"]="musgrave",
        Rcpp::_["scope"]="henderson_endpoint_filter_only",Rcpp::_["backend"]="native");
}
// [[Rcpp::export(name = ".native_x11_musgrave_filter")]]
Rcpp::List native_x11_musgrave_filter(SEXP values,SEXP frequency,SEXP length,SEXP ic_ratio) {
    const auto period=integer(frequency,"frequency"),span=integer(length,"Henderson length");
    const auto ratio=scalar(ic_ratio,"I/C ratio");
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
    const auto filtered=persephone::x11_musgrave_filter(input,period,span,ratio,[] {Rcpp::checkUserInterrupt();});
    Rcpp::NumericVector output(n);Rcpp::LogicalVector missing(n);
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        output[static_cast<R_xlen_t>(i)]=filtered.missing[i] ? NA_REAL : filtered.values[i];
        missing[static_cast<R_xlen_t>(i)]=filtered.missing[i]!=0;
    }
    return Rcpp::List::create(Rcpp::_["values"]=output,Rcpp::_["missing"]=missing);
}
