#include <Rcpp.h>
#include "core/x11_normalization.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
int integer(SEXP x,const char* label) {
    if((TYPEOF(x)!=REALSXP && TYPEOF(x)!=INTSXP) || Rf_isFactor(x) || Rf_xlength(x)!=1 || !Rf_isNull(Rf_getAttrib(x,R_DimSymbol)))
        throw std::invalid_argument(std::string(label)+" must be one integer");
    const auto value=Rcpp::as<double>(x);
    if(!std::isfinite(value) || value<0 || value!=std::floor(value) || value>std::numeric_limits<int>::max())
        throw std::invalid_argument(std::string(label)+" must be one supported integer");
    return static_cast<int>(value);
}
const char* rule(persephone::X11NormalizationRule value) {
    using R=persephone::X11NormalizationRule;
    switch(value) {
    case R::nearest:return "nearest_central_mean";
    case R::requested_stable:return "requested_stable";
    case R::short_span:return "short_span";
    case R::short_3x15_span:return "short_3x15_span";
    }
    throw std::logic_error("unknown normalization rule");
}
Rcpp::NumericVector values(const persephone::MaskedSeries& x) {
    Rcpp::NumericVector out(x.values.size());
    for(std::size_t j=0;j<x.values.size();++j) {
        if((j&4095U)==0)Rcpp::checkUserInterrupt();
        out[static_cast<R_xlen_t>(j)]=x.missing[j] ? NA_REAL : x.values[j];
    }
    return out;
}
Rcpp::IntegerVector sources(const std::vector<std::size_t>& x) {
    Rcpp::IntegerVector out(x.size());
    for(std::size_t j=0;j<x.size();++j) {
        if((j&4095U)==0)Rcpp::checkUserInterrupt();out[static_cast<R_xlen_t>(j)]=static_cast<int>(x[j]+1);
    }
    return out;
}
}
// [[Rcpp::export(name = ".native_x11_seasonal_normalize")]]
Rcpp::List native_x11_seasonal_normalize(SEXP input,SEXP frequency,SEXP first_period,SEXP filters,SEXP extend) {
    const auto f=integer(frequency,"frequency"),first=integer(first_period,"first period"),extension=integer(extend,"extend");
    if(f!=4 && f!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if((TYPEOF(input)!=REALSXP && TYPEOF(input)!=INTSXP) || Rf_isFactor(input) || !Rf_isNull(Rf_getAttrib(input,R_DimSymbol)) ||
        Rf_xlength(input)>std::numeric_limits<int>::max()-2.*extension)
        throw std::invalid_argument("normalization input and extension must have a supported real vector length");
    if(TYPEOF(filters)!=STRSXP || !Rf_isNull(Rf_getAttrib(filters,R_DimSymbol)) || (Rf_xlength(filters)!=1 && Rf_xlength(filters)!=f))
        throw std::invalid_argument("provide one or frequency fixed seasonal requests");
    Rcpp::CharacterVector requested(f);std::vector<int> choices(static_cast<std::size_t>(f));
    for(int j=0;j<f;++j) {
        const auto at=Rf_xlength(filters)==1 ? 0 : j;
        if(STRING_ELT(filters,at)==NA_STRING)throw std::invalid_argument("seasonal requests must not be missing");
        const std::string name=CHAR(STRING_ELT(filters,at));
        const auto filter=name=="stable" ? 0 : (name=="s3x1" ? 1 : (name=="s3x3" ? 3 :
            (name=="s3x5" ? 5 : (name=="s3x9" ? 9 : (name=="s3x15" ? 15 : -1)))));
        if(filter<0)throw std::invalid_argument("provide supported fixed seasonal requests");
        choices[static_cast<std::size_t>(j)]=filter;requested[j]=name;
    }
    const auto y=Rcpp::as<Rcpp::NumericVector>(input);const auto n=static_cast<std::size_t>(y.size());
    persephone::MaskedSeries data{std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    for(std::size_t j=0;j<n;++j) {
        if((j&4095U)==0)Rcpp::checkUserInterrupt();const auto value=y[static_cast<R_xlen_t>(j)];
        if(std::isnan(value))data.missing[j]=1;
        else if(!std::isfinite(value))throw std::invalid_argument("normalization input must be finite or missing");
        else data.values[j]=value;
    }
    const auto result=persephone::x11_seasonal_normalize(data,f,first,choices,static_cast<std::size_t>(extension),[]{Rcpp::checkUserInterrupt();});
    Rcpp::CharacterVector reasons(f),endpoints(f);
    for(int j=0;j<f;++j) {
        const auto value=result.rules[static_cast<std::size_t>(j)];reasons[j]=rule(value);
        endpoints[j]=value==persephone::X11NormalizationRule::nearest ? "nearest_central_mean" : "annual_copy";
    }
    return Rcpp::List::create(Rcpp::_["values"]=values(result.values),Rcpp::_["normalization"]=values(result.normalization),
        Rcpp::_["requested_filters"]=requested,Rcpp::_["endpoint_policies"]=endpoints,Rcpp::_["endpoint_reasons"]=reasons,
        Rcpp::_["normalization_source_rows"]=sources(result.normalization_sources),Rcpp::_["source_rows"]=sources(result.output_sources),
        Rcpp::_["frequency"]=f,Rcpp::_["extend"]=extension);
}
