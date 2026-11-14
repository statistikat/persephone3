#include "rcpp_x11_table_support.hpp"
#include "core/x11_seasonal_processing.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
int integer(SEXP value,const char* label) {
    if((TYPEOF(value)!=REALSXP && TYPEOF(value)!=INTSXP) || Rf_isFactor(value) ||
        Rf_xlength(value)!=1 || !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))
        throw std::invalid_argument(std::string(label)+" must be one integer");
    const auto x=Rcpp::as<double>(value);
    if(!std::isfinite(x) || x!=std::floor(x) || x<1 || x>12)
        throw std::invalid_argument(std::string(label)+" is outside its supported range");
    return static_cast<int>(x);
}
int filter_length(const std::string& type) {
    if(type=="stable")return 0;
    if(type=="s3x1")return 1;
    if(type=="s3x3")return 3;
    if(type=="s3x5")return 5;
    if(type=="s3x9")return 9;
    if(type=="s3x15")return 15;
    throw std::invalid_argument("seasonal processing supports stable, s3x1, s3x3, s3x5, s3x9 and s3x15");
}

}
// [[Rcpp::export(name = ".native_x11_seasonal_process")]]
Rcpp::List native_x11_seasonal_process(SEXP values,SEXP frequency,SEXP start_period,
    SEXP filters,SEXP short_series) {
    const int f=integer(frequency,"frequency"),start=integer(start_period,"start period");
    if(TYPEOF(filters)!=STRSXP || (Rf_xlength(filters)!=1 && Rf_xlength(filters)!=f) ||
        !Rf_isNull(Rf_getAttrib(filters,R_DimSymbol)))
        throw std::invalid_argument("filters must be one string or one per calendar period");
    std::vector<int> choices(static_cast<std::size_t>(f));
    for(int period=0;period<f;++period) {
        const auto value=STRING_ELT(filters,Rf_xlength(filters)==1 ? 0 : period);
        if(value==NA_STRING)throw std::invalid_argument("seasonal filter choices cannot be missing");
        choices[static_cast<std::size_t>(period)]=filter_length(Rcpp::as<std::string>(value));
    }
    if(TYPEOF(short_series)!=STRSXP || Rf_xlength(short_series)!=1 || STRING_ELT(short_series,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(short_series,R_DimSymbol)))throw std::invalid_argument("short_series must be one string");
    const auto policy=Rcpp::as<std::string>(short_series);
    if(policy!="reject" && policy!="x11")throw std::invalid_argument("short_series must be 'reject' or 'x11'");
    if((TYPEOF(values)!=REALSXP && TYPEOF(values)!=INTSXP) || Rf_isFactor(values) ||
        !Rf_isNull(Rf_getAttrib(values,R_DimSymbol)) || Rf_xlength(values)>std::numeric_limits<int>::max())
        throw std::invalid_argument("seasonal response must be a real numeric vector within the supported length");
    const auto y=Rcpp::as<Rcpp::NumericVector>(values);const auto n=static_cast<std::size_t>(y.size());
    persephone::MaskedSeries input{std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        const auto value=y[static_cast<R_xlen_t>(i)];
        if(std::isnan(value))input.missing[i]=1;
        else if(!std::isfinite(value))throw std::invalid_argument("seasonal response must be finite or missing");
        else input.values[i]=value;
    }
    const auto result=persephone::x11_seasonal_process(input,f,start,choices,
        policy=="reject" ? persephone::X11ShortSeriesPolicy::reject : persephone::X11ShortSeriesPolicy::x11,
        [] {Rcpp::checkUserInterrupt();});
    return persephone_x11_rcpp::seasonal_result(result,f,choices);
}
