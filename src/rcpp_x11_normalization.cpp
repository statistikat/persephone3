#include "rcpp_x11_table_support.hpp"
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
    std::vector<int> choices(static_cast<std::size_t>(f));
    for(int j=0;j<f;++j) {
        const auto at=Rf_xlength(filters)==1 ? 0 : j;
        if(STRING_ELT(filters,at)==NA_STRING)throw std::invalid_argument("seasonal requests must not be missing");
        const std::string name=CHAR(STRING_ELT(filters,at));
        const auto filter=name=="stable" ? 0 : (name=="s3x1" ? 1 : (name=="s3x3" ? 3 :
            (name=="s3x5" ? 5 : (name=="s3x9" ? 9 : (name=="s3x15" ? 15 : -1)))));
        if(filter<0)throw std::invalid_argument("provide supported fixed seasonal requests");
        choices[static_cast<std::size_t>(j)]=filter;
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
    return persephone_x11_rcpp::normalization_result(result,f,choices,extension);
}
