#include "rcpp_x11_table_support.hpp"
#include "core/x11_initial.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
int integer(SEXP value,const char* label) {
    if((TYPEOF(value)!=INTSXP && TYPEOF(value)!=REALSXP) || Rf_isFactor(value) || Rf_xlength(value)!=1 ||
        !Rf_isNull(Rf_getAttrib(value,R_DimSymbol)))throw std::invalid_argument(std::string(label)+" must be one integer");
    const auto x=Rcpp::as<double>(value);
    if(!std::isfinite(x) || x<1 || x>12 || x!=std::floor(x))throw std::invalid_argument(std::string(label)+" is outside the supported range");
    return static_cast<int>(x);
}
}
// [[Rcpp::export(name = ".native_x11_initial_additive")]]
Rcpp::List native_x11_initial_additive(SEXP input,SEXP frequency,SEXP first_period,SEXP filters) {
    const auto f=integer(frequency,"frequency"),first=integer(first_period,"first period");
    if(f!=4 && f!=12)throw std::invalid_argument("frequency must be 4 or 12");
    if(TYPEOF(filters)!=STRSXP || !Rf_isNull(Rf_getAttrib(filters,R_DimSymbol)) || (Rf_xlength(filters)!=1 && Rf_xlength(filters)!=f))
        throw std::invalid_argument("provide one or frequency fixed seasonal requests");
    std::vector<int> choices(static_cast<std::size_t>(f));
    for(int j=0;j<f;++j) {
        const auto at=Rf_xlength(filters)==1 ? 0 : j;
        if(STRING_ELT(filters,at)==NA_STRING)throw std::invalid_argument("seasonal requests must not be missing");
        const std::string name=CHAR(STRING_ELT(filters,at));
        const auto choice=name=="stable" ? 0 : (name=="s3x1" ? 1 : (name=="s3x3" ? 3 :
            (name=="s3x5" ? 5 : (name=="s3x9" ? 9 : (name=="s3x15" ? 15 : -1)))));
        if(choice<0)throw std::invalid_argument("provide supported fixed seasonal requests");
        choices[static_cast<std::size_t>(j)]=choice;
    }
    if((TYPEOF(input)!=REALSXP && TYPEOF(input)!=INTSXP) || Rf_isFactor(input) || !Rf_isNull(Rf_getAttrib(input,R_DimSymbol)) ||
        Rf_xlength(input)>std::numeric_limits<int>::max())throw std::invalid_argument("initial input must be a supported real numeric vector");
    const auto y=Rcpp::as<Rcpp::NumericVector>(input);const auto n=static_cast<std::size_t>(y.size());
    persephone::MaskedSeries series{std::vector<double>(n,0),std::vector<std::uint8_t>(n,0)};
    for(std::size_t j=0;j<n;++j) {
        if((j&4095U)==0)Rcpp::checkUserInterrupt();const auto value=y[static_cast<R_xlen_t>(j)];
        if(std::isnan(value))series.missing[j]=1;
        else if(!std::isfinite(value))throw std::invalid_argument("initial input must be finite or missing");
        else series.values[j]=value;
    }
    const auto result=persephone::x11_initial_additive(series,f,first,choices,[]{Rcpp::checkUserInterrupt();});
    using namespace persephone_x11_rcpp;
    return Rcpp::List::create(Rcpp::_["drop"]=static_cast<int>(result.drop),Rcpp::_["first_period"]=result.first_period,
        Rcpp::_["tables"]=Rcpp::List::create(Rcpp::_["B1"]=values(result.input),Rcpp::_["B2"]=values(result.trend),
            Rcpp::_["B3"]=values(result.seasonal_irregular),Rcpp::_["B4d"]=values(result.irregular)),
        Rcpp::_["seasonal"]=seasonal_result(result.seasonal,f,choices),
        Rcpp::_["normalization"]=normalization_result(result.normalization,f,choices,0));
}
