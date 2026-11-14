#include <Rcpp.h>
#include "core/x11_mean_endpoints.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
// [[Rcpp::export(name = ".native_x11_mean_filter")]]
Rcpp::List native_x11_mean_filter(SEXP values,SEXP frequency) {
    if((TYPEOF(frequency)!=REALSXP && TYPEOF(frequency)!=INTSXP) || Rf_isFactor(frequency) ||
        Rf_xlength(frequency)!=1 || !Rf_isNull(Rf_getAttrib(frequency,R_DimSymbol)))
        throw std::invalid_argument("frequency must be quarterly or monthly");
    const auto f=Rcpp::as<double>(frequency);
    if(f!=4 && f!=12)throw std::invalid_argument("frequency must be quarterly or monthly");
    if((TYPEOF(values)!=REALSXP && TYPEOF(values)!=INTSXP) || Rf_isFactor(values) ||
        !Rf_isNull(Rf_getAttrib(values,R_DimSymbol)) || Rf_xlength(values)>std::numeric_limits<int>::max()-12)
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
    const auto filtered=persephone::x11_mean_endpoints(input,static_cast<int>(f),[] {Rcpp::checkUserInterrupt();});
    Rcpp::NumericVector output(n);Rcpp::LogicalVector missing(n);
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        output[static_cast<R_xlen_t>(i)]=filtered.values.missing[i] ? NA_REAL : filtered.values.values[i];
        missing[static_cast<R_xlen_t>(i)]=filtered.values.missing[i]!=0;
    }
    return Rcpp::List::create(Rcpp::_["values"]=output,Rcpp::_["missing"]=missing,
        Rcpp::_["padding_means"]=Rcpp::NumericVector::create(
            Rcpp::_["left"]=filtered.left_missing ? NA_REAL : filtered.left_mean,
            Rcpp::_["right"]=filtered.right_missing ? NA_REAL : filtered.right_mean));
}
