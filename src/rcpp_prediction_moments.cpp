#include <Rcpp.h>
#include "core/prediction_moments.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

// [[Rcpp::export(name = ".native_prediction_moments")]]
Rcpp::List native_prediction_moments(SEXP location,SEXP covariance,SEXP transformation) {
    if((TYPEOF(location)!=REALSXP && TYPEOF(location)!=INTSXP) || Rf_isFactor(location) ||
        !Rf_isNull(Rf_getAttrib(location,R_DimSymbol)) || Rf_xlength(location)>std::numeric_limits<int>::max())
        throw std::invalid_argument("prediction locations must be a real numeric vector");
    const Rcpp::NumericVector m=Rcpp::as<Rcpp::NumericVector>(location);
    const auto dimensions=Rf_getAttrib(covariance,R_DimSymbol);
    if((TYPEOF(covariance)!=REALSXP && TYPEOF(covariance)!=INTSXP) || Rf_isFactor(covariance) ||
        TYPEOF(dimensions)!=INTSXP || Rf_xlength(dimensions)!=2 ||
        INTEGER(dimensions)[0]!=m.size() || INTEGER(dimensions)[1]!=m.size())
        throw std::invalid_argument("prediction covariance dimensions must match locations");
    if(TYPEOF(transformation)!=STRSXP || Rf_xlength(transformation)!=1 ||
        STRING_ELT(transformation,0)==NA_STRING || !Rf_isNull(Rf_getAttrib(transformation,R_DimSymbol)))
        throw std::invalid_argument("prediction transformation must be none or log");
    const auto name=Rcpp::as<std::string>(transformation);
    if(name!="none" && name!="log")throw std::invalid_argument("prediction transformation must be none or log");
    persephone::PredictionMoments result;
    if(m.size()) {
        const Rcpp::NumericVector values=Rcpp::as<Rcpp::NumericVector>(covariance);
        persephone::DenseMatrix matrix(static_cast<std::size_t>(m.size()),static_cast<std::size_t>(m.size()));
        matrix.values().assign(values.begin(),values.end());
        result=persephone::prediction_moments(std::vector<double>(m.begin(),m.end()),matrix,
            name=="log" ? persephone::Transform::logarithm : persephone::Transform::none,
            [] {Rcpp::checkUserInterrupt();});
    } else result.median_status=result.mean_status=result.covariance_status=result.standard_error_status="available";
    const auto vector=[](const std::vector<double>& values) {
        Rcpp::NumericVector out(values.size());
        for(std::size_t i=0;i<values.size();++i)out[static_cast<R_xlen_t>(i)]=std::isnan(values[i]) ? NA_REAL : values[i];
        return out;
    };
    Rcpp::List out=Rcpp::List::create(Rcpp::_["median_status"]=result.median_status,
        Rcpp::_["mean_status"]=result.mean_status,Rcpp::_["covariance_status"]=result.covariance_status,
        Rcpp::_["standard_error_status"]=result.standard_error_status,
        Rcpp::_["standard_error_message"]=result.standard_error_message,
        Rcpp::_["median_message"]=result.median_message,Rcpp::_["mean_message"]=result.mean_message,
        Rcpp::_["covariance_message"]=result.covariance_message,Rcpp::_["medians"]=vector(result.medians),
        Rcpp::_["means"]=vector(result.means),Rcpp::_["standard_errors"]=vector(result.standard_errors));
    out["covariance"]=R_NilValue;
    if(result.covariance)out["covariance"]=Rcpp::NumericMatrix(static_cast<int>(m.size()),static_cast<int>(m.size()),result.covariance->values().begin());
    else if(!m.size())out["covariance"]=Rcpp::NumericMatrix(0,0);
    out["available"]=result.mean_status=="available" && result.covariance_status=="available" && result.standard_error_status=="available";
    out["transformation"]=name;out["distribution"]=name=="log" ? "lognormal" : "Gaussian";
    return out;
}
