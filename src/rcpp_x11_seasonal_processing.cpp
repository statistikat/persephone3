#include <Rcpp.h>
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
std::string filter_name(int length) {return length==0 ? "stable" : "s3x"+std::to_string(length);}
const char* convention(int length) {
    if(length==0)return "phase_mean";
    if(length==15)return "x11_decimal_5";
    return length==3 || length==5 ? "virtual_values_rational" : "published_fixed_weights";
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
    Rcpp::NumericVector output(n);Rcpp::LogicalVector missing(n);
    Rcpp::CharacterVector methods(n);Rcpp::IntegerVector available(n);
    const char* method_names[]{"stable","symmetric","left_endpoint","right_endpoint","short_phase_mean"};
    for(std::size_t i=0;i<n;++i) {
        if((i&4095U)==0)Rcpp::checkUserInterrupt();
        const auto at=static_cast<R_xlen_t>(i);
        output[at]=result.values.missing[i] ? NA_REAL : result.values.values[i];missing[at]=result.values.missing[i]!=0;
        methods[at]=method_names[static_cast<std::size_t>(result.row_methods[i])];
        available[at]=result.available[i]<0 ? NA_INTEGER : result.available[i];
    }
    Rcpp::CharacterVector requested(f),selected(f),reasons(f),conventions(f);Rcpp::IntegerVector lengths(f);
    const char* reason_names[]{"requested_filter","requested_stable","fewer_than_five_complete_years",
        "s3x15_span_below_twenty_years","insufficient_endpoint_support"};
    for(int period=0;period<f;++period) {
        const auto p=static_cast<std::size_t>(period);
        requested[period]=filter_name(choices[p]);selected[period]=filter_name(result.selected_filters[p]);
        reasons[period]=reason_names[static_cast<std::size_t>(result.reasons[p])];
        conventions[period]=convention(result.selected_filters[p]);lengths[period]=static_cast<int>(result.phase_lengths[p]);
    }
    return Rcpp::List::create(Rcpp::_["values"]=output,Rcpp::_["missing"]=missing,
        Rcpp::_["requested_filters"]=requested,Rcpp::_["selected_filters"]=selected,
        Rcpp::_["phase_lengths"]=lengths,Rcpp::_["selection_reasons"]=reasons,
        Rcpp::_["coefficient_conventions"]=conventions,Rcpp::_["row_methods"]=methods,Rcpp::_["available"]=available);
}
