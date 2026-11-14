#include <Rcpp.h>
#include "core/x11_seasonal_plan.hpp"
#include <stdexcept>
namespace {
using Request=persephone::X11SeasonalRequest;using Stage=persephone::X11SeasonalStage;
Request request(const std::string& name) {
    if(name=="stable")return Request::stable;
    if(name=="s3x1")return Request::s3x1;
    if(name=="s3x3")return Request::s3x3;
    if(name=="s3x5")return Request::s3x5;
    if(name=="s3x9")return Request::s3x9;
    if(name=="s3x15")return Request::s3x15;
    if(name=="x11default")return Request::x11default;
    if(name=="msr")return Request::msr;
    throw std::invalid_argument("unsupported seasonal request");
}
Stage stage(const std::string& name) {
    if(name=="b_initial")return Stage::b_initial;if(name=="b_final")return Stage::b_final;
    if(name=="c_initial")return Stage::c_initial;if(name=="c_final")return Stage::c_final;
    if(name=="d_initial")return Stage::d_initial;if(name=="d_final")return Stage::d_final;
    throw std::invalid_argument("stage must identify b/c/d and initial/final");
}
}
// [[Rcpp::export(name = ".native_x11_seasonal_plan")]]
Rcpp::List native_x11_seasonal_plan(SEXP frequency,SEXP filters,SEXP requested_stage) {
    if((TYPEOF(frequency)!=INTSXP && TYPEOF(frequency)!=REALSXP) || Rf_isFactor(frequency) ||
        Rf_xlength(frequency)!=1 || !Rf_isNull(Rf_getAttrib(frequency,R_DimSymbol)))throw std::invalid_argument("frequency must be 4 or 12");
    const auto value=Rcpp::as<double>(frequency);
    if(value!=4 && value!=12)throw std::invalid_argument("frequency must be 4 or 12");
    const int f=static_cast<int>(value);
    if(TYPEOF(filters)!=STRSXP || (Rf_xlength(filters)!=1 && Rf_xlength(filters)!=f) ||
        !Rf_isNull(Rf_getAttrib(filters,R_DimSymbol)))throw std::invalid_argument("filters must be one string or one per calendar period");
    if(TYPEOF(requested_stage)!=STRSXP || Rf_xlength(requested_stage)!=1 || STRING_ELT(requested_stage,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(requested_stage,R_DimSymbol)))throw std::invalid_argument("stage must be one supported string");
    std::vector<Request> requests;Rcpp::CharacterVector requested(f);
    for(int period=0;period<f;++period) {
        const auto item=STRING_ELT(filters,Rf_xlength(filters)==1 ? 0 : period);
        if(item==NA_STRING)throw std::invalid_argument("seasonal requests cannot be missing");
        requested[period]=item;requests.push_back(request(Rcpp::as<std::string>(item)));
    }
    const auto result=persephone::x11_seasonal_plan(f,requests,stage(Rcpp::as<std::string>(requested_stage)));
    const char* reasons[]{"fixed_choice","x11default_initial","x11default_final","msr_initial",
        "msr_preliminary_final","msr_mixed_request","requires_msr"};
    Rcpp::CharacterVector base(f),resolved(f),resolution(f);
    for(int period=0;period<f;++period) {
        const auto p=static_cast<std::size_t>(period);const auto length=result.base_filters[p];
        base[period]=length==0 ? "stable" : "s3x"+std::to_string(length);
        if(result.requires_msr)resolved[period]=NA_STRING;else resolved[period]=base[period];
        resolution[period]=reasons[static_cast<std::size_t>(result.resolutions[p])];
    }
    return Rcpp::List::create(Rcpp::_["frequency"]=f,Rcpp::_["requested_filters"]=requested,
        Rcpp::_["base_filters"]=base,Rcpp::_["resolved_filters"]=resolved,Rcpp::_["resolution_reasons"]=resolution,
        Rcpp::_["global_msr"]=result.global_msr,Rcpp::_["requires_msr"]=result.requires_msr,
        Rcpp::_["status"]=result.requires_msr ? "requires_msr" : "available");
}
