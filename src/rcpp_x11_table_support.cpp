#include "rcpp_x11_table_support.hpp"
#include <stdexcept>
namespace persephone_x11_rcpp {
std::string filter_name(int length) {return length==0 ? "stable" : "s3x"+std::to_string(length);}
const char* convention(int length) {
    if(length==0)return "phase_mean";
    if(length==15)return "x11_decimal_5";
    return length==3 || length==5 ? "virtual_values_rational" : "published_fixed_weights";
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
Rcpp::List seasonal_result(const persephone::X11SeasonalProcessing& result,int f,const std::vector<int>& choices) {
    const auto n=result.values.values.size();
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

Rcpp::List normalization_result(const persephone::X11SeasonalNormalization& result,int f,const std::vector<int>& choices,int extension) {
    Rcpp::CharacterVector requested(f);
    for(int j=0;j<f;++j)requested[j]=filter_name(choices[static_cast<std::size_t>(j)]);
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
} // namespace persephone_x11_rcpp
