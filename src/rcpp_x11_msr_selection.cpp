#include "rcpp_x11_msr_support.hpp"
#include "core/x11_msr_selection.hpp"
#include <cmath>
#include <stdexcept>
namespace {
const char* band(persephone::X11MsrBand value) {
    using B=persephone::X11MsrBand;
    switch(value) {
    case B::unavailable:return "unavailable";
    case B::s3x3:return "s3x3";
    case B::lower_gap:return "lower_gap";
    case B::s3x5:return "s3x5";
    case B::upper_gap:return "upper_gap";
    case B::s3x9:return "s3x9";
    }
    throw std::logic_error("unknown MSR band");
}
const char* status(persephone::X11MsrPilotStatus value) {
    using S=persephone::X11MsrPilotStatus;
    switch(value) {
    case S::available:return "available";
    case S::insufficient_observations:return "insufficient_observations";
    case S::missing_observations:return "missing_observations";
    case S::nonpositive_relative_input:return "nonpositive_relative_input";
    case S::numerical_range:return "numerical_range";
    }
    throw std::logic_error("unknown MSR pilot status");
}
const char* status(persephone::X11MsrSelectionStatus value) {
    using S=persephone::X11MsrSelectionStatus;
    switch(value) {
    case S::available:return "available";
    case S::missing_observations:return "missing_observations";
    case S::nonpositive_relative_input:return "nonpositive_relative_input";
    case S::undefined_msr:return "undefined_msr";
    case S::numerical_range:return "numerical_range";
    case S::incomplete_table:return "incomplete_table";
    case S::budget_exhausted:return "budget_exhausted";
    }
    throw std::logic_error("unknown MSR selection status");
}
const char* reason(persephone::X11MsrSelectionReason value) {
    using R=persephone::X11MsrSelectionReason;
    switch(value) {
    case R::unavailable:return "unavailable";
    case R::decisive_ratio:return "decisive_ratio";
    case R::insufficient_initial_history:return "insufficient_initial_history";
    case R::insufficient_remaining_history:return "insufficient_remaining_history";
    case R::five_year_cap:return "five_year_cap";
    case R::iteration_budget:return "iteration_budget";
    }
    throw std::logic_error("unknown MSR selection reason");
}
Rcpp::CharacterVector choice(int filter) {
    return filter==0 ? Rcpp::CharacterVector::create(NA_STRING) :
        Rcpp::CharacterVector::create(std::string("s3x")+std::to_string(filter));
}
Rcpp::List pilot_result(const persephone::X11MsrPilot& value,int f,persephone::X11MsrMode mode) {
    Rcpp::List out=Rcpp::List::create(Rcpp::_["status"]=status(value.status),Rcpp::_["frequency"]=f,
        Rcpp::_["seasonal"]=R_NilValue,Rcpp::_["irregular"]=R_NilValue,
        Rcpp::_["component_rule"]=mode==persephone::X11MsrMode::relative ? "ratio" : "centered_difference");
    if(value.status==persephone::X11MsrPilotStatus::available) {
        out["seasonal"]=Rcpp::wrap(value.seasonal.values);out["irregular"]=Rcpp::wrap(value.irregular.values);
    }
    return out;
}
}
// [[Rcpp::export(name = ".native_x11_msr_policy")]]
Rcpp::List native_x11_msr_policy(SEXP ratio) {
    if((TYPEOF(ratio)!=REALSXP && TYPEOF(ratio)!=INTSXP) || Rf_isFactor(ratio) || Rf_xlength(ratio)!=1 ||
        !Rf_isNull(Rf_getAttrib(ratio,R_DimSymbol)))throw std::invalid_argument("MSR ratio must be one real number");
    const auto value=Rcpp::as<double>(ratio);const auto selected=persephone::x11_msr_band(value);
    const auto filter=selected==persephone::X11MsrBand::s3x3 ? 3 : (selected==persephone::X11MsrBand::s3x5 ? 5 :
        (selected==persephone::X11MsrBand::s3x9 ? 9 : 0));
    return Rcpp::List::create(Rcpp::_["ratio"]=value,Rcpp::_["band"]=band(selected),Rcpp::_["selected_filter"]=choice(filter),
        Rcpp::_["status"]=filter ? "available" : "ambiguous");
}
// [[Rcpp::export(name = ".native_x11_msr_pilot")]]
Rcpp::List native_x11_msr_pilot(SEXP values,SEXP frequency,SEXP mode) {
    using namespace persephone_msr_rcpp;
    const auto f=integer(frequency,"frequency");const auto measure=persephone_msr_rcpp::mode(mode);
    return pilot_result(persephone::x11_msr_pilot(series(values),f,measure,[]{Rcpp::checkUserInterrupt();}),f,measure);
}
// [[Rcpp::export(name = ".native_x11_msr_select")]]
Rcpp::List native_x11_msr_select(SEXP values,SEXP frequency,SEXP first_period,SEXP mode,SEXP iteration_policy,
    SEXP backcasts,SEXP forecasts,SEXP max_iterations) {
    using namespace persephone_msr_rcpp;
    const auto f=integer(frequency,"frequency"),first=integer(first_period,"first period"),back=integer(backcasts,"backcasts"),
        fore=integer(forecasts,"forecasts"),maximum=integer(max_iterations,"max_iterations");
    const auto measure=persephone_msr_rcpp::mode(mode);
    if(TYPEOF(iteration_policy)!=STRSXP || Rf_xlength(iteration_policy)!=1 || STRING_ELT(iteration_policy,0)==NA_STRING ||
        !Rf_isNull(Rf_getAttrib(iteration_policy,R_DimSymbol)))throw std::invalid_argument("provide one supported MSR iteration policy");
    const auto name=Rcpp::as<std::string>(iteration_policy);
    if(name!="jdplus" && name!="x11_five_year_cap")throw std::invalid_argument("unsupported MSR iteration policy");
    const auto policy=name=="jdplus" ? persephone::X11MsrIterationPolicy::jdplus : persephone::X11MsrIterationPolicy::x11_five_year_cap;
    const auto result=persephone::x11_msr_select(series(values),f,first,measure,policy,
        static_cast<std::size_t>(back),static_cast<std::size_t>(fore),static_cast<std::size_t>(maximum),[]{Rcpp::checkUserInterrupt();});
    const auto n=static_cast<R_xlen_t>(result.attempts.size());
    Rcpp::IntegerVector iterations(n),counts(n),removed(n),last(n);Rcpp::NumericVector ratios(n);
    Rcpp::CharacterVector bands(n),statuses(n);
    for(R_xlen_t j=0;j<n;++j) {
        if((j&4095)==0)Rcpp::checkUserInterrupt();const auto& attempt=result.attempts[static_cast<std::size_t>(j)];
        iterations[j]=static_cast<int>(j+1);counts[j]=static_cast<int>(attempt.count);removed[j]=static_cast<int>(attempt.years_removed);
        last[j]=back+counts[j];ratios[j]=std::isnan(attempt.global_msr) ? NA_REAL : attempt.global_msr;
        bands[j]=band(attempt.band);statuses[j]=status(attempt.status);
    }
    const auto trace=Rcpp::DataFrame::create(Rcpp::_["iteration"]=iterations,Rcpp::_["observations"]=counts,
        Rcpp::_["years_removed"]=removed,Rcpp::_["last_row"]=last,Rcpp::_["global_msr"]=ratios,
        Rcpp::_["band"]=bands,Rcpp::_["ratio_status"]=statuses);
    Rcpp::List out=Rcpp::List::create(Rcpp::_["status"]=status(result.status),Rcpp::_["reason"]=reason(result.reason),
        Rcpp::_["selected_filter"]=choice(result.selected_filter),Rcpp::_["using_default"]=result.using_default,
        Rcpp::_["iterations"]=static_cast<int>(n),Rcpp::_["iteration_policy"]=name,Rcpp::_["max_iterations"]=maximum,
        Rcpp::_["frequency"]=f,Rcpp::_["first_period"]=result.first_period,Rcpp::_["backcasts"]=back,Rcpp::_["forecasts"]=fore,
        Rcpp::_["estimation_count"]=static_cast<int>(result.estimation_count),Rcpp::_["partial_year_trim"]=static_cast<int>(result.partial_year_trim),
        Rcpp::_["calendar_count"]=static_cast<int>(result.calendar_count),Rcpp::_["final_count"]=static_cast<int>(result.final_count),
        Rcpp::_["attempts"]=trace,Rcpp::_["pilot"]=R_NilValue,Rcpp::_["table"]=R_NilValue);
    if(n>0)out["pilot"]=pilot_result(result.pilot,f,measure);
    if(result.has_table)out["table"]=table_result(result.table,f,measure);
    return out;
}
