#ifndef PERSEPHONE_RCPP_X11_MSR_SUPPORT_HPP
#define PERSEPHONE_RCPP_X11_MSR_SUPPORT_HPP
// Binding helpers only. No core header includes this Rcpp-dependent adapter.
#include <Rcpp.h>
#include "core/x11_msr.hpp"
namespace persephone_msr_rcpp {
int integer(SEXP value,const char* label);
persephone::MaskedSeries series(SEXP value);
persephone::X11MsrMode mode(SEXP value);
Rcpp::List table_result(const persephone::X11MsrTable& result,int frequency,persephone::X11MsrMode mode);
}
#endif
