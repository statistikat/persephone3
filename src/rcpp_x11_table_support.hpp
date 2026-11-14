#ifndef PERSEPHONE_RCPP_X11_TABLE_SUPPORT_HPP
#define PERSEPHONE_RCPP_X11_TABLE_SUPPORT_HPP
// Binding-only serialization shared by standalone primitives and composition.
#include <Rcpp.h>
#include "core/x11_seasonal_processing.hpp"
#include "core/x11_normalization.hpp"
namespace persephone_x11_rcpp {
Rcpp::NumericVector values(const persephone::MaskedSeries& series);
Rcpp::List seasonal_result(const persephone::X11SeasonalProcessing& result,int frequency,
    const std::vector<int>& choices);
Rcpp::List normalization_result(const persephone::X11SeasonalNormalization& result,int frequency,
    const std::vector<int>& choices,int extension);
}
#endif
