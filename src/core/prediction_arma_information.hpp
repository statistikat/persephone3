#ifndef PERSEPHONE_PREDICTION_ARMA_INFORMATION_HPP
#define PERSEPHONE_PREDICTION_ARMA_INFORMATION_HPP
#include "arma_information.hpp"
#include <optional>
namespace persephone {
struct PredictionArmaInformation {
    std::string status="conditional_unavailable",message;
    std::optional<ArmaInformation> arma;
    VectorJacobian derivatives;
    int evaluations=0;
    std::optional<DenseMatrix> covariance,increment,sensitivity,sensitivity_error;
    std::vector<double> standard_errors;
};
// Assemble a qualified delta increment from dimensionless point derivatives.
// The caller retains its conditional distribution on failure; no legacy or
// residual-df covariance is accepted as the parent profile-ML information.
void assemble_prediction_delta(PredictionArmaInformation& out,
    const DenseMatrix& conditional,const std::vector<double>& conditional_standard_errors,
    const Interrupt& interrupt);
} // namespace persephone
#endif
