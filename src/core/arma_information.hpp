#ifndef PERSEPHONE_ARMA_INFORMATION_HPP
#define PERSEPHONE_ARMA_INFORMATION_HPP
#include "arma_objective.hpp"
#include "arma_identification.hpp"
#include "information.hpp"

namespace persephone {
enum class ArmaInformationMode { ProfileML, ConditionalResidualDF, TramoResidual };
enum class ArmaCovarianceMode { Accurate, Legacy };
// Legacy covariance is a function of optimizer history, not final coefficients
// alone. A plain numeric state permits Java-free replay of the retained matrix.
// Coordinates and signed forward steps are free physical polynomial coefficients.
// Damping uses unscaled determinant-normalized residual units, z=e*D^(1/(2n)).
struct LegacyCovarianceState {
    std::vector<double> final_coefficients, working_coefficients, difference_steps;
    double damping = 0, normalized_sum_squares = 0;
    std::size_t effective_count = 0;
};
struct ArmaInformation {
    ObservedInformation information;
    ArmaIdentification structural;
    std::vector<double> coefficients;
    std::shared_ptr<const ArmaObjective> estimation_center;
    std::size_t free_regression = 0, residual_df = 0;
    double covariance_multiplier = 1;
    std::optional<ResidualInformation> residual_information;
    std::shared_ptr<const CompleteResiduals> residual_center;
    ArmaCovarianceMode covariance_mode = ArmaCovarianceMode::Accurate;
    std::optional<DenseMatrix> accurate_covariance, legacy_jacobian, legacy_information;
    double normalized_legacy_damping = 0;
    std::optional<LegacyCovarianceState> legacy_state;
};

struct ConditionalRegressionInformation {
    std::string status = "arma_information_unavailable";
    std::vector<std::size_t> free_columns;
    std::vector<double> coefficients;
    std::optional<DenseMatrix> covariance;
    std::size_t effective_count = 0, residual_df = 0, free_arma = 0;
    double residual_variance = 0;
};
// Conditional GLS uncertainty at a qualified fit, scaled by Q/(n-r-k).
// Free ARMA parameters reduce the denominator but their uncertainty is not
// propagated. Fixed coefficients are omitted, not assigned zero uncertainty.
ConditionalRegressionInformation conditional_regression_information(
    const ArmaInformation& fit, const Interrupt& interrupt = {});

struct JointRegressionInformation {
    std::string status = "arma_information_unavailable";
    ArmaInformation arma;
    std::vector<std::size_t> free_columns;
    std::vector<double> coefficients, normalized_eigenvalues;
    std::size_t effective_count = 0, residual_df = 0;
    int evaluations = 0;
    double ml_variance = 0, eigenvalue_ratio = 0;
    VectorJacobian derivatives;
    std::optional<DenseMatrix> conditional_ml_covariance, covariance,
        cross_covariance, joint_covariance, sensitivity, sensitivity_error;
};
// Joint profile-ML inference via the Gaussian Schur complement. The ARMA
// block must be accurate observed profile information: residual-df, GN and
// legacy matrices are different estimators and cannot be substituted here.
JointRegressionInformation joint_regression_information(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean=0, double rank_tolerance=1e-10, double log_jacobian=0,
    const InformationControl& control={}, const Interrupt& interrupt={});

// Independent, optional calculation at a supplied point; never changes solver
// convergence. Profiles regression and ML scale at every physical perturbation.
ArmaInformation arma_information(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean=0, double rank_tolerance=1e-10, double log_jacobian=0,
    const InformationControl& control={}, const Interrupt& interrupt={},
    ArmaInformationMode mode=ArmaInformationMode::ProfileML,
    ArmaCovarianceMode covariance_mode=ArmaCovarianceMode::Accurate,
    const std::optional<LegacyCovarianceState>& legacy_state=std::nullopt);
} // namespace persephone
#endif
