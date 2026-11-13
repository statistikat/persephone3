#ifndef PERSEPHONE_DIFFUSE_STATE_SPACE_HPP
#define PERSEPHONE_DIFFUSE_STATE_SPACE_HPP

// Independently authored fixed-parameter exact diffuse filtering. No arbitrary
// large-variance initialization, parameter optimization or seasonal adjustment.
#include "state_space.hpp"

#include <cstddef>
#include <string>
#include <vector>

namespace persephone {

struct DiffuseStateSpace {
    DenseMatrix transition;
    std::vector<double> observation;
    DenseMatrix noise_loading;
    double observation_variance;
};

struct DiffuseInitialState {
    std::vector<double> state;
    DenseMatrix finite_loading; // P_star = C C', m x k, k >= 1.
    // B is m x diffuse_rank, column-major. Empty storage expresses rank zero.
    // Its columns must be numerically independent; no silent rank reduction.
    std::vector<double> diffuse_loading;
    std::size_t diffuse_rank;
};

struct DiffuseNumerics {
    double rank_tolerance;
    double projection_tolerance;
    std::size_t propagation_rank_checks;
    double maximum_projection_residual;
    bool exact_history_certification;
    std::size_t exact_history_zero_count;
    std::size_t exact_history_pivot_count;
};

struct DiffuseLikelihood {
    std::vector<double> predictions;
    MaskedSeries innovations; // Raw innovations at all observed times.
    MaskedSeries standardized_innovations; // Available only at finite updates.
    std::vector<double> finite_variances;
    std::vector<double> diffuse_variances;
    // 0 = missing, 1 = observed diffuse pivot, 2 = observed finite innovation.
    std::vector<int> update_kind;
    std::vector<std::size_t> rank_before;
    std::vector<std::size_t> rank_after;
    std::size_t observed_count;
    std::size_t diffuse_count;
    std::size_t finite_count;
    std::size_t initial_diffuse_rank;
    std::size_t remaining_diffuse_rank;
    std::size_t last_diffuse_index; // One-based; zero if no diffuse pivot.
    double sum_squared_standardized;
    double sum_log_finite_variances;
    double sum_log_diffuse_variances;
    double fixed_log_likelihood;
    double profiled_variance;
    double profiled_log_likelihood;
    // likelihood_status: available, no_observations, unresolved_diffuse.
    // profile_status: available, zero_scale_boundary, no_finite_innovations,
    // or unresolved_diffuse.
    std::string likelihood_status;
    std::string profile_status;
    std::vector<double> final_state;
    DenseMatrix initial_finite_covariance;
    DenseMatrix initial_diffuse_covariance;
    DenseMatrix final_finite_covariance;
    DenseMatrix final_diffuse_covariance;
    DiffuseNumerics numerics;
};

// y_t = mean + Z a_t + e_t; a_(t+1) = T a_t + L u_t.
// Finite disturbances have covariance variance * LL' and variance * h.
// Initial covariance is variance * CC' + kappa * BB', kappa -> infinity.
// Normalization is lim (2*pi*kappa)^(r/2) L_kappa in the supplied B coordinates.
// Missing observations advance time but consume no diffuse direction.
// Any loss of diffuse rank under propagation is rejected, not identified.
// At most 32 states/loading columns; no time-varying matrices in this pilot.
DiffuseLikelihood diffuse_innovations(
    const MaskedSeries& series, const DiffuseStateSpace& model,
    const DiffuseInitialState& initial, double mean = 0, double variance = 1,
    const Interrupt& interrupt = {});

struct SarimaLikelihood {
    DiffuseStateSpace model;
    std::vector<double> expanded_ar;
    std::vector<double> expanded_ma;
    std::vector<double> difference;
    DiffuseLikelihood likelihood;
};

// Delta(B) y_t follows the stationary ARMA/SARMA convention from 20a.1.
// State = [stationary ARMA state; y_(t-1),...,y_(t-k)], k=degree Delta.
// Initial finite covariance is blockdiag(P_stationary,0), diffuse covariance
// blockdiag(0,I_k). This fixes the flat-prior coordinates to the initial history.
// d and seasonal_d are in 0..2, period is 4 or 12, and total states <= 32.
// Integrated models require mean == 0; drift/regression is not defined here.
SarimaLikelihood sarima_innovations(
    const MaskedSeries& series, const std::vector<double>& ar,
    const std::vector<double>& ma, const std::vector<double>& seasonal_ar,
    const std::vector<double>& seasonal_ma, int period, int d, int seasonal_d,
    double mean = 0, double variance = 1, const Interrupt& interrupt = {});

} // namespace persephone
#endif
