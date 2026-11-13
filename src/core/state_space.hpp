#ifndef PERSEPHONE_STATE_SPACE_HPP
#define PERSEPHONE_STATE_SPACE_HPP

// Independently authored stationary, fixed-parameter likelihood primitives.
// These do not estimate ARIMA parameters or perform seasonal adjustment.
#include "linear_algebra.hpp"

#include <cstddef>
#include <string>
#include <vector>

namespace persephone {

// y_t = mean + Z a_t + e_t; a_(t+1) = T a_t + L u_t.
// u_t ~ N(0, variance I), e_t ~ N(0, variance h), mutually independent.
// L is supplied instead of Q: Q=L L' is positive semidefinite by construction,
// including singular state disturbances. All matrices are time invariant.
// The pilot accepts 1..16 states and 1..16 disturbance loading columns.
struct StationaryStateSpace {
    DenseMatrix transition;
    std::vector<double> observation;
    DenseMatrix noise_loading;
    double observation_variance;
};

struct StationaryInitialization {
    DenseMatrix covariance; // Unit-variance P before observing y_1.
    DenseMatrix process_covariance;
    double scaled_residual;
    double certificate_reciprocal_condition;
};

// Solve P-TPT'=LL' and S-TST'=I using a bounded Kronecker system and pivoted
// QR (rank tolerance 1e-12). S positive definite certifies Schur stability.
// Numerical rank/residual/certificate failures reject the model; they are not
// all proofs of mathematical nonstationarity. No diffuse approximation is used.
StationaryInitialization stationary_initialization(
    const StationaryStateSpace& model, const Interrupt& interrupt = {});

struct InnovationsLikelihood {
    std::vector<double> predictions;
    MaskedSeries innovations;
    MaskedSeries standardized_innovations; // v_t / sqrt(F_t), unit scale.
    std::vector<double> innovation_variances; // F_t before scale, even at gaps.
    std::vector<double> final_state; // Posterior a_n (zero mean if no data).
    DenseMatrix final_covariance; // Unit-scale posterior covariance at n.
    StationaryInitialization initialization;
    std::size_t observed_count;
    double sum_squared_standardized;
    double sum_log_variances;
    double fixed_log_likelihood;
    double profiled_variance;
    double profiled_log_likelihood;
    // "available", "zero_scale_boundary", or "no_observations".
    std::string profile_status;
};

// The first observation uses the stationary prior directly (no extra first
// transition). Gaps skip the update and density contribution, not time.
// Joseph covariance updates preserve the semidefinite disturbance contract.
InnovationsLikelihood stationary_innovations(
    const MaskedSeries& series, const StationaryStateSpace& model,
    double mean = 0, double variance = 1, const Interrupt& interrupt = {});

struct StationaryArma {
    StationaryStateSpace state_space;
    std::vector<double> ar; // Expanded positive AR coefficients.
    std::vector<double> ma; // Expanded positive MA coefficients.
};

// (1-sum phi_i B^i)(1-sum Phi_j B^(s*j))(y_t-mean)
//   = (1+sum theta_i B^i)(1+sum Theta_j B^(s*j)) epsilon_t.
// Coefficients are fixed; MA invertibility is not required or imposed.
// Differencing, diffuse initialization and coefficient estimation are absent.
StationaryArma stationary_arma(
    const std::vector<double>& ar, const std::vector<double>& ma,
    const std::vector<double>& seasonal_ar, const std::vector<double>& seasonal_ma,
    int period, const Interrupt& interrupt = {});

} // namespace persephone
#endif
