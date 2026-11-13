#ifndef PERSEPHONE_FINITE_STATE_FACTOR_HPP
#define PERSEPHONE_FINITE_STATE_FACTOR_HPP

#include "linear_algebra.hpp"

#include <cstddef>
#include <vector>

namespace persephone {
namespace detail {

struct FiniteObservation {
    double variance;
    std::vector<double> pz;
};

// A unit-scale finite covariance P=C C'. Unlike a Cholesky covariance API,
// zero/rank-deficient factors are allowed. QR compression keeps all columns;
// it never decides the diffuse rank or drops weak positive finite directions.
class FiniteStateFactor {
public:
    explicit FiniteStateFactor(const DenseMatrix& covariance,
                               const Interrupt& interrupt = {});
    std::size_t dimension() const { return factor_.rows(); }
    std::size_t compression_count() const { return compression_count_; }
    std::size_t clipped_negative_count() const { return clipped_negative_count_; }
    double initialization_residual() const { return initialization_residual_; }

    FiniteObservation observation(const std::vector<double>& z, double h,
                                   const Interrupt& interrupt = {}) const;
    void update(const std::vector<double>& gain, const std::vector<double>& z,
                 double h, const Interrupt& interrupt = {});
    void predict(const DenseMatrix& transition, const DenseMatrix& noise_loading,
                  const Interrupt& interrupt = {});
    DenseMatrix covariance(const Interrupt& interrupt = {}) const;

private:
    std::vector<double> observation_loading(const std::vector<double>& z,
                                            const Interrupt& interrupt) const;
    void compress(const DenseMatrix& loading, const Interrupt& interrupt);

    DenseMatrix factor_;
    std::size_t compression_count_ = 0;
    std::size_t clipped_negative_count_ = 0;
    double initialization_residual_ = 0;
};

} // namespace detail
} // namespace persephone
#endif
