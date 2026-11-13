#ifndef PERSEPHONE_LINEAR_ALGEBRA_HPP
#define PERSEPHONE_LINEAR_ALGEBRA_HPP

// Independently authored adapters to R-supported LAPACK, not translated Java.
// This header owns its storage and does not depend on R or Rcpp. Linking the
// implementation requires R's LAPACK/BLAS libraries; it is not a standalone
// implementation of either factorization.
#include "numerics.hpp"

#include <cstddef>
#include <vector>

namespace persephone {

// Column-major storage. Empty dimensions and storage/dimensions exceeding the
// signed LAPACK integer range are deliberately unsupported by these primitives.
class DenseMatrix {
public:
    DenseMatrix(std::size_t rows, std::size_t columns);
    DenseMatrix(std::size_t rows, std::size_t columns, std::vector<double> values);
    std::size_t rows() const noexcept { return rows_; }
    std::size_t columns() const noexcept { return columns_; }
    const std::vector<double>& values() const noexcept { return values_; }
    std::vector<double>& values() noexcept { return values_; }
    double operator()(std::size_t row, std::size_t column) const;
    double& operator()(std::size_t row, std::size_t column);
    void validate(const Interrupt& interrupt = {}) const;

private:
    std::size_t rows_;
    std::size_t columns_;
    std::vector<double> values_;
};

struct CholeskyFactor {
    DenseMatrix upper;
    double log_determinant;
    // DPOCON estimate of 1 / (||A||_1 ||A^-1||_1), not a rank threshold.
    double reciprocal_condition;
};

// Symmetry tolerance is 64 * machine epsilon * max(abs(matrix)). Pairs within
// this roundoff tolerance are averaged explicitly before factorization; larger
// asymmetry is an error. Positive semidefinite/singular matrices are rejected.
CholeskyFactor cholesky_factor(const DenseMatrix& matrix,
                              const Interrupt& interrupt = {});
DenseMatrix cholesky_solve(const CholeskyFactor& factor, const DenseMatrix& rhs,
                           const Interrupt& interrupt = {});

struct LeastSquaresResult {
    DenseMatrix coefficients;
    // B - A * coefficients, in the original observation order and units.
    DenseMatrix residuals;
    int rank;
    // One-based pivot order: column j of A*P came from column pivots[j] of A.
    // Coefficients are already in original column order (never pivot again).
    std::vector<int> pivots;
    double rank_tolerance;
};

// DGELSY uses pivoted QR followed by a complete orthogonal factorization for
// minimum-norm solutions, including rank-deficient and underdetermined cases.
// rank_tolerance must be in (0, 1). It controls LAPACK's estimate of the rank
// of the leading triangular block, not a singular-value cutoff or an estimate
// of the condition number of the complete matrix. No covariance is inferred.
LeastSquaresResult qr_least_squares(const DenseMatrix& matrix,
                                    const DenseMatrix& rhs,
                                    double rank_tolerance = 1e-10,
                                    const Interrupt& interrupt = {});

// DGESVD singular values in descending order. No vectors or covariance are
// computed. The input is copied; all LAPACK work stays on the calling thread.
std::vector<double> singular_values(const DenseMatrix& matrix,
                                    const Interrupt& interrupt = {});

// DSYEV eigenvalues of a symmetric matrix in ascending order; no eigenvectors.
// Uses the same symmetry/finite input contract as Cholesky, but permits
// indefinite and singular matrices. No statistical rank is inferred here.
std::vector<double> symmetric_eigenvalues(const DenseMatrix& matrix,
                                         const Interrupt& interrupt = {});

} // namespace persephone
#endif
