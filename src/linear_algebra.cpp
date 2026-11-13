#include "core/linear_algebra.hpp"

// Exact interfaces are declared by R, including Fortran character lengths.
// References: Writing R Extensions sections 6.6/6.7; Netlib DPOTRF, DPOTRS,
// DPOCON and DGELSY documentation. No numerical factorization is reimplemented.
#include <R_ext/Lapack.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <string>
#include <utility>

namespace persephone {
namespace {

std::size_t checked_size(std::size_t rows, std::size_t columns) {
    const auto limit = static_cast<std::size_t>(std::numeric_limits<int>::max());
    if (rows == 0 || columns == 0) {
        throw std::invalid_argument("matrix dimensions must be positive");
    }
    if (rows > limit || columns > limit || rows > limit / columns ||
        rows * columns > std::vector<double>().max_size()) {
        throw std::length_error("matrix storage exceeds the supported LAPACK integer range");
    }
    return rows * columns;
}

void checkpoint(const Interrupt& interrupt) {
    if (interrupt) interrupt();
}

void check_info(int info, const char* routine) {
    if (info != 0) {
        throw std::runtime_error(std::string(routine) + " failed with LAPACK info=" +
                                 std::to_string(info));
    }
}

int workspace_size(double query, std::size_t minimum) {
    const double limit = static_cast<double>(std::numeric_limits<int>::max());
    if (!std::isfinite(query) || query < 1 || query > limit ||
        minimum > static_cast<std::size_t>(std::numeric_limits<int>::max())) {
        throw std::length_error("LAPACK workspace exceeds the supported integer range");
    }
    return static_cast<int>(std::max(std::ceil(query), static_cast<double>(minimum)));
}

double matrix_one_norm(const DenseMatrix& matrix, const Interrupt& interrupt) {
    long double result = 0;
    for (std::size_t column = 0; column < matrix.columns(); ++column) {
        checkpoint(interrupt);
        long double sum = 0;
        for (std::size_t row = 0; row < matrix.rows(); ++row) {
            sum += std::abs(static_cast<long double>(matrix(row, column)));
        }
        result = std::max(result, sum);
    }
    const auto norm = static_cast<double>(result);
    if (!std::isfinite(norm)) {
        throw std::overflow_error("matrix 1-norm exceeds the finite numeric range");
    }
    return norm;
}

DenseMatrix residuals(const DenseMatrix& matrix, const DenseMatrix& rhs,
                      const DenseMatrix& coefficients, const Interrupt& interrupt) {
    DenseMatrix result = rhs;
    const int m = static_cast<int>(matrix.rows());
    const int n = static_cast<int>(rhs.columns());
    const int k = static_cast<int>(matrix.columns());
    const double minus_one = -1;
    const double one = 1;
    checkpoint(interrupt);
    F77_CALL(dgemm)("N", "N", &m, &n, &k, &minus_one, matrix.values().data(), &m,
                    coefficients.values().data(), &k, &one,
                    result.values().data(), &m FCONE FCONE);
    result.validate(interrupt);
    return result;
}

} // namespace

DenseMatrix::DenseMatrix(std::size_t rows, std::size_t columns)
    : rows_(rows), columns_(columns), values_(checked_size(rows, columns), 0.0) {}

DenseMatrix::DenseMatrix(std::size_t rows, std::size_t columns,
                         std::vector<double> values)
    : rows_(rows), columns_(columns), values_(std::move(values)) {
    validate();
}

double DenseMatrix::operator()(std::size_t row, std::size_t column) const {
    if (row >= rows_ || column >= columns_) throw std::out_of_range("matrix index is outside dimensions");
    return values_.at(row + column * rows_);
}

double& DenseMatrix::operator()(std::size_t row, std::size_t column) {
    if (row >= rows_ || column >= columns_) throw std::out_of_range("matrix index is outside dimensions");
    return values_.at(row + column * rows_);
}

void DenseMatrix::validate(const Interrupt& interrupt) const {
    if (values_.size() != checked_size(rows_, columns_)) {
        throw std::invalid_argument("matrix storage does not match dimensions");
    }
    for (std::size_t i = 0; i < values_.size(); ++i) {
        if ((i & 4095U) == 0) checkpoint(interrupt);
        if (!std::isfinite(values_[i])) {
            throw std::invalid_argument("matrix values must be finite; missing values are unsupported");
        }
    }
}

std::vector<double> singular_values(const DenseMatrix& matrix, const Interrupt& interrupt) {
    matrix.validate(interrupt);
    const int m = static_cast<int>(matrix.rows()), n = static_cast<int>(matrix.columns());
    const auto small = static_cast<std::size_t>(std::min(m, n));
    const auto minimum = std::max(3 * small + static_cast<std::size_t>(std::max(m, n)), 5 * small);
    workspace_size(1, minimum);
    auto a = matrix;
    std::vector<double> values(small);
    double unused_u = 0, unused_v = 0, query = 0;
    const int one = 1;
    int info = 0, lwork = -1;
    checkpoint(interrupt);
    F77_CALL(dgesvd)("N", "N", &m, &n, a.values().data(), &m, values.data(),
        &unused_u, &one, &unused_v, &one, &query, &lwork, &info FCONE FCONE);
    check_info(info, "DGESVD workspace query");
    lwork = workspace_size(query, minimum);
    std::vector<double> work(static_cast<std::size_t>(lwork));
    checkpoint(interrupt);
    F77_CALL(dgesvd)("N", "N", &m, &n, a.values().data(), &m, values.data(),
        &unused_u, &one, &unused_v, &one, work.data(), &lwork, &info FCONE FCONE);
    check_info(info, "DGESVD");
    checkpoint(interrupt);
    double previous = std::numeric_limits<double>::infinity();
    for (const auto value : values) {
        if (!std::isfinite(value) || value < 0 || value > previous) {
            throw std::runtime_error("DGESVD returned invalid singular values");
        }
        previous = value;
    }
    return values;
}

CholeskyFactor cholesky_factor(const DenseMatrix& matrix, const Interrupt& interrupt) {
    matrix.validate(interrupt);
    if (matrix.rows() != matrix.columns()) {
        throw std::invalid_argument("Cholesky matrix must be square");
    }
    DenseMatrix factor = matrix;
    double scale = 0;
    for (const auto value : matrix.values()) scale = std::max(scale, std::abs(value));
    constexpr double tolerance = 64 * std::numeric_limits<double>::epsilon();
    for (std::size_t column = 0; column < matrix.columns(); ++column) {
        checkpoint(interrupt);
        for (std::size_t row = 0; row < column; ++row) {
            const double left = matrix(row, column);
            const double right = matrix(column, row);
            if (scale > 0 && std::abs(left / scale - right / scale) > tolerance) {
                throw std::invalid_argument("Cholesky matrix must be symmetric within roundoff tolerance");
            }
            // Accepted pairs are close enough that this difference cannot
            // overflow. Averaging makes the tolerance policy deterministic.
            const double average = left + (right - left) * 0.5;
            factor(row, column) = average;
            factor(column, row) = average;
        }
    }
    const double norm = matrix_one_norm(factor, interrupt);
    const int n = static_cast<int>(matrix.rows());
    int info = 0;
    checkpoint(interrupt);
    F77_CALL(dpotrf)("U", &n, factor.values().data(), &n, &info FCONE);
    if (info > 0) {
        throw std::invalid_argument("Cholesky matrix is not positive definite (leading minor " +
                                    std::to_string(info) + ")");
    }
    check_info(info, "DPOTRF");
    factor.validate(interrupt);
    long double log_determinant = 0;
    for (std::size_t column = 0; column < matrix.columns(); ++column) {
        log_determinant += 2 * std::log(static_cast<long double>(factor(column, column)));
        for (std::size_t row = column + 1; row < matrix.rows(); ++row) factor(row, column) = 0;
    }
    const double logdet = static_cast<double>(log_determinant);
    if (!std::isfinite(logdet)) throw std::overflow_error("Cholesky log determinant is non-finite");
    const auto work_size = checked_size(3, static_cast<std::size_t>(n));
    std::vector<double> work(work_size);
    std::vector<int> iwork(static_cast<std::size_t>(n));
    double reciprocal_condition = 0;
    checkpoint(interrupt);
    F77_CALL(dpocon)("U", &n, factor.values().data(), &n, &norm, &reciprocal_condition,
                     work.data(), iwork.data(), &info FCONE);
    check_info(info, "DPOCON");
    checkpoint(interrupt);
    if (!std::isfinite(reciprocal_condition) || reciprocal_condition < 0 ||
        reciprocal_condition > 1 + 64 * std::numeric_limits<double>::epsilon()) {
        throw std::runtime_error("DPOCON returned an invalid reciprocal condition estimate");
    }
    // Roundoff in norm * inverse-norm may put a scalar/diagonal estimate a few
    // ulps above its mathematical upper bound. Do not reject such a valid SPD.
    reciprocal_condition = std::min(1.0, reciprocal_condition);
    return {std::move(factor), logdet, reciprocal_condition};
}

DenseMatrix cholesky_solve(const CholeskyFactor& factor, const DenseMatrix& rhs,
                           const Interrupt& interrupt) {
    factor.upper.validate(interrupt);
    rhs.validate(interrupt);
    const auto size = factor.upper.rows();
    if (factor.upper.columns() != size || rhs.rows() != size) {
        throw std::invalid_argument("Cholesky solve dimensions do not match");
    }
    for (std::size_t column = 0; column < size; ++column) {
        if (factor.upper(column, column) <= 0) {
            throw std::invalid_argument("Cholesky factor diagonal must be positive");
        }
        for (std::size_t row = column + 1; row < size; ++row) {
            if (factor.upper(row, column) != 0) {
                throw std::invalid_argument("Cholesky factor must be upper triangular");
            }
        }
    }
    DenseMatrix result = rhs;
    const int n = static_cast<int>(size);
    const int nrhs = static_cast<int>(rhs.columns());
    int info = 0;
    checkpoint(interrupt);
    F77_CALL(dpotrs)("U", &n, &nrhs, factor.upper.values().data(), &n,
                     result.values().data(), &n, &info FCONE);
    check_info(info, "DPOTRS");
    result.validate(interrupt);
    return result;
}

LeastSquaresResult qr_least_squares(const DenseMatrix& matrix, const DenseMatrix& rhs,
                                    double rank_tolerance, const Interrupt& interrupt) {
    matrix.validate(interrupt);
    rhs.validate(interrupt);
    if (matrix.rows() != rhs.rows()) throw std::invalid_argument("least-squares row dimensions do not match");
    if (!std::isfinite(rank_tolerance) || rank_tolerance <= 0 || rank_tolerance >= 1) {
        throw std::invalid_argument("rank tolerance must be finite and strictly between zero and one");
    }
    const int m = static_cast<int>(matrix.rows());
    const int n = static_cast<int>(matrix.columns());
    const int nrhs = static_cast<int>(rhs.columns());
    const int ldb = std::max(m, n);
    const auto mn = static_cast<std::uint64_t>(std::min(m, n));
    const auto minimum_work_wide = std::max(mn + 3 * static_cast<std::uint64_t>(n) + 1,
                                            2 * mn + static_cast<std::uint64_t>(nrhs));
    // Check arithmetic before asking LAPACK for its optimal workspace.
    if (minimum_work_wide > static_cast<std::uint64_t>(std::numeric_limits<int>::max())) {
        throw std::length_error("LAPACK workspace exceeds the supported integer range");
    }
    const auto minimum_work = static_cast<std::size_t>(minimum_work_wide);
    workspace_size(1, minimum_work);
    DenseMatrix a = matrix;
    DenseMatrix b(static_cast<std::size_t>(ldb), rhs.columns());
    for (std::size_t column = 0; column < rhs.columns(); ++column) {
        checkpoint(interrupt);
        std::copy_n(rhs.values().data() + column * rhs.rows(), rhs.rows(),
                    b.values().data() + column * b.rows());
    }
    std::vector<int> pivots(matrix.columns(), 0);
    int rank = 0;
    int info = 0;
    int lwork = -1;
    double query = 0;
    checkpoint(interrupt);
    F77_CALL(dgelsy)(&m, &n, &nrhs, a.values().data(), &m, b.values().data(), &ldb,
                     pivots.data(), &rank_tolerance, &rank, &query, &lwork, &info);
    check_info(info, "DGELSY workspace query");
    lwork = workspace_size(query, minimum_work);
    std::vector<double> work(static_cast<std::size_t>(lwork));
    checkpoint(interrupt);
    F77_CALL(dgelsy)(&m, &n, &nrhs, a.values().data(), &m, b.values().data(), &ldb,
                     pivots.data(), &rank_tolerance, &rank, work.data(), &lwork, &info);
    check_info(info, "DGELSY");
    checkpoint(interrupt);
    if (rank < 0 || rank > std::min(m, n)) throw std::runtime_error("DGELSY returned an invalid rank");
    // LAPACK's all-zero-matrix quick return does not initialize JPVT.
    if (rank == 0 && std::all_of(pivots.begin(), pivots.end(), [](int value) { return value == 0; })) {
        for (std::size_t i = 0; i < pivots.size(); ++i) pivots[i] = static_cast<int>(i + 1);
    }
    std::vector<bool> seen(matrix.columns(), false);
    for (const auto pivot : pivots) {
        if (pivot < 1 || pivot > n || seen[static_cast<std::size_t>(pivot - 1)]) {
            throw std::runtime_error("DGELSY returned an invalid column permutation");
        }
        seen[static_cast<std::size_t>(pivot - 1)] = true;
    }
    DenseMatrix coefficients(matrix.columns(), rhs.columns());
    for (std::size_t column = 0; column < rhs.columns(); ++column) {
        checkpoint(interrupt);
        std::copy_n(b.values().data() + column * b.rows(), matrix.columns(),
                    coefficients.values().data() + column * coefficients.rows());
    }
    coefficients.validate(interrupt);
    auto errors = residuals(matrix, rhs, coefficients, interrupt);
    return {std::move(coefficients), std::move(errors), rank, std::move(pivots), rank_tolerance};
}

} // namespace persephone
