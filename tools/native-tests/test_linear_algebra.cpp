#include "core/linear_algebra.hpp"
#include <cassert>
#include <cmath>
#include <limits>
#include <stdexcept>

template <class F> void rejects(F f) {
    bool threw = false;
    try { f(); } catch (const std::exception&) { threw = true; }
    assert(threw);
}

int main() {
    using namespace persephone;
    rejects([] { DenseMatrix x(0, 1); });
    rejects([] { DenseMatrix x(std::numeric_limits<std::size_t>::max(), 2); });
    rejects([] { DenseMatrix x(65536, 65536); });
    rejects([] { DenseMatrix x(2, 2, {1, 2}); });
    rejects([] { DenseMatrix x(1, 1, {std::numeric_limits<double>::infinity()}); });
    DenseMatrix a(2, 2, {4, 1, 1, 3});
    DenseMatrix b(2, 1, {1, 2});
    const auto f = cholesky_factor(a);
    const auto x = cholesky_solve(f, b);
    assert(std::abs(x(0, 0) - 1.0 / 11) < 1e-14);
    assert(std::abs(x(1, 0) - 7.0 / 11) < 1e-14);
    rejects([&] { a(2, 0) = 1; });
    rejects([&] { a(0, 2) = 1; });
    a.values().pop_back();
    rejects([&] { cholesky_factor(a); });
    auto broken = f;
    broken.upper(1, 0) = 1;
    rejects([&] { cholesky_solve(broken, b); });
    broken = f;
    broken.upper(0, 0) = 0;
    rejects([&] { cholesky_solve(broken, b); });
    DenseMatrix wide(2, 3, {1, 0, 0, 1, 1, 1});
    const auto ls = qr_least_squares(wide, DenseMatrix(2, 1, {1, 1}));
    assert(ls.rank == 2);
    assert(std::abs(ls.coefficients(0, 0) - 1.0 / 3) < 1e-14);
    assert(std::abs(ls.coefficients(2, 0) - 2.0 / 3) < 1e-14);
    int callbacks = 0;
    qr_least_squares(wide, DenseMatrix(2, 1, {1, 1}), 1e-10, [&] { ++callbacks; });
    assert(callbacks > 0);
    rejects([&] { qr_least_squares(wide, b, 1e-10, [] { throw std::runtime_error("interrupted"); }); });
    assert(qr_least_squares(wide, b).rank == 2);
}
