#include "core/transforms.hpp"

#include <cassert>
#include <cmath>
#include <stdexcept>

int main() {
    using namespace persephone;
    const MaskedSeries input{{1.0, std::exp(2.0), 0.0}, {0, 0, 1}};
    const auto result = transform_series(input, Transform::logarithm);
    assert(result.series.values[0] == 0.0);
    assert(std::abs(result.series.values[1] - 2.0) < 1e-15);
    assert(result.log_jacobian.values[1] == -result.series.values[1]);
    assert(result.series.missing == input.missing);
    assert(result.log_jacobian.missing == input.missing);
    assert(inverse_transform(result.series, Transform::logarithm).values == input.values);
    assert(transform_series(input, Transform::none).log_jacobian.values[1] == 0.0);
    bool failed = false;
    try { transform_series({{0.0}, {0}}, Transform::logarithm); }
    catch (const std::invalid_argument&) { failed = true; }
    assert(failed);
    failed = false;
    try { inverse_transform({{1000.0}, {0}}, Transform::logarithm); }
    catch (const std::overflow_error&) { failed = true; }
    assert(failed);
    failed = false;
    try { inverse_transform({{-1000.0}, {0}}, Transform::logarithm); }
    catch (const std::overflow_error&) { failed = true; }
    assert(failed);
    failed = false;
    try { transform_series(input, static_cast<Transform>(999)); }
    catch (const std::invalid_argument&) { failed = true; }
    assert(failed);
    failed = false;
    try { transform_series(input, Transform::logarithm, [] {
        throw std::runtime_error("interrupted");
    }); }
    catch (const std::runtime_error&) { failed = true; }
    assert(failed);
    assert(transform_series(input, Transform::logarithm).series.values == result.series.values);
}
