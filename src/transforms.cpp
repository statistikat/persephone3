#include "core/transforms.hpp"

#include <cmath>
#include <stdexcept>

namespace persephone {
namespace {
void validate_transform(Transform transform) {
    if (transform != Transform::none && transform != Transform::logarithm) {
        throw std::invalid_argument("unsupported transformation");
    }
}
}

TransformedSeries transform_series(const MaskedSeries& series, Transform transform,
                                    const Interrupt& interrupt) {
    series.validate();
    validate_transform(transform);
    TransformedSeries result{series, {std::vector<double>(series.values.size(), 0.0),
                                      series.missing}};
    for (std::size_t i = 0; i < series.values.size(); ++i) {
        if ((i & 4095U) == 0 && interrupt) interrupt();
        if (series.missing[i]) continue;
        if (transform == Transform::logarithm) {
            if (series.values[i] <= 0.0) {
                throw std::invalid_argument("log transformation requires positive observed values");
            }
            const double value = std::log(series.values[i]);
            if (!std::isfinite(value)) {
                throw std::overflow_error("log transformation produced a non-finite value");
            }
            result.series.values[i] = value;
            result.log_jacobian.values[i] = -value;
        }
    }
    return result;
}

MaskedSeries inverse_transform(const MaskedSeries& series, Transform transform,
                               const Interrupt& interrupt) {
    series.validate();
    validate_transform(transform);
    MaskedSeries result = series;
    for (std::size_t i = 0; i < series.values.size(); ++i) {
        if ((i & 4095U) == 0 && interrupt) interrupt();
        if (series.missing[i] || transform == Transform::none) continue;
        const double value = std::exp(series.values[i]);
        if (!std::isfinite(value) || value <= 0.0) {
            throw std::overflow_error("inverse log transformation overflowed or underflowed");
        }
        result.values[i] = value;
    }
    return result;
}
} // namespace persephone
