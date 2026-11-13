#ifndef PERSEPHONE_TRANSFORMS_HPP
#define PERSEPHONE_TRANSFORMS_HPP

#include "numerics.hpp"

namespace persephone {

enum class Transform { none, logarithm };

struct TransformedSeries {
    MaskedSeries series;
    // d log(y) / dy = 1/y. Keep contributions separate: the likelihood decides
    // which observations belong to its effective sample, particularly when
    // differencing or initialization removes observations.
    MaskedSeries log_jacobian;
};

TransformedSeries transform_series(const MaskedSeries& series, Transform transform,
                                    const Interrupt& interrupt = {});

// An inverse transformation of a point is not a lognormal expectation or a
// bias-adjusted forecast. That requires an explicitly supplied variance.
MaskedSeries inverse_transform(const MaskedSeries& series, Transform transform,
                               const Interrupt& interrupt = {});

} // namespace persephone
#endif
