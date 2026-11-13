#include "core/numerics.hpp"
#include "core/parallel.hpp"

#include <cmath>
#include <limits>
#include <stdexcept>
#include <unordered_set>

namespace persephone {
namespace {

std::int64_t checked_add(std::int64_t left, std::int64_t right) {
    const auto low = std::numeric_limits<std::int64_t>::min();
    const auto high = std::numeric_limits<std::int64_t>::max();
    if ((right > 0 && left > high - right) ||
        (right < 0 && left < low - right)) {
        throw std::overflow_error("time-domain index overflow");
    }
    return left + right;
}

void check_frequency(int frequency) {
    if (frequency != 4 && frequency != 12) {
        throw std::invalid_argument("frequency must be 4 or 12");
    }
}

void check_finite(const std::vector<double>& values, const char* message) {
    for (double value : values) {
        if (!std::isfinite(value)) throw std::invalid_argument(message);
    }
}

void poll(const Interrupt& interrupt, std::size_t operation) {
    if (interrupt && (operation & 4095U) == 0) interrupt();
}

double checked_sum_product(double sum, double left, double right) {
    const double value = sum + left * right;
    if (!std::isfinite(value)) {
        throw std::overflow_error("non-finite result in filter arithmetic");
    }
    return value;
}

} // namespace

TimeDomain::TimeDomain(std::int64_t year, int period, int frequency,
                       std::int64_t length)
    : start_(0), length_(length), frequency_(frequency) {
    check_frequency(frequency);
    if (period < 1 || period > frequency) {
        throw std::invalid_argument("start period must be between 1 and frequency");
    }
    if (length < 0) throw std::invalid_argument("length must be nonnegative");
    if (year > std::numeric_limits<std::int64_t>::max() / frequency ||
        year < std::numeric_limits<std::int64_t>::min() / frequency) {
        throw std::overflow_error("time-domain year overflow");
    }
    start_ = checked_add(year * frequency, period - 1);
    // Both the last observation and the next period must be representable.
    checked_add(start_, length_);
}

std::pair<std::int64_t, int> TimeDomain::at(std::int64_t offset) const {
    if (offset < 0 || offset > length_) {
        throw std::out_of_range("time-domain offset must lie between 0 and length");
    }
    const auto ordinal = checked_add(start_, offset);
    auto year = ordinal / frequency_;
    auto remainder = ordinal % frequency_;
    if (remainder < 0) {
        --year;
        remainder += frequency_;
    }
    return {year, static_cast<int>(remainder + 1)};
}

void MaskedSeries::validate() const {
    if (values.size() != missing.size()) {
        throw std::invalid_argument("values and missing mask must have equal lengths");
    }
    if (values.size() > static_cast<std::size_t>(
            std::numeric_limits<std::int64_t>::max())) {
        throw std::length_error("series exceeds supported index range");
    }
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (missing[i] > 1) throw std::invalid_argument("missing mask must be binary");
        if (!std::isfinite(values[i])) {
            throw std::invalid_argument("numeric storage must be finite, including masked values");
        }
    }
}

std::vector<double> convolve(const std::vector<double>& left,
                             const std::vector<double>& right,
                             const Interrupt& interrupt) {
    if (left.empty() || right.empty()) {
        throw std::invalid_argument("polynomials must contain at least one coefficient");
    }
    check_finite(left, "polynomial coefficients must be finite");
    check_finite(right, "polynomial coefficients must be finite");
    const auto maximum = std::vector<double>().max_size();
    if (left.size() > maximum || right.size() - 1 > maximum - left.size()) {
        throw std::length_error("convolution result is too large");
    }
    std::vector<double> result(left.size() + right.size() - 1, 0.0);
    const auto work = std::min(left.size(), right.size());
    if (work > parallel_work_threshold) {
        // Very long single reductions retain fine-grained serial polling.
        // They cannot be split/reassociated without changing summation order.
        const auto serial_interrupt = in_parallel_region() ? Interrupt{} : interrupt;
        std::size_t operations = 0;
        for (std::size_t i = 0; i < left.size(); ++i) {
            for (std::size_t j = 0; j < right.size(); ++j) {
                poll(serial_interrupt, operations++);
                result[i + j] = checked_sum_product(result[i + j], left[i], right[j]);
            }
        }
    } else {
        parallel_for(result.size(), work, interrupt, [&](std::size_t output) {
            const auto first = output >= right.size() ? output - (right.size() - 1) : 0;
            const auto last = std::min(output, left.size() - 1);
            double value = 0;
            // Same ascending left-index addition order as the original loop.
            for (auto i = first; i <= last; ++i) value = checked_sum_product(value, left[i], right[output - i]);
            result[output] = value;
        });
    }
    return result;
}

std::vector<double> difference_polynomial(int d, int seasonal_d, int frequency,
                                         const Interrupt& interrupt) {
    check_frequency(frequency);
    if (d < 0 || seasonal_d < 0 || d > 64 || seasonal_d > 64) {
        throw std::invalid_argument("d and seasonal_d must be integers between 0 and 64");
    }
    std::vector<double> result{1.0};
    const std::vector<double> regular{1.0, -1.0};
    std::vector<double> seasonal(static_cast<std::size_t>(frequency + 1), 0.0);
    seasonal.front() = 1.0;
    seasonal.back() = -1.0;
    for (int i = 0; i < d; ++i) result = convolve(result, regular, interrupt);
    for (int i = 0; i < seasonal_d; ++i) result = convolve(result, seasonal, interrupt);
    return result;
}

MaskedSeries finite_filter(const MaskedSeries& series,
                           const std::vector<double>& weights,
                           const std::vector<std::int64_t>& offsets,
                           const Interrupt& interrupt) {
    series.validate();
    if (weights.empty() || weights.size() != offsets.size()) {
        throw std::invalid_argument("weights and offsets must have equal nonzero lengths");
    }
    check_finite(weights, "filter weights must be finite");
    std::unordered_set<std::int64_t> seen;
    for (auto offset : offsets) {
        if (!seen.insert(offset).second) {
            throw std::invalid_argument("filter offsets must be unique");
        }
    }
    MaskedSeries result{std::vector<double>(series.values.size(), 0.0),
                        std::vector<std::uint8_t>(series.values.size(), 0)};
    const auto length = static_cast<std::int64_t>(series.values.size());
    const bool long_window = weights.size() > parallel_work_threshold / 2;
    const auto serial_interrupt = in_parallel_region() ? Interrupt{} : interrupt;
    std::size_t operations = 0;
    const auto calculate = [&](std::size_t output) {
        const auto i = static_cast<std::int64_t>(output);
        double value = 0.0;
        bool missing = false;
        // Resolve the entire missingness window before doing arithmetic. This
        // makes the mask independent of tap ordering and avoids overflow in a
        // result that must be missing because another active tap is absent.
        for (std::size_t j = 0; j < weights.size(); ++j) {
            if (long_window) poll(serial_interrupt, operations++);
            if (weights[j] == 0.0) continue;
            // Compare before adding so even INT64_MIN/MAX offsets are safe.
            if (offsets[j] < -i || offsets[j] >= length - i) {
                missing = true;
                break;
            }
            const auto index = static_cast<std::size_t>(i + offsets[j]);
            if (series.missing[index]) {
                missing = true;
                break;
            }
        }
        result.missing[static_cast<std::size_t>(i)] = missing;
        if (!missing) {
            for (std::size_t j = 0; j < weights.size(); ++j) {
                if (long_window) poll(serial_interrupt, operations++);
                if (weights[j] == 0.0) continue;
                const auto index = static_cast<std::size_t>(i + offsets[j]);
                value = checked_sum_product(value, weights[j], series.values[index]);
            }
            result.values[static_cast<std::size_t>(i)] = value;
        }
    };
    if (long_window) {
        for (std::size_t i = 0; i < series.values.size(); ++i) calculate(i);
    } else parallel_for(series.values.size(), 2 * weights.size(), interrupt, calculate);
    return result;
}

} // namespace persephone
