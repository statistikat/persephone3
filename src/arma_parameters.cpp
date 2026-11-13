#include "core/arma_parameters.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <utility>

namespace persephone {
namespace {

void validate_margin(double margin) {
    if (!std::isfinite(margin) || margin <= 0 || margin >= 1 || 1 - margin == 1) {
        throw std::invalid_argument("reflection margin must be representable and strictly between zero and one");
    }
}

// Direct algebraic Levinson recursion, independently implemented from its
// defining equations. For MA, apply the AR recursion to minus theta.
std::vector<double> from_reflections(const std::vector<double>& reflection,
                                      bool moving_average) {
    std::vector<double> coefficients;
    for (const auto k : reflection) {
        std::vector<double> next(coefficients.size() + 1);
        for (std::size_t j = 0; j < coefficients.size(); ++j) {
            next[j] = static_cast<double>(static_cast<long double>(coefficients[j]) -
                static_cast<long double>(k) * coefficients[coefficients.size() - 1 - j]);
        }
        next.back() = k;
        coefficients = std::move(next);
    }
    if (moving_average) for (auto& coefficient : coefficients) coefficient = -coefficient;
    return coefficients;
}

} // namespace

ArmaAdmissibility arma_admissibility(const std::vector<double>& coefficients,
                                     bool moving_average, double margin) {
    validate_margin(margin);
    if (coefficients.size() > 16) throw std::invalid_argument("ARMA factors support at most 16 coefficients");
    const double unavailable = std::numeric_limits<double>::quiet_NaN();
    ArmaAdmissibility result{"admissible", std::vector<double>(coefficients.size(), unavailable), 0};
    auto current = coefficients;
    for (auto& value : current) {
        if (!std::isfinite(value)) { result.status = "nonfinite_parameters"; return result; }
        if (moving_average) value = -value;
    }
    for (auto order = current.size(); order > 0; --order) {
        const auto k = current[order - 1];
        result.reflections[order - 1] = k;
        result.failed_order = order;
        if (!std::isfinite(k)) { result.status = "numerical_failure"; return result; }
        if (std::abs(k) >= 1) { result.status = "inadmissible_parameters"; return result; }
        if (std::abs(k) >= 1 - margin) { result.status = "parameter_boundary"; return result; }
        std::vector<double> previous(order - 1);
        const long double denominator = (1 - static_cast<long double>(k)) * (1 + static_cast<long double>(k));
        for (std::size_t j = 0; j + 1 < order; ++j) {
            previous[j] = static_cast<double>((static_cast<long double>(current[j]) +
                static_cast<long double>(k) * current[order - 2 - j]) / denominator);
            if (!std::isfinite(previous[j])) { result.status = "numerical_failure"; return result; }
        }
        current = std::move(previous);
    }
    result.failed_order = 0;
    // Check the finite-precision inverse against the original polynomial.
    const auto rebuilt = from_reflections(result.reflections, moving_average);
    double scale = 1, error = 0;
    for (std::size_t j = 0; j < coefficients.size(); ++j) {
        scale = std::max(scale, std::abs(coefficients[j]));
        error = std::max(error, std::abs(rebuilt[j] - coefficients[j]));
    }
    if (error > 256 * std::numeric_limits<double>::epsilon() * scale *
        static_cast<double>(std::max<std::size_t>(1, coefficients.size()))) result.status = "numerical_failure";
    return result;
}

ArmaParameterMap::ArmaParameterMap(std::array<ArmaParameterBlock, 4> blocks, double margin)
    : blocks_(std::move(blocks)), margin_(margin) {
    validate_margin(margin_);
    for (std::size_t b = 0; b < blocks_.size(); ++b) {
        const auto& block = blocks_[b];
        if (block.initial.size() > 16 || block.fixed.size() != block.initial.size()) {
            throw std::invalid_argument("ARMA coefficient states must match at most 16 initial values per factor");
        }
        std::size_t fixed = 0;
        for (const auto flag : block.fixed) {
            if (flag > 1) throw std::invalid_argument("ARMA fixed states must be zero or one");
            fixed += flag;
        }
        const auto check = arma_admissibility(block.initial, b == 1 || b == 3, margin_);
        if (check.status != "admissible") throw std::invalid_argument("initial ARMA factor " +
            std::to_string(b + 1) + " is not admissible: " + check.status);
        modes_[b] = block.initial.empty() ? "empty" : fixed == 0 ? "reflection" :
            fixed == block.initial.size() ? "fixed" : "coefficient";
        for (std::size_t j = 0; j < block.initial.size(); ++j) {
            if (block.fixed[j]) continue;
            coordinates_.push_back(fixed == 0 ? std::atanh(check.reflections[j] / (1 - margin_)) : block.initial[j]);
            if (!std::isfinite(coordinates_.back())) throw std::invalid_argument("initial ARMA coordinates are not representable");
        }
    }
}

ArmaDecoded ArmaParameterMap::decode(const std::vector<double>& coordinates) const {
    if (coordinates.size() != coordinates_.size()) throw std::invalid_argument("ARMA coordinates must match the free parameter count");
    ArmaDecoded result{"admissible", -1, 0, {}, {}};
    for (std::size_t b = 0; b < blocks_.size(); ++b) result.coefficients[b] = blocks_[b].initial;
    std::size_t offset = 0;
    std::array<bool, 4> boundary{};
    // Decode every block before checking admissibility, so fixed coefficients
    // and the complete proposal remain inspectable even on a rejected candidate.
    for (std::size_t b = 0; b < blocks_.size(); ++b) {
        auto& coefficients = result.coefficients[b];
        if (modes_[b] == "reflection") {
            std::vector<double> reflection(coefficients.size());
            bool finite = true;
            for (auto& k : reflection) {
                const auto coordinate = coordinates[offset++];
                if (!std::isfinite(coordinate)) {
                    finite = false;
                }
                k = (1 - margin_) * std::tanh(coordinate);
                if (std::abs(k) >= 1 - margin_) boundary[b] = true;
            }
            coefficients = finite ? from_reflections(reflection, b == 1 || b == 3) :
                std::vector<double>(coefficients.size(), std::numeric_limits<double>::quiet_NaN());
        } else {
            for (std::size_t j = 0; j < coefficients.size(); ++j) {
                if (!blocks_[b].fixed[j]) coefficients[j] = coordinates[offset++];
            }
        }
    }
    for (std::size_t b = 0; b < blocks_.size(); ++b) {
        auto check = arma_admissibility(result.coefficients[b], b == 1 || b == 3, margin_);
        if (boundary[b] && check.status == "admissible") check.status = "parameter_boundary";
        result.reflections[b] = check.reflections;
        if (check.status != "admissible" && result.status == "admissible") {
            result.status = check.status; result.failed_block = static_cast<int>(b); result.failed_order = check.failed_order;
        }
    }
    return result;
}

} // namespace persephone
