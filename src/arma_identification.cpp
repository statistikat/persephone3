#include "core/arma_identification.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace persephone {
ArmaIdentification arma_identification(const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period,
    double rank_tolerance, double near_tolerance, const Interrupt& interrupt) {
    if (period != 4 && period != 12) throw std::invalid_argument("ARMA identification requires period 4 or 12");
    if (!std::isfinite(rank_tolerance) || !std::isfinite(near_tolerance) ||
        rank_tolerance <= 0 || rank_tolerance >= near_tolerance || near_tolerance >= 1) {
        throw std::invalid_argument("identification tolerances require 0 < rank < near < 1");
    }
    const auto& blocks = map.blocks();
    const auto p = blocks[0].initial.size() + static_cast<std::size_t>(period) * blocks[2].initial.size();
    const auto q = blocks[1].initial.size() + static_cast<std::size_t>(period) * blocks[3].initial.size();
    if (std::max(p, q + 1) > 16) throw std::invalid_argument("ARMA identification exceeds the 16 stationary state bound");
    if (interrupt) interrupt();
    const auto decoded = map.decode(coordinates);
    ArmaIdentification result{};
    result.rank_tolerance = rank_tolerance; result.near_tolerance = near_tolerance;
    result.free_parameters = map.initial_coordinates().size();
    result.parameter_status = decoded.status;
    result.singular_ratio = std::numeric_limits<double>::quiet_NaN();
    result.minimum_reflection_slack = std::numeric_limits<double>::quiet_NaN();
    if (decoded.status != "admissible") { result.status = "parameters_unavailable"; return result; }
    for (const auto& factor : decoded.reflections) for (const auto k : factor) {
        const auto slack = 1 - std::abs(k);
        if (!std::isfinite(result.minimum_reflection_slack) || slack < result.minimum_reflection_slack) result.minimum_reflection_slack = slack;
    }
    if (result.free_parameters == 0) { result.status = "no_free_parameters"; return result; }
    std::array<std::vector<double>, 4> factors;
    for (std::size_t b = 0; b < 4; ++b) {
        const auto lag = b >= 2 ? static_cast<std::size_t>(period) : 1;
        factors[b].assign(1 + lag * blocks[b].initial.size(), 0);
        factors[b][0] = 1;
        for (std::size_t j = 0; j < decoded.coefficients[b].size(); ++j) {
            factors[b][lag * (j + 1)] = (b % 2 == 0 ? -1 : 1) * decoded.coefficients[b][j];
        }
    }
    // Both AR and MA numerator derivatives have positive leading sign under
    // A=1-phi(B), M=1+theta(B). All other factors appear once in each column.
    DenseMatrix jacobian(p + q + 1, result.free_parameters);
    std::size_t column = 0;
    for (std::size_t b = 0; b < 4; ++b) {
        auto product = std::vector<double>{1};
        for (std::size_t other = 0; other < 4; ++other) if (other != b) {
            product = convolve(product, factors[other], interrupt);
        }
        const auto lag = b >= 2 ? static_cast<std::size_t>(period) : 1;
        for (std::size_t j = 0; j < blocks[b].fixed.size(); ++j) {
            if (blocks[b].fixed[j]) continue;
            if (interrupt) interrupt();
            for (std::size_t k = 0; k < product.size(); ++k) jacobian(lag * (j + 1) + k, column) = product[k];
            ++column;
        }
    }
    auto normalized = jacobian;
    for (std::size_t j = 0; j < jacobian.columns(); ++j) {
        long double squared = 0;
        for (std::size_t i = 0; i < jacobian.rows(); ++i) squared += static_cast<long double>(jacobian(i, j)) * jacobian(i, j);
        const auto norm = static_cast<double>(std::sqrt(squared));
        if (!std::isfinite(norm) || norm <= 0) throw std::runtime_error("ARMA sensitivity column norm is unavailable");
        result.column_norms.push_back(norm);
        for (std::size_t i = 0; i < jacobian.rows(); ++i) normalized(i, j) /= norm;
    }
    result.singular_values = singular_values(normalized, interrupt);
    // The declared orders ensure at least as many polynomial rows as columns.
    if (result.singular_values.size() != result.free_parameters || result.singular_values.front() <= 0) {
        throw std::logic_error("ARMA sensitivity dimensions or spectrum are invalid");
    }
    result.singular_ratio = result.singular_values.back() / result.singular_values.front();
    for (const auto value : result.singular_values) if (value / result.singular_values.front() > rank_tolerance) ++result.rank;
    result.status = static_cast<std::size_t>(result.rank) < result.free_parameters ? "rank_deficient_transfer" :
        result.singular_ratio <= near_tolerance ? "near_redundant_transfer" : "full_rank_transfer";
    result.numerator_jacobian = std::move(jacobian);
    return result;
}
} // namespace persephone
