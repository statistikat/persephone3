#ifndef PERSEPHONE_ARMA_PARAMETERS_HPP
#define PERSEPHONE_ARMA_PARAMETERS_HPP

#include <array>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace persephone {

// In increasing lag order; AR polynomial 1-phi(B), MA polynomial 1+theta(B).
// Seasonal factors use the same coordinates in B^period. No R dependency.
struct ArmaParameterBlock {
    std::vector<double> initial;
    std::vector<std::uint8_t> fixed;
};

struct ArmaAdmissibility {
    std::string status;
    std::vector<double> reflections;
    std::size_t failed_order; // One-based recursion order, zero on success.
};

// Schur/Levinson step-down test on each factor, with an explicitly declared
// reflection margin. This is NOT a lower bound on polynomial root distance.
ArmaAdmissibility arma_admissibility(const std::vector<double>& coefficients,
                                     bool moving_average, double margin = 1e-8);

struct ArmaDecoded {
    std::string status;
    int failed_block; // ar, ma, seasonal_ar, seasonal_ma = 0..3; -1 on success.
    std::size_t failed_order;
    std::array<std::vector<double>, 4> coefficients;
    std::array<std::vector<double>, 4> reflections;
};

class ArmaParameterMap {
public:
    explicit ArmaParameterMap(std::array<ArmaParameterBlock, 4> blocks,
                              double margin = 1e-8);
    ArmaDecoded decode(const std::vector<double>& coordinates) const;
    const std::array<ArmaParameterBlock, 4>& blocks() const noexcept { return blocks_; }
    const std::array<std::string, 4>& modes() const noexcept { return modes_; }
    const std::vector<double>& initial_coordinates() const noexcept { return coordinates_; }
    double margin() const noexcept { return margin_; }

private:
    std::array<ArmaParameterBlock, 4> blocks_;
    std::array<std::string, 4> modes_;
    std::vector<double> coordinates_;
    double margin_;
};

} // namespace persephone
#endif
