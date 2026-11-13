#ifndef PERSEPHONE_ARMA_IDENTIFICATION_HPP
#define PERSEPHONE_ARMA_IDENTIFICATION_HPP

#include "arma_parameters.hpp"
#include "linear_algebra.hpp"
#include <optional>

namespace persephone {

struct ArmaIdentification {
    std::string status;
    std::string parameter_status;
    std::size_t free_parameters = 0;
    int rank = 0;
    double singular_ratio = 0;
    double minimum_reflection_slack = 0;
    double rank_tolerance = 1e-10;
    double near_tolerance = 1e-4;
    std::vector<double> singular_values, column_norms;
    std::optional<DenseMatrix> numerator_jacobian;
};

// Local sensitivity of H(B)=M(B)/A(B), in free polynomial coefficients.
// J_j(B)=A(B)*dM/dc_j-M(B)*dA/dc_j. Column-normalized SVD screens
// redundancy, including structured seasonal and fixed/free constraints.
// This is independent of data, convergence, regression rank and Fisher
// information. Full column rank does not certify statistical identification.
ArmaIdentification arma_identification(const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period,
    double rank_tolerance = 1e-10, double near_tolerance = 1e-4,
    const Interrupt& interrupt = {});

} // namespace persephone
#endif
