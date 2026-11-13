#ifndef PERSEPHONE_ARMA_INFORMATION_HPP
#define PERSEPHONE_ARMA_INFORMATION_HPP
#include "arma_objective.hpp"
#include "arma_identification.hpp"
#include "information.hpp"

namespace persephone {
enum class ArmaInformationMode { ProfileML, ConditionalResidualDF };
struct ArmaInformation {
    ObservedInformation information;
    ArmaIdentification structural;
    std::vector<double> coefficients;
    std::shared_ptr<const ArmaObjective> estimation_center;
    std::size_t free_regression = 0, residual_df = 0;
    double covariance_multiplier = 1;
};

// Independent, optional calculation at a supplied point; never changes solver
// convergence. Profiles regression and ML scale at every physical perturbation.
ArmaInformation arma_information(const MaskedSeries& series,
    const RegressionDesign& design, const ArmaParameterMap& map,
    const std::vector<double>& coordinates, int period, int d, int seasonal_d,
    double mean=0, double rank_tolerance=1e-10, double log_jacobian=0,
    const InformationControl& control={}, const Interrupt& interrupt={},
    ArmaInformationMode mode=ArmaInformationMode::ProfileML);
} // namespace persephone
#endif
