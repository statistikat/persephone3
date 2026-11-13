#include "core/state_space.hpp"

#include <cassert>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <string>

template<class Function>
bool rejects(Function function) {
    try { function(); } catch (const std::exception&) { return true; }
    return false;
}

int main() {
    using namespace persephone;
    const auto model = stationary_arma({.7}, {}, {}, {}, 12);
    const MaskedSeries observations{{0, 2, 0, 1, 0}, {1, 0, 1, 0, 1}};
    const auto fit = stationary_innovations(observations, model.state_space);
    assert(fit.observed_count == 2);
    assert(std::abs(fit.initialization.covariance(0, 0) - 1 / (1 - .7 * .7)) < 1e-12);
    assert(fit.innovations.missing == observations.missing);
    assert(std::abs(fit.predictions[3] - .7 * .7 * 2) < 1e-12);
    assert(std::abs(fit.innovation_variances[3] - (1 + .7 * .7)) < 1e-12);
    assert(std::abs(fit.final_state[0] - .7) < 1e-12);
    const auto white = stationary_arma({}, {}, {}, {}, 1);
    assert(stationary_innovations({{0}, {0}}, white.state_space).profile_status == "zero_scale_boundary");
    assert(stationary_innovations({{}, {}}, white.state_space).profile_status == "no_observations");
    assert(rejects([&] { stationary_innovations(observations, model.state_space, 0, 0); }));
    assert(rejects([&] { stationary_initialization(stationary_arma({1}, {}, {}, {}, 1).state_space); }));
    assert(rejects([&] { stationary_initialization(stationary_arma({1.1}, {}, {}, {}, 1).state_space); }));
    assert(rejects([&] { stationary_arma({}, {}, {.2, .1}, {}, 12); }));
    assert(rejects([&] { stationary_innovations({{1e308}, {0}}, white.state_space); }));
    assert(rejects([&] { stationary_innovations({{1e-200}, {0}}, white.state_space); }));

    // Every callback boundary must unwind without altering caller-owned data.
    int polls = 0;
    stationary_innovations(observations, model.state_space, 0, 1, [&] { ++polls; });
    assert(polls > 5);
    for (int cancel_at = 1; cancel_at <= polls; ++cancel_at) {
        int count = 0;
        bool cancelled = false;
        try {
            stationary_innovations(observations, model.state_space, 0, 1, [&] {
                if (++count == cancel_at) throw std::runtime_error("cancelled");
            });
        } catch (const std::runtime_error& error) {
            cancelled = std::string(error.what()) == "cancelled";
        }
        assert(cancelled);
    }
    const auto again = stationary_innovations(observations, model.state_space);
    assert(again.fixed_log_likelihood == fit.fixed_log_likelihood);
    assert(observations.values == std::vector<double>({0, 2, 0, 1, 0}));
}
