#include "core/arma_parameters.hpp"

#include <cassert>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>

namespace {
bool close(double x, double y) { return std::abs(x-y) < 1e-12 * (1+std::abs(y)); }
template<class F> void rejects(F f) {
    bool rejected = false;
    try { f(); } catch(const std::invalid_argument&) { rejected = true; }
    assert(rejected);
}
}

int main() {
    using namespace persephone;
    const double margin = 1e-8;
    const auto known = arma_admissibility({.5,.2}, false);
    assert(known.status == "admissible");
    assert(close(known.reflections[0], .625) && close(known.reflections[1], .2));
    assert(arma_admissibility({-.5,-.2}, true).reflections == known.reflections);
    // Independent AR(2) stability triangle: phi2 > -1,
    // phi1+phi2 < 1, phi2-phi1 < 1; stay well away from boundaries.
    for (int i=-19; i<=19; ++i) for (int j=-19; j<=19; ++j) {
        const double a = i / 10.0 + .003, b = j / 10.0 + .007;
        const bool expected = b > -1 && a+b < 1 && b-a < 1;
        assert((arma_admissibility({a,b}, false).status == "admissible") == expected);
        assert((arma_admissibility({-a,-b}, true).status == "admissible") == expected);
    }
    std::array<ArmaParameterBlock,4> blocks{};
    blocks[0] = {{.5,.2},{0,0}};
    blocks[1] = {{.1,-.2},{0,1}};
    blocks[2] = {{.3},{1}};
    const ArmaParameterMap map(blocks);
    assert(map.modes()[0] == "reflection" && map.modes()[1] == "coefficient");
    assert(map.modes()[2] == "fixed" && map.modes()[3] == "empty");
    const auto start = map.initial_coordinates();
    assert(start.size()==3 && close(start[0],std::atanh(.625/(1-margin))));
    assert(close(start[1],std::atanh(.2/(1-margin))) && start[2]==.1);
    const auto result = map.decode(start);
    assert(result.status=="admissible");
    assert(close(result.coefficients[0][0],.5) && close(result.coefficients[0][1],.2));
    assert(result.coefficients[1]==blocks[1].initial && result.coefficients[2]==blocks[2].initial);
    auto proposal = start;
    proposal[2] = 5;
    auto rejected = map.decode(proposal);
    assert(rejected.status=="inadmissible_parameters" && rejected.failed_block==1);
    assert(rejected.coefficients[1][1]==-.2 && rejected.coefficients[2][0]==.3);
    proposal = start; proposal[0] = std::numeric_limits<double>::infinity();
    rejected = map.decode(proposal);
    assert(rejected.status=="nonfinite_parameters" && rejected.failed_block==0);
    assert(rejected.coefficients[2][0]==.3);
    blocks = {}; blocks[0]={{0},{0}};
    const ArmaParameterMap single(blocks);
    assert(single.decode({1000}).status=="parameter_boundary");
    assert(single.decode({-1000}).status=="parameter_boundary");
    assert(single.decode({0}).status=="admissible");
    assert(arma_admissibility({1},false).status=="inadmissible_parameters");
    assert(arma_admissibility({1-margin/2},false).status=="parameter_boundary");
    assert(arma_admissibility({1-2*margin},false).status=="admissible");
    rejects([&]{single.decode({});});
    rejects([&]{ArmaParameterMap invalid(blocks, 0);});
    rejects([&]{ArmaParameterMap invalid(blocks, 1e-30);});
    blocks[0].fixed={2}; rejects([&]{ArmaParameterMap invalid(blocks);});
    blocks[0].fixed={}; rejects([&]{ArmaParameterMap invalid(blocks);});
    blocks[0]={{1},{1}}; rejects([&]{ArmaParameterMap invalid(blocks);});
    rejects([&]{arma_admissibility(std::vector<double>(17,0),false);});
    std::cout << "PASS: independent AR(2) stability region, parameter transforms, fixed states and boundaries\n";
}
