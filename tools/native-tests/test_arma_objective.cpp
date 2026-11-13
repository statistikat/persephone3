#include "core/arma_objective.hpp"

#include <cassert>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>

namespace {
bool close(double x, double y) { return std::abs(x-y) < 1e-11*(1+std::abs(y)); }
template<class F> void rejects(F f) {
    bool rejected = false;
    try { f(); } catch(const std::invalid_argument&) { rejected = true; }
    assert(rejected);
}
}

int main() {
    using namespace persephone;
    const MaskedSeries series{{1,2,-1,3,0,2},std::vector<std::uint8_t>(6,0)};
    const RegressionDesign empty{0,{},{},{}};
    std::array<ArmaParameterBlock,4> blocks{};
    blocks[0]={{0},{0}};
    const ArmaParameterMap map(blocks);
    const auto white = evaluate_arma_objective(series,empty,map,{0},4,0,0);
    const auto scale = 19.0/6;
    const auto expected = 3*(std::log(2*std::acos(-1.0))+1+std::log(scale));
    assert(white.status=="available" && close(white.value,expected));
    const auto adjusted = evaluate_arma_objective(series,empty,map,{0},4,0,0,0,1e-10,-7);
    assert(close(adjusted.value,expected+7));
    assert(close(adjusted.likelihood->profiled_variance,scale));
    const auto boundary = evaluate_arma_objective(series,empty,map,{1000},4,0,0);
    assert(boundary.status=="parameter_boundary" && std::isnan(boundary.value) && !boundary.likelihood);
    const auto zero = evaluate_arma_objective({std::vector<double>(6,0),series.missing},empty,map,{0},4,0,0);
    assert(zero.status=="zero_scale_boundary" && std::isnan(zero.value));
    const RegressionDesign alias{1,std::vector<double>(6,0),{0},{0}};
    const auto rank = evaluate_arma_objective(series,alias,map,{0},4,0,0);
    assert(rank.status=="rank_deficient" && std::isnan(rank.value));
    auto huge = series; huge.values[0]=1e200;
    const auto numerical = evaluate_arma_objective(huge,empty,map,{0},4,0,0);
    assert(numerical.status=="numerical_failure" && !numerical.message.empty() && std::isnan(numerical.value));
    auto invalid = series; invalid.values[0]=std::numeric_limits<double>::infinity();
    rejects([&]{evaluate_arma_objective(invalid,empty,map,{1000},4,0,0);});
    rejects([&]{evaluate_arma_objective(series,empty,map,{1000},3,0,0);});
    rejects([&]{evaluate_arma_objective(series,empty,map,{1000},4,3,0);});
    rejects([&]{evaluate_arma_objective(series,empty,map,{1000},4,0,0,0,0);});
    // A runtime_error thrown by the caller is an interrupt, not a candidate's
    // numerical failure, even inside the guarded likelihood evaluation.
    int calls=0;
    evaluate_arma_objective(series,empty,map,{.2},4,0,0,0,1e-10,0,[&]{++calls;});
    for (int stop=1;stop<=calls;++stop) {
        int count=0; bool interrupted=false;
        try {
            evaluate_arma_objective(series,empty,map,{.2},4,0,0,0,1e-10,0,[&]{
                if(++count==stop) throw std::runtime_error("caller cancellation");
            });
        } catch(const std::runtime_error& error) {
            interrupted=std::string(error.what())=="caller cancellation";
        }
        assert(interrupted);
    }
    assert(evaluate_arma_objective(series,empty,map,{0},4,0,0).value==white.value);
    std::cout << "PASS: concentrated objective, statuses and " << calls << " cancellation/reuse boundaries\n";
}
