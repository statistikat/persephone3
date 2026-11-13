#include "core/arma_identification.hpp"

#include <cassert>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <thread>

int main() {
    using namespace persephone;
    const DenseMatrix diagonal(3, 2, {3, 0, 0, 0, 2, 0});
    const auto values = singular_values(diagonal);
    assert(values.size() == 2 && std::abs(values[0] - 3) < 1e-14 && std::abs(values[1] - 2) < 1e-14);
    const auto wide = singular_values(DenseMatrix(2, 3, {1, 0, 0, 0, 0, 0}));
    assert(wide.size() == 2 && wide[0] == 1 && wide[1] == 0);
    const auto zero = singular_values(DenseMatrix(2, 2));
    assert(zero[0] == 0 && zero[1] == 0);
    bool invalid = false;
    try { singular_values(DenseMatrix(1, 1, {std::numeric_limits<double>::infinity()})); }
    catch (const std::invalid_argument&) { invalid = true; }
    assert(invalid);

    std::array<ArmaParameterBlock, 4> blocks{};
    blocks[0] = {{.4}, {0}}; blocks[1] = {{-.4}, {0}};
    const ArmaParameterMap common(blocks);
    const auto redundant = arma_identification(common, common.initial_coordinates(), 4);
    assert(redundant.status == "rank_deficient_transfer" && redundant.rank == 1);
    assert(redundant.numerator_jacobian->rows() == 3 && redundant.numerator_jacobian->columns() == 2);
    assert((*redundant.numerator_jacobian)(1, 0) == 1);
    assert(std::abs((*redundant.numerator_jacobian)(2, 0) + .4) < 1e-15);
    assert(redundant.singular_ratio < 1e-14);
    blocks[1].initial[0] += 1e-6;
    const ArmaParameterMap near(blocks);
    assert(arma_identification(near, near.initial_coordinates(), 12).status == "near_redundant_transfer");
    blocks[0].fixed[0] = 1;
    const ArmaParameterMap constrained(blocks);
    assert(arma_identification(constrained, constrained.initial_coordinates(), 12).status == "full_rank_transfer");
    blocks[1].fixed[0] = 1;
    const ArmaParameterMap fixed(blocks);
    assert(arma_identification(fixed, {}, 4).status == "no_free_parameters");
    assert(arma_identification(common, {1000, 0}, 4).status == "parameters_unavailable");
    for (const int period : {0, 5}) {
        invalid = false;
        try { arma_identification(common, common.initial_coordinates(), period); }
        catch (const std::invalid_argument&) { invalid = true; }
        assert(invalid);
    }
    const auto main_thread = std::this_thread::get_id();
    int callbacks = 0;
    arma_identification(common, common.initial_coordinates(), 4, 1e-10, 1e-4, [&] {
        assert(std::this_thread::get_id() == main_thread); ++callbacks;
    });
    for (int stop = 1; stop <= callbacks; ++stop) {
        int count = 0; bool interrupted = false;
        try { arma_identification(common, common.initial_coordinates(), 4, 1e-10, 1e-4, [&] {
            if (++count == stop) throw std::runtime_error("identification cancellation");
        }); } catch (const std::runtime_error& e) { interrupted = std::string(e.what()) == "identification cancellation"; }
        assert(interrupted);
    }
    const auto reused = arma_identification(common, common.initial_coordinates(), 4);
    assert(reused.singular_values == redundant.singular_values && reused.rank == redundant.rank);
    std::cout << "PASS: SVD, constrained transfer sensitivity and " << callbacks << " cancellation/reuse boundaries\n";
}
