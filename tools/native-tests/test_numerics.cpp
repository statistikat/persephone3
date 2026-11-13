// Standalone tests of the R-independent core. Compile with:
// c++ -std=c++17 -Isrc src/numerics.cpp tools/native-tests/test_numerics.cpp -o /tmp/persephone-numerics-test
#include "core/numerics.hpp"

#include <cassert>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <vector>

template <typename Function>
bool throws(Function function) {
    try { function(); } catch (const std::exception&) { return true; }
    return false;
}

int main() {
    using namespace persephone;
    const TimeDomain monthly(2020, 11, 12, 4);
    assert(monthly.at(0) == std::make_pair(std::int64_t(2020), 11));
    assert(monthly.at(3) == std::make_pair(std::int64_t(2021), 2));
    assert(monthly.at(4) == std::make_pair(std::int64_t(2021), 3));
    assert(TimeDomain(-1, 4, 4, 2).at(1) == std::make_pair(std::int64_t(0), 1));
    assert(TimeDomain(-1, 1, 4, 0).at(0) == std::make_pair(std::int64_t(-1), 1));
    assert(throws([] { TimeDomain(2020, 1, 52, 2); }));
    assert(throws([] { TimeDomain(2020, 0, 12, 2); }));
    assert(throws([] { TimeDomain(2020, 1, 12, -2); }));
    assert(throws([] { TimeDomain(std::numeric_limits<std::int64_t>::max(), 1, 12, 2); }));
    assert(throws([&] { monthly.at(-1); }));
    assert(throws([&] { monthly.at(5); }));
    assert(convolve({1, 2}, {3, 4}) == std::vector<double>({3, 10, 8}));
    assert(difference_polynomial(1, 1, 4) == std::vector<double>({1, -1, 0, 0, -1, 1}));
    assert(difference_polynomial(0, 0, 12) == std::vector<double>({1}));
    assert(throws([] { convolve({}, {1}); }));
    assert(throws([] { convolve({1e308}, {10}); }));
    assert(throws([] { difference_polynomial(65, 1, 4); }));

    const MaskedSeries x{{1, 2, 4, 0, 16}, {0, 0, 0, 1, 0}};
    auto filtered = finite_filter(x, {1, -1}, {0, -1});
    assert(filtered.missing == std::vector<std::uint8_t>({1, 0, 0, 1, 1}));
    assert(filtered.values == std::vector<double>({0, 1, 2, 0, 0}));
    auto zeros = finite_filter(x, {0}, {std::numeric_limits<std::int64_t>::min()});
    assert(zeros.values == std::vector<double>(5, 0));
    assert(zeros.missing == std::vector<std::uint8_t>(5, 0));
    for (const auto offset : {std::numeric_limits<std::int64_t>::min(),
                              std::numeric_limits<std::int64_t>::max()}) {
        assert(finite_filter(x, {1}, {offset}).missing == std::vector<std::uint8_t>(5, 1));
    }
    assert(throws([&] { finite_filter(x, {1, 2}, {0, 0}); }));
    assert(throws([] { MaskedSeries{{1}, {}}.validate(); }));
    assert(throws([] { MaskedSeries{{0}, {2}}.validate(); }));
    assert(throws([] { MaskedSeries{{std::numeric_limits<double>::infinity()}, {0}}.validate(); }));

    // Callback-based cancellation remains R-independent and unwinds safely.
    int polls = 0;
    assert(throws([&] {
        convolve({1, 2}, {3, 4}, [&] { ++polls; throw std::runtime_error("cancelled"); });
    }));
    assert(polls == 1);
    assert(convolve({1, 2}, {3, 4}) == std::vector<double>({3, 10, 8}));
    assert(x.values == std::vector<double>({1, 2, 4, 0, 16}));
}
