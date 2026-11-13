#include "core/parallel.hpp"
#include "core/transforms.hpp"

#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <set>
#include <stdexcept>
#include <string>
#include <thread>

namespace {
void threads(const char* value) {
#ifdef _WIN32
    assert(_putenv_s("PERSEPHONE_NUM_THREADS", value) == 0);
#else
    assert(setenv("PERSEPHONE_NUM_THREADS", value, 1) == 0);
#endif
}
template <typename Function>
bool fails(Function function, const std::string& message = "") {
    try { function(); }
    catch (const std::exception& error) { return message.empty() || message == error.what(); }
    return false;
}
}

int main() {
    using namespace persephone;
    const auto main_thread = std::this_thread::get_id();
    const Interrupt interrupt = [&] { assert(std::this_thread::get_id() == main_thread); };
    threads("1");
    assert(parallel_info().selected_threads == 1);
    assert(parallel_probe_threads(parallel_info()) == 1);
    const std::size_t n = 200000;
    std::vector<std::thread::id> workers(n);
    std::vector<std::size_t> serial(n), parallel(n);
    parallel_for(n, 1, interrupt, [&](std::size_t i) { serial[i] = i * 3; });
    threads("4");
    const auto info = parallel_info();
    parallel_for(n, 1, interrupt, [&](std::size_t i) {
        parallel[i] = i * 3;
        workers[i] = std::this_thread::get_id();
    });
    assert(parallel == serial);
    const std::set<std::thread::id> used(workers.begin(), workers.begin() + parallel_chunk_outputs);
    assert(used.size() <= static_cast<std::size_t>(info.selected_threads));
    const auto require = std::getenv("PERSEPHONE_REQUIRE_OPENMP");
    if (require && std::string(require) == "1") assert(info.compiled && used.size() > 1);
    if (!info.compiled) assert(used.size() == 1 && info.version == 0);

    // Separate blocks can fail concurrently; the earliest output wins after
    // every worker has joined, so no C++ exception crosses the OpenMP region.
    for (int repetition = 0; repetition < 8; ++repetition) {
        assert(fails([&] { parallel_for(n, 1, interrupt, [](std::size_t i) {
            if (i == 100) throw std::runtime_error("earlier");
            if (i == 45000) throw std::runtime_error("later");
        }); }, "earlier"));
    }
    for (int at = 1; at <= 3; ++at) {
        int calls = 0;
        assert(fails([&] { parallel_for(n, 1, [&] {
            assert(std::this_thread::get_id() == main_thread);
            if (++calls == at) throw std::runtime_error("cancelled");
        }, [&](std::size_t i) { parallel[i] = i; }); }, "cancelled"));
    }
    parallel_for(n, 1, interrupt, [&](std::size_t i) { parallel[i] = i * 3; });
    assert(parallel == serial);
#ifdef _OPENMP
    // Nested calls stay serial and do not execute a caller's interrupt from an
    // outer worker. No process-wide nesting or team settings are changed.
#pragma omp parallel num_threads(2)
    {
#pragma omp single
        {
            assert(parallel_info().nested && parallel_info().selected_threads == 1);
            const auto outer_thread = std::this_thread::get_id();
            parallel_for(n, 1, [] { throw std::runtime_error("worker interrupt"); },
                [&](std::size_t) { assert(std::this_thread::get_id() == outer_thread); });
        }
    }
#endif

    MaskedSeries values{std::vector<double>(n), std::vector<std::uint8_t>(n)};
    for (std::size_t i = 0; i < n; ++i) {
        values.missing[i] = i % 101 == 0;
        values.values[i] = values.missing[i] ? 0 : std::exp(std::sin(static_cast<double>(i) / 13));
    }
    threads("1");
    const auto transformed_serial = transform_series(values, Transform::logarithm, interrupt);
    const auto inverse_serial = inverse_transform(transformed_serial.series, Transform::logarithm, interrupt);
    threads("4");
    const auto transformed_parallel = transform_series(values, Transform::logarithm, interrupt);
    assert(transformed_serial.series.values == transformed_parallel.series.values);
    assert(transformed_serial.log_jacobian.values == transformed_parallel.log_jacobian.values);
    assert(transformed_serial.series.missing == transformed_parallel.series.missing);
    assert(inverse_serial.values == inverse_transform(transformed_parallel.series, Transform::logarithm, interrupt).values);
    auto invalid = values;
    invalid.values[1000] = -1;
    assert(fails([&] { transform_series(invalid, Transform::logarithm, interrupt); }));
    invalid.values[1000] = 1000;
    assert(fails([&] { inverse_transform(invalid, Transform::logarithm, interrupt); }));
    assert(transform_series(values, Transform::logarithm, interrupt).series.values == transformed_serial.series.values);

    std::vector<double> left(2048), right(129), reference(left.size() + right.size() - 1, 0);
    for (std::size_t i = 0; i < left.size(); ++i) left[i] = std::sin(static_cast<double>(i) / 7);
    for (std::size_t j = 0; j < right.size(); ++j) right[j] = std::cos(static_cast<double>(j) / 11);
    for (std::size_t i = 0; i < left.size(); ++i) {
        for (std::size_t j = 0; j < right.size(); ++j) reference[i + j] += left[i] * right[j];
    }
    threads("1");
    const auto convolution_serial = convolve(left, right, interrupt);
    threads("4");
    assert(convolve(left, right, interrupt) == convolution_serial);
    assert(convolution_serial == reference);

    std::vector<double> weights(31);
    std::vector<std::int64_t> offsets(31);
    for (std::size_t i = 0; i < weights.size(); ++i) {
        weights[i] = i % 3 == 0 ? 0 : 1.0 / static_cast<double>(i + 1);
        offsets[i] = static_cast<std::int64_t>(i) - 15;
    }
    threads("1");
    const auto filter_serial = finite_filter(values, weights, offsets, interrupt);
    threads("4");
    const auto filter_parallel = finite_filter(values, weights, offsets, interrupt);
    assert(filter_serial.values == filter_parallel.values);
    assert(filter_serial.missing == filter_parallel.missing);

    for (const auto bad : {"0", "65", "-1", "+2", "2.5", " 2", "two", "999999999999"}) {
        threads(bad);
        assert(fails([] { parallel_info(); }));
    }
    threads("1");
    assert(convolve({1, 2}, {3, 4}) == std::vector<double>({3, 10, 8}));
    std::cout << "PASS: " << (info.compiled ? "OpenMP" : "serial fallback")
              << "; actual worker threads=" << used.size()
              << "; deterministic outputs, errors and main-thread cancellation\n";
}
