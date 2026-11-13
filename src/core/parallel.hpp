#ifndef PERSEPHONE_PARALLEL_HPP
#define PERSEPHONE_PARALLEL_HPP

#include "numerics.hpp"

#include <algorithm>
#include <cstdlib>
#include <exception>
#include <stdexcept>
#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace persephone {

// No process-global OpenMP setting is changed. The environment is read before
// regions start; callers must not mutate it concurrently with a core call.
struct ParallelInfo {
    bool compiled;
    int version;
    int requested_threads;
    int runtime_max_threads;
    int selected_threads;
    bool nested;
};

inline bool in_parallel_region() noexcept {
#ifdef _OPENMP
    return omp_in_parallel() != 0;
#else
    return false;
#endif
}

inline ParallelInfo parallel_info() {
    int requested = 2;
    const auto setting = std::getenv("PERSEPHONE_NUM_THREADS");
    if (setting && *setting) {
        requested = 0;
        for (const char* p = setting; *p; ++p) {
            if (*p < '0' || *p > '9' || requested > 64) {
                throw std::invalid_argument("PERSEPHONE_NUM_THREADS must be an integer from 1 to 64");
            }
            requested = requested * 10 + (*p - '0');
        }
        if (requested < 1 || requested > 64) {
            throw std::invalid_argument("PERSEPHONE_NUM_THREADS must be an integer from 1 to 64");
        }
    }
#ifdef _OPENMP
    const int maximum = std::max(1, omp_get_max_threads());
    const bool nested = in_parallel_region();
    return {true, _OPENMP, requested, maximum,
            nested ? 1 : std::min(requested, maximum), nested};
#else
    return {false, 0, requested, 1, 1, false};
#endif
}

// Probe only our own team. Runtime limits/dynamic adjustment can make the
// actual team smaller than the requested per-region limit.
inline int parallel_probe_threads(const ParallelInfo& info) {
    int actual = 1;
#ifdef _OPENMP
    if (info.selected_threads > 1 && !info.nested) {
#pragma omp parallel num_threads(info.selected_threads)
        {
#pragma omp single
            actual = omp_get_num_threads();
        }
    }
#else
    (void)info;
#endif
    return actual;
}

constexpr std::size_t parallel_work_threshold = 65536;
constexpr std::size_t parallel_chunk_work = 262144;
constexpr std::size_t parallel_chunk_outputs = 65536;

// Every worker writes distinct preallocated output positions. Each output's
// arithmetic order remains serial. Callers must use only C++ operations here:
// no R/Rcpp/LAPACK, callbacks, allocation of R objects, or shared mutation.
// An exception never crosses an OpenMP boundary: each contiguous block keeps
// its first exception, and the lowest-index failing block is rethrown after
// joining. Main-thread interrupts occur between bounded chunks only.
template <typename Function>
void parallel_for(std::size_t count, std::size_t work_per_output,
                  const Interrupt& interrupt, const Function& function) {
    const auto info = parallel_info();
    work_per_output = std::max<std::size_t>(1, work_per_output);
    const bool substantial = work_per_output >= parallel_work_threshold ||
        count >= (parallel_work_threshold + work_per_output - 1) / work_per_output;
    const bool parallel = info.compiled && info.selected_threads > 1 &&
        count >= 2 && substantial && work_per_output <= parallel_work_threshold;
    const auto chunk_size = std::max<std::size_t>(1, std::min(parallel_chunk_outputs,
        parallel_chunk_work / std::min(work_per_output, parallel_chunk_work)));
    for (std::size_t first = 0; first < count;) {
        if (interrupt && !info.nested) interrupt();
        const auto width = std::min(chunk_size, count - first);
        if (parallel && width > 1) {
            const auto blocks = static_cast<int>(std::min(width,
                static_cast<std::size_t>(info.selected_threads)));
            std::vector<std::exception_ptr> failures(static_cast<std::size_t>(blocks));
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(blocks)
#endif
            for (int block = 0; block < blocks; ++block) {
                const auto begin = first + width * static_cast<std::size_t>(block) /
                    static_cast<std::size_t>(blocks);
                const auto end = first + width * static_cast<std::size_t>(block + 1) /
                    static_cast<std::size_t>(blocks);
                try {
                    for (auto i = begin; i < end; ++i) function(i);
                } catch (...) {
                    failures[static_cast<std::size_t>(block)] = std::current_exception();
                }
            }
            for (const auto& failure : failures) if (failure) std::rethrow_exception(failure);
        } else {
            for (std::size_t i = first; i < first + width; ++i) function(i);
        }
        first += width;
    }
    if (interrupt && !info.nested) interrupt();
}

} // namespace persephone
#endif
