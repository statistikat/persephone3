# Explicit 3×15 endpoint evidence (22a.2b.2b)

`inst/tinytest/fixtures/native/x11-seasonal15/` adds 30 pinned public cases:
two frequencies, five lengths and three starting periods. Lengths are twenty
years, twenty years+1, twenty years+frequency−1, twenty-one years, and
thirty-seven years+3. Public enum/factory/processor calls capture every output
and every endpoint operator row of one relative phase. 24 separate controls
characterize stable versus 3×15 selection around the twentieth year; native
explicit support conservatively rejects them rather than choosing a filter.

Only public signatures and API execution were inspected. No numerical source,
bytecode, decompilation or copied implementation constants were used. The
independently derived mean-extension/decimal-closure rule matches every probed
boundary row. A separate dense R construction qualifies the arithmetic.
Capture scripts and deterministic inputs are hashed; new CSV artifacts have a
SHA-256 manifest. Prior corpora and historical reports are unchanged.

```sh
Rscript tools/reference/verify-x11-seasonal15.R
mkdir /tmp/persephone-x11-seasonal15-report
PERSEPHONE_NUM_THREADS=2 OMP_NUM_THREADS=2 Rscript tools/reference/verify-x11-seasonal15.R \
  --live --timing --output=/tmp/persephone-x11-seasonal15-report
```

The benchmark uses all 30 short cases and two 40,003-observation cases.
Native includes filtering and full R calendar/phase/boundary metadata; public
calls include enum/factory/processor execution and R-vector serialization.
Use the installed optimized native library, with actual OpenMP workers.
Three warmed alternating rounds use calibrated batches of at least 50 ms where
possible. Startup/loading and explicit pre-batch GC are excluded; natural
in-batch GC is included. These are filter-only comparisons, not complete X-11
or fitted-model timings; totals from different earlier matrices stay separate.

The declared coefficient convention is `x11_decimal_5`, distinct from
unrounded virtual extension. Qualification records their point difference
without treating the unrounded operator as a reference target. Prospective
absolute coefficient `1e-14` and scaled-point `2e-12` budgets are unchanged.
See `tools/native-tests/x11-seasonal15-contract.md` for exact definitions and
support/missingness semantics.

The offline verifier passes **1,255 checks / 30 cases**, and live replay/timing
passes **1,532 checks**. Stored public endpoint weights match exactly as
doubles. Maximum independent absolute coefficient error is **2.915e-16**;
scaled frozen, independent and live point errors are **4.993e-15**,
**5.377e-16** and **5.303e-16**, respectively. On the thirty input cases, the
maximum point difference from unrounded extension ranges from **8.587e-5** to
**2.115e-4**. That is a declared method difference, not a relaxed parity budget.
The previous 120-case seasonal verifier still passes all 1,875 offline checks.

The report records **72,057 timed calls**, including warm-up/calibration.
Median summed filter-only times from three warmed alternating rounds are:

| Input matrix | Native seconds | JDemetra seconds |
| --- | ---: | ---: |
| 30 short cases, 80–447 observations | 0.006289 | 0.004837 |
| 2 long cases, 40,003 observations each | 0.008308 | 0.001296 |

Native takes about 30% longer on the short matrix and is 6.41× slower on the
long matrix. Calendar/phase/boundary metadata is included on the native side;
the public side includes factories, bulk processing and serialization. No
complete decomposition or fitted-model speed claim follows. Raw timings,
errors, source hashes and actual two-worker metadata are versioned in
`tools/reference/information/2026-11-14-x11-seasonal15/`.
