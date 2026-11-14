# Residual/interpolation qualification and reporting study

`verify-residuals.R` evaluates accurate native outputs at the same physical ARMA
coefficients as 64 pinned public X-13 and TRAMO fits. The independent dense
Gaussian projection qualifies complete residuals and interpolation points;
mandatory tests separately qualify chronological observed innovations. Prospective
scaled point/residual tolerance remains `2e-8*(1+abs(target))`, and dates/counts
are exact. The frozen corpus is never changed.

The report separates independent qualification from public-output parity.
Accurate outputs match **104/128 fits**. The other **24 fits**, comprising seasonal
AR, seasonal MA and mixed-factor models with `d=0`, `D=1` and internal gaps at
both frequencies and transformations, fail public parity. Their likelihood `Q`,
coefficient estimates, finite counts and df still agree.

In these 12 cases, let `a` be the public preliminary transformed fill with fixed
effects restored, and `m` the accurate conditional Gaussian point. Public
interpolation is `2*a-m`, while the accurate point is `m`. Independently whitening
the public filled series at the fitted coefficients reproduces its diagnostic
residual vector. That vector's squared norm can differ from the reported
likelihood quadratic form. This is a behavioral identity observed in the
qualified matrix, not a claim about every upstream execution path.

## Closed example

Use quarterly data

```
7, 21, 3, 13, 9, NA, 8, 16, 11, 27, 6, 18, 14, 31, 12, 19
```

with a seasonal random walk, no AR/MA coefficients and no regression mean.
The missing value in row 6 lies between same-season observations 21 and 27,
so its conditional Gaussian point is **24**. Both native and public likelihoods
give `Q=130`. JDemetra's preliminary adjacent-time fill is `(9+8)/2=8.5`, and
both public engines report **−7 = 2*8.5−24** as the missing estimate.

In this zero-order case the public residual accessor reports an 11-element
compressed, rotated vector with norm squared 130, whereas the accurate complete
vector has 12 elements. Its values also differ from the 11 chronological observed
innovations. Consequently a generic legacy implementation cannot merely insert
reflected points and call the accurate complete filter in every model. Public
reported timestamps must not be confused with observed-innovation dates.

Only public model accessors and independently derived Gaussian calculations
were used. No upstream numerical implementation was translated or decompiled.
The native runtime does not invoke Java.

## Decision and follow-up

The user requested **Java-free legacy reporting for comparisons, with accurate
outputs as the default** on 2026-11-14. The accurate contract is delivered in
20c.2b.1a; 20c.2b.1b must qualify preliminary filling, missing-correction sign,
residual-basis selection and timestamp behavior before advertising legacy parity.
Unsupported reporting paths must be explicit. Prediction errors in 20c.2b.2
remain based on the accurate conditional distribution.

## Timing protocol

Optional `--timing` measures prepared native fit plus residual/interpolation
outputs against prepared public processing, serialization and public residual/
interpolation accessors. Three warmed rounds alternate engine order, calibrate
batches to at least 50 ms where possible and record all measured calls. Setup,
loading, startup and explicit pre-batch GC are excluded; natural in-batch GC is
included. These API workloads differ and some reported outputs differ as
documented above; this is not a matched-operation microbenchmark. Versioned
matrices, runtime/source hashes and timings are stored in
`information/2026-11-14-residuals/`.
