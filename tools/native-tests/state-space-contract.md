# Stationary innovations likelihood pilot (20a.1)

This independently authored kernel evaluates fixed parameters. It does not
estimate coefficients, initialize diffuse states, difference observations,
select a model, or perform seasonal adjustment. The Rcpp functions remain
internal; requesting a native seasonal-adjustment fit remains unsupported.

## Mathematical contract

The time-invariant scalar-observation model is

\[
y_t=\mu+Za_t+e_t,\qquad a_{t+1}=Ta_t+Lu_t,
\]

with independent Gaussian disturbances having variances
\(\operatorname{Var}(u_t)=\sigma^2 I\) and
\(\operatorname{Var}(e_t)=\sigma^2h\). The caller supplies the loading matrix
\(L\), not an arbitrary covariance: \(Q=LL'\) is positive semidefinite by
construction, including rank-deficient noise. All coefficients must be finite,
\(h\ge0\), and the fixed scale \(\sigma^2>0\). The initial state mean is zero.

The covariance before the first observation solves
\(P-TPT'=Q\). A second equation, \(S-TST'=I\), supplies a stability
certificate: a positive definite solution exists exactly when every eigenvalue
of \(T\) has modulus below one, by the discrete-time theorem in
[Boyd's Lyapunov notes](https://web.stanford.edu/class/ee363/notes/lq-lyap-notes.pdf).
Both equations use the same column-major
Kronecker system and R-supported LAPACK pivoted QR solve. Thus singular process
noise does not require a positive definite \(P\). The first observation uses
this prior directly, without an extra transition or an arbitrary large variance.
Stationary unconditional initialization follows the state-space treatment in
[Casals and Sotoca](https://www.ucm.es/data/cont/docs/562-2013-11-11-fvinitEcoLet.pdf).

For each observed time, the kernel computes
\(v_t=y_t-\mu-Za_{t|t-1}\) and
\(F_t=ZP_{t|t-1}Z'+h\). The posterior covariance uses Joseph's form,
\((I-KZ)P(I-KZ)'+hKK'\), with \(K=PZ'/F_t\). A missing observation skips
the measurement update and likelihood contribution. State propagation still
advances one period, including leading, consecutive, and trailing gaps.

The complete fixed-scale Gaussian log likelihood is

\[
-\tfrac12\{n_o\log(2\pi\sigma^2)+\sum_{t\in O}\log F_t+
\sigma^{-2}\sum_{t\in O}v_t^2/F_t\}.
\]

The result separately returns the profiled scale
\(\widehat{\sigma^2}=\sum v_t^2/(F_t n_o)\) and its complete log likelihood.
The innovations variances, initial/final covariance, and standardized errors
are **unit scale**. The final state/covariance are the posterior at the last
input time, including propagation through trailing missing periods.

If the residual sum is zero, the scale MLE is on the zero boundary and the
profile likelihood is unbounded: the status is `zero_scale_boundary`, variance
is zero, and profile log likelihood is positive infinity. With no observations,
the fixed log likelihood is zero; profile quantities are unavailable and the
status is `no_observations`. A zero prediction variance at an observed time is
unsupported, rather than treated as an ordinary Gaussian density.

## ARMA and seasonal conventions

The wrapper implements the explicitly fixed, stationary model

\[
(1-\sum\phi_iB^i)(1-\sum\Phi_jB^{sj})(y_t-\mu)
=(1+\sum\theta_iB^i)(1+\sum\Theta_jB^{sj})\epsilon_t.
\]

The mean is the unconditional series mean, not the intercept in a lag equation.
MA coefficients use R's positive-sign convention; no invertibility transform is
performed. Seasonal polynomials multiply before construction of the state model.
The resulting dimension is \(\max(p,q+1,1)\) for the expanded orders.
These parameter meanings are checked against the official
[R `arima` documentation](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.html).

Parity tests select `SSinit="Rossignol2011"` explicitly in `makeARIMA`.
For R's `KalmanLike`, the reported `Lik` is converted to complete profile log
likelihood by `-n_observed * (Lik + (log(2*pi)+1)/2)`. `KalmanRun$resid` is compared
with the unit-scale standardized innovation. R documents the state timing and
initialization in its [Kalman filtering reference](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/help/KalmanForecast.html).

## Numerical limits and validation

The pilot permits at most 16 states and 16 loading columns. The QR rank
tolerance is `1e-12`; the symmetrized Lyapunov solutions must have a scaled
maximum residual at most `1e-10`. Cholesky must certify \(S\) positive definite
with estimated reciprocal condition above `1e-12`. These are explicit
computational limits: rejection for numerical rank or conditioning is not a
proof that an exact mathematical model is nonstationary. In particular, highly
nonnormal systems can be rejected even when their eigenvalues are stable.
No silent diffuse or approximate-likelihood fallback occurs.

The Rcpp boundary accepts real or integer vectors and two-dimensional matrices,
with explicit shape checks before conversion. Complex, logical, character and
factor data are rejected, including scalar arguments. Internal optional scalar
arguments use `NULL` sentinels for the effective defaults: mean zero,
observation variance zero, and innovation scale variance one.

Tests compare white-noise and AR(1) closed forms, R's stationary ARMA likelihood,
fixed quarterly/monthly SARMA fits, and an independently assembled joint Gaussian
covariance density with gaps. They also cover singular noise, near-unit AR roots,
stable/unstable complex rotation roots, unobserved unstable modes, exact unit
roots, nonnormal transitions, zero scale, malformed inputs, and overflow.

The standalone harness also cancels at each interruption callback and verifies
that a subsequent call gives the same answer. On a Unix R installation:

```sh
${CXX:-c++} -std=c++17 -Wall -Wextra -Wconversion -pedantic \
  -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc \
  $(R CMD config --cppflags) src/numerics.cpp src/linear_algebra.cpp \
  src/state_space.cpp tools/native-tests/test_state_space.cpp \
  $(R CMD config LAPACK_LIBS) $(R CMD config BLAS_LIBS) \
  $(R CMD config --ldflags) -o /tmp/persephone-state-space
/tmp/persephone-state-space
```

This instruments the independently authored core and adapters; the installed
BLAS/LAPACK binaries are not themselves rebuilt with sanitizers.
