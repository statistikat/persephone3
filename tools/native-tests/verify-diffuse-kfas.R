#!/usr/bin/env Rscript
# Optional public-API oracle. KFAS is never a package dependency or installed by
# this script. Use PERSEPHONE_KFAS_LIB for a separate oracle library; --source
# loads an already-compiled working tree with pkgload (does not rebuild it).
oracle_library <- Sys.getenv("PERSEPHONE_KFAS_LIB", "/tmp/persephone-diffuse-oracle-lib")
if (dir.exists(oracle_library)) .libPaths(c(oracle_library, .libPaths()))
if (!requireNamespace("KFAS", quietly = TRUE)) {
  stop("Install optional KFAS into a separate library and set PERSEPHONE_KFAS_LIB.")
}
if ("--source" %in% commandArgs(trailingOnly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE, compile = FALSE)
} else {
  library(persephone3)
}
suppressPackageStartupMessages(library(KFAS))
native <- getFromNamespace(".native_diffuse_likelihood", "persephone3")

model_kfas <- function(values, settings, variance = 1) {
  y <- values - settings$mean
  Z <- matrix(settings$observation, 1)
  T <- settings$transition
  R <- settings$noise_loading
  Q <- diag(ncol(R)) * variance
  a1 <- settings$initial_state
  P1 <- tcrossprod(settings$finite_loading) * variance
  P1inf <- tcrossprod(settings$diffuse_loading)
  H <- settings$observation_variance * variance
  model <- SSModel(y ~ -1 + SSMcustom(Z = Z, T = T, R = R, Q = Q,
    a1 = a1, P1 = P1, P1inf = P1inf), H = H)
  # SSMcustom clears diffuse flags for nonzero finite diagonal entries. Do not
  # accidentally compare a proper model: these fixtures deliberately have
  # disjoint finite and diffuse initial coordinates and diagonal P1inf.
  stopifnot(isTRUE(all.equal(unname(model$P1inf), unname(P1inf))))
  model
}

fixtures <- list(
  random_walk = list(transition = matrix(1), observation = 1,
    noise_loading = matrix(.7), initial_state = .2,
    finite_loading = matrix(0), diffuse_loading = matrix(1),
    observation_variance = .2, mean = .3),
  second_order = list(transition = matrix(c(2, 1, -1, 0), 2), observation = c(1, 0),
    noise_loading = matrix(c(1, 0), 2), initial_state = c(.2, -.1),
    finite_loading = matrix(0, 2, 1), diffuse_loading = diag(2),
    observation_variance = .3, mean = -.2),
  mixed = list(transition = diag(c(1, .6)), observation = c(1, .4),
    noise_loading = matrix(c(.4, .2), 2), initial_state = c(0, .3),
    finite_loading = diag(c(0, .4)), diffuse_loading = matrix(c(1, 0), 2),
    observation_variance = .25, mean = .1)
)
for (period in c(4, 12)) {
  transition <- matrix(0, period, period)
  transition[1, period] <- 1
  transition[cbind(2:period, 1:(period - 1))] <- 1
  loading <- matrix(c(1, rep(0, period - 1)), period)
  fixtures[[paste0("seasonal_", period)]] <- list(transition = transition,
    observation = drop(loading), noise_loading = loading, initial_state = rep(0, period),
    finite_loading = matrix(0, period, 1), diffuse_loading = diag(period),
    observation_variance = .1, mean = .2)
}

checks <- 0L
maximum_likelihood_error <- 0
close <- function(actual, expected, label, tolerance = 2e-8) {
  checks <<- checks + 1L
  result <- all.equal(unname(actual), unname(expected), tolerance = tolerance)
  if (!isTRUE(result)) stop(label, ": ", paste(result, collapse = "; "))
}
for (name in names(fixtures)) {
  settings <- fixtures[[name]]
  n <- 48L
  complete <- sin(seq_len(n) / 3) + seq_len(n) / 30
  for (gaps in list(integer(), c(1, 2, 17:19, 31, 47, 48))) {
    values <- complete
    values[gaps] <- NA_real_
    label <- paste(name, if (length(gaps)) "gaps" else "complete")
    native_one <- do.call(native, c(list(values = values), settings))
    one <- model_kfas(values, settings, 1)
    two <- model_kfas(values, settings, 2)
    ll_one <- as.numeric(logLik(one, marginal = FALSE))
    ll_two <- as.numeric(logLik(two, marginal = FALSE))
    degrees <- sum(!is.na(values)) - ncol(settings$diffuse_loading)
    # Infer SSE from two independently evaluated fixed-scale likelihoods.
    # l(2)-l(1) = -.5*n_eff*log(2) + .25*SSE.
    expected_scale <- (4 * (ll_two - ll_one) + 2 * degrees * log(2)) / degrees
    expected_profile <- as.numeric(logLik(model_kfas(values, settings, expected_scale), marginal = FALSE))
    close(native_one$fixed_log_likelihood, ll_one, paste(label, "unit likelihood"))
    close(native_one$profiled_variance, expected_scale, paste(label, "profile scale"))
    close(native_one$profiled_log_likelihood, expected_profile, paste(label, "profile likelihood"))
    close(native_one$finite_count, degrees, paste(label, "finite count"))
    native_scaled <- do.call(native, c(list(values = values, variance = 1.7), settings))
    ll_scaled <- as.numeric(logLik(model_kfas(values, settings, 1.7), marginal = FALSE))
    close(native_scaled$fixed_log_likelihood, ll_scaled, paste(label, "scaled likelihood"))
    maximum_likelihood_error <- max(maximum_likelihood_error,
      abs(native_one$fixed_log_likelihood - ll_one),
      abs(native_one$profiled_log_likelihood - expected_profile),
      abs(native_scaled$fixed_log_likelihood - ll_scaled))

    filtered <- KFS(one, filtering = "state", smoothing = "none")
    prediction <- drop(filtered$a[seq_len(n), , drop = FALSE] %*% settings$observation) + settings$mean
    finite <- which(native_one$update_kind == 2L)
    observed <- which(!is.na(values))
    close(native_one$predictions, prediction, paste(label, "predictions"))
    close(native_one$innovations[observed], as.numeric(filtered$v)[observed], paste(label, "innovations"))
    close(native_one$finite_variances[finite], as.numeric(filtered$F)[finite], paste(label, "finite variances"))
    close(native_one$standardized_innovations[finite],
          as.numeric(filtered$v)[finite] / sqrt(as.numeric(filtered$F)[finite]),
          paste(label, "standardized innovations"))
    close(native_one$final_state, as.numeric(filtered$att[n, ]), paste(label, "final state"))
    close(native_one$last_diffuse_index, filtered$d, paste(label, "last diffuse index"))
  }
}
cat(sprintf("KFAS %s; %d checks; maximum absolute likelihood error %.6g\n",
            as.character(packageVersion("KFAS")), checks, maximum_likelihood_error))
