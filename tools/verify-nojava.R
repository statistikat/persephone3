# Verify the installed package without access to Java-related R packages.
# Usage: Rscript tools/verify-nojava.R [--library=/isolated/library] [--native]
# --library isolates this R process only; it never edits the user's libraries.
args <- commandArgs(trailingOnly = TRUE)
lib_arg <- args[startsWith(args, "--library=")]
if (length(lib_arg)) {
  stopifnot(length(lib_arg) == 1L)
  lib <- normalizePath(sub("^--library=", "", lib_arg), mustWork = TRUE)
  for (name in c(".Library", ".Library.site")) {
    locked <- bindingIsLocked(name, baseenv())
    if (locked) unlockBinding(name, baseenv())
    assign(name, if (name == ".Library") lib else character(), baseenv())
    if (locked) lockBinding(name, baseenv())
  }
  .libPaths(lib)
}
forbidden <- c("rJava", "rjd3toolkit", "rjd3x13", "rjd3tramoseats", "rjd3jars", "RProtoBuf")
stopifnot(!any(forbidden %in% loadedNamespaces()))
stopifnot(!any(vapply(forbidden, function(p) nzchar(system.file(package=p)), logical(1))))
library(persephone3)
stopifnot(!any(forbidden %in% loadedNamespaces()))
cap <- backend_capabilities("native")
stopifnot(identical(cap$fit, FALSE), identical(cap$status, "unimplemented"))
options(persephone.backend = "native")
err <- tryCatch(perX13(AirPassengers), error = identity)
stopifnot(inherits(err, "error"), grepl("native backend does not yet implement", conditionMessage(err)))
err <- tryCatch(perTramo(AirPassengers, backend = "rjd3"), error = identity)
stopifnot(inherits(err, "error"), grepl("optional package", conditionMessage(err)))
if ("--native" %in% args) {
  convolve <- getFromNamespace(".native_convolve", "persephone3")
  difference <- getFromNamespace(".native_difference_polynomial", "persephone3")
  stopifnot(identical(convolve(c(1, -1), c(1, 1)), c(1, 0, -1)))
  stopifnot(identical(difference(1, 0, 12), c(1, -1)))
  solve_spd <- getFromNamespace(".native_cholesky_solve", "persephone3")
  solve_qr <- getFromNamespace(".native_qr_solve", "persephone3")
  stopifnot(isTRUE(all.equal(solve_spd(diag(2), matrix(c(2, 3), 2))$solution,
                             matrix(c(2, 3), 2))))
  stopifnot(solve_qr(diag(2), matrix(c(2, 3), 2))$rank == 2L)
  prepare <- getFromNamespace("native_prepare_inputs", "persephone3")
  prepared <- prepare(ts(c(1, exp(1)), start = c(2000, 1), frequency = 12),
                      transformation = "log")
  stopifnot(isTRUE(all.equal(as.numeric(prepared$transformed), c(0, 1))))
  likelihood <- getFromNamespace(".native_arma_likelihood", "persephone3")
  white <- likelihood(c(1, NA_real_, 2), numeric(), numeric(), numeric(), numeric(), 12)
  stopifnot(white$observed_count == 2,
            isTRUE(all.equal(white$fixed_log_likelihood, sum(dnorm(c(1, 2), log = TRUE)))))
  sarima <- getFromNamespace(".native_sarima_likelihood", "persephone3")
  walk <- sarima(c(2, 3, NA_real_, 5), numeric(), numeric(), numeric(), numeric(), 12, d = 1)
  stopifnot(walk$diffuse_count == 1, walk$finite_count == 2,
            walk$remaining_diffuse_rank == 0,
            isTRUE(all.equal(walk$fixed_log_likelihood,
              dnorm(1, log = TRUE) + dnorm(2, sd = sqrt(2), log = TRUE))))
  fixed_likelihood <- getFromNamespace("native_fixed_likelihood", "persephone3")
  windowed <- fixed_likelihood(ts(exp(c(0, 1, 2, 3)), start = c(2000, 1),
                                 frequency = 12), d = 1, transformation = "log",
                              estimation_start = c(2000, 2))
  stopifnot("fixed_likelihood" %in% cap$primitives,
            identical(windowed$initialization, "fresh_prior_at_span_start"),
            identical(windowed$adjustment$policy, "jdplus_3.9.0_calendar_window"),
            isTRUE(all.equal(windowed$likelihood$adjusted_fixed,
                             2 * dnorm(1, log = TRUE) - 5)))
  regress <- getFromNamespace("native_regression_likelihood", "persephone3")
  x <- ts(seq_len(20), start = c(2000, 1), frequency = 4)
  regression <- regress(2 + 3 * x, list(x = x), include_mean = TRUE)
  stopifnot("regression_likelihood" %in% cap$primitives,
            regression$regression$rank == 2L,
            isTRUE(all.equal(unname(regression$regression$coefficients), c(3, 2))),
            regression$likelihood$finite_count == 20L)
  objective_setup <- getFromNamespace("native_arma_objective_setup", "persephone3")
  objective_value <- getFromNamespace("native_arma_objective", "persephone3")
  objective <- objective_value(objective_setup(sin(x), ar = NA_real_))
  stopifnot("arma_objective" %in% cap$primitives, objective$available,
            is.finite(objective$value), objective$likelihood$nparams_profiled == 2)
  optimize_arma <- getFromNamespace("native_arma_optimize", "persephone3")
  fitted <- optimize_arma(objective_setup(sin(x), ar = NA_real_))
  stopifnot("arma_optimization" %in% cap$primitives, fitted$converged,
            fitted$final$available, is.finite(fitted$final$value))
  stopifnot("arma_identification" %in% cap$primitives,
            fitted$parameter_diagnostics$status == "full_rank_transfer",
            fitted$parameter_diagnostics$statistical_identification == "not_assessed")
  information <- getFromNamespace("native_arma_information", "persephone3")
  uncertainty <- information(objective_setup(sin(x), ar = NA_real_), fitted$coordinates)
  stopifnot("arma_information" %in% cap$primitives, uncertainty$available,
            is.matrix(uncertainty$covariance), all(is.finite(uncertainty$standard_errors)))
  residual <- information(objective_setup(sin(x), ar = NA_real_), fitted$coordinates, method="tramo_residual")
  b <- residual$residual_center
  state <- list(final_coefficients=unname(residual$coefficients),working_coefficients=unname(residual$coefficients),
    difference_steps=-1e-6,damping=.01,normalized_sum_squares=b$sum_squares*exp(b$log_determinant/b$effective_count),
    effective_count=b$effective_count)
  legacy <- information(objective_setup(sin(x), ar = NA_real_), fitted$coordinates,
    method="tramo_residual",covariance="legacy",legacy_state=state)
  stopifnot("arma_residual_information" %in% cap$primitives,"arma_legacy_covariance" %in% cap$primitives,
    residual$available,legacy$available,legacy$comparison_only,identical(legacy$accurate_covariance,residual$covariance))
  regression_information <- getFromNamespace("native_regression_information","persephone3")
  regression_uncertainty <- regression_information(objective_setup(sin(x),include_mean=TRUE))
  stopifnot("regression_information" %in% cap$primitives,regression_uncertainty$available,
    regression_uncertainty$free_regression==1L,!regression_uncertainty$propagates_ARMA_uncertainty,
    all(is.finite(regression_uncertainty$standard_errors)))
  joint_context <- objective_setup(sin(x),ar=NA_real_,include_mean=TRUE)
  joint_fit <- optimize_arma(joint_context)
  joint_uncertainty <- regression_information(joint_context,joint_fit$coordinates,method="joint_profile_ml")
  stopifnot("joint_regression_information" %in% cap$primitives,joint_uncertainty$available,
    joint_uncertainty$propagates_ARMA_uncertainty,identical(dim(joint_uncertainty$joint_covariance),c(2L,2L)))
  fitted_residuals <- getFromNamespace("native_arma_residuals","persephone3")(
    objective_setup(ts(c(1,NA_real_,3,4),frequency=4),ar=.5))
  stopifnot("arma_residuals" %in% cap$primitives,fitted_residuals$available,fitted_residuals$complete$available,
    is.na(fitted_residuals$observed$standardized_innovations[2]),
    abs(fitted_residuals$interpolation$points_original-1.6)<1e-12)
  legacy_residuals <- getFromNamespace("native_arma_residuals","persephone3")(
    objective_setup(ts(c(7,21,3,13,9,NA,8,16,11,27,6,18,14,31,12,19),frequency=4),seasonal_d=1L),reporting="legacy")
  stopifnot("arma_legacy_residuals" %in% cap$primitives,legacy_residuals$available,legacy_residuals$comparison_only,
    abs(legacy_residuals$interpolation$points_original+7)<1e-12,
    abs(legacy_residuals$accurate_interpolation$points_original-24)<1e-12,
    abs(legacy_residuals$reported$unit_residuals[1]-3-sqrt(2))<1e-12,
    length(legacy_residuals$reported$source_rows)==0L)
}
stopifnot(!any(forbidden %in% loadedNamespaces()))
cat("PASS: installed package loads, capabilities are honest and no Java fallback is attempted",
    if ("--native" %in% args) "; compiled primitives run" else "", "\n", sep="")
