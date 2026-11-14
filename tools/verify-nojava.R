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
  interpolation <- getFromNamespace("native_interpolation_information","persephone3")(
    objective_setup(ts(c(1,NA,3,NA,4,5),frequency=4),include_mean=TRUE))
  stopifnot("interpolation_information" %in% cap$primitives,interpolation$available,
    abs(interpolation$points_original[1]-3.25)<1e-12,
    abs(interpolation$covariance[1,2]-8.75/16)<1e-12,
    !interpolation$propagates_ARMA_uncertainty)
  missing_series <- ts(sin(seq_len(80)*1.7)+.3*cos(seq_len(80)*.6),frequency=4)
  missing_series[31] <- NA_real_
  missing_context <- objective_setup(missing_series,ar=NA_real_)
  missing_fit <- optimize_arma(missing_context)
  propagated <- getFromNamespace("native_interpolation_information","persephone3")(
    missing_context,missing_fit$coordinates,method="delta_profile_ml")
  stopifnot("interpolation_arma_information" %in% cap$primitives,propagated$available,
    propagated$conditional_available,propagated$arma_information_status=="available",
    propagated$covariance[1,1]>=propagated$conditional_covariance[1,1],
    propagated$propagates_ARMA_uncertainty,propagated$sensitivity_status=="available")
  endpoint <- getFromNamespace("native_arma_forecasts","persephone3")(
    objective_setup(ts(c(1,3,2,4,5,2,3,5,4,2,3,6,2,4,5,3),frequency=4),
      d=1L,include_drift=TRUE,backcast_horizon=3L,forecast_horizon=4L))
  stopifnot("forecast_information" %in% cap$primitives,endpoint$available,
    abs(endpoint$points_transformed$backcast[3]-(1-2/15))<1e-12,
    abs(endpoint$points_transformed$forecast[1]-(3+2/15))<1e-12,
    all(endpoint$covariance[1:3,4:7]<0),!endpoint$propagates_ARMA_uncertainty)
  endpoint_context <- objective_setup(missing_series,ar=NA_real_,backcast_horizon=2L,forecast_horizon=3L)
  endpoint_delta <- getFromNamespace("native_arma_forecasts","persephone3")(
    endpoint_context,missing_fit$coordinates,method="delta_profile_ml")
  stopifnot("forecast_arma_information" %in% cap$primitives,endpoint_delta$available,
    endpoint_delta$conditional_available,endpoint_delta$arma_information_status=="available",
    all(diag(endpoint_delta$covariance)>=diag(endpoint_delta$conditional_covariance)),
    endpoint_delta$propagates_ARMA_uncertainty,endpoint_delta$sensitivity_status=="available")
  x11_filter <- getFromNamespace("native_x11_filter","persephone3")
  filtered <- x11_filter(ts(1:20,frequency=4),"trend")
  identity <- x11_filter(ts(1:20,frequency=4),"henderson",3L)
  stopifnot("x11_symmetric_filters" %in% cap$primitives,
    max(abs(filtered$values[3:18]-(3:18)))<1e-12,
    identical(as.numeric(identity$values),as.numeric(1:20)),!cap$fit)
  musgrave <- x11_filter(ts(1:20,frequency=4),"henderson",5L,"musgrave",0)
  stopifnot("x11_musgrave_endpoints" %in% cap$primitives,
    max(abs(musgrave$values-(1:20)))<1e-12,musgrave$endpoint_policy=="musgrave")
  mean_endpoints <- x11_filter(ts(1:5,frequency=4),"trend",endpoints="mean")
  stopifnot("x11_mean_endpoints" %in% cap$primitives,
    max(abs(mean_endpoints$values-c(27/16,35/16,3,61/16,69/16)))<1e-12)
  seasonal_endpoints <- x11_filter(ts(1:60,frequency=12),"s3x3",endpoints="seasonal")
  stopifnot("x11_seasonal_endpoints" %in% cap$primitives,
    abs(seasonal_endpoints$values[1]-sum(c(1,13,25)*c(11,11,5)/27))<1e-12,
    identical(seasonal_endpoints$available_rows,1:60),seasonal_endpoints$minimum_phase_length==5L)
  seasonal15 <- x11_filter(ts(1:240,frequency=12),"s3x15",endpoints="seasonal")
  stopifnot(abs(seasonal15$values[1]-sum(seq(1,97,12)*c(rep(.16,5),.06667,.06667,.04444,.02222)))<1e-12,
    seasonal15$minimum_phase_length==20L,seasonal15$coefficient_convention=="x11_decimal_5")
  seasonal_short <- getFromNamespace("native_x11_seasonal_process","persephone3")(
    ts(1:20,frequency=4),c("s3x5","stable","s3x9","s3x1"))
  stopifnot("x11_seasonal_processing" %in% cap$primitives,
    seasonal_short$row_methods$method[9]=="short_phase_mean",abs(seasonal_short$values[9]-9)<1e-12,
    identical(unname(seasonal_short$selected_filters),c("s3x5","stable","stable","s3x1")))
  trend_choice <- getFromNamespace("native_x11_trend_select","persephone3")(
    ts(10+(-1)^seq_len(121L),frequency=4))
  stopifnot("x11_trend_selection" %in% cap$primitives,trend_choice$status=="available",
    abs(trend_choice$comparison_ic_ratio-504/25)<1e-10,
    trend_choice$selected_length==7L,trend_choice$endpoint_ic_ratio==4.5)
  seasonal_plan <- getFromNamespace("native_x11_seasonal_plan","persephone3")
  pending_msr <- seasonal_plan(4,"msr","d_final")
  mixed_plan <- seasonal_plan(4,c("msr","x11default","s3x3","stable"),"d_final")
  stopifnot("x11_seasonal_stage_policy" %in% cap$primitives,
    pending_msr$requires_msr,all(is.na(pending_msr$resolved_filters)),
    !mixed_plan$global_msr,identical(unname(mixed_plan$resolved_filters),c("s3x5","s3x5","s3x3","stable")))
  msr <- getFromNamespace("native_x11_msr_table","persephone3")(
    ts(1:20,frequency=4),ts(2*(1:20),frequency=4))
  stopifnot("x11_msr_tables" %in% cap$primitives,msr$status=="available",
    abs(msr$global_msr-2*1.01779/1.55291)<1e-12,all(msr$phases$changes==4L))
  msr_select <- getFromNamespace("native_x11_msr_select","persephone3")
  year <- seq_len(20L)
  msr_input <- ts(rep(year+2.6133797380008934*sin(1.37*year),each=4L),frequency=4)
  jd_choice <- msr_select(msr_input)
  capped_choice <- msr_select(msr_input,iteration_policy="x11_five_year_cap")
  resolved <- getFromNamespace("native_x11_msr_resolve","persephone3")(pending_msr,jd_choice)
  stopifnot(all(c("x11_msr_pilot","x11_msr_selection") %in% cap$primitives),
    jd_choice$iterations==16L,jd_choice$selected_filter=="s3x9",!jd_choice$using_default,
    capped_choice$iterations==6L,capped_choice$reason=="five_year_cap",capped_choice$using_default,
    !resolved$requires_msr,all(resolved$resolved_filters=="s3x9"))
  normalized <- getFromNamespace("native_x11_seasonal_normalize","persephone3")(
    ts(1:24,frequency=4),"s3x3",2L)
  stopifnot("x11_additive_normalization" %in% cap$primitives,
    max(abs(as.numeric(normalized$values)-c(0,0,-2,-1,rep(0,20),1,2,0,0)))<1e-12,
    all(normalized$endpoint_policies=="nearest_central_mean"),identical(normalized$original_rows,3:26))
  original_moments <- getFromNamespace("native_prediction_moments","persephone3")(
    c(-1000,-1000),matrix(c(1000,500,500,1000),2),"log")
  stopifnot("prediction_moments" %in% cap$primitives,original_moments$available,
    original_moments$median_status=="original_median_range",
    max(abs(diag(original_moments$covariance)-1))<1e-12)
}
stopifnot(!any(forbidden %in% loadedNamespaces()))
cat("PASS: installed package loads, capabilities are honest and no Java fallback is attempted",
    if ("--native" %in% args) "; compiled primitives run" else "", "\n", sep="")
