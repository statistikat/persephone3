# Independent intrinsic-Gaussian endpoint conditioning. The shared dense oracle
# has an explicit endpoint-retention option; production filtering is not used.
forecast_oracle <- function(context,blocks,backcasts,forecasts) {
  expanded <- context;n <- length(context$prepared$inputs$transformed)
  data <- context$prepared
  rows <- data$inputs$backcast_horizon-backcasts+seq_len(backcasts+n+forecasts)
  expanded$prepared$inputs$transformed <- c(rep(NA_real_,backcasts),
    as.numeric(data$inputs$transformed),rep(NA_real_,forecasts))
  expanded$prepared$design <- data$inputs$regressors[rows,,drop=FALSE]
  full <- interpolation_oracle(expanded,blocks,include_endpoints=TRUE)
  requested <- c(seq_len(backcasts),backcasts+n+seq_len(forecasts))
  selected <- match(requested,full$rows)
  list(rows=requested,points=full$points[selected],known=full$known[selected,selected,drop=FALSE],
    regression=full$regression[selected,selected,drop=FALSE],covariance=full$covariance[selected,selected,drop=FALSE],
    Q=full$Q,n=full$n)
}
forecast_cases <- function() {
  cases <- arma_fit_cases()
  for(i in seq_along(cases)) {
    ca <- cases[[i]];b <- ca$period
    time <- seq.int(1L-b,156L)
    ca$arguments$regressors <- list(
      x=ts(sin(time/7)+.2*cos(time*sqrt(2)),start=c(2000,1L-b),frequency=b),
      fixed=ts(cos(time/9),start=c(2000,1L-b),frequency=b))
    ca$arguments$backcast_horizon <- b
    cases[[i]] <- ca
  }
  cases
}
