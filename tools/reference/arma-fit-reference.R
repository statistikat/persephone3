# Public pinned wrapper preparation shared by fit and information comparisons.
prepare_reference <- function(case, method) {
  spec <- if (method == "x13") rjd3x13::regarima_spec("rg0") else rjd3tramoseats::tramo_spec("tr0")
  # JD+ orders factors AR, seasonal AR, MA, seasonal MA; AR signs are negated.
  order <- c("ar", "seasonal_ar", "ma", "seasonal_ma")
  coefficient <- unlist(case$specification[order], use.names = FALSE)
  sign <- rep(c(-1, -1, 1, 1), lengths(case$specification[order]))
  free <- is.na(coefficient); coefficient[free] <- 0
  spec <- rjd3toolkit::set_arima(spec, mean = 0, mean.type = "Undefined",
    p = length(case$specification$ar), d = case$d, q = length(case$specification$ma),
    bp = length(case$specification$seasonal_ar), bd = case$D, bq = length(case$specification$seasonal_ma),
    coef = coefficient * sign, coef.type = ifelse(free, "Initial", "Fixed"))
  spec <- rjd3toolkit::set_automodel(spec, enabled = FALSE)
  spec <- rjd3toolkit::set_transform(spec, fun = if (case$arguments$transformation == "log") "Log" else "None")
  spec <- rjd3toolkit::set_estimate(spec, tol = 1e-12, exact.ml = TRUE)
  spec <- rjd3toolkit::add_usrdefvar(spec, name = "x", regeffect = "Trend")
  spec <- rjd3toolkit::add_usrdefvar(spec, name = "fixed", coef = .2, regeffect = "Trend")
  list(spec = spec, context = rjd3toolkit::modelling_context(variables = list(r = case$arguments$regressors)),
    series = window(case$arguments$series, start = case$arguments$estimation_start,
      end = case$arguments$estimation_end), free_parameters = sum(free))
}
reference_executor <- function(method) {
  force(method)
  function(input) if (method == "x13") rjd3x13::regarima_fast(input$series, input$spec, context = input$context) else
    rjd3tramoseats::tramo_fast(input$series, input$spec, context = input$context)
}
