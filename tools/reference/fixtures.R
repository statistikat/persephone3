# No RNG state or external data: the formula is part of the reference contract.
reference_series <- function(frequency, kind = "fixed") {
  n <- if (frequency == 12L) 180L else 80L
  k <- seq_len(n)
  seasonal <- 15 * sin(2 * pi * k / frequency) + 4 * cos(4 * pi * k / frequency)
  noise <- 2 * sin(k * sqrt(2)) + sin(k * sqrt(5))
  y <- 100 + 0.25 * k + seasonal + noise
  if (kind == "automatic") y <- exp(4 + 0.003 * k + seasonal / 100 + noise / 70)
  if (kind == "manual") y[ceiling(n / 2)] <- y[ceiling(n / 2)] + 35
  ts(y, start = c(2001L, if (frequency == 12L) 3L else 2L), frequency = frequency)
}

reference_cases <- function() {
  expand.grid(method = c("x13", "tramoseats"), frequency = c(12L, 4L),
              kind = c("fixed", "manual", "automatic"), stringsAsFactors = FALSE)
}

reference_spec <- function(method, kind, series) {
  package <- if (method == "x13") "rjd3x13" else "rjd3tramoseats"
  constructor <- getExportedValue(package, paste0(method, "_spec"))
  spec <- constructor(if (kind == "automatic") "rsa3" else "rsa0")
  if (kind == "manual") {
    index <- ceiling(length(series) / 2)
    period <- as.integer(cycle(series)[index])
    year <- floor(time(series)[index] + 1e-8)
    month <- 1L + (period - 1L) * (12L / frequency(series))
    date <- sprintf("%04d-%02d-01", year, month)
    spec <- rjd3toolkit::add_outlier(spec, type = "AO", date = date)
  }
  spec
}

reference_fit <- function(method, series, specification, requested, context = NULL) {
  package <- if (method == "x13") "rjd3x13" else "rjd3tramoseats"
  getExportedValue(package, paste0(method, "_fast"))(
    ts = series, spec = specification, context = context, userdefined = requested)
}

reference_fields <- function(x, path = "") {
  row <- data.frame(path = if (nzchar(path)) path else "/", type = typeof(x),
                    class = paste(class(x), collapse = ";"), length = length(x),
                    dim = paste(dim(x), collapse = ";"), tsp = paste(tsp(x), collapse = ";"),
                    stringsAsFactors = FALSE)
  if (is.list(x) && length(x)) {
    nm <- names(x)
    for (i in seq_along(x)) {
      key <- if (!is.null(nm) && nzchar(nm[[i]])) nm[[i]] else paste0("[", i, "]")
      row <- rbind(row, reference_fields(x[[i]], paste0(path, "/", key)))
    }
  }
  row
}

reference_tolerance <- function(path, type) {
  if (type != "double") return(c(atol = 0, rtol = 0))
  if (grepl("/(arima\\.(p|d|q|bp|bd|bq)|regression\\.(nout|ntd))$|/(neffectiveobs|nparams|nobs|ndf)$", path)) {
    return(c(atol = 0, rtol = 0))
  }
  if (grepl("pvalue$|/(diagnostics|mstats)/|/user_defined/(diagnostics\\.|residuals\\.(doornikhansen|lb)|regression\\.td-ftest|m-statistics\\.)", path)) {
    return(c(atol = 1e-10, rtol = 1e-7))
  }
  if (grepl("/estimation/(parameters|b|bvar|likelihood)", path)) {
    return(c(atol = 1e-9, rtol = 1e-7))
  }
  c(atol = 1e-8, rtol = 1e-7)
}
