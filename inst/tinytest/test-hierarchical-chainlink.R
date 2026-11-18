# Test the indirect adjustment of chain linked series and the 'groups' mode of
# perHts (several aggregates with their own weights and chain linking factors)

# three chain linked sub-series with known unchained levels, so that the
# indirectly adjusted aggregate can be compared with a hand computed reference
set.seed(42)
n_cl <- 12 * 20
tt_cl <- seq_len(n_cl)
ul_cl <- list(
  a = 100 + 0.5 * tt_cl + 5 * sin(2 * pi * tt_cl / 12),
  b = 60 + 0.3 * tt_cl + 3 * sin(2 * pi * tt_cl / 12 + 1),
  c = 40 + 0.2 * tt_cl + 2 * sin(2 * pi * tt_cl / 12 + 2)
)
# the factor of b is re-based in the middle of the sample, as it happens when a
# chain linked index is re-linked
lf_cl <- list(
  a = 0.5 + 0.002 * tt_cl,
  b = ifelse(tt_cl <= 12 * 10, 0.6, 0.42) + 0.001 * tt_cl,
  c = 0.55 + 0.0015 * tt_cl
)
w_cl <- c(0.5, 0.3, 0.2)
ul_agg_cl <- Reduce("+", Map("*", ul_cl, as.list(w_cl)))
lf_agg_cl <- Reduce(
  "+",
  Map(function(u, l, wi) u * l * wi, ul_cl, lf_cl, as.list(w_cl))
) / ul_agg_cl
# the aggregate is re-linked in the middle of the sample as well, so that its
# factor does not follow from the factors of the sub-series
lf_agg_cl <- lf_agg_cl * ifelse(tt_cl <= 12 * 10, 1, 0.7)
series_cl <- c(
  Map(function(u, l) u * l, ul_cl, lf_cl),
  list(agg = ul_agg_cl * lf_agg_cl)
)
# two sub-aggregates with overlapping sub-series, used for the DAG test
w_ab <- c(0.7, 0.3)
w_bc <- c(0.4, 0.6)
ul_ab <- ul_cl$a * w_ab[1] + ul_cl$b * w_ab[2]
ul_bc <- ul_cl$b * w_bc[1] + ul_cl$c * w_bc[2]
lf_ab <- (ul_cl$a * lf_cl$a * w_ab[1] + ul_cl$b * lf_cl$b * w_ab[2]) / ul_ab
lf_bc <- (ul_cl$b * lf_cl$b * w_bc[1] + ul_cl$c * lf_cl$c * w_bc[2]) / ul_bc
lf_cl$ab <- lf_ab
lf_cl$bc <- lf_bc
lf_cl$agg <- lf_agg_cl

mts_cl <- ts(do.call("cbind", series_cl), start = c(2000, 1), frequency = 12)
colnames(mts_cl) <- names(series_cl)
lfm_cl <- ts(cbind(do.call("cbind", lf_cl[c("a", "b", "c")]), agg = lf_agg_cl),
             start = c(2000, 1), frequency = 12)
colnames(lfm_cl) <- names(series_cl)
# the same data with the two sub-aggregates ab and bc added as chain linked
# series (the indirect adjustment needs the aggregate series themselves)
mts_multi <- cbind(
  as.matrix(mts_cl),
  ab = as.numeric(ul_ab * lf_ab),
  bc = as.numeric(ul_bc * lf_bc)
)
mts_multi <- ts(mts_multi, start = c(2000, 1), frequency = 12)
colnames(mts_multi) <- c(colnames(mts_cl), "ab", "bc")
lfm_multi <- ts(
  cbind(
    as.matrix(lfm_cl[, c("a", "b", "c")]),
    ab = as.numeric(lf_ab), bc = as.numeric(lf_bc),
    agg = as.numeric(lfm_cl[, "agg"])
  ),
  start = c(2000, 1), frequency = 12
)
colnames(lfm_multi) <- c("a", "b", "c", "ab", "bc", "agg")

# the indirect aggregate of the adjusted sub-series of an object, computed by
# hand with the given sub-series, weights and aggregate chain factor
hand_indirect <- function(obj, leaves, weights, agg = "agg", ref_year = 2010) {
  ch <- Reduce(
    "+",
    Map(
      function(nm, wi) {
        as.numeric(obj$getComponent(nm)$adjusted) / as.numeric(lf_cl[[nm]]) * wi
      },
      leaves,
      as.list(weights)
    )
  )
  ch <- ch * as.numeric(lf_cl[[agg]])
  i <- (ref_year - 2000) * 12 + 1:12
  ch / mean(ch[i]) * 100
}

message("Test chain linked groups - structure of the groups argument")

# an aggregate that is itself listed as a sub-series is replaced by its own
# sub-series: aggregates are always built from their leaves
groups_struct <- list(
  a_b = c("a", "b"),
  c_d = c("c", "d"),
  a_b_c_d = c("a_b", "c_d"),
  all = c("a_b", "c_d")
)
struct <- persephone3:::cl_groups_structure(
  groups_struct,
  c("a", "b", "c", "d", "a_b", "c_d", "a_b_c_d", "all")
)
expect_identical(struct$leaves$a_b_c_d, c("a", "b", "c", "d"))
expect_identical(struct$leaves$all, c("a", "b", "c", "d"))
expect_identical(struct$leaves$a_b, c("a", "b"))
expect_identical(
  struct$aggregates, c("a_b", "c_d", "a_b_c_d", "all")
)

message("Test chain linked groups - cycles and unknown series are rejected")

expect_error(
  persephone3:::cl_groups_structure(
    list(x = c("y", "z"), y = c("x", "z")), c("x", "y", "z")
  ),
  "cycle"
)
expect_error(
  persephone3:::cl_groups_structure(list(x = c("y", "q")), c("x", "y", "z")),
  "not provided"
)
expect_error(
  persephone3:::cl_groups_structure(list(x = c("y", "y")), c("x", "y")),
  "duplicates"
)

message("Test chain linked groups - unchaining, rechain and re-referencing")

x_cl <- ts(100 + seq_len(36), start = c(2000, 1), frequency = 12)
f_cl <- ts(rep(c(0.5, 0.6, 0.7), 12), start = c(2000, 1), frequency = 12)
expect_equal(
  as.numeric(persephone3:::cl_rechain(x_cl, f_cl)),
  as.numeric(x_cl) * as.numeric(f_cl)
)
expect_equal(
  as.numeric(persephone3:::cl_unchain(persephone3:::cl_rechain(x_cl, f_cl), f_cl)),
  as.numeric(x_cl),
  info = "unchaining reverses chain linking"
)
expect_equal(as.numeric(persephone3:::cl_unchain(x_cl, NULL)), as.numeric(x_cl),
             info = "without a factor a series is not chain linked")

r_cl <- persephone3:::cl_rereference(x_cl, 2001)
expect_equal(
  mean(window(r_cl, start = c(2001, 1), end = c(2001, 12))), 100
)
expect_equal(
  as.numeric(r_cl)[1],
  as.numeric(x_cl)[1] /
    mean(window(x_cl, start = c(2001, 1), end = c(2001, 12))) * 100
)
expect_error(persephone3:::cl_rereference(x_cl, 1999), "outside the span")

message("Test chain linked groups - aligning weights and chain factors")

# a weight series of a lower frequency covers all periods of its own period
annual_cl <- ts(c(0.6, 0.7, 0.8), start = 2000, frequency = 1)
monthly_cl <- ts(rep(1, 36), start = c(2000, 1), frequency = 12)
w_al <- suppressWarnings(persephone3:::cl_align(annual_cl, monthly_cl))
expect_equal(as.numeric(w_al)[1:12], rep(0.6, 12))
expect_equal(as.numeric(w_al)[13:24], rep(0.7, 12))
expect_equal(as.numeric(w_al)[25:36], rep(0.8, 12))
# a series that does not cover the whole span is extended by its ends
short_cl <- ts(c(0.4, 0.5), start = c(2001, 1), frequency = 12)
w_al2 <- suppressWarnings(persephone3:::cl_align(short_cl, monthly_cl))
expect_equal(as.numeric(w_al2)[1:12], rep(0.4, 12))
expect_equal(as.numeric(w_al2)[13:24], c(rep(0.4, 1), rep(0.5, 11)))
expect_equal(as.numeric(w_al2)[25:36], rep(0.5, 12))
# quarterly factors on monthly series
quarterly_cl <- ts(c(0.5, 0.6, 0.7, 0.8), start = c(2000, 1), frequency = 4)
w_al3 <- suppressWarnings(persephone3:::cl_align(quarterly_cl, monthly_cl))
expect_equal(as.numeric(w_al3)[1:3], rep(0.5, 3))
expect_equal(as.numeric(w_al3)[4:6], rep(0.6, 3))

message("Test chain linked groups - the weighted sum is not a weighted mean")

s1 <- ts(rep(100, 12), start = c(2000, 1), frequency = 12)
s2 <- ts(rep(50, 12), start = c(2000, 1), frequency = 12)
expect_equal(
  as.numeric(persephone3:::cl_weighted_sum(list(s1, s2), c(0.6, 0.4)))[1], 80
)
expect_equal(
  as.numeric(persephone3:::cl_weighted_sum(list(s1, s2), c(6, 4)))[1], 800,
  info = "the weights are used as given, they are not normalized"
)
expect_equal(as.numeric(persephone3:::cl_weighted_sum(list(s1, s2)))[1], 150)

message("Test chain linked groups - the five steps of the indirect adjustment")

hts_cl <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa0",
  groups = list(agg = c("a", "b", "c")), weights = list(agg = w_cl),
  cl_weights = lfm_cl, ref_year = 2010, adjustment = "both"
))
expect_identical(hts_cl$getSeries(), c("a", "b", "c", "agg"))
expect_identical(hts_cl$getSeries(aggregates = TRUE), "agg")
expect_identical(hts_cl$groups, list(agg = c("a", "b", "c")))
hts_cl$run()

ind_cl <- hts_cl$getAdjustedIndirect("agg")
ref_unchained <- Reduce(
  "+",
  Map(
    function(nm, wi) {
      as.numeric(hts_cl$getComponent(nm)$adjusted) / as.numeric(lf_cl[[nm]]) * wi
    },
    c("a", "b", "c"),
    as.list(w_cl)
  )
)
expect_equal(as.numeric(hts_cl$getUnchainedIndirect("agg")), ref_unchained)
expect_equal(
  as.numeric(ind_cl), hand_indirect(hts_cl, c("a", "b", "c"), w_cl),
  tol = 1e-8
)
expect_equal(
  mean(window(ind_cl, start = c(2010, 1), end = c(2010, 12))), 100,
  info = "the indirect aggregate is re-referenced to ref_year"
)
expect_inherits(hts_cl$getAdjustedDirect("agg"), "ts")
expect_identical(names(hts_cl$adjustedIndirect), "agg")
expect_equal(as.numeric(hts_cl$adjustedIndirect$agg), as.numeric(ind_cl))
expect_identical(names(hts_cl$adjusted), "agg")
expect_inherits(hts_cl$adjusted$agg, "ts")
expect_equal(tsp(ind_cl), tsp(hts_cl$getComponent("a")$adjusted))

message("Test chain linked groups - the chain handling changes the result")

naive_cl <- Reduce(
  "+",
  Map(function(nm, wi) as.numeric(hts_cl$getComponent(nm)$adjusted) * wi,
      c("a", "b", "c"), as.list(w_cl))
)
naive_cl <- naive_cl / mean(naive_cl[(2010 - 2000) * 12 + 1:12]) * 100
expect_true(
  mean(abs(as.numeric(ind_cl) - naive_cl)) > 0.5,
  info = "summing up the chain linked adjusted series does not give the aggregate"
)

message("Test chain linked groups - several aggregates with overlapping sub-series")

groups_multi <- list(ab = c("a", "b"), bc = c("b", "c"), agg = c("a", "b", "c"))
hts_multi <- suppressWarnings(perHts(
  list = mts_multi, method = "x13", template = "rsa0",
  groups = groups_multi,
  weights = list(ab = w_ab, bc = w_bc, agg = w_cl),
  cl_weights = lfm_multi, ref_year = 2010, adjustment = "indirect"
))
hts_multi$run()
expect_identical(hts_multi$getSeries(aggregates = TRUE), c("ab", "bc", "agg"))
expect_null(
  hts_multi$getComponent("agg")$output,
  info = "with adjustment = 'indirect' the aggregates are not adjusted themselves"
)
expect_false(is.null(hts_multi$getComponent("a")$output))
expect_true(all(vapply(hts_multi$adjustedDirect, is.null, TRUE)))
expect_identical(names(hts_multi$adjusted), c("ab", "bc", "agg"))
expect_identical(names(hts_multi$adjustedIndirect), c("ab", "bc", "agg"))
expect_equal(
  as.numeric(hts_multi$getAdjustedIndirect("ab")),
  hand_indirect(hts_multi, c("a", "b"), w_ab, "ab"), tol = 1e-8
)
expect_equal(
  as.numeric(hts_multi$getAdjustedIndirect("bc")),
  hand_indirect(hts_multi, c("b", "c"), w_bc, "bc"), tol = 1e-8
)
expect_equal(
  as.numeric(hts_multi$getAdjustedIndirect("agg")),
  hand_indirect(hts_multi, c("a", "b", "c"), w_cl), tol = 1e-8
)

message("Test chain linked groups - printing and diagnostics")

out <- capture.output(print(hts_cl))
expect_true(length(out) > 0)
expect_true(all(c("a", "b", "c", "agg") %in% diagnosticsTable(hts_cl)$component))
expect_equal(nrow(diagnosticsTable(hts_cl)), 4L)
expect_equal(
  nrow(diagnosticsTable(hts_multi)), 6L,
  info = "every series is listed exactly once"
)

message("Test chain linked groups - plots")

expect_error(plot(hts_multi), "'series' must be given")
expect_error(plot(hts_multi, series = "nope"), "not an aggregate")
expect_true(inherits(plot(hts_multi, series = "ab"), "htmlwidget"))
expect_true(inherits(
  plot(hts_multi, series = "ab", plotEngine = "plotly"), "htmlwidget"
))
expect_true(inherits(plot(hts_cl), "htmlwidget"))

message("Test chain linked groups - input checks")

expect_error(
  perHts(list = mts_cl, groups = list(agg = c("a", "nope"))), "not provided"
)
expect_error(
  perHts(list = mts_cl, groups = list(agg = c("a"))), "at least two"
)
expect_error(
  perHts(list = mts_cl, groups = list(agg = c("a", "b")), weights = list(agg = 1:3)),
  "3 columns/values"
)
expect_error(
  perHts(list = mts_cl, groups = list(agg = c("a", "b")), ref_year = "x"),
  "single year"
)
expect_error(
  perHts(list = mts_cl, groups = list(agg = c("a", "b")),
         cl_weights = lfm_cl, ref_year = 1990),
  "outside the span"
)
expect_error(
  perHts(hts = hts_cl, other = perX13(AirPassengers, "rsa0"),
         groups = list(x = c("hts", "other"))),
  "single series"
)
expect_error(
  perHts(list = mts_cl, method = "x13", groups = list(agg = c("a", "b")),
         adjustment = "nope"),
  "arg"
)
expect_error(
  {
    hx <- suppressWarnings(perHts(
      list = mts_cl, method = "x13", template = "rsa0",
      groups = list(agg = c("a", "b"))
    ))
    hx$getAdjustedIndirect("agg")
  },
  "run"
)
hx1 <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa0",
  groups = list(agg = c("a", "b"))
))
hx1$run()
expect_error(hx1$getAdjustedIndirect("nope"), "not an aggregate")
expect_error(hx1$getComponent("nope"), "not found")
expect_error(hx1$getUnchainedIndirect(), "'agg' is needed")
expect_error(
  suppressWarnings(hx1$getAdjustedDirect("nope")), "not an aggregate"
)

message("Test chain linked groups - a series that belongs to no aggregate warns")

warned <- NULL
collect <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  invisible(NULL)
}
collect(perHts(
  list = mts_cl, method = "x13", template = "rsa0",
  groups = list(agg = c("a", "b"))
))
expect_true(any(grepl("neither sub-series nor aggregates", warned)))
warned <- NULL
collect(perHts(
  list = mts_cl, method = "x13", template = "rsa0",
  groups = list(agg = c("a", "b")), weights = list(agg = c(0.5, 0.2))
))
expect_true(any(grepl("do not sum to 1", warned)))

message("Test chain linked groups - time varying and annual weights")

tv_w <- cbind(
  a = seq(0.4, 0.6, length.out = n_cl),
  b = seq(0.3, 0.2, length.out = n_cl)
)
tv_w <- cbind(tv_w, c = 1 - rowSums(tv_w))
tv_w_ts <- ts(tv_w, start = c(2000, 1), frequency = 12)
hts_tv <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa0",
  groups = list(agg = c("a", "b", "c")), weights = list(agg = tv_w_ts)
))
hts_tv$run()
hand_tv <- as.numeric(hts_tv$getComponent("a")$adjusted) * tv_w[, 1] +
  as.numeric(hts_tv$getComponent("b")$adjusted) * tv_w[, 2] +
  as.numeric(hts_tv$getComponent("c")$adjusted) * tv_w[, 3]
expect_equal(as.numeric(hts_tv$getAdjustedIndirect("agg")), hand_tv, tol = 1e-8)

annual_w <- ts(cbind(a = rep(0.5, 20), b = rep(0.3, 20), c = rep(0.2, 20)),
               start = 2000, frequency = 1)
hts_ann <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa0",
  groups = list(agg = c("a", "b", "c")), weights = list(agg = annual_w)
))
hts_ann$run()
expect_equal(
  as.numeric(hts_ann$getAdjustedIndirect("agg")),
  as.numeric(hts_ann$getComponent("a")$adjusted) * 0.5 +
    as.numeric(hts_ann$getComponent("b")$adjusted) * 0.3 +
    as.numeric(hts_ann$getComponent("c")$adjusted) * 0.2,
  tol = 1e-8,
  info = "annual weights are repeated over the months of their year"
)

message("Test chain linked groups - classic mode is unchanged")

objX13 <- perX13(AirPassengers, "rsa3")
ht <- suppressWarnings(perHts(a = objX13, b = objX13, method = "x13"))
expect_true(is.null(ht$adjustedDirect))
expect_true(is.null(ht$adjustedIndirect))
expect_true(is.null(ht$groups))
expect_equal(ht$indirect, NA)
ht$run()
expect_inherits(ht$adjustedDirect, "ts")
expect_inherits(ht$adjustedIndirect, "ts")
expect_equal(
  as.numeric(ht$adjustedIndirect),
  as.numeric(ht$components$a$adjusted) + as.numeric(ht$components$b$adjusted),
  tol = 1e-6
)
expect_warning(ht$adjusted, "not recorded yet")
expect_error(ht$getAdjustedIndirect("a"), "groups")

ht_dir <- suppressWarnings(perHts(
  a = objX13, b = objX13, method = "x13", adjustment = "direct"
))
expect_false(is.na(ht_dir$indirect))
ht_dir$run()
expect_equal(as.numeric(ht_dir$adjusted), as.numeric(ht_dir$adjustedDirect))

ht_ind <- suppressWarnings(perHts(
  a = objX13, b = objX13, method = "x13", adjustment = "indirect"
))
expect_true(ht_ind$indirect)
ht_ind$run()
expect_true(is.null(ht_ind$adjustedDirect))
expect_equal(
  as.numeric(ht_ind$adjusted), as.numeric(ht_ind$adjustedIndirect)
)

message("Test chain linked groups - weights mts in classic mode")

w_mts <- ts(matrix(c(0.5, 0.5), ncol = 2, nrow = 121), start = c(1949, 1),
            frequency = 12)
ht_w <- suppressWarnings(perHts(
  a = AirPassengers, b = AirPassengers * 2, method = "x13", template = "rsa0",
  weights = w_mts
))
expect_equal(ncol(ht_w$weights), 2L)
expect_identical(colnames(ht_w$weights), c("a", "b"))
ht_w$run()
expect_equal(
  as.numeric(ht_w$adjustedIndirect),
  (as.numeric(ht_w$components$a$adjusted) * 0.5 +
     as.numeric(ht_w$components$b$adjusted) * 0.5),
  tol = 1e-6
)

message("Test chain linked groups - trading day regressors per series")

td_mts <- genTd(
  freq = 12, firstYear = 2000, lastYear = 2020,
  hd = list("01-01", "01-06", "05-01", "easter+1", "easter+39", "easter+50",
            "easter+60", "08-15", "10-26", "11-01", "12-08", "12-24", "12-25",
            "12-26", "12-31"),
  weight = c(rep(1, 11), 0.5, rep(1, 2), 0.5)
)[[3]][, 1:6, drop = FALSE]
# the trading day regressors of one series; test = "None" keeps them in the
# model, otherwise the automatic model selection can drop them
td_a <- td_context(
  ts = window(mts_cl[, "a"]), td = td_mts, method = "x13", test = "None"
)
td_b <- td_context(
  ts = window(mts_cl[, "b"]), td = td_mts, method = "x13", test = "None"
)
# a series is trading day adjusted if a regressor of the type TD is estimated
has_td <- function(node) {
  vars <- node$output$preprocessing$description$variables
  "TD" %in% unlist(lapply(vars, function(v) as.character(v$type)))
}

hts_td <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa3",
  groups = list(agg = c("a", "b", "c")), weights = list(agg = w_cl),
  td = list(a = td_a)
))
expect_true("REGS_TD" %in% names(hts_td$getComponent("a")$context$variables))
expect_false("REGS_TD" %in%
               names(hts_td$getComponent("b")$context$variables %||% list()))
hts_td$run()
expect_true(has_td(hts_td$getComponent("a")),
            info = "the named series gets the trading day regressors")
expect_false(has_td(hts_td$getComponent("b")))
expect_false(has_td(hts_td$getComponent("c")))
expect_false(is.null(hts_td$getAdjustedIndirect("agg")))

# a single regressor set is used for all series
hts_td2 <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa3",
  groups = list(agg = c("a", "b")), td = td_a
))
expect_true(all(vapply(c("a", "b", "agg"), function(nm)
  "REGS_TD" %in% names(hts_td2$getComponent(nm)$context$variables), TRUE)))

# updateParams(td = ) for a single component
hts_td3 <- suppressWarnings(perHts(
  list = mts_cl, method = "x13", template = "rsa3",
  groups = list(agg = c("a", "b", "c"))
))
hts_td3$updateParams(component = "b", td = td_b)
hts_td3$run()
expect_true(has_td(hts_td3$getComponent("b")))
expect_false(has_td(hts_td3$getComponent("a")))
expect_false(has_td(hts_td3$getComponent("c")))

expect_error(
  perHts(list = mts_cl, method = "x13", template = "rsa3",
         groups = list(agg = c("a", "b")), td = list(nope = td_a)),
  "must be the names of the series"
)
