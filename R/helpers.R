#' @importFrom highcharter hchart hc_title
#' @export
highcharter::hchart

#' @export
highcharter::hc_title

#' @importFrom highcharter %>%
#' @export
highcharter::`%>%`

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Null-Coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

rescale <- function(x, min, max) {
  stopifnot(
    is.numeric(x),
    is.numeric(min),
    is.numeric(max),
    length(min) == 1,
    length(max) == 1
  )
  rg <- range(x)
  min + (x - rg[1]) / (rg[2] - rg[1]) * (max - min)
}

# interne Template-Mengen je Methode (einzig wahre Quelle)
sa_templates <- function(method = c("x13", "tramoseats")) {
  switch(
    match.arg(method),
    x13 = c("rsa0", "rsa1", "rsa2c", "rsa3", "rsa4", "rsa5c"),
    tramoseats = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
  )
}

# expliziter Exakt-Check (match.arg wuerde partiell matchen: "rsa5" -> "rsa5c")
check_template <- function(template, method) {
  allowed <- sa_templates(method)
  if (length(template) != 1L || !(template %in% allowed)) {
    stop(sprintf(
      "Invalid template '%s' for method '%s'. Allowed values: %s",
      template,
      method,
      paste(allowed, collapse = ", ")
    ))
  }
  template
}

# Methode ist die R6-Klasse und laesst sich an bestehenden Objekten nicht
# aendern; iterate = TRUE kann sie daher nicht umschreiben.
check_component_method <- function(components, method, iterate = FALSE) {
  comp_method <- vapply(
    components,
    function(x) {
      if (is.persephone(x)) x$method else NA_character_
    },
    character(1)
  )
  conflict <- names(comp_method)[!is.na(comp_method) & comp_method != method]
  if (length(conflict) == 0) {
    return(invisible(NULL))
  }
  methods <- paste(unique(comp_method[conflict]), collapse = "/")
  msg <- sprintf(
    paste0(
      "Components %s are already '%s' objects and cannot be switched to ",
      "method = \"%s\"; they stay '%s'."
    ),
    paste(sQuote(conflict), collapse = ", "),
    methods,
    method,
    methods
  )
  if (isTRUE(iterate)) {
    msg <- paste0(
      msg,
      paste0(
        " iterate = TRUE updates their spec parameters but not their method; ",
        "pass them as plain ts/mts objects to have them created with '",
        method,
        "'."
      )
    )
  }
  warning(msg, call. = FALSE)
  invisible(NULL)
}

# Names only: resolving backend functions while loading the namespace would
# make Java mandatory even for users selecting the native backend.
specfun_list <- as.list(c(
  arima = "set_arima", automdl = "set_automodel", basic = "set_basic",
  easter = "set_easter", estimate = "set_estimate", setout = "set_outlier",
  td = "set_tradingdays", transform = "set_transform", addout = "add_outlier",
  remout = "remove_outlier", addramp = "add_ramp", remramp = "remove_ramp",
  usrdefvar = "add_usrdefvar", x11 = "set_x11", benchmarking = "set_benchmarking"
))

# persephone3 internal spec names
p3_spec_names <- c("outliers")

# ---- Oracle JVM math-flag capture domains (oracle-lane test infrastructure) ----
# The live-JVM parity blocks of the five bridge-parity files consume the
# tramo-seats-bridge / x13-estimated JVM captures, which were produced on
# macOS ARM64 with Temurin 21.0.11+10 and HotSpot's _dexp/_dlog intrinsics
# DISABLED (kalos/docs/contracts/tramo-seats-bridge-contract.md; proof and
# evidence: tmp_opencode/m6-c-verify/rebaseline-ledger-2026-11-24.md, scratch
# notes, not vendored). The demand-profile captures of test-parity.R pin the
# opposite domain on this architecture (x86_64-default, intrinsics ENABLED).
# On x86-64 the two HotSpot math codegen domains differ measurably (default
# JVM: 118 bridge-parity failures; capture flags: 16 x86_64-default demand
# failures) and no single JVM greens both; on aarch64 the flags are inert and
# the domains coincide. The oracle lane therefore runs in TWO passes on
# x86-64, announced by PERSEPHONE_ORACLE_PASS:
#   default -- every file under a default JVM; the five bridge-parity files
#              DEFER their live-JVM blocks to the bridge pass (documented
#              skip message, backed by the required pass step);
#   bridge  -- the five bridge-parity files under
#              JAVA_TOOL_OPTIONS="-XX:+UnlockDiagnosticVMOptions -XX:DisableIntrinsic=_dexp,_dlog"
#              (tools/run-oracle-bridge-pass.R); the x86_64-default demand
#              dispatch of test-parity.R defers back to the default pass.
# oracle_domain_allows() reports whether a block of a given capture domain may
# run in THIS process: it reads the running JVM's runtime input arguments via
# rJava's ManagementFactory RuntimeMXBean (JAVA_TOOL_OPTIONS options appear
# there -- probed 2026-11-24) plus the JVM os.arch. TRUE = run as today;
# FALSE = documented deferral (message once per file and pass). A domain
# mismatch with PERSEPHONE_ORACLE_PASS unset, or a pass whose live JVM does
# not match its name, STOPS: a naive single-pass run must never turn into a
# silent false green. On aarch64 everything below is inert.
oracle_domain_deferrals <- new.env(parent = emptyenv())

persephone_jvm_math_domain <- function() {
  if (!requireNamespace("rJava", quietly = TRUE)) {
    stop("the oracle capture-domain guard requires rJava (live-JVM lane only)",
         call. = FALSE)
  }
  rJava::.jinit()
  rt <- rJava::.jcall("java/lang/management/ManagementFactory",
                      "Ljava/lang/management/RuntimeMXBean;", "getRuntimeMXBean")
  args <- tryCatch({
    lst <- rJava::.jcall(rt, "Ljava/util/List;", "getInputArguments")
    vapply(seq_len(rJava::.jcall(lst, "I", "size")) - 1L, function(i)
      rJava::.jstrVal(rJava::.jcall(lst, "Ljava/lang/Object;", "get", i)), character(1))
  }, error = function(e)
    stop("cannot read RuntimeMXBean input arguments from the running JVM: ",
         conditionMessage(e), call. = FALSE))
  arch <- rJava::.jcall("java/lang/System", "S", "getProperty", "os.arch")
  if (!arch %in% c("x86_64", "amd64", "i386", "i486", "i586", "i686")) {
    return(list(x86 = FALSE, domain = "portable", arch = arch, args = args))
  }
  disabled <- unlist(strsplit(sub("^-XX:DisableIntrinsic=", "",
                                  grep("^-XX:DisableIntrinsic=", args, value = TRUE)),
                              ",", fixed = TRUE))
  domain <- if (all(c("_dexp", "_dlog") %in% disabled)) "bridge" else
    if (!length(disabled)) "default" else "other"
  list(x86 = TRUE, domain = domain, arch = arch, args = args)
}

oracle_domain_allows <- function(domain, file) {
  stopifnot(identical(domain, "default") || identical(domain, "bridge"))
  jvm <- persephone_jvm_math_domain()
  if (!jvm$x86) return(TRUE)  # domains coincide (aarch64): run as today
  flag_note <- paste(grep("^-XX:DisableIntrinsic=", jvm$args, value = TRUE), collapse = " ")
  guide <- paste0(
    "the oracle suite runs in two JVM math-flag passes on ", jvm$arch, ": pass A with",
    " PERSEPHONE_ORACLE_PASS=default under a default JVM, pass B with",
    " PERSEPHONE_ORACLE_PASS=bridge and JAVA_TOOL_OPTIONS=\"-XX:+UnlockDiagnosticVMOptions",
    " -XX:DisableIntrinsic=_dexp,_dlog\" (tools/run-oracle-bridge-pass.R;",
    " evidence tmp_opencode/m6-c-verify/rebaseline-ledger-2026-11-24.md)")
  pass <- tolower(trimws(Sys.getenv("PERSEPHONE_ORACLE_PASS")))
  if (!nzchar(pass)) {
    if (!identical(jvm$domain, domain)) {
      stop(sprintf(paste0("oracle capture-domain guard: the live JVM math-flag domain is '%s'%s ",
                          "but the %s block was captured in the '%s' domain; refusing a cross-domain ",
                          "comparison. Invoke the two passes explicitly because %s"),
                   jvm$domain, if (nzchar(flag_note)) paste0(" (", flag_note, ")") else "",
                   sQuote(file), domain, guide), call. = FALSE)
    }
    return(TRUE)
  }
  if (!pass %in% c("default", "bridge")) {
    stop("oracle capture-domain guard: PERSEPHONE_ORACLE_PASS must be 'default' or ",
         "'bridge', not '", pass, "'", call. = FALSE)
  }
  if (!identical(pass, domain)) {
    key <- paste(file, pass, sep = "|")
    if (is.null(oracle_domain_deferrals[[key]])) {
      oracle_domain_deferrals[[key]] <- TRUE
      message(sprintf(paste0("SKIP (documented deferral): %s runs its '%s'-domain live-JVM parity ",
                             "block in the '%s' JVM math-flag pass; this process is the '%s' pass ",
                             "(PERSEPHONE_ORACLE_PASS=%s). The block is deferred there and backed by ",
                             "a required pass step -- not a silent skip. %s"),
                      sQuote(file), domain, domain, pass, pass, guide))
    }
    return(FALSE)
  }
  if (!identical(jvm$domain, domain)) {
    stop(sprintf(paste0("oracle capture-domain guard: PERSEPHONE_ORACLE_PASS='%s' but the live JVM ",
                        "math-flag domain is '%s'%s; the %s pass is misconfigured. %s"),
                 pass, jvm$domain, if (nzchar(flag_note)) paste0(" (", flag_note, ")") else "",
                 pass, guide), call. = FALSE)
  }
  TRUE
}
