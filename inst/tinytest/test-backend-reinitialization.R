# R's example runner detaches search-path objects between examples while keeping
# namespaces loaded. rjd3's protobuf lookups must recover lazily from that state.
local({
  optional <- c("rjd3toolkit", "rjd3x13", "rjd3tramoseats")
  if (!all(vapply(optional, function(pkg) nzchar(system.file(package = pkg)), logical(1)))) {
    return(invisible(NULL))
  }
  package_path <- getNamespaceInfo(asNamespace("persephone3"), "path")
  result <- callr::r(function(package_path) {
    if (file.exists(file.path(package_path, "Meta", "package.rds"))) {
      library("persephone3", lib.loc = dirname(package_path), character.only = TRUE)
    } else {
      pkgload::load_all(package_path, quiet = TRUE, compile = FALSE)
    }
    lazy_namespaces <- !any(c("rJava", "RProtoBuf", "rjd3toolkit", "rjd3x13",
                              "rjd3tramoseats") %in% loadedNamespaces())
    lazy_search <- !"RProtoBuf:DescriptorPool" %in% search()
    backend_capabilities("rjd3")
    native <- perX13(AirPassengers, backend = "native")
    native_error <- tryCatch(perX13(AirPassengers, "rsa0", backend = "native"),
                            error = conditionMessage)
    lazy_after_native <- !any(c("rJava", "RProtoBuf") %in% loadedNamespaces()) &&
      !"RProtoBuf:DescriptorPool" %in% search()

    constructors <- list(perX13, perTramo)
    original <- lapply(constructors, function(constructor) {
      x <- constructor(AirPassengers, "rsa3", backend = "rjd3")
      x$run()
      x
    })
    baseline <- lapply(original, function(x) x$result$components$sa)
    pool_count <- function() sum(search() == "RProtoBuf:DescriptorPool")
    detach_pool <- function() {
      detach("RProtoBuf:DescriptorPool", character.only = TRUE)
      stopifnot(pool_count() == 0L)
    }

    # Restore while the method namespace is still loaded, including when the
    # first requested operation is fitting an already-created model.
    fit_recovered <- logical(2)
    construct_recovered <- logical(2)
    unique_pool <- logical(2)
    descriptor_still_present <- logical(2)
    for (i in seq_along(constructors)) {
      detach_pool()
      descriptor_name <- c("x13.Spec", "tramoseats.Spec")[i]
      descriptor_still_present[i] <- inherits(
        getExportedValue("RProtoBuf", "P")(descriptor_name), "Descriptor")
      original[[i]]$run()
      fit_recovered[i] <- isTRUE(all.equal(original[[i]]$result$components$sa, baseline[[i]]))
      detach_pool()
      fresh <- constructors[[i]](AirPassengers, "rsa3", backend = "rjd3")
      fresh$run()
      construct_recovered[i] <- isTRUE(all.equal(fresh$result$components$sa, baseline[[i]]))
      fresh$run()
      unique_pool[i] <- pool_count() == 1L
    }

    # Incremental updates enter through rjd3toolkit, before any method-specific
    # constructor or fitting call; this checks the jd3.TsData recovery probe.
    toolkit_recovered <- vapply(original, function(x) {
      expected <- getExportedValue("rjd3toolkit", "set_transform")(x$params, fun = "None")
      detach_pool()
      x$updateParams(transform.fun = "None")
      identical(x$params, expected) && pool_count() == 1L &&
        inherits(getExportedValue("RProtoBuf", "P")("jd3.TsData"), "Descriptor")
    }, logical(1))

    # A detached pool also survives normal method/toolkit namespace unloading;
    # their reload may reread schemas but must not duplicate the lookup database.
    detach_pool()
    for (package in c("rjd3x13", "rjd3tramoseats", "rjd3toolkit")) unloadNamespace(package)
    reloaded <- lapply(constructors, function(constructor) {
      x <- constructor(AirPassengers, "rsa3", backend = "rjd3")
      x$run()
      x$result$components$sa
    })
    # Simulate an incompatible pool representation in this disposable process.
    # The adapter must fail explicitly instead of attaching an invalid object.
    detach_pool()
    proto_namespace <- asNamespace("RProtoBuf")
    original_pool <- get("OTABLE", envir = proto_namespace, inherits = FALSE)
    replace_pool <- function(value) {
      unlockBinding("OTABLE", proto_namespace)
      assign("OTABLE", value, envir = proto_namespace)
      lockBinding("OTABLE", proto_namespace)
    }
    compatibility_error <- tryCatch({
      replace_pool(list(incompatible = TRUE))
      perX13(AirPassengers, "rsa3", backend = "rjd3")
      NULL
    }, error = conditionMessage, finally = replace_pool(original_pool))
    rejected_invalid_pool <- pool_count() == 0L
    perX13(AirPassengers, "rsa3", backend = "rjd3")
    list(lazy_namespaces = lazy_namespaces, lazy_search = lazy_search,
         lazy_after_native = lazy_after_native, native_error = native_error,
         native_template = native$specification$template,
         descriptor_still_present = descriptor_still_present,
         fit_recovered = fit_recovered, construct_recovered = construct_recovered,
         unique_pool = unique_pool, toolkit_recovered = toolkit_recovered,
         reloaded = vapply(seq_along(baseline), function(i) {
           isTRUE(all.equal(reloaded[[i]], baseline[[i]]))
         }, logical(1)), final_pool_count = pool_count(),
         compatibility_error = compatibility_error,
         rejected_invalid_pool = rejected_invalid_pool)
  }, args = list(package_path), libpath = .libPaths(), timeout = 120)

  expect_true(result$lazy_namespaces)
  expect_true(result$lazy_search)
  expect_true(result$lazy_after_native)
  expect_identical(result$native_template, "rsa3")
  expect_true(grepl("No Java fallback", result$native_error, fixed = TRUE))
  expect_equal(result$descriptor_still_present, c(TRUE, TRUE))
  expect_equal(result$fit_recovered, c(TRUE, TRUE))
  expect_equal(result$construct_recovered, c(TRUE, TRUE))
  expect_equal(result$unique_pool, c(TRUE, TRUE))
  expect_equal(result$toolkit_recovered, c(TRUE, TRUE))
  expect_equal(result$reloaded, c(TRUE, TRUE))
  expect_equal(result$final_pool_count, 1L)
  expect_true(result$rejected_invalid_pool)
  expect_true(grepl("could not restore RProtoBuf's descriptor pool", result$compatibility_error,
                   fixed = TRUE))
  expect_true(grepl("Restart R", result$compatibility_error, fixed = TRUE))
})
