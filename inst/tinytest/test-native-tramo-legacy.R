local({
  setup <- getFromNamespace("native_arma_objective_setup","persephone3")
  information <- getFromNamespace("native_arma_information","persephone3")
  source("fixtures/native/arma-fit-cases.R",local=TRUE)
  cases <- arma_fit_cases()
  recorded <- dget("fixtures/native/tramo-legacy-states.R")
  for(id in names(recorded)) {
    record <- recorded[[id]]; args <- cases[[id]]$arguments; args$initial <- record$initial
    context <- do.call(setup,args)
    accurate <- information(context,method="tramo_residual")
    legacy <- information(context,method="tramo_residual",covariance="legacy",legacy_state=record$state)
    expect_identical(accurate$status,"available",info=id)
    expect_identical(legacy$status,"available",info=id)
    expect_identical(accurate$covariance,legacy$accurate_covariance)
    expect_identical(legacy$covariance_mode,"legacy")
    expect_true(legacy$comparison_only)
    expected <- record$reference_covariance
    expect_true(sqrt(sum((legacy$covariance-expected)^2)/sum(expected^2))<=2e-5,info=id)
    expect_equal(unname(legacy$standard_errors),sqrt(diag(expected)),tolerance=2e-5)
    expect_identical(legacy$covariance,t(legacy$covariance))
    expect_identical(unserialize(serialize(legacy,NULL)),legacy)
    expect_equal(legacy$normalized_legacy_damping,record$state$damping/record$state$normalized_sum_squares,tolerance=1e-8)
    if(id %in% c("4-ar-complete-none","12-ar-complete-none","4-ma-complete-log")) {
      expect_true(max(abs(accurate$standard_errors-legacy$standard_errors))>1e-5)
    }
    # Both modes run entirely in C++; recorded state supplies optimizer history,
    # not a covariance lookup. A changed damping must change the replayed matrix.
    shifted <- record$state; shifted$damping <- shifted$damping+.01
    changed <- information(context,method="tramo_residual",covariance="legacy",legacy_state=shifted)
    expect_identical(changed$status,"available")
    expect_true(all(diag(changed$covariance)<diag(legacy$covariance)))
  }
})
