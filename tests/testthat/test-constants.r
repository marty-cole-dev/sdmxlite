# The exported signatures spell the choices out, so that they show up in the
# documentation: they must stay in sync with the constants.
testthat::test_that("documented choices match the constants", {
  testthat::expect_equal(
    eval(formals(sdmx_add_provider)$version), SDMX_VERSIONS
  )
  testthat::expect_setequal(eval(formals(sdmx_proxy)$auth), names(CURLAUTH))
  testthat::expect_setequal(names(WILDCARDS), SDMX_VERSIONS)
})

testthat::test_that("options fall back to the defaults", {
  old <- options(sdmxlite.timeout = NULL, sdmxlite.lang = c("it", "en"))
  on.exit(options(old))
  testthat::expect_equal(sdmx_option("timeout"), DEFAULT_OPTIONS$timeout)
  testthat::expect_equal(sdmx_option("lang"), c("it", "en"))
})

testthat::test_that("HTTP hints: known codes, then client or server error", {
  testthat::expect_true(all(c("401", "404", "407") %in% names(HTTP_HINTS)))
})
