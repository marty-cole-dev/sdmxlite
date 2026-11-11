test_that("periods to dates, start and end", {
  periods <- c(
    "2020", "2020-Q2", "2020Q4", "2020-S2", "2020-H1", "2020-02", "2020-M11",
    "2020-W01", "2020-D032", "2020-05-04", "2020-05-04T10:00:00"
  )

  expect_equal(
    format(sdmx_period_to_date(periods)),
    c(
      "2020-01-01", "2020-04-01", "2020-10-01", "2020-07-01", "2020-01-01",
      "2020-02-01", "2020-11-01", "2019-12-30", "2020-02-01", "2020-05-04",
      "2020-05-04"
    )
  )

  expect_equal(
    format(sdmx_period_to_date(periods, "end")),
    c(
      "2020-12-31", "2020-06-30", "2020-12-31", "2020-12-31", "2020-06-30",
      "2020-02-29", "2020-11-30", "2020-01-05", "2020-02-01", "2020-05-04",
      "2020-05-04"
    )
  )
  expect_warning(result <- sdmx_period_to_date("boh"), "Unrecognized")
  expect_true(is.na(result))
})

test_that("sdmx_to_zoo: one series per key, natural index, constant metadata", {
  local_fake_provider()
  series <- sdmx_to_zoo(sdmx_data("ECB", "EXR"))
  expect_equal(names(series), c("M.USD.EUR", "A.GBP.EUR"))
  expect_s3_class(zoo::index(series[["M.USD.EUR"]]), "yearmon")
  expect_equal(zoo::index(series[["A.GBP.EUR"]]), 2023L)
  series_meta <- attr(series[["M.USD.EUR"]], "series")
  expect_equal(series_meta$CURRENCY, "USD")
  expect_equal(series_meta$TITLE, "US dollar/Euro")
  expect_false("OBS_STATUS" %in% names(series_meta)) # varies over the series
  series_end <- sdmx_to_zoo(sdmx_data("ECB", "EXR"), index = "end")

  expect_equal(
    format(zoo::index(series_end[["M.USD.EUR"]])),
    c("2024-01-31", "2024-02-29", "2024-03-31")
  )

  expect_error(sdmx_to_zoo(data.frame(a = 1)), "must come from sdmx_data")
})

testthat::test_that("sdmx_to_xts: xts series, plain years become dates", {
  testthat::skip_if_not_installed("xts")
  local_fake_provider()
  series <- sdmx_to_xts(sdmx_data("ECB", "EXR"))
  testthat::expect_equal(names(series), c("M.USD.EUR", "A.GBP.EUR"))
  testthat::expect_true(all(vapply(series, xts::is.xts, TRUE)))
  testthat::expect_s3_class(zoo::index(series[["M.USD.EUR"]]), "yearmon")
  annual_index <- zoo::index(series[["A.GBP.EUR"]])
  testthat::expect_s3_class(annual_index, "Date")
  testthat::expect_equal(format(annual_index), "2023-01-01")
  meta <- xts::xtsAttributes(series[["M.USD.EUR"]][1:2])$series
  testthat::expect_equal(meta$CURRENCY, "USD")   # kept after subsetting
})

testthat::test_that("revisions cannot become zoo or xts series", {
  revisions <- data.frame(
    series_key = "A.X", TIME_PERIOD = c("2023", "2023"),
    OBS_VALUE = c(1, 1.1), validFromDate = c("2024-01-01", "2024-06-01")
  )
  testthat::expect_error(sdmx_to_zoo(revisions), "Revisions")
  testthat::skip_if_not_installed("xts")
  testthat::expect_error(sdmx_to_xts(revisions), "Revisions")
})
