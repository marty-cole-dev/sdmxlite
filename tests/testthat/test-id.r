test_that("parse_id: flow, key, qualified flows and missing keys", {
  expect_equal(
    parse_id("DWA/Q.I10.S14.A+L+N.LE.NWA.WSE.EUR.S.N"),
    list(flow = "DWA", key = "Q.I10.S14.A+L+N.LE.NWA.WSE.EUR.S.N")
  )
  expect_equal(
    parse_id(" ECB,EXR,1.0/M..EUR "),
    list(flow = "ECB,EXR,1.0", key = "M..EUR")
  )
  expect_equal(parse_id("EXR"), list(flow = "EXR", key = NULL))
  expect_equal(parse_id("EXR/"), list(flow = "EXR", key = NULL))
  expect_error(parse_id("/M.USD"), "no dataflow")
  expect_error(parse_id(c("A/B", "C/D")), "single string")
})

test_that("build_id: DSD order, '+' between codes, empty = all", {
  dims <- c("FREQ", "CURRENCY", "CURRENCY_DENOM")
  key <- list(CURRENCY = c("USD", "GBP"), FREQ = "M")
  expect_equal(build_id("EXR", dims, key), "EXR/M.USD+GBP.")
  expect_equal(build_id("EXR", dims, list()), "EXR/..")
})

test_that("sdmx_fetch: the id goes to sdmx_data, as table or zoo", {
  urls <- local_fake_provider()
  table <- sdmx_fetch("ECB", "EXR/M+A.USD+GBP.EUR", start = "2020")
  expect_equal(
    utils::tail(urls(), 1),
    "https://data-api.ecb.europa.eu/service/data/ECB,EXR,1.0/M+A.USD+GBP.EUR?startPeriod=2020" # nolint
  )
  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 4)

  series <- sdmx_fetch("ECB", "EXR/M+A.USD+GBP.EUR", as = "zoo")
  expect_equal(names(series), c("M.USD.EUR", "A.GBP.EUR"))
  expect_true(zoo::is.zoo(series[[1]]))

  sdmx_fetch("ECB", "EXR")
  expect_match(utils::tail(urls(), 1), "/data/ECB,EXR,1.0/all$")
  expect_error(sdmx_fetch("ECB", "EXR/M.USD"), "has 2 parts")
})

test_that("sdmx_fetch(as = 'xts')", {
  skip_if_not_installed("xts")
  local_fake_provider()
  series <- sdmx_fetch("ECB", "EXR/M+A.USD+GBP.EUR", as = "xts")
  expect_true(xts::is.xts(series[["M.USD.EUR"]]))
})

test_that("sdmx_fetch: revisions only as a table, refused before download", {
  urls <- local_fake_provider()
  expect_error(
    sdmx_fetch("ECB", "EXR/M.USD.EUR", as = "zoo", include_history = TRUE),
    "Revisions"
  )
  expect_error(
    sdmx_fetch("ECB", "EXR/M.USD.EUR", as = "xts", include_history = TRUE),
    "Revisions"
  )
  expect_length(urls(), 0)                       # nothing downloaded

  sdmx_fetch("ECB", "EXR/M.USD.EUR", include_history = TRUE,
             updated_after = "2024-01-01")
  expect_match(
    utils::tail(urls(), 1), "updatedAfter=2024-01-01&includeHistory=true"
  )
})
