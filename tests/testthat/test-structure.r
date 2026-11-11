test_that("registry: data.frame, auth normalised, editable", {
  providers <- sdmx_providers()
  expect_s3_class(providers, "data.frame")
  expect_true(all(c("ECB", "ESTAT", "IMF", "ISTAT", "BIS") %in% providers$name))
  expect_true(all(providers$auth == "none"))
  expect_true(all(grepl("^https://", providers$endpoint)))
  expect_equal(providers$version[providers$name == "OECD_V3"], "3.0")

  sdmx_add_provider(
    "TESTP", "https://example.org/rest/",
    version = "3.0", auth = "basic"
  )

  test_provider <- provider_info("TESTP")
  expect_equal(test_provider$endpoint, "https://example.org/rest")
  expect_equal(test_provider$auth, "basic")
  expect_error(provider_info("NOPE"), "Unknown provider")
  expect_error(
    sdmx_add_provider("X", "https://x.org", auth = "kerberos"),
    "Unknown authentication"
  )
  expect_error(
    new_providers("X", "x", "https://x.org", "2.0", "none"),
    "Unknown SDMX version"
  )
})

test_that("flows: vectorised parsing, names in the preferred language", {
  flows <- parse_flows(fixture("dataflows21.xml"))
  expect_equal(flows$id, c("EXR", "BSI", "EXR_OLD"))
  expect_equal(
    flows$name,
    c("Exchange Rates", "Balance Sheet Items", "Taux de change (ancien)")
  )

  expect_equal(flows$dsd, c("ECB,ECB_EXR1,1.0", NA, NA))
  old <- options(sdmxlite.lang = c("de", "en"))
  on.exit(options(old))
  expect_equal(parse_flows(fixture("dataflows21.xml"))$name[1], "Wechselkurse")
})

test_that("sdmx_flows: plain-text, case-insensitive search on id and name", {
  urls <- local_fake_provider()
  expect_equal(sdmx_flows("ECB", "exr")$id, c("EXR", "EXR_OLD"))
  expect_equal(sdmx_flows("ECB", "sheet")$id, "BSI")
  expect_equal(
    sdmx_flows("ECB", "(ancien)")$id, "EXR_OLD"
  ) # plain text, not a regex

  expect_equal(nrow(sdmx_flows("ECB", "E.R")), 0)
  expect_equal(nrow(sdmx_flows("ECB")), 3)
  expect_length(urls(), 1)                              # cached
  expect_equal(
    urls(),
    "https://data-api.ecb.europa.eu/service/dataflow/all/all/latest"
  )
})

test_that("structure URLs for 2.1 and 3.0", {
  expect_equal(
    structure_url(
      provider_info("ECB"), "datastructure", "ECB", "ECB_EXR1", "1.0",
      list(references = "descendants")
    ),
    "https://data-api.ecb.europa.eu/service/datastructure/ECB/ECB_EXR1/1.0?references=descendants" # nolint
  )

  expect_equal(
    flows_url(provider_info("OECD_V3")),
    "https://sdmx.oecd.org/public/rest/v2/structure/dataflow/*/*/~"
  )
})

test_that("DSD: dimensions in position order, time dimension, measure, codes", {
  dsd <- parse_structure(fixture("dsd21.xml"))
  expect_equal(dsd$id, "ECB,ECB_EXR1,1.0")
  expect_equal(dsd$dimensions$id, c("FREQ", "CURRENCY", "CURRENCY_DENOM"))
  expect_equal(dsd$dimensions$codelist[2], "ECB,CL_CURRENCY,1.0")
  expect_equal(dsd$time_dimension, "TIME_PERIOD")
  expect_equal(dsd$measure, "OBS_VALUE")
  expect_equal(dsd$attributes$id, c("OBS_STATUS", "TITLE"))
  expect_equal(dsd$codes$name[dsd$codes$code == "M"], "Monthly")
})

test_that("sdmx_dimensions and sdmx_codes return data.frames", {
  local_fake_provider()
  dims <- sdmx_dimensions("ECB", "EXR")
  expect_equal(dims$id, c("FREQ", "CURRENCY", "CURRENCY_DENOM"))
  expect_equal(attr(dims, "dsd"), "ECB,ECB_EXR1,1.0")
  currencies <- sdmx_codes("ECB", "EXR", "CURRENCY")
  expect_equal(currencies$code, c("USD", "GBP", "EUR"))

  expect_equal(
    unique(sdmx_codes("ECB", "EXR")$dimension),
    c("FREQ", "CURRENCY", "CURRENCY_DENOM")
  )
  expect_equal(
    sdmx_codes("ECB", "EXR", "OBS_STATUS")$name,
    "Normal"
  ) # coded attribute

  expect_error(
    sdmx_codes("ECB", "EXR", "NOPE"),
    "dimensions are: FREQ, CURRENCY"
  )

  expect_error(
    sdmx_dimensions("ECB", "BSI"),
    "does not reference a data structure"
  )

  expect_error(
    sdmx_dimensions("ECB", "XXX"), "not found"
  )

  expect_error(sdmx_dimensions("ECB", "ECB,EXR,9.9"), "not found")
})

testthat::test_that("codelists declared in the concepts (Ref and URN)", {
  dsd <- parse_structure(fixture("dsd_concepts.xml"))
  testthat::expect_equal(dsd$dimensions$id, c("COUNTRY", "FREQUENCY"))
  testthat::expect_equal(
    dsd$dimensions$codelist, c("IMF,CL_COUNTRY,2.0", "IMF,CL_FREQ,1.0")
  )
  codes <- dsd$codes[dsd$codes$codelist == "IMF,CL_COUNTRY,2.0", ]
  testthat::expect_equal(codes$name, c("United Kingdom", "France"))
})
