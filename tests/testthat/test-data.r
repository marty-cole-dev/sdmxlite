dims <- c("FREQ", "CURRENCY", "CURRENCY_DENOM")

test_that("keys: string or named list, checked against the DSD", {
  expect_equal(
    key_parts("M.USD+GBP.", dims), list("M", c("USD", "GBP"), character(0))
  )
  expect_equal(key_parts(list(CURRENCY = c("USD", "GBP")), dims),
               list(character(0), c("USD", "GBP"), character(0)))
  expect_equal(key_string(key_parts(NULL, dims)), "all")
  expect_equal(key_string(key_parts("..", dims)), "all")
  expect_error(
    key_parts("M.USD", dims),
    "has 2 parts, the dataflow has 3 dimensions: FREQ.CURRENCY"
  )

  expect_error(
    key_parts(list(CURRENCY = "USD", FOO = "x"), dims), "unknown: FOO"
  )
  expect_equal(
    key_filter(key_parts("A+M.USD.", dims), dims),
    "c[FREQ]=A,M&c[CURRENCY]=USD"
  )
})

test_that("data URLs for SDMX 2.1 and 3.0", {
  flow_def <- data.frame(agency = "ECB", id = "EXR", version = "1.0")
  parts <- key_parts("M.USD+GBP.EUR", dims)
  expected_url <- (
    "https://data-api.ecb.europa.eu/service/data/ECB,EXR,1.0/M.USD+GBP.EUR?startPeriod=2020" # nolint
  )

  expect_equal(
    data_url(provider_info("ECB"), flow_def, parts, dims, start = "2020"),
    expected_url
  )


  expect_equal(
    data_url(
      provider_info("ECB"), flow_def, parts, dims,
      updated_after = "2024-01-01T00:00:00+01:00", include_history = TRUE
    ),
    paste0(
      "https://data-api.ecb.europa.eu/service/data/ECB,EXR,1.0/M.USD+GBP.EUR",
      "?updatedAfter=2024-01-01T00%3A00%3A00%2B01%3A00&includeHistory=true"
    )
  )

  expect_equal(
    data_url(
      provider_info("OECD_V3"), flow_def, parts, dims,
      start = "2020", end = "2021"
    ),
    paste0(
      "https://sdmx.oecd.org/public/rest/v2/data/dataflow/ECB/EXR/1.0",
      "?c[FREQ]=M&c[CURRENCY]=USD,GBP&c[CURRENCY_DENOM]=EUR",
      "&c[TIME_PERIOD]=ge:2020+le:2021"
    )
  )

  expect_error(
    data_url(
      provider_info("ECB"), flow_def, parts, dims,
      filter = "c[X]=1"
    ), "only for SDMX 3.0"
  )

  sdmx_add_provider("ENC", "https://example.org/rest", url_encoding = TRUE)

  expect_match(
    data_url(provider_info("ENC"), flow_def, parts, dims),
    "M.USD%2BGBP.EUR$"
  )
})

test_that("sdmx_data: long data.frame, NA for missing values, attributes merged", { # nolint
  urls <- local_fake_provider()
  result <- sdmx_data(
    "ECB", "EXR", "M+A.USD+GBP.EUR", start = as.Date("2020-01-01")
  )

  expect_equal(
    utils::tail(urls(), 1),
    "https://data-api.ecb.europa.eu/service/data/ECB,EXR,1.0/M+A.USD+GBP.EUR?startPeriod=2020-01-01" # nolint
  )

  expect_equal(
    names(result)[1:6], c("series_key", dims, "TIME_PERIOD", "OBS_VALUE")
  )

  expect_equal(result$series_key, c(rep("M.USD.EUR", 3), "A.GBP.EUR"))
  expect_equal(result$OBS_VALUE, c(1.0905, 1.0795, NA, 0.86979))
  expect_equal(result$OBS_STATUS, c("A", NA, "A", NA))
  expect_equal(result$TITLE, c(rep("US dollar/Euro", 3), NA))
  expect_equal(result$action, rep("Replace", 4))
  expect_equal(attr(result, "flow"), "ECB,EXR,1.0")
})

test_that("sdmx_data: no match is zero rows, not an error", {
  local_fake_provider(data = NULL)
  result <- sdmx_data("ECB", "EXR", "M.JPY.EUR")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("series_key", dims, "TIME_PERIOD", "OBS_VALUE"))
})

test_that("sdmx_data: AllDimensions layout (observations without series)", {
  # nolint starts
  doc <- xml2::read_xml('<m:StructureSpecificData xmlns:m="m"><m:DataSet>
    <Obs FREQ="A" CURRENCY="USD" CURRENCY_DENOM="EUR" TIME_PERIOD="2020" OBS_VALUE="1.1"/>
    <Obs FREQ="A" CURRENCY="GBP" CURRENCY_DENOM="EUR" TIME_PERIOD="2020" OBS_VALUE="x"/>
    </m:DataSet></m:StructureSpecificData>')
  # nolint ends
  dsd <- parse_structure(fixture("dsd21.xml"))
  expect_warning(
    result <- parse_data(doc, dsd), "1 non-numeric OBS_VALUE"
  )
  expect_equal(result$series_key, c("A.USD.EUR", "A.GBP.EUR"))
  expect_equal(result$OBS_VALUE, c(1.1, NA))
})

test_that("footer messages become warnings", {
  # nolint starts
  doc <- xml2::read_xml('<m:StructureSpecificData xmlns:m="m"><m:DataSet/>
    <m:Footer><m:Message code="413"><c:Text xmlns:c="c">Too many series</c:Text></m:Message></m:Footer>
    </m:StructureSpecificData>')
  # nolint ends
  expect_warning(
    parse_data(doc, parse_structure(fixture("dsd21.xml"))),
    "Too many series"
  )
})

test_that("sdmx_data_url: the URL sdmx_data() sends, without sending it", {
  urls <- local_fake_provider()
  url <- sdmx_data_url("ECB", "EXR", "M.USD.EUR", start = "2024")
  expect_equal(
    as.character(url),
    "https://data-api.ecb.europa.eu/service/data/ECB,EXR,1.0/M.USD.EUR?startPeriod=2024" # nolint
  )
  expect_equal(attr(url, "accept"), ACCEPT_DATA)
  expect_false(any(grepl("/data/", urls())))     # no data request
  result <- sdmx_data("ECB", "EXR", "M.USD.EUR", start = "2024")
  expect_equal(attr(result, "url"), as.character(url))
})

test_that("an empty key list means all codes", {
  expect_equal(key_string(key_parts(list(), dims)), KEY_ALL)
})

test_that("attributes declared in the DSD are found beyond the sample", {
  dsd <- parse_structure(fixture("dsd21.xml"))
  n <- ATTRIBUTE_SAMPLE_SIZE + 10
  late <- ATTRIBUTE_SAMPLE_SIZE + 5
  series <- paste0(
    '<Series FREQ="A" CURRENCY="C', seq_len(n), '" CURRENCY_DENOM="EUR"',
    ifelse(seq_len(n) == late, ' TITLE="late series"', ""), ">",
    '<Obs TIME_PERIOD="2023" OBS_VALUE="1"',
    ifelse(seq_len(n) == late, ' OBS_STATUS="A"', ""), "/></Series>",
    collapse = ""
  )
  doc <- xml2::read_xml(paste0(
    '<m:D xmlns:m="m"><m:DataSet>', series, "</m:DataSet></m:D>"
  ))
  result <- parse_data(doc, dsd)
  expect_equal(nrow(result), n)
  expect_equal(sum(!is.na(result$TITLE)), 1)
  expect_equal(result$TITLE[late], "late series")
  expect_equal(result$OBS_STATUS[late], "A")
})

test_that("attributes absent from the answer give no column", {
  local_fake_provider()
  result <- sdmx_data("ECB", "EXR")
  expect_false(any(c("validFromDate", "validToDate") %in% names(result)))
})
