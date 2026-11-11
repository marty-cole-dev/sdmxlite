# HTTP layer, with responses mocked by httr2: no network is used.

ecb <- function() provider_info("ECB")
url <- "https://data-api.ecb.europa.eu/service/data/EXR"

respond <- function(status, body = "", type = "application/xml") {
  function(req) {
    httr2::response(
      status_code = status,
      headers = list("Content-Type" = type),
      body = if (is.raw(body)) body else charToRaw(body)
    )
  }
}

fixture_bytes <- function(name) {
  path <- testthat::test_path("fixtures", name)
  readBin(path, "raw", file.size(path))
}

get_error <- function(mock, ...) {
  httr2::with_mocked_responses(mock, tryCatch(
    sdmx_get(ecb(), url, ACCEPT_DATA, ...),
    error = function(err) err
  ))
}

testthat::test_that("an SDMX-ML answer is returned parsed", {
  doc <- httr2::with_mocked_responses(
    respond(200, fixture_bytes("data21.xml")),
    sdmx_get(ecb(), url, ACCEPT_DATA)
  )
  testthat::expect_s3_class(doc, "xml_document")
})

testthat::test_that("404 is 'no data' only when asked", {
  testthat::expect_null(httr2::with_mocked_responses(
    respond(404), sdmx_get(ecb(), url, ACCEPT_DATA, not_found_ok = TRUE)
  ))
  err <- get_error(respond(404))
  expect_s3_class(err, "sdmx_http_404")
  expect_s3_class(err, "sdmx_error")
  expect_equal(err$status, 404L)
})

test_that("HTTP errors: known hints, then client or server error", {
  expect_match(conditionMessage(get_error(respond(401))),
               "sdmx_set_credentials")
  expect_match(conditionMessage(get_error(respond(407))),
               "sdmx_proxy")
  expect_match(conditionMessage(get_error(respond(418))),
               HTTP_CLIENT_ERROR_HINT)
  err <- get_error(respond(503))
  expect_s3_class(err, "sdmx_http_503")
  expect_match(conditionMessage(err), HTTP_SERVER_ERROR_HINT)
})

test_that("answers that are not SDMX-ML are refused", {
  empty <- get_error(respond(200, ""))
  expect_s3_class(empty, "sdmx_invalid_response")
  expect_s3_class(empty, "sdmx_error")
  expect_match(conditionMessage(empty), "empty answer")

  html <- get_error(
    respond(200, "Service temporarily unavailable", "text/html")
  )
  expect_s3_class(html, "sdmx_invalid_response")
  expect_match(conditionMessage(html), "not XML")
  expect_match(conditionMessage(html), "text/html")
  expect_match(conditionMessage(html), "Service temporarily")
  expect_equal(html$content_type, "text/html")

  xhtml <- get_error(respond(
    200, '<html xmlns="http://www.w3.org/1999/xhtml"><body>down</body></html>'
  ))
  expect_match(conditionMessage(xhtml), "root element <html>")
})

test_that("network errors become sdmx_connection_error", {
  err <- get_error(function(req) stop("Could not resolve host"))
  expect_s3_class(err, "sdmx_connection_error")
  expect_match(conditionMessage(err), "Could not resolve host")

  proxy <- get_error(function(req) stop("CONNECT tunnel failed, response 407"))
  expect_match(conditionMessage(proxy), "rejected the credentials")
})

test_that("requests carry Accept, language, agent and timeouts", {
  old <- options(sdmxlite.lang = c("it", "en"), sdmxlite.timeout = 60)
  on.exit(options(old))
  req <- build_request(ecb(), url, ACCEPT_DATA)
  headers <- httr2::req_get_headers(req, "reveal")
  expect_equal(headers$Accept, ACCEPT_DATA)
  expect_equal(headers$`Accept-Language`, "it,en")
  expect_equal(req$options$useragent, USER_AGENT)
  expect_equal(req$options$connecttimeout, DEFAULT_OPTIONS$connect.timeout)
  expect_equal(req$options$accept_encoding, ACCEPT_ENCODING)
  expect_equal(req$options$timeout_ms, 60000)
})

test_that("verbose prints the URL", {
  old <- options(sdmxlite.verbose = TRUE)
  on.exit(options(old))
  expect_message(
    httr2::with_mocked_responses(
      respond(
        200,
        fixture_bytes("data21.xml")
      ),
      sdmx_get(ecb(), url, ACCEPT_DATA)
    ),
    paste("GET", url), fixed = TRUE
  )
})

test_that("credentials: basic and bearer headers", {
  on.exit({
    .state$credentials[c("BASICP", "TOKENP")] <- NULL
    .state$providers <- NULL   # back to the bundled registry
  })
  sdmx_add_provider("BASICP", "https://b.example.org/rest", auth = "basic")
  sdmx_set_credentials("BASICP", user = "me", password = "pw")
  basic <- build_request(provider_info("BASICP"), url, ACCEPT_DATA)
  expect_equal(
    httr2::req_get_headers(basic, "reveal")$Authorization,
    "Basic bWU6cHc="  # base64 of "me:pw"
  )

  sdmx_add_provider("TOKENP", "https://t.example.org/rest", auth = "bearer")
  sdmx_set_credentials("TOKENP", token = "abc")
  bearer <- build_request(provider_info("TOKENP"), url, ACCEPT_DATA)
  expect_equal(
    httr2::req_get_headers(bearer, "reveal")$Authorization, "Bearer abc"
  )

  expect_error(
    sdmx_set_credentials("BASICP", password = "pw"), "`user` is required"
  )
  expect_error(
    sdmx_set_credentials("ECB", user = "me", password = "pw"),
    "does not use authentication"
  )
})

test_that("an authenticated proxy sets libcurl options", {
  on.exit(sdmx_proxy(NULL))
  sdmx_proxy("http://proxy.example.org:8080", user = "me", password = "pw",
             auth = "ntlm", noproxy = "localhost")
  req <- build_request(ecb(), url, ACCEPT_DATA)
  expect_equal(req$options$proxy, "http://proxy.example.org:8080")
  expect_equal(req$options$proxyauth, CURLAUTH[["ntlm"]])
  expect_equal(req$options$proxyuserpwd, "me:pw")
  expect_equal(req$options$noproxy, "localhost")

  sdmx_proxy(NULL)
  expect_null(build_request(ecb(), url, ACCEPT_DATA)$options$proxy)
})

test_that("URLs: empty parts and parameters dropped, values encoded", {
  expect_equal(
    sdmx_url(
      ecb(),
      c("data", NA, "", "EXR"),
      list(a = "x y", b = NULL, c = "")
    ),
    "https://data-api.ecb.europa.eu/service/data/EXR?a=x%20y"
  )
})

testthat::test_that("HTTP errors quote the provider's explanation", {
  fault <- get_error(respond(400, fixture_bytes("estat_fault.xml")))
  testthat::expect_s3_class(fault, "sdmx_http_400")
  testthat::expect_match(conditionMessage(fault), "The provider says: INVALID_QUERY")
  testthat::expect_match(fault$detail, "FREQ=W")

  sdmx_error_body <- paste0(
    '<mes:Error xmlns:mes="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message">',
    '<mes:ErrorMessage code="100"><com:Text xmlns:com="c">No results found</com:Text>',
    "</mes:ErrorMessage></mes:Error>"
  )
  testthat::expect_equal(
    get_error(respond(500, sdmx_error_body))$detail, "No results found"
  )
  testthat::expect_equal(
    get_error(respond(502, "<html><head><title>Bad Gateway</title></head></html>",
                      "text/html"))$detail,
    "Bad Gateway"
  )
  testthat::expect_equal(get_error(respond(503, "  down \n for maintenance "))$detail,
                         "down for maintenance")
  testthat::expect_true(is.na(get_error(respond(503))$detail))
  long <- get_error(respond(500, strrep("x", 1000)))$detail
  testthat::expect_equal(nchar(long), SERVER_MESSAGE_CHARS + 3)
})
