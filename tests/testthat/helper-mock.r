# Offline provider: answers sdmx_get() with the fixtures, records the URLs.
fixture <- function(name) xml2::read_xml(testthat::test_path("fixtures", name))

local_fake_provider <- function(data = "data21.xml", env = parent.frame()) {
  urls <- character(0)
  sdmx_clear_cache()
  testthat::local_mocked_bindings(
    sdmx_get = function(provider_def, url, accept, not_found_ok = FALSE) {
      urls <<- c(urls, url)

      if (grepl("/dataflow/", url)) {
        return(fixture("dataflows21.xml"))
      }

      if (grepl("/datastructure/", url)) {
        return(fixture("dsd21.xml"))
      }

      if (grepl("/data/", url)) {
        return(
          if (is.null(data)) NULL else fixture(data)
        )
      }

      stop("unexpected url ", url)
    },
    .env = env
  )

  function() urls
}
