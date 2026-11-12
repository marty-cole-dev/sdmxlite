# sdmxlite 0.3.1

* delegate to `httr2/curl` the handling of proxies.
* added data providers 

# sdmxlite 0.3.0

* First release.
* `sdmx_providers()`, `sdmx_add_provider()` and `sdmx_set_credentials()`
  manage the registry of SDMX providers, bundled as a CSV file.
* `sdmx_flows()`, `sdmx_dimensions()` and `sdmx_codes()` return dataflows,
  dimensions and codes as data frames.
* `sdmx_data()` downloads observations from SDMX 2.1 and 3.0 providers as a
  long data frame. The key is checked against the data structure.
* `sdmx_data_url()` returns the URL of the request `sdmx_data()` would send,
  without downloading the data.
* `sdmx_to_zoo()` and `sdmx_period_to_date()` convert observations to `zoo`
  series and SDMX periods to dates.
* `sdmx_fetch()` downloads a series id such as "EXR/M.USD+GBP.EUR.SP00.A" as a
  data frame, as zoo series or as xts series (`sdmx_to_xts()`).
* `sdmx_help()` builds a series id interactively in the console.
* `sdmx_proxy()` configures an authenticated HTTP proxy.
