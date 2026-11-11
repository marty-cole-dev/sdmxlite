# sdmxlite

[![R-CMD-check](https://github.com/marty-cole-dev/sdmxlite/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marty-cole-dev/sdmxlite/actions/workflows/R-CMD-check.yaml)

A lightweight SDMX 2.1 / 3.0 REST client in pure R: results as plain
`data.frame`s, `zoo` or `xts` lists.

## Installation

```r
remotes::install_github("marty-cole-dev/sdmxlite")
```

## Usage

```r
library(sdmxlite)

sdmx_providers()                               # registered providers
sdmx_flows("ECB", "exchange")                  # plain-text search on id and name
sdmx_dimensions("ECB", "EXR")                  # dimensions in key order
sdmx_codes("ECB", "EXR", "CURRENCY")           # codes with their names

d <- sdmx_data("ECB", "EXR", "M.USD+GBP.EUR.SP00.A", start = "2020")
d <- sdmx_data("ECB", "EXR", list(FREQ = "M", CURRENCY = c("USD", "GBP")))
sdmx_data_url("ECB", "EXR", "M.USD.EUR.SP00.A")   # the GET URL, nothing downloaded
z <- sdmx_to_zoo(d)                            # one zoo series per series_key
sdmx_period_to_date(d$TIME_PERIOD, at = "end")

# a series id is "FLOW/KEY", as built by sdmx_help()
sdmx_fetch("ECB", "EXR/M.USD+GBP.EUR.SP00.A", start = "2020")              # data.frame
sdmx_fetch("ECB", "EXR/M.USD+GBP.EUR.SP00.A", start = "2020", as = "zoo")  # zoo series
sdmx_fetch("ECB", "EXR/M.USD+GBP.EUR.SP00.A", start = "2020", as = "xts")  # xts series

sdmx_help()                                    # builds the id interactively
```

`sdmx_data()` returns one row per observation, with these columns:
- `series_key`;
- one column per dimension;
- `TIME_PERIOD`;
- `OBS_VALUE`: numeric, `NA` when missing;
- one column per attribute present in the answer.

The key is checked against the data structure of the dataflow. A query that
matches nothing returns zero rows instead of an error. The same function works
with SDMX 2.1 and 3.0 providers.

## Providers

| name | provider | SDMX |
|---|---|---|
| ABS | Australian Bureau of Statistics | 2.1 |
| BIS | Bank for International Settlements | 2.1 |
| ECB | European Central Bank | 2.1 |
| ESTAT | Eurostat | 2.1 |
| ESTAT_COMEXT | Eurostat international trade (Comext) | 2.1 |
| ILO | International Labour Organization | 2.1 |
| IMF | International Monetary Fund | 2.1 |
| INSEE | Institut national de la statistique et des études économiques | 2.1 |
| ISTAT | Istituto Nazionale di Statistica | 2.1 |
| NB | Norges Bank | 2.1 |
| OECD | OECD Data Explorer | 2.1 |
| OECD_V3 | OECD Data Explorer | 3.0 |
| PDH | Pacific Data Hub | 2.1 |
| UNICEF | UNICEF Data Warehouse | 2.1 |
| UNSD | United Nations Statistics Division | 2.1 |

To add a provider for the session, use `sdmx_add_provider()`. To replace the
whole list, set `options(sdmxlite.providers = "my_providers.csv")`. The CSV has
these columns:
- `name`, `description`, `endpoint`;
- `version`: `2.1` or `3.0`;
- `auth`: `none`, `basic` or `bearer`.

## Proxy

HTTP goes through libcurl, which honors `http_proxy`, `https_proxy` and
`no_proxy`. With those variables set, or with a local authenticating proxy
such as cntlm, nothing has to be configured. For a proxy that asks for
credentials:

```r
sdmx_proxy("http://proxy.example.org:8080", user = "<user>")   # the password is prompted
```

libcurl picks the authentication scheme (basic, NTLM, Negotiate).

**If you get an HTTP 407, do not retry in a loop.** On many proxies
every attempt counts as a failed login and can lock the account.

## Options

| option | default | |
|---|---|---|
| `sdmxlite.lang` | `"en"` | preferred languages for names, in order |
| `sdmxlite.connect.timeout` | `30` | seconds |
| `sdmxlite.timeout` | `0` | seconds for the whole request, 0 = no limit |
| `sdmxlite.verbose` | `FALSE` | print the URLs |
| `sdmxlite.providers` | bundled file | alternative providers CSV |

## License

GPL-2: the GNU General Public License version 2, the license of the Linux
kernel. See [LICENSE.md](LICENSE.md).
