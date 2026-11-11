# Constants of the SDMX REST protocol and of the package, in one place.

# nolint start

# --- SDMX REST -----------------------------------------------------------

SDMX_V21 <- "2.1"
SDMX_V3 <- "3.0"
SDMX_VERSIONS <- c(SDMX_V21, SDMX_V3)

ACCEPT_STRUCTURE <- "application/vnd.sdmx.structure+xml;version=2.1"
ACCEPT_DATA <- "application/vnd.sdmx.structurespecificdata+xml;version=2.1"

# "Any agency or id" and "latest version" in structure queries.
WILDCARDS <- list(
  "2.1" = c(any = "all", latest = "latest"),
  "3.0" = c(any = "*", latest = "~")
)

KEY_ALL <- "all"   # SDMX 2.1 key matching every series
KEY_SEP <- "."     # separates the dimensions of a key: M.USD.EUR
KEY_OR <- "+"      # several codes of one dimension in a key: USD+GBP
KEY_ANY <- "*"     # any code, same as an empty part of the key
FILTER_OR <- ","   # several codes in an SDMX 3.0 filter: c[CURRENCY]=USD,GBP
FILTER_AND <- "&"  # joins SDMX 3.0 filters
TIME_FILTER <- "c[TIME_PERIOD]"  # SDMX 3.0 component used for start/end
REF_SEP <- ","     # AGENCY,ID,VERSION
ID_SEP <- "/"      # separates flow and key in a series id: EXR/M.USD.EUR.SP00.A

DEFAULT_TIME_DIMENSION <- "TIME_PERIOD"  # when the DSD does not declare one
DEFAULT_MEASURE <- "OBS_VALUE"

# Revisions have several values for the same date: only as a table.
REVISIONS_ERROR <- paste(
  "Revisions (include_history = TRUE) have several values for the same date",
  "and cannot become time series: use the table, e.g.",
  "sdmx_fetch(..., as = \"table\")."
)

# Dataset attributes present when include_history = TRUE.
HISTORY_ATTRIBUTES <- c("action", "validFromDate", "validToDate")

# OBS_VALUE strings that mean "missing" rather than "not a number".
MISSING_VALUE_CODES <- c("", "NaN", "NA", "-")

# Namespace of the root element of every SDMX-ML 2.1 / 3.0 message, e.g.
# http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message
SDMX_NAMESPACE_PATTERN <- "sdmx\\.org/resources/sdmxml"

# How much of a non-SDMX answer is quoted in the error message.
BODY_PREVIEW_BYTES <- 80L

# SDMX 3.0 reference as text: "urn:sdmx:...=AGENCY:ID(VERSION)", captured as
# agency, id, version.
URN_PATTERN <- "=([^:]+):([^(]+)\\(([^)]+)\\)"
URN_GROUPS <- 3L

# Concept URN "...Concept=AGENCY:SCHEME(VERSION).CONCEPT": scheme and concept.
CONCEPT_URN_PATTERN <- "=[^:]+:([^(]+)\\([^)]+\\)\\.(.+)$"

# SDMX time periods; the group, if any, captures the sub-annual number.
PERIOD_PATTERNS <- list(
  year = "^\\d{4}$",
  quarter = "^\\d{4}-?Q([1-4])$",
  half = "^\\d{4}-[SH]([12])$",
  month = "^\\d{4}-M?(\\d{2})$",
  week = "^\\d{4}-W(\\d{2})$",
  day_of_year = "^\\d{4}-D(\\d{3})$",
  date = "^\\d{4}-\\d{2}-\\d{2}"
)

# --- sdmx_data() result --------------------------------------------------

KEY_COLUMN <- "series_key"
TIME_COLUMN <- "TIME_PERIOD"
VALUE_COLUMN <- "OBS_VALUE"

# Attribute names are collected from this many series/observations only.
ATTRIBUTE_SAMPLE_SIZE <- 50L

# --- provider registry ---------------------------------------------------

PROVIDER_COLUMNS <- c(
  "name", "description", "endpoint", "version", "auth"
)

AUTH_NONE <- "none"
AUTH_BASIC <- "basic"
AUTH_BEARER <- "bearer"
AUTH_METHODS <- c(AUTH_NONE, AUTH_BASIC, AUTH_BEARER)

# --- HTTP ----------------------------------------------------------------

USER_AGENT <- "sdmxlite (R)"
ACCEPT_ENCODING <- "gzip, deflate"

# libcurl CURLAUTH_* bit masks for CURLOPT_PROXYAUTH; "any" is
# CURLAUTH_ANY = ~CURLAUTH_DIGEST_IE.
CURLAUTH <- c(any = -17L, basic = 1L, digest = 2L, negotiate = 4L, ntlm = 8L)

# Text of the libcurl error when the proxy refuses the CONNECT credentials.
PROXY_AUTH_FAILURE <- "response 407"

HTTP_NOT_FOUND <- 404L
HTTP_CLIENT_ERROR <- 400L  # first status code that is an error
HTTP_SERVER_ERROR <- 500L

HTTP_HINTS <- c(
  "401" = "authentication required, see sdmx_set_credentials()",
  "403" = "access denied",
  "404" = "not found",
  "407" = "the proxy requires authentication, see sdmx_proxy()",
  "413" = "response too large, narrow the query"
)
# How much of the provider's explanation is quoted in an HTTP error.
SERVER_MESSAGE_CHARS <- 300L

HTTP_SERVER_ERROR_HINT <- "server error, retry later"
HTTP_CLIENT_ERROR_HINT <- "request rejected"

# --- options and cache ---------------------------------------------------

OPTION_PREFIX <- "sdmxlite."

DEFAULT_OPTIONS <- list(
  lang = "en",           # preferred languages for names, in order
  connect.timeout = 30,  # seconds
  timeout = 0,           # seconds for the whole request, 0 = no limit
  verbose = FALSE,       # print the URLs
  providers = NULL       # providers CSV; NULL = the bundled one
)

CACHE_SEP <- "|"

# nolint end

sdmx_option <- function(name) {
  getOption(paste0(OPTION_PREFIX, name), DEFAULT_OPTIONS[[name]])
}

is_v3 <- function(provider_def) identical(provider_def$version, SDMX_V3)
