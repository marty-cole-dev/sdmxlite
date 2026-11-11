# Provider registry and session state.
#
# The registry is read from inst/extdata/providers.csv (one row per provider:
# name, description, endpoint, version, auth) and kept as a
# data.frame. Session state (registry, credentials, proxy, cache) lives in one
# environment.

.state <- new.env(parent = emptyenv())

read_providers <- function(path) {
  reg <- utils::read.csv(path,
    colClasses = "character", na.strings = "",
    encoding = "UTF-8", strip.white = TRUE
  )
  missing <- setdiff(PROVIDER_COLUMNS, names(reg))
  if (length(missing)) {
    stop(
      sprintf("%s: missing columns %s.", path, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  compression <- if (is.null(reg$compression)) {
    TRUE
  } else {
    !(reg$compression %in% "false")
  }

  url_encoding <- if (is.null(reg$url_encoding)) {
    FALSE
  } else {
    reg$url_encoding %in% "true"
  }

  new_providers(
    reg$name,
    reg$description,
    reg$endpoint,
    reg$version, reg$auth,
    compression = compression,
    url_encoding = url_encoding
  )
}


new_providers <- function(
  name,
  description,
  endpoint,
  version,
  auth,
  compression = TRUE,
  url_encoding = FALSE
) {
  bad <- setdiff(version, SDMX_VERSIONS)
  if (length(bad)) {
    stop(sprintf(
      "Unknown SDMX version '%s': use %s.", bad[1],
      paste0("'", SDMX_VERSIONS, "'", collapse = " or ")
    ), call. = FALSE)
  }

  auth <- ifelse(is.na(auth), AUTH_NONE, tolower(auth))
  bad <- setdiff(auth, AUTH_METHODS)
  if (length(bad)) {
    stop(sprintf(
      "Unknown authentication '%s': use %s.",
      bad[1], paste0("'", AUTH_METHODS, "'", collapse = ", ")
    ), call. = FALSE)
  }

  data.frame(
    name = name,
    description = description,
    endpoint = sub("/+$", "", endpoint),
    version = version,
    auth = auth,
    compression = compression,
    url_encoding = url_encoding,
    stringsAsFactors = FALSE
  )
}


registry <- function() {
  if (is.null(.state$providers)) {
    path <- sdmx_option("providers") %||%
      system.file("extdata", "providers.csv", package = "sdmxlite")
    .state$providers <- read_providers(path)
  }
  .state$providers
}


provider_info <- function(provider) {
  if (!is.character(provider) || length(provider) != 1 || !nzchar(provider)) {
    stop(
      "`provider` must be a provider name, see sdmx_providers().",
      call. = FALSE
    )
  }
  reg <- registry()
  row <- reg[reg$name == provider, , drop = FALSE]

  if (nrow(row) == 0) {
    stop(
      sprintf("Unknown provider '%s', see sdmx_providers().", provider),
      call. = FALSE
    )
  }

  if (is.na(row$endpoint) || !nzchar(row$endpoint)) {
    stop(
      sprintf("Provider '%s' has no endpoint.", provider),
      call. = FALSE
    )
  }

  as.list(row)
}

#' Registered SDMX providers
#'
#' @return data.frame with one row per provider: name, description, endpoint,
#'   SDMX version ("2.1" or "3.0"), authentication ("none", "basic",
#'   "bearer"), compression and URL encoding flags.
#' @examples
#' providers <- sdmx_providers()
#' providers[, c("name", "description", "version")]
#' @export
sdmx_providers <- function() {
  reg <- registry()
  reg <- reg[order(reg$name), , drop = FALSE]
  rownames(reg) <- NULL
  reg
}

#' Register a provider for this session
#'
#' Replaces a provider with the same name.
#'
#' @param name Provider name.
#' @param endpoint REST endpoint, e.g. "https://stats.example.org/rest".
#' @param version SDMX REST version: "2.1" or "3.0".
#' @param auth "none", "basic" or "bearer".
#' @param description Free text.
#' @param compression Whether the provider supports gzip.
#' @param url_encoding TRUE if the provider needs '+' in keys encoded as \%2B.
#' @return The provider name, invisibly.
#' @examples
#' sdmx_add_provider(
#'  "MY",
#'  "https://stats.example.org/rest", description = "My provider"
#' )
#'
#' "MY" %in% sdmx_providers()$name
#' @export
sdmx_add_provider <- function(
  name,
  endpoint,
  version = c("2.1", "3.0"),
  auth = "none",
  description = name,
  compression = TRUE,
  url_encoding = FALSE
) {
  version <- match.arg(version)
  reg <- registry()
  reg <- reg[reg$name != name, , drop = FALSE]
  .state$providers <- rbind(reg, new_providers(
    name, description, endpoint, version, auth,
    isTRUE(compression), isTRUE(url_encoding)
  ))
  sdmx_clear_cache(name)
  invisible(name)
}

#' Set the credentials of a provider
#'
#' @param provider Provider name.
#' @param user,password Basic authentication. The password is asked
#'   interactively when missing.
#' @param token Bearer token (for providers with auth = "bearer"), asked
#'   interactively when missing.
#' @return TRUE, invisibly. The credentials are kept only in the session.
#' @examples
#' sdmx_add_provider(
#'    "PRIVATE",
#'    "https://stats.example.org/rest", auth = "basic"
#' )
#' sdmx_set_credentials("PRIVATE", user = "me", password = "secret")
#' @export
sdmx_set_credentials <- function(
  provider,
  user = NULL,
  password = NULL,
  token = NULL
) {
  provider_def <- provider_info(provider)
  cred <- if (provider_def$auth == AUTH_BEARER) {
    list(
      token = token %||% ask_secret(sprintf("Bearer token for %s: ", provider))
    )
  } else if (provider_def$auth == AUTH_BASIC) {
    if (is.null(user)) {
      stop("`user` is required for basic authentication.", call. = FALSE)
    }
    list(
      user = user,
      password = password %||% ask_secret(
        sprintf("Password for %s@%s: ", user, provider)
      )
    )
  } else {
    stop(
      sprintf("Provider '%s' does not use authentication.", provider),
      call. = FALSE
    )
  }

  .state$credentials[[provider]] <- cred
  invisible(TRUE)
}


ask_secret <- function(prompt) {
  if (!interactive()) {
    stop("A secret is required and the session is not interactive.",
      call. = FALSE
    )
  }

  secret <- if (requireNamespace("getPass", quietly = TRUE)) {
    getPass::getPass(prompt)
  } else {
    readline(prompt)
  }

  if (is.null(secret) || !nzchar(secret)) {
    stop("Nothing entered.", call. = FALSE)
  }

  secret
}

# --- session cache -------------------------------------------------------

cache_key <- function(...) paste(..., sep = CACHE_SEP)

cached <- function(key, expr) {
  if (!is.null(.state$cache[[key]])) {
    return(.state$cache[[key]])
  }
  value <- expr
  .state$cache[[key]] <- value
  value
}

#' Empty the session cache
#'
#' @param provider Provider whose dataflows and structures are forgotten;
#'   NULL empties everything.
#' @return NULL, invisibly.
#' @examples
#' sdmx_clear_cache("ECB")
#' sdmx_clear_cache()
#' @export
sdmx_clear_cache <- function(provider = NULL) {
  if (is.null(provider)) {
    .state$cache <- NULL
  } else if (!is.null(.state$cache)) {
    keys <- names(.state$cache)
    .state$cache[startsWith(keys, paste0(provider, CACHE_SEP))] <- NULL
  }
  invisible(NULL)
}

`%||%` <- function(value, default) if (is.null(value)) default else value
