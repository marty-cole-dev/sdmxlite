# HTTP layer. libcurl honors http_proxy/https_proxy/no_proxy, so with a local
# authenticating proxy (e.g. cntlm) nothing has to be configured;
# sdmx_proxy() is for a proxy that asks for credentials.

#' Use an authenticated proxy for this session
#'
#' Without this, the http_proxy/https_proxy/no_proxy environment variables are
#' used as they are.
#'
#' @param host Proxy host, e.g. "proxy.example.org"; if NULL
#'   goes back to curl implementation.
#' @param port port number where the proxy is bound.
#' @param user Proxy user; NULL for no authentication.
#' @param password Asked interactively when missing.
#'   BEWARE: if you script your password somewhere, it's naturally compromised.
#' @param auth passed to the underlaying implementation (now `httr2`)
#' @param noproxy Hosts reached directly, as in no_proxy.
#' @return TRUE (invisibly) when a proxy is set, NULL when it is removed.
#' @examples
#' \dontrun{
#' sdmx_proxy("http://proxy.example.org:8080", user = "me")  # asks the password
#' }
#' sdmx_proxy(NULL)   # back to the environment variables
#' @export
sdmx_proxy <- function(
  host,
  port = NULL,
  user = NULL,
  password = NULL,
  auth = c("basic", "digest", "digest_ie", "gssnegotiate", "ntlm", "any"),
  noproxy = Sys.getenv("no_proxy", Sys.getenv("NO_PROXY"))
) {
  if (is.null(host)) {
    .state$proxy <- NULL
    return(invisible(NULL))
  }

  auth <- match.arg(auth)
  if (!is.null(user) && is.null(password)) {
    password <- ask_secret(sprintf("Proxy password for %s: ", user))
  }
  
  .state$proxy <- list(
    host = host,
    port = port,
    user = user,
    password = password,
    auth = if (is.null(auth)) "any" else auth,
    noproxy = noproxy
  )
  
  invisible(TRUE)
}

# Path elements are percent-encoded except for the characters that are part
# of the SDMX syntax (',' '+' '*' '~' '.'); query values are fully encoded.
# `filter` is a ready SDMX 3.0 query string (c[DIM]=...) appended as it is.
sdmx_url <- function(provider_def, path, query = list(), filter = NULL) {
  path <- unlist(path)
  path <- path[!is.na(path) & nzchar(path)]
  path <- vapply(
    path, utils::URLencode, "",
    reserved = FALSE, USE.NAMES = FALSE
  )

  if (isTRUE(provider_def$url_encoding)) {
    encoded_or <- utils::URLencode(KEY_OR, reserved = TRUE)
    path <- gsub(KEY_OR, encoded_or, path, fixed = TRUE)
  }

  query <- Filter(function(value) {
    length(value) == 1 && !is.na(value) && nzchar(value)
  }, query)

  query_string <- c(
    filter[!is.null(filter) && nzchar(filter)],
    if (length(query)) {
      paste0(
        names(query),
        "=",
        vapply(as.character(query), utils::URLencode, "", reserved = TRUE)
      )
    }
  )

  url <- paste(c(provider_def$endpoint, path), collapse = "/")

  if (length(query_string)) {
    paste0(url, "?", paste(query_string, collapse = "&"))
  } else {
    url
  }
}

build_request <- function(provider_def, url, accept) {
  req <- httr2::request(url)
  req <- httr2::req_headers(
    req,
    Accept = accept,
    `Accept-Language` = paste(sdmx_lang(), collapse = ",")
  )

  req <- httr2::req_user_agent(req, USER_AGENT)

  # req_timeout() resets connecttimeout, so it has to come first.
  timeout <- sdmx_option("timeout")
  if (timeout > 0) req <- httr2::req_timeout(req, timeout)

  req <- httr2::req_options(
    req,
    ## are we sure this is not a param?
    connecttimeout = sdmx_option("connect.timeout")
  )

  if (isTRUE(provider_def$compression)) {
    req <- httr2::req_options(req, accept_encoding = ACCEPT_ENCODING)
  }

  if (provider_def$auth != AUTH_NONE) {
    if (is.null(.state$credentials[[provider_def$name]])) {
      sdmx_set_credentials(provider_def$name)
    }

    cred <- .state$credentials[[provider_def$name]]
    req <- if (provider_def$auth == AUTH_BEARER) {
      httr2::req_auth_bearer_token(req, cred$token)
    } else {
      httr2::req_auth_basic(req, cred$user, cred$password)
    }
  }

  proxy <- .state$proxy
  if (!is.null(proxy)) {
    req <- httr2::req_options(req, noproxy = proxy$noproxy)
    ## watch out here, is req_proxy, not req_options ;)
    req <- httr2::req_proxy(
      req,
      proxy$host,
      port = proxy$port,
      username = proxy$user,
      password = proxy$password,
      auth = proxy$auth
    )
  }

  httr2::req_error(req, is_error = function(resp) FALSE)
}

sdmx_condition <- function(message, class, ...) {
  structure(
    class = c(class, "sdmx_error", "error", "condition"),
    list(message = message, call = NULL, ...)
  )
}

# GET an SDMX-ML document. Returns the parsed XML, or NULL when
# `not_found_ok` and the provider answers 404 (SDMX 2.1 "no results").
sdmx_get <- function(provider_def, url, accept, not_found_ok = FALSE) {
  if (isTRUE(sdmx_option("verbose"))) message("GET ", url)

  resp <- tryCatch(
    httr2::req_perform(build_request(provider_def, url, accept)),
    error = function(err) {
      msg <- conditionMessage(err)
      if (!is.null(err$parent)) msg <- paste(msg, conditionMessage(err$parent))
      if (grepl(PROXY_AUTH_FAILURE, msg, fixed = TRUE)) {
        msg <- paste(
          msg,
          "\nThe proxy rejected the credentials (407): check them or",
          "sdmx_proxy()."
        )
      }

      stop(sdmx_condition(
        sprintf("Cannot reach %s (%s): %s", provider_def$name, url, msg),
        "sdmx_connection_error",
        url = url
      ))
    }
  )

  status <- httr2::resp_status(resp)
  if (status == HTTP_NOT_FOUND && not_found_ok) return(NULL)
  if (status >= HTTP_CLIENT_ERROR) {
    hint <- unname(HTTP_HINTS[as.character(status)])
    if (is.na(hint)) {
      hint <- if (status >= HTTP_SERVER_ERROR) {
        HTTP_SERVER_ERROR_HINT
      } else {
        HTTP_CLIENT_ERROR_HINT
      }
    }

    detail <- server_message(resp)
    stop(sdmx_condition(
      sprintf(
        "HTTP %d from %s (%s): %s%s", status, provider_def$name, url, hint,
        if (is.na(detail)) "" else paste0(".\nThe provider says: ", detail)
      ),
      c(paste0("sdmx_http_", status), "sdmx_http_error"),
      status = status, url = url, detail = detail
    ))
  }

  read_sdmx_xml(resp, provider_def, url)
}

# The explanation the provider puts in an error answer: the text of an SDMX
# error message or of a SOAP fault (Eurostat), the <title> of an HTML page, or
# the start of a plain text. NA when there is nothing useful.
server_message <- function(resp) {
  if (!httr2::resp_has_body(resp)) return(NA_character_)
  body <- httr2::resp_body_raw(resp)
  text <- iconv(rawToChar(body), "UTF-8", "UTF-8", sub = "?")
  doc <- tryCatch(
    xml2::read_xml(body),
    error = function(err) {
      tryCatch(xml2::read_html(body), error = function(err) NULL)
    }
  )
  if (!is.null(doc)) {
    found <- xml2::xml_text(xml2::xml_find_all(doc, paste(
      "//*[local-name()='ErrorMessage']//*[local-name()='Text']",
      "//*[local-name()='faultstring']",
      "//*[local-name()='title']",
      sep = " | "
    )))
    text <- if (length(found)) {
      paste(found, collapse = " | ")
    } else {
      xml2::xml_text(doc)
    }
  }
  text <- gsub("\\s+", " ", trimws(text))
  if (!nzchar(text)) return(NA_character_)
  if (nchar(text) > SERVER_MESSAGE_CHARS) {
    text <- paste0(substr(text, 1, SERVER_MESSAGE_CHARS), "...")
  }
  text
}

# The body must be an SDMX-ML document: XML whose root element is in an SDMX-ML
# namespace. Anything else (an empty body, an HTML maintenance page, JSON,
# XHTML) is an sdmx_invalid_response error that says what arrived.
read_sdmx_xml <- function(resp, provider_def, url) {
  body <- if (httr2::resp_has_body(resp)) httr2::resp_body_raw(resp) else raw(0)
  content_type <- tryCatch(
    httr2::resp_content_type(resp),
    error = function(err) NA_character_
  )

  invalid <- function(what) {
    start <- if (length(body)) {
      text <- iconv(rawToChar(utils::head(body, BODY_PREVIEW_BYTES)), "UTF-8",
                    "UTF-8", sub = "?")
      sprintf(", starting with '%s'", gsub("\\s+", " ", trimws(text)))
    } else {
      ""
    }
    stop(sdmx_condition(
      sprintf("%s did not return SDMX-ML (%s): %s, content type '%s'%s.",
              provider_def$name, url, what, content_type, start),
      "sdmx_invalid_response",
      url = url, content_type = content_type
    ))
  }

  if (!length(body)) invalid("empty answer")
  doc <- tryCatch(xml2::read_xml(body), error = function(err) NULL)
  if (is.null(doc)) invalid("not XML")

  root_namespace <- xml2::xml_find_chr(doc, "string(namespace-uri(/*))")
  if (!grepl(SDMX_NAMESPACE_PATTERN, root_namespace, ignore.case = TRUE)) {
    invalid(sprintf("XML with root element <%s>", xml2::xml_name(doc)))
  }

  doc
}

sdmx_lang <- function() sdmx_option("lang")
