# Data retrieval.
#
# One entry point, sdmx_data(), for SDMX 2.1 and 3.0 providers. The key is
# checked against the data structure, so a wrong number of dimensions is an
# error here and not an empty answer from the server. The result is a long
# data.frame, one row per observation; sdmx_to_zoo() turns it into series.

# Key as a list of code vectors in DSD order (character(0) = all codes).
# `key` is NULL, an SDMX key string ("M.USD+GBP.EUR..") or a named list
# list(FREQ = "M", CURRENCY = c("USD", "GBP")); an empty list means all codes.
key_parts <- function(key, dims) {
  everything <- is.null(key) || identical(key, "") || identical(key, KEY_ALL) ||
    (is.list(key) && length(key) == 0)
  if (everything) return(rep(list(character(0)), length(dims)))

  if (is.list(key)) {
    unknown <- setdiff(names(key), dims)
    if (is.null(names(key)) || any(!nzchar(names(key))) || length(unknown)) {
      stop(
        sprintf(
          "Key names must be dimensions of the dataflow (%s)%s.",
          paste(dims, collapse = ", "),
          if (length(unknown)) {
            paste0("; unknown: ", paste(unknown, collapse = ", "))
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }

    return(lapply(dims, function(dim_id) {
      as.character(key[[dim_id]] %||% character(0))
    }))
  }

  if (!is.character(key) || length(key) != 1) {
    stop("`key` must be NULL, a key string or a named list.", call. = FALSE)
  }

  parts <- strsplit(key, KEY_SEP, fixed = TRUE)[[1]]

  if (endsWith(key, KEY_SEP)) parts <- c(parts, "")

  if (length(parts) != length(dims)) {
    stop(sprintf(
      "The key '%s' has %d parts, the dataflow has %d dimensions: %s.",
      key, length(parts), length(dims), paste(dims, collapse = KEY_SEP)
    ), call. = FALSE)
  }

  lapply(parts, function(part) {
    if (nzchar(part) && part != KEY_ANY) {
      strsplit(part, KEY_OR, fixed = TRUE)[[1]]
    } else {
      character(0)
    }
  })
}


key_string <- function(parts) {
  key <- paste(vapply(parts, paste, "", collapse = KEY_OR), collapse = KEY_SEP)
  if (all(lengths(parts) == 0)) KEY_ALL else key
}


key_filter <- function(parts, dims) {
  set <- lengths(parts) > 0
  if (!any(set)) {
    return(NULL)
  }

  paste0(
    "c[",
    dims[set], "]=",
    vapply(
      parts[set], paste, "",
      collapse = FILTER_OR
    ),
    collapse = FILTER_AND
  )
}


time_filter <- function(start, end) {
  ops <- c(
    if (!is.null(start)) paste0("ge:", start),
    if (!is.null(end)) paste0("le:", end)
  )
  if (length(ops)) paste0(TIME_FILTER, "=", paste(ops, collapse = "+"))
}

data_url <- function(
  provider_def,
  flow_def,
  parts,
  dims,
  start = NULL,
  end = NULL,
  updated_after = NULL,
  include_history = FALSE,
  filter = NULL
) {
  history <- if (isTRUE(include_history)) "true"
  if (is_v3(provider_def)) {
    sdmx_url(
      provider_def,
      c("data", "dataflow", flow_def$agency, flow_def$id, flow_def$version),
      list(updatedAfter = updated_after, includeHistory = history),
      filter = paste(
        c(key_filter(parts, dims), filter, time_filter(start, end)),
        collapse = FILTER_AND
      )
    )
  } else {
    if (!is.null(filter)) {
      stop("`filter` is only for SDMX 3.0 providers.", call. = FALSE)
    }

    sdmx_url(
      provider_def,
      c(
        "data",
        ref_string(
          flow_def$agency, flow_def$id, flow_def$version
        ), key_string(parts)
      ),
      list(
        startPeriod = start,
        endPeriod = end,
        updatedAfter = updated_after,
        includeHistory = history
      )
    )
  }
}

as_period <- function(value, arg) {
  if (is.null(value) || identical(value, "")) {
    return(NULL)
  }

  if (inherits(value, "Date")) return(format(value, "%Y-%m-%d"))

  if (!is.character(value) && !is.numeric(value) || length(value) != 1) {
    msg <- "`%s` must be a single period such as 2020, \"2020-01\" or a Date."
    stop(sprintf(msg, arg), call. = FALSE)
  }

  as.character(value)
}

#' Download observations
#'
#' @param provider Provider name, see sdmx_providers().
#' @param flow Dataflow id, or "AGENCY,ID,VERSION" when the id is ambiguous.
#' @param key NULL for everything, an SDMX key string ("M.USD+GBP.EUR.SP00.A",
#'   empty parts mean all codes) or a named list such as
#'   `list(FREQ = "M", CURRENCY = c("USD", "GBP"))`. The number and names of
#'   the dimensions are checked against the data structure.
#' @param start,end Optional bounds: a period ("2020", "2020-Q1", "2020-01") or
#'   a Date.
#' @param updated_after Only observations changed after this time (a Date or an
#'   ISO 8601 timestamp).
#' @param include_history TRUE to get every revision; the answer then has the
#'   columns action, validFromDate and validToDate.
#' @param filter SDMX 3.0 only: extra filter such as "c[OBS_STATUS]=A".
#' @return data.frame with one row per observation: series_key, one column per
#'   dimension, TIME_PERIOD, OBS_VALUE (numeric, NA when missing) and one column
#'   per attribute present in the answer. No match gives zero rows.
#' @examples
#' \donttest{
#' rates <- tryCatch(
#'   sdmx_data("ECB", "EXR", "M.USD+GBP.EUR.SP00.A", start = "2024"),
#'   sdmx_error = function(err) message(conditionMessage(err))
#' )
#'
#' # the same key as a list; empty dimensions mean all codes
#' # sdmx_data("ECB", "EXR", list(FREQ = "M", CURRENCY = c("USD", "GBP")))
#' }
#' @seealso [sdmx_data_url()] to see the request without sending it.
#' @export
sdmx_data <- function(
  provider, flow, key = NULL, start = NULL, end = NULL,
  updated_after = NULL, include_history = FALSE, filter = NULL
) {
  query <- data_query(
    provider, flow, key, start, end, updated_after, include_history, filter
  )

  doc <- sdmx_get(
    query$provider_def, query$url, ACCEPT_DATA,
    not_found_ok = TRUE
  )

  out <- if (is.null(doc)) {
    empty_data(query$dsd)
  } else {
    parse_data(doc, query$dsd)
  }

  attr(out, "provider") <- provider
  attr(out, "flow") <- ref_string(
    query$flow_def$agency, query$flow_def$id, query$flow_def$version
  )
  attr(out, "url") <- query$url

  out
}

#' URL of a data request, without sending it
#'
#' Builds the GET request that [sdmx_data()] would send with the same
#' arguments and returns its URL, so that it can be checked, opened in a
#' browser or used with another tool. The observations are not downloaded;
#' the dataflow and its data structure are still read from the provider (and
#' cached), because the key is checked against the dimensions.
#'
#' @inheritParams sdmx_data
#' @return The URL, a character string. The attribute "accept" is the media
#'   type requested in the Accept header.
#' @examples
#' \donttest{
#' tryCatch(
#'   sdmx_data_url("ECB", "EXR", "M.USD+GBP.EUR.SP00.A", start = "2024"),
#'   sdmx_error = function(err) message(conditionMessage(err))
#' )
#' }
#' @export
sdmx_data_url <- function(
  provider, flow, key = NULL, start = NULL, end = NULL,
  updated_after = NULL, include_history = FALSE, filter = NULL
) {
  query <- data_query(
    provider, flow, key, start, end, updated_after, include_history, filter
  )
  structure(query$url, accept = ACCEPT_DATA)
}

# Everything a data request needs: provider, dataflow, data structure and the
# URL. Shared by sdmx_data() and sdmx_data_url(), so the URL shown is the one
# that is sent.
data_query <- function(
  provider, flow, key, start, end, updated_after, include_history, filter
) {
  provider_def <- provider_info(provider)
  flow_def <- resolve_flow(provider, flow)
  dsd <- flow_structure(provider, flow)
  parts <- key_parts(key, dsd$dimensions$id)
  url <- data_url(
    provider_def,
    flow_def,
    parts,
    dsd$dimensions$id,
    as_period(start, "start"),
    as_period(end, "end"),
    as_period(updated_after, "updated_after"), include_history, filter
  )

  list(provider_def = provider_def, flow_def = flow_def, dsd = dsd, url = url)
}

empty_data <- function(dsd) {
  cols <- c(KEY_COLUMN, dsd$dimensions$id, TIME_COLUMN)
  out <- as.data.frame(
    stats::setNames(
      rep(list(character(0)), length(cols)),
      cols
    ),
    stringsAsFactors = FALSE
  )

  out[[VALUE_COLUMN]] <- numeric(0)

  out
}

parse_value <- function(raw) {
  values <- suppressWarnings(as.numeric(raw))
  bad <- is.na(values) & !is.na(raw) & !raw %in% MISSING_VALUE_CODES
  if (any(bad)) {
    msg <- "%d non-numeric OBS_VALUE set to NA (e.g. '%s')."
    warning(sprintf(msg, sum(bad), raw[bad][1]), call. = FALSE)
  }

  values[is.nan(values)] <- NA

  values
}

# XML attribute names of a sample of nodes, without namespaced ones (xsi:type).
# Used only for attributes that the DSD does not declare.
attr_names <- function(nodes, sample_size = ATTRIBUTE_SAMPLE_SIZE) {
  attr_ids <- unique(
    unlist(
      lapply(utils::head(nodes, sample_size), function(node) {
        names(xml2::xml_attrs(node))
      })
    )
  )
  attr_ids[!grepl(":", attr_ids, fixed = TRUE)]
}

parse_data <- function(doc, dsd) {
  footer_messages(doc)
  dims <- dsd$dimensions$id
  datasets <- xml2::xml_find_all(doc, xpath_local("DataSet"))
  series <- xml2::xml_find_all(datasets, xpath_local("Series", from = "./"))
  obs_in_series <- xml2::xml_find_all(series, xpath_local("Obs", from = "./"))
  obs_flat <- xml2::xml_find_all(
    datasets, xpath_local("Obs", from = "./")
  ) # AllDimensions layout

  if (length(obs_in_series) + length(obs_flat) == 0) return(empty_data(dsd))

  per_series <- child_counts(series, "Obs")
  series_dataset <- rep(seq_along(datasets), child_counts(datasets, "Series"))
  obs_dataset <- c(
    rep(series_dataset, per_series),
    rep(seq_along(datasets), child_counts(datasets, "Obs"))
  )

  # Attributes declared in the DSD are always looked for, at both levels;
  # the sample of nodes only adds attributes the DSD does not declare.
  declared <- stats::na.omit(dsd$attributes$id)
  obs_cols <- union(
    c(dsd$time_dimension, dsd$measure, declared),
    attr_names(c(obs_in_series, obs_flat))
  )

  series_cols <- union(c(dims, declared), attr_names(series))
  column_values <- function(name) {
    from_obs <- c(
      xml2::xml_attr(obs_in_series, name),
      xml2::xml_attr(obs_flat, name)
    )
    if (name %in% series_cols) {
      from_series <- c(
        rep(xml2::xml_attr(series, name), per_series),
        rep(NA, length(obs_flat))
      )

      from_obs[is.na(from_obs)] <- from_series[is.na(from_obs)]
    }

    from_obs
  }

  out <- as.data.frame(
    stats::setNames(lapply(dims, column_values), dims),
    stringsAsFactors = FALSE
  )

  series_key <- do.call(paste, c(unname(out), sep = KEY_SEP))
  out <- cbind(
    stats::setNames(
      data.frame(series_key, stringsAsFactors = FALSE),
      KEY_COLUMN
    ),
    out
  )

  out[[TIME_COLUMN]] <- column_values(dsd$time_dimension)
  out[[VALUE_COLUMN]] <- parse_value(column_values(dsd$measure))

  attribute_cols <- setdiff(
    c(series_cols, obs_cols),
    c(dims, dsd$time_dimension, dsd$measure)
  )

  for (column in attribute_cols) {
    values <- column_values(column)
    if (!all(is.na(values))) out[[column]] <- values
  }

  for (column in HISTORY_ATTRIBUTES) {
    values <- xml2::xml_attr(datasets, column)
    if (!all(is.na(values))) out[[column]] <- values[obs_dataset]
  }

  rownames(out) <- NULL
  out
}
