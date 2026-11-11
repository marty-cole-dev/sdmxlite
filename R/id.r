# Series ids: "FLOW/KEY", e.g. "DWA/Q.I10.S14.A+L+N.LE.NWA.WSE.EUR.S.N".
# FLOW is a dataflow id (or "AGENCY,ID,VERSION"), KEY an SDMX key; a missing
# or empty KEY means every series of the dataflow.

parse_id <- function(id) {
  if (!is.character(id) || length(id) != 1 || !nzchar(trimws(id))) {
    stop("`id` must be a single string such as \"EXR/M.USD.EUR.SP00.A\".",
      call. = FALSE
    )
  }

  id <- trimws(id)
  sep <- regexpr(ID_SEP, id, fixed = TRUE)
  if (sep < 0) return(list(flow = id, key = NULL))

  flow <- substr(id, 1, sep - 1)
  key <- substr(id, sep + 1, nchar(id))
  if (!nzchar(flow)) {
    stop(sprintf("`id` '%s' has no dataflow before '%s'.", id, ID_SEP),
      call. = FALSE
    )
  }

  list(flow = flow, key = if (nzchar(key)) key else NULL)
}

# "FLOW/KEY" from the codes chosen for each dimension (a named list, in any
# order); dimensions without codes are left empty, which means all codes.
build_id <- function(flow, dims, key) {
  parts <- vapply(dims, function(dim_id) {
    paste(key[[dim_id]] %||% character(0), collapse = KEY_OR)
  }, "")
  paste0(flow, ID_SEP, paste(parts, collapse = KEY_SEP))
}

#' Download series by id
#'
#' Downloads the series of an id such as "EXR/M.USD+GBP.EUR.SP00.A": the
#' dataflow, then the SDMX key. [sdmx_help()] builds such ids interactively.
#'
#' @param provider Provider name, see sdmx_providers().
#' @param id "FLOW/KEY". FLOW is a dataflow id, or "AGENCY,ID,VERSION" when the
#'   id is ambiguous; KEY is an SDMX key, where '+' separates several codes and
#'   an empty part means all codes. Without KEY every series of the dataflow
#'   is downloaded.
#' @param start,end Optional bounds: a period ("2020", "2020-Q1", "2020-01") or
#'   a Date.
#' @param as "table" for the data.frame of [sdmx_data()], one row per
#'   observation; "zoo" for a named list of zoo series, as [sdmx_to_zoo()];
#'   "xts" for a named list of xts series, as [sdmx_to_xts()] (needs 'xts').
#' @param updated_after Only observations changed after this time (a Date or
#'   an ISO 8601 timestamp).
#' @param include_history TRUE to get every revision. Revisions have several
#'   values for the same date, so they are only available with
#'   `as = "table"`; with "zoo" or "xts" this is an error, raised before
#'   anything is downloaded.
#' @return A data.frame, or a list of zoo or xts series, see `as`.
#' @examples
#' \donttest{
#' tryCatch(
#'   sdmx_fetch("ECB", "EXR/M.USD+GBP.EUR.SP00.A", start = "2024"),
#'   sdmx_error = function(err) message(conditionMessage(err))
#' )
#' }
#' @export
sdmx_fetch <- function(
  provider, id, start = NULL, end = NULL, as = c("table", "zoo", "xts"),
  updated_after = NULL, include_history = FALSE
) {
  as <- match.arg(as)
  if (isTRUE(include_history) && as != "table") {
    stop(REVISIONS_ERROR, call. = FALSE)
  }

  target <- parse_id(id)
  data <- sdmx_data(
    provider, target$flow, target$key,
    start = start, end = end,
    updated_after = updated_after, include_history = include_history
  )
  switch(as,
    table = data,
    zoo = sdmx_to_zoo(data),
    xts = sdmx_to_xts(data)
  )
}
