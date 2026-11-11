# SDMX periods and conversion to zoo.
#
# SDMX time periods are self-describing ("2020", "2020-Q1", "2020-S1",
# "2020-01", "2020-M01", "2020-W05", "2020-D032", "2020-01-31", timestamps),
# so the format is read from the string, not from the FREQ dimension.

month_end <- function(year, month) {
  next_year <- ifelse(month == 12, year + 1, year)
  next_month <- ifelse(month == 12, 1, month + 1)
  as.Date(sprintf("%04d-%02d-01", next_year, next_month)) - 1
}

iso_week_monday <- function(year, week) {
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  jan4 - (as.integer(format(jan4, "%u")) - 1) + (week - 1) * 7
}

#' Convert SDMX periods to dates
#'
#' @param periods Character vector of SDMX periods.
#' @param at "start" or "end" of the period.
#' @return Date vector; NA (with a warning) for unrecognized periods.
#' @examples
#' sdmx_period_to_date(c("2020", "2020-Q2", "2020-03", "2020-W01"), at = "end")
#' @export
sdmx_period_to_date <- function(periods, at = c("start", "end")) {
  at <- match.arg(at)
  periods <- as.character(periods)
  out <- rep(as.Date(NA), length(periods))
  year <- suppressWarnings(as.integer(substr(periods, 1, 4)))
  end <- at == "end"
  fill_matching <- function(pattern, convert) {
    rows <- which(is.na(out) & grepl(pattern, periods))
    if (length(rows)) out[rows] <<- convert(rows)
  }

  captured_int <- function(rows, pattern) {
    as.integer(sub(pattern, "\\1", periods[rows]))
  }

  first_day <- function(rows, month) {
    as.Date(sprintf("%04d-%02d-01", year[rows], month))
  }

  fill_matching(PERIOD_PATTERNS$year, function(rows) {
    if (end) month_end(year[rows], 12) else first_day(rows, 1)
  })

  fill_matching(PERIOD_PATTERNS$quarter, function(rows) {
    quarter <- captured_int(rows, PERIOD_PATTERNS$quarter)
    if (end) {
      month_end(year[rows], quarter * 3)
    } else {
      first_day(rows, quarter * 3 - 2)
    }
  })

  fill_matching(PERIOD_PATTERNS$half, function(rows) {
    half <- captured_int(rows, PERIOD_PATTERNS$half)
    if (end) {
      month_end(year[rows], half * 6)
    } else {
      first_day(rows, half * 6 - 5)
    }
  })

  fill_matching(PERIOD_PATTERNS$month, function(rows) {
    month <- captured_int(rows, PERIOD_PATTERNS$month)
    if (end) month_end(year[rows], month) else first_day(rows, month)
  })

  fill_matching(PERIOD_PATTERNS$week, function(rows) {
    week <- captured_int(rows, PERIOD_PATTERNS$week)
    iso_week_monday(year[rows], week) + if (end) 6 else 0
  })

  fill_matching(PERIOD_PATTERNS$day_of_year, function(rows) {
    day_of_year <- captured_int(rows, PERIOD_PATTERNS$day_of_year)
    first_day(rows, 1) + day_of_year - 1
  })

  fill_matching(PERIOD_PATTERNS$date, function(rows) {
    as.Date(substr(periods[rows], 1, 10))
  })
  bad <- is.na(out) & !is.na(periods)

  if (any(bad)) {
    warning(
      sprintf("Unrecognized period '%s'.", periods[bad][1]),
      call. = FALSE
    )
  }

  out
}

# Natural zoo index for one series: integer years, yearqtr, yearmon, or Date.
period_index <- function(periods, at) {
  if (all(grepl(PERIOD_PATTERNS$year, periods))) {
    return(as.integer(periods))
  }
  if (all(grepl(PERIOD_PATTERNS$quarter, periods))) {
    return(zoo::as.yearqtr(sub("^(\\d{4})-?Q", "\\1 Q", periods)))
  }
  if (all(grepl(PERIOD_PATTERNS$month, periods))) {
    return(zoo::as.yearmon(sub("^(\\d{4})-M?", "\\1-", periods)))
  }
  sdmx_period_to_date(periods, at)
}

# Revisions (include_history = TRUE) hold several versions of the same
# observation: a time series would get the same date twice, so they are only
# available as a table. The check is on the data, not on the history columns,
# because providers send `action` also in ordinary answers.
check_no_revisions <- function(data) {
  repeated <- duplicated(data[c(KEY_COLUMN, TIME_COLUMN)])
  if (any(repeated)) {
    first <- data[which(repeated)[1], ]
    stop(REVISIONS_ERROR, sprintf(
      " First repeated: %s at %s.", first[[KEY_COLUMN]], first[[TIME_COLUMN]]
    ), call. = FALSE)
  }
}

#' Split sdmx_data() results into zoo series
#'
#' Revisions (`include_history = TRUE`) are refused: they have several values
#' for the same date.
#'
#' @param data data.frame returned by sdmx_data().
#' @param index "natural" uses integer years, yearqtr and yearmon where
#'   possible and dates otherwise; "start" and "end" always use the dates of
#'   the start or end of each period.
#' @return Named list of zoo objects, one per series_key. Each has the
#'   attribute "series": a one-row data.frame with the dimensions and the
#'   attributes that are constant over the series.
#' @examples
#' obs <- data.frame(
#'   series_key = rep(c("A.X", "M.X"), c(2, 3)),
#'   FREQ = rep(c("A", "M"), c(2, 3)),
#'   TIME_PERIOD = c("2020", "2021", "2020-01", "2020-02", "2020-03"),
#'   OBS_VALUE = c(1, 2, 10, 11, 12)
#' )
#' series <- sdmx_to_zoo(obs)
#' series[["M.X"]]
#' attr(series[["M.X"]], "series")
#' @export
sdmx_to_zoo <- function(data, index = c("natural", "start", "end")) {
  index <- match.arg(index)
  data_cols <- c(KEY_COLUMN, TIME_COLUMN, VALUE_COLUMN)
  if (!all(data_cols %in% names(data))) {
    stop("`data` must come from sdmx_data().", call. = FALSE)
  }
  check_no_revisions(data)
  meta_cols <- setdiff(names(data), data_cols)

  lapply(
    split(data, factor(data[[KEY_COLUMN]], unique(data[[KEY_COLUMN]]))),
    function(series_rows) {
      index_values <- if (index == "natural") {
        period_index(series_rows[[TIME_COLUMN]], "start")
      } else {
        sdmx_period_to_date(series_rows[[TIME_COLUMN]], index)
      }
      series <- zoo::zoo(series_rows[[VALUE_COLUMN]], order.by = index_values)

      const <- meta_cols[
        vapply(meta_cols, function(column) {
          length(unique(series_rows[[column]])) == 1
        }, TRUE)
      ]

      meta <- series_rows[1, const, drop = FALSE]
      rownames(meta) <- NULL
      attr(series, "series") <- meta
      series
    }
  )
}

#' Split sdmx_data() results into xts series
#'
#' Like [sdmx_to_zoo()], but returns xts objects; needs the 'xts' package.
#' xts does not accept plain years as an index, so annual series are indexed
#' by the date of the start of the year; monthly and quarterly series keep
#' the yearmon and yearqtr index.
#'
#' @inheritParams sdmx_to_zoo
#' @return Named list of xts objects, one per series_key. The one-row
#'   data.frame with the constant dimensions and attributes is in
#'   `xts::xtsAttributes(x)$series`, which survives subsetting.
#' @examples
#' if (requireNamespace("xts", quietly = TRUE)) {
#'   obs <- data.frame(
#'     series_key = rep(c("A.X", "M.X"), c(2, 3)),
#'     FREQ = rep(c("A", "M"), c(2, 3)),
#'     TIME_PERIOD = c("2020", "2021", "2020-01", "2020-02", "2020-03"),
#'     OBS_VALUE = c(1, 2, 10, 11, 12)
#'   )
#'   series <- sdmx_to_xts(obs)
#'   series[["A.X"]]
#'   xts::xtsAttributes(series[["M.X"]])$series
#' }
#' @export
sdmx_to_xts <- function(data, index = c("natural", "start", "end")) {
  if (!requireNamespace("xts", quietly = TRUE)) {
    stop("xts series need the 'xts' package: install.packages(\"xts\").",
      call. = FALSE
    )
  }

  lapply(sdmx_to_zoo(data, index), function(series) {
    meta <- attr(series, "series")
    if (is.integer(zoo::index(series))) {  # plain years
      years <- as.character(zoo::index(series))
      zoo::index(series) <- sdmx_period_to_date(years, "start")
    }
    out <- xts::as.xts(series)
    xts::xtsAttributes(out) <- list(series = meta)
    out
  })
}
