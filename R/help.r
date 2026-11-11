# sdmx_help(): console query builder (provider -> dataflow -> codes).

#' Interactive query builder
#'
#' Choose a provider, search a dataflow and pick the codes of each dimension;
#' the result is a series id such as "EXR/M.USD+GBP.EUR.SP00.A" and the
#' [sdmx_fetch()] call that downloads it. At the end you are asked whether to
#' download the data now. Every choice accepts the number shown in the list or
#' the key itself (provider name, dataflow id, code), case-insensitive.
#' Several codes are separated by spaces, commas or '+'; an empty answer means
#' all codes.
#'
#' @param provider Optional provider name; asked if missing.
#' @return The id (invisibly), with the attribute provider; if you chose to
#'   download, the data.frame returned by sdmx_fetch() is in the attribute
#'   data. NULL if canceled.
#' @examples
#' if (interactive()) sdmx_help("ECB")
#' @export
sdmx_help <- function(provider = NULL) {
  if (!interactive()) {
    stop("sdmx_help() needs an interactive session.", call. = FALSE)
  }
  if (is.null(provider)) {
    providers <- sdmx_providers()
    provider <- pick(
      providers$name,
      sprintf("%-16s %s", providers$name, providers$description),
      "Provider"
    )

    if (length(provider) == 0) return(invisible(NULL))
  }

  repeat {
    search <- readline("Search dataflows (text, empty = all): ")
    flows <- sdmx_flows(provider, search)
    if (nrow(flows)) break
    message("No dataflow contains '", search, "'.")
  }

  full_ids <- ref_string(flows$agency, flows$id, flows$version)
  flow <- pick(
    full_ids,
    sprintf("%-30s %s", full_ids, flows$name),
    sprintf("Dataflow (%d)", nrow(flows)),
    aliases = flows$id
  )

  if (length(flow) == 0) return(invisible(NULL))

  if (sum(flows$id == flows$id[full_ids == flow]) == 1) {
    flow <- flows$id[full_ids == flow]
  }

  dims <- sdmx_dimensions(provider, flow)
  codes <- sdmx_codes(provider, flow)
  key <- lapply(stats::setNames(dims$id, dims$id), function(dim_id) {
    dim_codes <- codes[codes$dimension == dim_id, ]
    pick(
      dim_codes$code,
      sprintf("%-12s %s", dim_codes$code, dim_codes$name),
      sprintf("%s (empty = all)", dim_id),
      multiple = TRUE
    )
  })

  id <- build_id(flow, dims$id, key)
  cat("\nid: ", id, "\n\n",
      sprintf("sdmx_fetch(\"%s\", \"%s\")", provider, id), "\n\n",
      sep = "")

  result <- structure(id, provider = provider)
  if (ask_yes_no("Download the data now?")) {
    attr(result, "data") <- sdmx_fetch(provider, id)
    message(nrow(attr(result, "data")), " observations downloaded.")
  }

  invisible(result)
}

# TRUE for y/yes, FALSE for anything else (the default is no).
ask_yes_no <- function(question, input = function(prompt) readline(prompt)) {
  answer <- tolower(trimws(input(paste0(question, " [y/N] "))))
  answer %in% c("y", "yes")
}

# Numbered console list; accepts numbers or keys (and `aliases`),
# case-insensitive. A key that is also a valid number wins over the position
# only when it is typed exactly as a key. Returns the selected keys,
# character(0) for an empty answer.
pick <- function(
  keys, labels, title, multiple = FALSE, aliases = NULL,
  input = function(prompt) readline(prompt)
) {
  # title, if it wasn't clear enough...
  cat("\n", title, "\n", sep = "")

  # the pick entries:
  cat(
    sprintf("%*d: %s", nchar(length(keys)), seq_along(keys), labels),
    sep = "\n"
  )

  prompt <- if (multiple) {
    "Numbers or keys (empty = all): "
  } else {
    "Number or key (empty = cancel): "
  }

  is_integer <- function(hit, token) {
    is_integer <- is.na(hit) &&
      grepl("^[0-9]+$", token) &&
      as.integer(token) %in% seq_along(keys)
  }

  lookup <- function(token) {
    ## we try everythig on the token...
    hit <- match(token, keys)
    if (is.na(hit)) hit <- match(token, aliases)

    if (is_integer(hit, token)) hit <- as.integer(token)

    if (is.na(hit)) hit <- match(toupper(token), toupper(keys))

    if (is.na(hit) && !is.null(aliases)) {
      hit <- match(toupper(token), toupper(aliases))
    }

    # if it's still an NA, we yield an NA else the match on keys
    if (is.na(hit)) NA_character_ else keys[hit]
  }

  repeat {
    answer <- trimws(input(prompt))
    if (!nzchar(answer)) return(character(0))
    tokens <- if (multiple) strsplit(answer, "[[:space:],+]+")[[1]] else answer
    selected <- vapply(tokens, lookup, "", USE.NAMES = FALSE)
    if (!anyNA(selected)) return(unique(selected))
    message(
      "Not in the list: ",
      paste(tokens[is.na(selected)], collapse = ", "),
      ". Try again."
    )
  }
}
