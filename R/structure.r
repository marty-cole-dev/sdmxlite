# Structural metadata: dataflows, data structure definitions, codelists.

structure_url <- function(
  provider_def,
  type,
  agency = NULL,
  id = NULL,
  version = NULL,
  query = list()
) {
  wildcard <- WILDCARDS[[provider_def$version]]
  sdmx_url(provider_def, c(
    if (is_v3(provider_def)) "structure", type,
    agency %||% wildcard[["any"]],
    id %||% wildcard[["any"]],
    version %||% wildcard[["latest"]]
  ), query)
}

flows_url <- function(provider_def) structure_url(provider_def, "dataflow")

parse_flows <- function(doc) {
  nodes <- xml2::xml_find_all(doc, xpath_local("Dataflows", "Dataflow"))
  dsd <- node_refs(
    xml2::xml_find_first(nodes, xpath_local("Structure", from = "./"))
  )
  data.frame(
    agency = xml2::xml_attr(nodes, "agencyID"),
    id = xml2::xml_attr(nodes, "id"),
    version = xml2::xml_attr(nodes, "version"),
    name = node_names(nodes),
    dsd = ref_string(dsd$agency, dsd$id, dsd$version),
    stringsAsFactors = FALSE
  )
}

all_flows <- function(provider) {
  provider_def <- provider_info(provider)
  cached(cache_key(provider, "flows", paste(sdmx_lang(), collapse = ",")), {
    answer <- sdmx_get(provider_def, flows_url(provider_def), ACCEPT_STRUCTURE)
    flows <- parse_flows(answer)
    if (nrow(flows) == 0) {
      stop(
        sprintf("Provider '%s' returned no dataflows.", provider),
        call. = FALSE
      )
    }
    flows[order(flows$id, flows$agency), , drop = FALSE]
  })
}

#' Dataflows of a provider
#'
#' @param provider Provider name, see sdmx_providers().
#' @param search Optional text searched (case-insensitive, as plain text, not
#'   a pattern) in the dataflow id and name.
#' @return data.frame with agency, id, version, name and
#'   dsd ("AGENCY,ID,VERSION" of the data structure).
#' @examples
#' \donttest{
#' tryCatch(sdmx_flows("ECB", "exchange"),
#'   sdmx_error = function(err) message(conditionMessage(err))
#' )
#' }
#' @export
sdmx_flows <- function(provider, search = NULL) {
  flows <- all_flows(provider)
  if (!is.null(search) && nzchar(search)) {
    search_text <- tolower(search)
    hit <- grepl(search_text, tolower(flows$id), fixed = TRUE) |
      grepl(search_text, tolower(flows$name), fixed = TRUE)
    flows <- flows[hit, , drop = FALSE]
  }
  rownames(flows) <- NULL
  flows
}

# One dataflow, as a one-row data.frame. `flow` is "ID" or "AGENCY,ID,VERSION".
# An id shared by several agencies or versions is an error unless qualified.
resolve_flow <- function(provider, flow) {
  if (!is.character(flow) || length(flow) != 1 || !nzchar(flow)) {
    stop("`flow` must be a dataflow id, see sdmx_flows().", call. = FALSE)
  }
  qualified <- grepl(REF_SEP, flow, fixed = TRUE)
  ref <- if (qualified) {
    split_ref(flow)
  } else {
    list(agency = NA, id = flow, version = NA)
  }
  flows <- all_flows(provider)
  hit <- flows[flows$id == ref$id, , drop = FALSE]

  if (!is.na(ref$agency)) {
    hit <- hit[hit$agency == ref$agency, , drop = FALSE]
  }
  if (!is.na(ref$version)) {
    hit <- hit[hit$version == ref$version, , drop = FALSE]
  }

  if (nrow(hit) == 0) {
    stop(
      sprintf(
        "Dataflow '%s' not found in %s, see sdmx_flows().",
        flow, provider
      ),
      call. = FALSE
    )
  }

  if (nrow(hit) > 1) {
    stop(sprintf(
      "Dataflow '%s' is ambiguous in %s: use one of %s.", flow, provider,
      paste(ref_string(hit$agency, hit$id, hit$version), collapse = ", ")
    ), call. = FALSE)
  }

  if (is.na(hit$dsd)) {
    stop(
      sprintf("Dataflow '%s' does not reference a data structure.", flow),
      call. = FALSE
    )
  }
  hit
}

parse_codelists <- function(doc) {
  lists <- xml2::xml_find_all(doc, xpath_local("Codelists", "Codelist"))

  if (length(lists) == 0) {
    return(
      data.frame(codelist = character(), code = character(), name = character())
    )
  }

  codes <- xml2::xml_find_all(lists, xpath_local("Code", from = "./"))
  codes_per_list <- child_counts(lists, "Code")
  data.frame(
    codelist = rep(
      ref_string(
        xml2::xml_attr(lists, "agencyID"),
        xml2::xml_attr(lists, "id"),
        xml2::xml_attr(lists, "version")
      ),
      codes_per_list
    ),
    code = xml2::xml_attr(codes, "id"),
    name = node_names(codes),
    stringsAsFactors = FALSE
  )
}

# Codelist of each concept that declares one in its CoreRepresentation, named
# "SCHEME,CONCEPT". Components without a LocalRepresentation take the
# codelist from their concept (the IMF does so).
concept_codelists <- function(doc) {
  schemes <- xml2::xml_find_all(doc, xpath_local("Concepts", "ConceptScheme"))
  concepts <- xml2::xml_find_all(schemes, xpath_local("Concept", from = "./"))
  if (!length(concepts)) return(character(0))
  scheme_ids <- rep(
    xml2::xml_attr(schemes, "id"), child_counts(schemes, "Concept")
  )
  refs <- node_refs(xml2::xml_find_first(
    concepts, xpath_local("CoreRepresentation", "Enumeration", from = "./")
  ))
  stats::setNames(
    ref_string(refs$agency, refs$id, refs$version),
    paste(scheme_ids, xml2::xml_attr(concepts, "id"), sep = REF_SEP)
  )
}

# "SCHEME,CONCEPT" of the ConceptIdentity of each component: a <Ref> with
# maintainableParentID (SDMX 2.1) or a URN "...=AGENCY:SCHEME(VERSION).CONCEPT".
concept_keys <- function(nodes) {
  identity <- xml2::xml_find_first(
    nodes, xpath_local("ConceptIdentity", from = "./")
  )
  ref <- xml2::xml_find_first(identity, xpath_local("Ref", from = "./"))
  keys <- paste(xml2::xml_attr(ref, "maintainableParentID"),
                xml2::xml_attr(ref, "id"), sep = REF_SEP)
  keys[is.na(xml2::xml_attr(ref, "id"))] <- NA_character_
  urn <- is.na(keys) & !is.na(xml2::xml_text(identity))
  if (any(urn)) {
    urn_text <- xml2::xml_text(identity[urn])
    parts <- regmatches(urn_text, regexec(CONCEPT_URN_PATTERN, urn_text))
    found <- lengths(parts) == 3
    keys[which(urn)[found]] <- vapply(parts[found], function(match) {
      paste(match[2], match[3], sep = REF_SEP)
    }, "")
  }
  keys
}

components <- function(dsd_node, path, concept_codelists = character(0)) {
  nodes <- xml2::xml_find_all(dsd_node, xpath_local(path, from = ".//"))
  codelist_refs <- node_refs(xml2::xml_find_first(
    nodes,
    xpath_local("LocalRepresentation", "Enumeration", from = "./")
  ))
  id <- xml2::xml_attr(nodes, "id")
  no_id <- is.na(id)
  if (any(no_id)) { # SDMX 2.1 allows the id to be taken from the concept
    id[no_id] <- node_refs(xml2::xml_find_first(
      nodes[no_id],
      xpath_local("ConceptIdentity", from = "./")
    ))$id
  }

  codelist <- ref_string(
    codelist_refs$agency, codelist_refs$id,
    codelist_refs$version
  )
  from_concept <- is.na(codelist)
  if (any(from_concept) && length(concept_codelists)) {
    codelist[from_concept] <- unname(
      concept_codelists[concept_keys(nodes[from_concept])]
    )
  }

  data.frame(
    id = id,
    position = suppressWarnings(as.integer(xml2::xml_attr(nodes, "position"))),
    codelist = codelist,
    stringsAsFactors = FALSE
  )
}

parse_structure <- function(doc) {
  dsd_node <- xml2::xml_find_first(
    doc, xpath_local("DataStructures", "DataStructure")
  )

  if (inherits(dsd_node, "xml_missing")) {
    stop("The answer contains no data structure.", call. = FALSE)
  }

  concept_cls <- concept_codelists(doc)
  dims <- components(dsd_node, c("DimensionList", "Dimension"), concept_cls)
  if (!anyNA(dims$position)) dims <- dims[order(dims$position), , drop = FALSE]
  dims$position <- seq_len(nrow(dims))
  time <- components(dsd_node, c("DimensionList", "TimeDimension"))$id

  measure <- c(
    components(dsd_node, c("MeasureList", "PrimaryMeasure"))$id,
    components(dsd_node, c("MeasureList", "Measure"))$id
  )

  list(
    id = ref_string(
      xml2::xml_attr(dsd_node, "agencyID"),
      xml2::xml_attr(dsd_node, "id"),
      xml2::xml_attr(dsd_node, "version")
    ),
    dimensions = dims,
    time_dimension = if (length(time)) time[1] else DEFAULT_TIME_DIMENSION,
    measure = if (length(measure)) measure[1] else DEFAULT_MEASURE,
    attributes = components(
      dsd_node, c("AttributeList", "Attribute"), concept_cls
    ),
    codes = parse_codelists(doc)
  )
}

split_ref <- function(ref) {
  parts <- strsplit(ref, REF_SEP, fixed = TRUE)[[1]]
  list(agency = parts[1], id = parts[2], version = parts[3])
}

# Data structure of a dataflow, with the codes of its dimensions and coded
# attributes. Codelists the provider did not include are fetched one by one.
flow_structure <- function(provider, flow) {
  provider_def <- provider_info(provider)
  flow_def <- resolve_flow(provider, flow)

  cached_key <- cache_key(
    provider, "dsd", flow_def$dsd, paste(sdmx_lang(), collapse = ",")
  )

  cached(cached_key, {
    ref <- split_ref(flow_def$dsd)
    dsd <- parse_structure(sdmx_get(
      provider_def, structure_url(
        provider_def, "datastructure", ref$agency, ref$id, ref$version,
        list(references = "descendants")
      ),
      ACCEPT_STRUCTURE
    ))
    missing <- setdiff(
      stats::na.omit(c(dsd$dimensions$codelist, dsd$attributes$codelist)),
      dsd$codes$codelist
    )
    for (cl in missing) {
      ref <- split_ref(cl)
      doc <- sdmx_get(
        provider_def,
        structure_url(
          provider_def, "codelist", ref$agency, ref$id, ref$version
        ),
        ACCEPT_STRUCTURE,
        not_found_ok = TRUE
      )
      if (!is.null(doc)) dsd$codes <- rbind(dsd$codes, parse_codelists(doc))
    }
    dsd
  })
}

#' Dimensions of a dataflow
#'
#' @param provider Provider name.
#' @param flow Dataflow id, or "AGENCY,ID,VERSION" when the id is ambiguous.
#' @return data.frame with position, id and codelist of each dimension, in key
#'   order. The time dimension and the measure are in the attributes
#'   "time_dimension" and "measure".
#' @examples
#' \donttest{
#' tryCatch(sdmx_dimensions("ECB", "EXR"),
#'   sdmx_error = function(err) message(conditionMessage(err))
#' )
#' }
#' @export
sdmx_dimensions <- function(provider, flow) {
  dsd <- flow_structure(provider, flow)
  dims <- dsd$dimensions[, c("position", "id", "codelist")]
  attr(dims, "time_dimension") <- dsd$time_dimension
  attr(dims, "measure") <- dsd$measure
  attr(dims, "dsd") <- dsd$id
  dims
}

#' Codes of the dimensions of a dataflow
#'
#' @param provider Provider name.
#' @param flow Dataflow id.
#' @param dimension Dimension (or coded attribute) id; NULL for all dimensions.
#' @return data.frame with dimension, code and name.
#' @examples
#' \donttest{
#' tryCatch(sdmx_codes("ECB", "EXR", "FREQ"),
#'   sdmx_error = function(err) message(conditionMessage(err))
#' )
#' }
#' @export
sdmx_codes <- function(provider, flow, dimension = NULL) {
  dsd <- flow_structure(provider, flow)
  if (is.null(dimension)) {
    comps <- dsd$dimensions[, c("id", "codelist")]
  } else {
    comps <- rbind(
      dsd$dimensions[, c("id", "codelist")],
      dsd$attributes[, c("id", "codelist")]
    )

    comps <- comps[match(dimension, comps$id, nomatch = 0), , drop = FALSE]
    unknown <- setdiff(dimension, comps$id)
    if (length(unknown)) {
      stop(sprintf(
        "'%s' is not a component of %s; dimensions are: %s",
        paste(unknown, collapse = "', '"), flow,
        paste(dsd$dimensions$id, collapse = ", ")
      ), call. = FALSE)
    }
  }

  out <- merge(
    data.frame(
      dimension = comps$id, codelist = comps$codelist,
      order = seq_len(nrow(comps)), stringsAsFactors = FALSE
    ),
    dsd$codes,
    by = "codelist", sort = FALSE
  )
  out <- out[order(out$order), c("dimension", "code", "name"), drop = FALSE]
  rownames(out) <- NULL
  out
}
