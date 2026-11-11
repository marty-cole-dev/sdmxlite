# Namespace-agnostic, vectorised helpers for SDMX-ML 2.1 / 3.0 documents.

# XPath "//A/B" on local names, ignoring the namespace prefixes.
xpath_local <- function(..., from = "//") {
  paste0(from, paste0("*[local-name()='", c(...), "']", collapse = "/"))
}

# For each node, its <Name> in the first available preferred language, or the
# first <Name> when none matches.
node_names <- function(nodes, lang = sdmx_lang()) {
  out <- rep(NA_character_, length(nodes))
  for (language in lang) {
    todo <- is.na(out)
    if (!any(todo)) break
    name_xpath <- sprintf(
      "%s[@*[local-name()='lang']='%s']",
      xpath_local("Name", from = "./"), language
    )
    hit <- xml2::xml_find_first(nodes[todo], name_xpath)
    out[todo] <- xml2::xml_text(hit)
  }
  todo <- is.na(out)
  if (any(todo)) {
    out[todo] <- xml2::xml_text(
      xml2::xml_find_first(nodes[todo], xpath_local("Name", from = "./"))
    )
  }
  out
}


# Reference (agency, id, version) found below each node: a <Ref> element, or
# an SDMX 3.0 URN "urn:...=AGENCY:ID(VERSION)" as text.
node_refs <- function(nodes) {
  ref <- xml2::xml_find_first(nodes, xpath_local("Ref", from = ".//"))
  out <- data.frame(
    agency = xml2::xml_attr(ref, "agencyID"),
    id = xml2::xml_attr(ref, "id"),
    version = xml2::xml_attr(ref, "version"),
    stringsAsFactors = FALSE
  )

  urn <- is.na(out$id) & !is.na(xml2::xml_text(nodes))

  if (any(urn)) {
    urn_parts <- regmatches(
      xml2::xml_text(nodes[urn]),
      regexec(URN_PATTERN, xml2::xml_text(nodes[urn]))
    )
    # a match is the whole text plus the agency, id and version groups
    ok <- lengths(urn_parts) == URN_GROUPS + 1
    groups <- lapply(urn_parts[ok], `[`, -1)
    out[which(urn)[ok], ] <- do.call(rbind, groups)
  }

  out
}

ref_string <- function(agency, id, version, sep = REF_SEP) {
  out <- paste(agency, id, version, sep = sep)
  out[is.na(id)] <- NA_character_
  out
}

# Number of children named `name` for each node, used to map flattened node
# sets back to their parents (document order is preserved by xml2).
child_counts <- function(nodes, name) {
  as.integer(
    xml2::xml_find_num(
      nodes,
      sprintf("count(./*[local-name()='%s'])", name)
    )
  )
}

footer_messages <- function(doc) {
  txt <- xml2::xml_text(
    xml2::xml_find_all(
      doc,
      paste0(xpath_local("Footer"), xpath_local("Text", from = "//"))
    )
  )

  if (length(txt)) {
    warning("Message from the provider: ", paste(txt, collapse = " | "),
      call. = FALSE
    )
  }
}
