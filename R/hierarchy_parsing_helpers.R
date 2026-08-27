#' Get next available category ID for a source
#'
#' @param con Database connection object
#' @param source_id Integer source ID
#'
#' @return Integer, next available category ID
#' @export
get_next_category_id <- function(con, source_id) {
  query <- sprintf(
    "SELECT COALESCE(MAX(id), -1) as max_id FROM platform.category WHERE source_id = %d",
    source_id
  )
  result <- DBI::dbGetQuery(con, query)
  result$max_id + 1
}

#' Get ancestor folders for a dataset from TOC
#'
#' @param dataset_code Character, Eurostat dataset code
#' @param toc Data frame, output from eurostat::get_eurostat_toc()
#'
#' @return Data frame with columns: title, hierarchy, parent_title, instance
#' @export
get_dataset_ancestors <- function(dataset_code, toc) {
  dataset_idx <- which(toc$code == dataset_code)
  if (length(dataset_idx) == 0) stop("Dataset not found in TOC")

  # Process each instance separately
  all_ancestors <- purrr::map_dfr(dataset_idx, function(idx) {
    dataset_row <- toc[idx, ]
    ancestors <- list()
    current_level <- dataset_row$hierarchy

    for (i in (idx - 1):1) {
      if (toc$type[i] == "folder" && toc$hierarchy[i] < current_level) {
        ancestors <- c(list(toc[i, ]), ancestors)
        current_level <- toc$hierarchy[i]
        if (current_level == 0) break
      }
    }

    if (length(ancestors) == 0) return(NULL)

    result <- dplyr::bind_rows(ancestors) |>
      dplyr::select(title, hierarchy)

    # Add parent_title column
    result$parent_title <- c("Eurostat", result$title[-nrow(result)])
    result$instance <- idx  # Track which TOC instance this came from

    result
  })

  all_ancestors
}

#' Check if category exists with specific parent
#'
#' @param con Database connection object
#' @param name Character, category name
#' @param parent_id Integer, parent category ID
#' @param source_id Integer source ID
#'
#' @return Integer category ID if exists, NULL otherwise
#' @export
category_exists_with_parent <- function(con, name, parent_id, source_id) {
  name_escaped <- gsub("'", "''", name)

  if (parent_id == 0) {
    # Root category - check for category with parent_id = 0 OR no relationship
    query <- sprintf("
      SELECT c.id
      FROM platform.category c
      LEFT JOIN platform.category_relationship cr
        ON c.id = cr.category_id AND c.source_id = cr.source_id
      WHERE c.source_id = %d
        AND c.name = '%s'
        AND (cr.parent_id = 0 OR cr.parent_id IS NULL)
    ", source_id, name_escaped)
  } else {
    # Non-root - must match parent in relationship table
    query <- sprintf("
      SELECT c.id
      FROM platform.category c
      JOIN platform.category_relationship cr
        ON c.id = cr.category_id AND c.source_id = cr.source_id
      WHERE c.source_id = %d
        AND c.name = '%s'
        AND cr.parent_id = %d
    ", source_id, name_escaped, parent_id)
  }

  result <- DBI::dbGetQuery(con, query)
  if (nrow(result) > 0) result$id[1] else NULL
}

#' Parse a Eurostat TOC (navigation tree) XML document
#'
#' Walks the navigation tree recursively, producing two flat data frames: one
#' row per node (folder / dataset / table, by code) and one row per parent ->
#' child edge. English titles only. Both `branch` and `leaf` elements may
#' contain children (datasets nest tables), so the walk recurses on both. A
#' node reached via multiple parents is emitted once (deduplicated on
#' code/type/title) but keeps one edge per parent.
#'
#' Separated from \code{\link{fetch_toc}} so the parsing logic can be unit
#' tested against a fixture without network access.
#'
#' @param doc An XML document as returned by \code{xml2::read_xml()}.
#'
#' @return A list with `nodes` (data frame: code, type, title), `edges`
#'   (data frame: parent_code, child_code), and `creation_date` (POSIXct, from
#'   the tree's creationDate attribute, or NA).
#' @export
parse_toc <- function(doc) {
  ns   <- c(nt = "urn:eu.europa.ec.eurostat.navtree")
  tree <- xml2::xml_root(doc)

  creation_raw <- xml2::xml_attr(tree, "creationDate")
  creation_date <- if (is.na(creation_raw)) as.POSIXct(NA) else
    as.POSIXct(strptime(creation_raw, "%Y%m%dT%H%M", tz = "UTC"))

  # accumulators: collect one-row lists, bind once (avoid quadratic rbind)
  node_acc <- new.env(parent = emptyenv()); node_acc$rows <- list()
  edge_acc <- new.env(parent = emptyenv()); edge_acc$rows <- list()

  en_title <- function(el) {
    t <- xml2::xml_find_first(el, "./nt:title[@language='en']", ns)
    if (inherits(t, "xml_missing")) NA_character_ else xml2::xml_text(t)
  }
  node_type <- function(el) {
    if (xml2::xml_name(el) == "branch") "folder" else xml2::xml_attr(el, "type")
  }
  el_code <- function(el) {
    c <- xml2::xml_find_first(el, "./nt:code", ns)
    if (inherits(c, "xml_missing")) NA_character_ else xml2::xml_text(c)
  }

  # recursive walk: emit this node, an edge from parent (if any), then recurse
  walk <- function(el, parent_code) {
    code <- el_code(el)
    node_acc$rows[[length(node_acc$rows) + 1L]] <-
      list(code = code, type = node_type(el), title = en_title(el))
    if (!is.null(parent_code))
      edge_acc$rows[[length(edge_acc$rows) + 1L]] <-
      list(parent_code = parent_code, child_code = code)
    kids <- xml2::xml_find_all(el, "./nt:children/nt:branch | ./nt:children/nt:leaf", ns)
    for (k in kids) walk(k, code)
  }
  for (b in xml2::xml_find_all(tree, "./nt:branch", ns)) walk(b, NULL)

  nodes <- unique(as.data.frame(data.table::rbindlist(node_acc$rows)))
  edges <- if (length(edge_acc$rows))
    as.data.frame(data.table::rbindlist(edge_acc$rows)) else
      data.frame(parent_code = character(0), child_code = character(0))

  list(nodes = nodes, edges = edges, creation_date = creation_date)
}


#' Fetch and parse the Eurostat TOC (navigation tree) XML
#'
#' Downloads the TOC XML, computes its SHA-256 (for snapshot dedup), and parses
#' it into flat node and edge data frames via \code{\link{parse_toc}}.
#'
#' @param url Character; the TOC XML endpoint.
#'
#' @return A list with `nodes` (data frame: code, type, title), `edges`
#'   (data frame: parent_code, child_code), `creation_date` (POSIXct or NA),
#'   and `sha` (character SHA-256 of the downloaded file).
#' @export
fetch_toc <- function(url = "https://ec.europa.eu/eurostat/api/dissemination/catalogue/toc/xml") {
  tmp <- tempfile(fileext = ".xml")
  httr2::request(url) |> httr2::req_perform(path = tmp)
  sha <- digest::digest(file = tmp, algo = "sha256")
  res <- parse_toc(xml2::read_xml(tmp))
  c(res, list(sha = sha))
}
