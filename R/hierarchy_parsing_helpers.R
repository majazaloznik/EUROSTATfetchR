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
