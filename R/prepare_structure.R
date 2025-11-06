#' Prepare table to insert into `source` table

#' Helper function that manually prepares the new line for the source table.
#'
#' @param con connection to the database.
#' @param schema the schema to use for the connection, default is "platform"
#' @return a dataframe with the `name`, `name_long` and `url` columns.
#' for this table.
#' @export
prepare_source_table <- function(con, schema = "platform") {
  DBI::dbExecute(con, paste0("set search_path to ", schema))
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "Eurostat", schema)
  if (is.null(source_id)){
    id <- dplyr::tbl(con, "source") |>
      dplyr::summarise(max = max(id, na.rm = TRUE)) |>
      dplyr::pull() + 1
    data.frame(id = id,
               name = "Eurostat",
               name_long = "Eurostat database",
               url = "https://ec.europa.eu/eurostat/web/main/data/database")} else {
                 message("Eurostat already listed in the source table.")}
}

#' Prepare table to insert into `table` table
#'
#' Returns table ready to insert into the `table` table with the db_writing family
#' of functions.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param toc output from eurostat::get_eurostat_toc()
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#' @param keep_vintage boolean whether to keep vintages
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(code, toc, con, schema = "platform", keep_vintage = FALSE) {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "EUROSTAT", schema)
  data.frame(code = code,
             name = unique(toc$title[toc$code == code]),
             source_id = source_id,
             url = NA,
             notes = jsonlite::toJSON(list(), auto_unbox = TRUE),
             keep_vintage = keep_vintage)
}


#' Prepare table to insert into `category_table` table
#'
#' Helper function that extracts the parent category for each table from the full
#' hierarchy toc, and fills up the category_table table with the table ids and
#' their categories (parents). A single table can have multiple parents - meaning
#' it is member of several categories. Returns table
#' ready to insert into the `category_table` table with the db_writing family of functions.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param toc output from eurostat::get_eurostat_toc()
#' @param con a connection to the database
#' @param source_id integer, the source ID (default 7 for Eurostat)
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
prepare_category_table_table <- function(code, toc, con, source_id = 7, schema = "platform") {
  # Get table ID from database
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)

  # Get all ancestor paths for this dataset
  ancestors <- get_dataset_ancestors(code, toc)

  # For each instance (hierarchy path), get the leaf category (the immediate parent)
  # The leaf is the last category in each path
  parent_categories <- ancestors |>
    dplyr::group_by(instance) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()

  # For each parent category, find its ID in the database
  # We need to walk up the hierarchy to identify the correct category by its full path
  category_ids <- purrr::map_int(seq_len(nrow(parent_categories)), function(i) {
    parent_row <- parent_categories[i, ]
    inst <- parent_row$instance

    # Get full path for this instance
    inst_ancestors <- ancestors |>
      dplyr::filter(instance == inst) |>
      dplyr::arrange(hierarchy)

    # Walk down the path, finding each category ID
    parent_id <- 0
    for (j in seq_len(nrow(inst_ancestors))) {
      cat_name <- inst_ancestors$title[j]
      cat_id <- category_exists_with_parent(con, cat_name, parent_id, source_id)

      if (is.null(cat_id)) {
        stop(sprintf("Category '%s' with parent_id=%d not found. Run EUROSTAT_import_structure first.",
                     cat_name, parent_id))
      }

      parent_id <- cat_id
    }

    # The final cat_id is the leaf category for this instance
    parent_id
  })

  # Build result dataframe
  tibble::tibble(
    category_id = category_ids,
    table_id = tbl_id,
    source_id = source_id
  )
}
