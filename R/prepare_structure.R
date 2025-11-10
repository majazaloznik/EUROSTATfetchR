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

#' Extract dimension structure from Eurostat dataset
#'
#' Helper function that downloads a Eurostat dataset and extracts all dimensions
#' with their unique levels and labels. Dimensions with only one unique value are excluded
#' as they are constants rather than true dimensions. This output is used by both
#' prepare_table_dimensions_table and prepare_dimension_levels_table.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#'
#' @return a named list where each element is a dataframe with columns `level_value`
#'   and `level_text` for that dimension. Only dimensions with 2+ unique values are included.
#' @export
extract_dimension_structure <- function(code) {
  # Download data
  data <- eurostat::get_eurostat(code)

  # Identify and exclude non-dimension columns
  exclude_cols <- c("TIME_PERIOD", "values", "freq")

  # Get dimension columns
  dim_cols <- setdiff(names(data), exclude_cols)

  # Extract unique levels for each dimension with their labels
  dim_structure <- purrr::map(dim_cols, function(dim) {
    # Get unique codes from data
    codes <- unique(data[[dim]]) |> sort()

    # Get labels from dictionary
    dic <- eurostat::get_eurostat_dic(dim)

    # Match codes to labels
    labels <- dic$full_name[match(codes, dic[[1]])]  # First column is the code

    # Return as dataframe
    tibble::tibble(
      level_value = codes,
      level_text = labels
    )
  })
  names(dim_structure) <- dim_cols

  # Remove dimensions with only one unique value
  dim_structure <- purrr::keep(dim_structure, ~ nrow(.x) > 1)

  dim_structure
}



#' Prepare table to insert into `table_dimensions` table
#'
#' Extracts dimension information from a Eurostat dataset and prepares it for
#' insertion into the table_dimensions table. Only non-time dimensions are included.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param dim_structure output from extract_dimension_structure() (optional, will be computed if not provided)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with `table_id`, `dimension`, and `is_time` columns
#' @export
prepare_table_dimensions_table <- function(code, dim_structure = NULL, con, schema = "platform") {
  # Get table ID from database
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)

  # Get dimension structure if not provided
  if (is.null(dim_structure)) {
    dim_structure <- extract_dimension_structure(code)
  }

  # Build result - all dimensions have is_time = FALSE
  tibble::tibble(
    table_id = tbl_id,
    dimension = names(dim_structure),
    is_time = FALSE
  )
}


#' Prepare table to insert into `dimension_levels` table
#'
#' Extracts dimension levels from a Eurostat dataset and prepares them for
#' insertion into the dimension_levels table.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param dim_structure output from extract_dimension_structure() (optional, will be computed if not provided)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with `tab_dim_id`, `level_value`, and `level_text` columns
#' @export
prepare_dimension_levels_table <- function(code, dim_structure = NULL, con, schema = "platform") {
  # Get table ID from database
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)

  # Get dimension structure if not provided
  if (is.null(dim_structure)) {
    dim_structure <- extract_dimension_structure(code)
  }

  # For each dimension, get its tab_dim_id and build the levels dataframe
  result <- purrr::map_dfr(names(dim_structure), function(dim_name) {
    # Get the dimension ID from database
    tab_dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(
      tbl_id,
      dim_name,
      con,
      schema
    )

    # Get the levels for this dimension
    levels_df <- dim_structure[[dim_name]]

    # Add tab_dim_id column
    levels_df |>
      dplyr::mutate(tab_dim_id = tab_dim_id, .before = 1)
  })

  result
}
