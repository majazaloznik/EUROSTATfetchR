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
             notes = as.character(jsonlite::toJSON(list(), auto_unbox = TRUE)),
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



#' Prepare table to insert into `table_dimensions` table
#'
#' Extracts dimension information from a Eurostat dataset and prepares it for
#' insertion into the table_dimensions table. Only non-time dimensions are included.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param dim_structure output from extract_dimension_structure()$dimensions (optional, will be computed if not provided)
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
    dim_structure <- extract_dimension_structure(code)$dimensions
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
#' @param dim_structure output from extract_dimension_structure()$dimensions (optional, will be computed if not provided)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with `tab_dim_id`, `dimension`, `level_value`, and `level_text` columns
#' @export
prepare_dimension_levels_table <- function(code, dim_structure = NULL, con, schema = "platform") {
  # Get table ID from database
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)

  # Get dimension structure if not provided
  if (is.null(dim_structure)) {
    dim_structure <- extract_dimension_structure(code)$dimensions
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
      dplyr::mutate(tab_dim_id = tab_dim_id,
                    dimension = dim_name, .before = 1)
  })

  result
}

#' Prepare table to insert into `series` table
#'
#' This is a big one. Prepares the table to insert into the series table, which
#' along expanding the levels to get all the series and their codes as well, which
#' include also figuring out their time interval, this function also tries to
#' extract the unit for each series,
#' Returns table ready to insert into the `series`table with the
#' db_writing family of functions.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param dim_structure  output from extract_dimension_structure() (optional, will be computed if not provided)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#'
#' @return a dataframe with the following columns: `series_title`, `series_code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export

prepare_series_table <- function(code, con, dim_structure = NULL, schema = "platform"){
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)

  # Get dimension structure if not provided
  if (is.null(dim_structure)) {
    dim_structure <- extract_dimension_structure(code)
  }

  # Get actual dimension level combinations from database (respects user filtering)
  level_combos <- expand_to_level_codes(tbl_id, con, schema)

  # Get dimension names from database to match with unit_mapping columns
  dim_names <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema)$dimension

  # Rename level_combos columns to match dimension names
  names(level_combos) <- dim_names

  # Check if freq is a dimension (variable interval case)
  has_freq_dimension <- "freq" %in% dim_names

  # If freq is a dimension, we need to handle the join differently
  if (has_freq_dimension) {
    # Rename freq to interval in level_combos to match unit_mapping
    level_combos <- level_combos |>
      dplyr::rename(interval = freq)
    # Update dim_names
    dim_names[dim_names == "freq"] <- "interval"
  }

  # Handle units and intervals
  if (!is.null(dim_structure$unit_mapping)) {
    # Join to get units (and possibly intervals)
    series_with_units <- level_combos |>
      dplyr::left_join(dim_structure$unit_mapping, by = dim_names)

    # If no freq dimension, add constant interval
    if (!has_freq_dimension) {
      series_with_units$interval <- dim_structure$interval
    }
  } else {
    # No unit dimension - prompt user for each unique combination
    message("Table '", code, "' has no unit dimension. Please specify units for each series type.")

    unique_combos <- level_combos |>
      dplyr::distinct()

    # Prompt for each unique combination
    units <- character(nrow(unique_combos))
    for (i in seq_len(nrow(unique_combos))) {
      combo_desc <- paste(names(unique_combos), unique_combos[i, ], sep = "=", collapse = ", ")
      prompt_msg <- sprintf("Enter unit for: %s", combo_desc)
      units[i] <- readline(prompt = paste0(prompt_msg, ": "))
    }

    unique_combos$unit <- units

    series_with_units <- level_combos |>
      dplyr::left_join(unique_combos, by = dim_names)

    # Add interval
    if (!has_freq_dimension) {
      if (is.null(dim_structure$interval)) {
        stop("Table has no constant interval and no interval in mapping. Cannot proceed.")
      }
      series_with_units$interval <- dim_structure$interval
    }
  }

  # Remove interval from dim_names for series code construction (it goes at the end)
  dim_names_for_code <- setdiff(dim_names, "interval")

  # Build series table
  series_with_units |>
    dplyr::rowwise() |>
    dplyr::mutate(unit_id = get_umar_unit_id(unit, con, schema)) |>
    dplyr::ungroup() |>
    tidyr::unite("internal_code", dplyr::all_of(dim_names_for_code), sep = "--", remove = FALSE) |>
    dplyr::mutate(internal_code = paste0("EUROSTAT--", code, "--",
                                         internal_code, "--", interval)) |>
    cbind(expand_to_series_titles(tbl_id, con, schema)) |>
    dplyr::mutate(table_id = tbl_id) |>
    dplyr::rename(code = internal_code,
                  interval_id = interval) |>
    dplyr::select(table_id, name_long, unit_id, code, interval_id)
}




#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions.
#'
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @return a dataframe with the `series_id`, `tab_dim_id`, `level_value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(code, con, schema = "platform") {
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)
  dimz <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(is_time != TRUE) |>
    dplyr::pull(id)

  UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(table_id == tbl_id) |>
    dplyr::select(table_id, id, code)  |>
    tidyr::separate(code, into = c("x1", "x2", paste0(dimz), "x3"), sep = "--") |>
    dplyr::select(series_id = id,  paste0(dimz)) |>
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id") |>
    dplyr::rename(level_value = value) |>
    dplyr::mutate(tab_dim_id = as.numeric(tab_dim_id)) |>
    as.data.frame()
}




