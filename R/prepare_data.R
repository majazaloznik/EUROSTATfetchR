#' Prepare table to insert into `vintage` table
#'
#' Helper function that populates the vintage table with the new vintages. It gets
#' the series id's from the database and adds the publication date from the px.
#'
#' Returns table ready to insert into the `vintage`table with the
#' UMARimportr::insert family of functions.
#'
#' @param code Character, Eurostat dataset code
#' @param con a connection to the database
#' @param toc output from eurostat::get_eurostat_toc()
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with the `series_id` and `published` columns
#' for all the series in this table.
#' @export
#'
prepare_vintage_table <- function(code, con, toc, schema = "platform"){
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)
  # get metadata from eurostat
  published <- toc |>
    dplyr::filter(code == !!code) |>
    dplyr::summarise(last.update.of.data = dplyr::first(`last update of data`)) |>
    dplyr::pull(last.update.of.data) |>
    as.Date(format = "%d.%m.%Y")
  last_published <- UMARaccessR::sql_get_last_publication_date_from_table_id(tbl_id, con, schema)
  if(!is.null(last_published) && published == last_published) {
    stop(paste0("These vintages for table ", code,
                " are not new, they will not be inserted again."))
  } else {
    series_ids <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema)
    data.frame(series_ids, published) |>
      dplyr::rename(series_id = id)
  }
}





#' Prepare EUROSTAT data table for insertion
#'
#' Processes raw EUROSTAT data into a format ready for database insertion
#'
#' @param code Character, Eurostat dataset code
#' @param con Database connection
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return A list containing:
#'  - data: The processed data frame
#'  - table_id: The table ID
#'  - interval_id: The interval ID
#'  - dimension_ids: The non-time dimension IDs
#'  - dimension_names: The names of the dimensions
#' @export
prepare_eurostat_data_for_insert <- function(code, con, schema = "platform") {
  # Get raw data
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)
  raw <- eurostat::get_eurostat(code, keepFlags = TRUE)

  # remove levels that we are not tracking
  dim_levels_in_db <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema)
  dim_list <- split(dim_levels_in_db$level_value, dim_levels_in_db$dimension)
  names(dim_list)<- make.names(names(dim_list))
  dim_cols <- names(dim_list)
  keep_rows <- rep(TRUE, nrow(raw))
  for (col in dim_cols) {
    keep_rows <- keep_rows & raw[[col]] %in% dim_list[[col]]
  }
  df <- raw[keep_rows, ] |>
    dplyr::rename(value = values,
                  interval_id = freq,
                  flag = flags) |>
    dplyr::mutate(flag = tolower(flag)) |>
    dplyr::rowwise() |>
    dplyr::mutate(time = format_period_id(TIME_PERIOD, interval_id))

  # Get metadata
  dim_ids <- UMARaccessR::sql_get_non_time_dimensions_from_table_id(tbl_id, con, schema)

  # Return structured result
  list(
    data = df,
    table_id = tbl_id,
    dimension_ids = dim_ids$id,
    dimension_names = dim_ids$dimension
  )
}
