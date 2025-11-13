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
    dplyr::summarise(last.update.of.data = dplyr::first(last.update.of.data)) |>
    dplyr::pull(last.update.of.data) |>
    as.Date(format = "%d.%m.%Y")
  last_published <- UMARaccessR::sql_get_last_publication_date_from_table_id(tbl_id, con, schema)
  if(!is.null(last_published) && published == last_published) {
    stop(paste0("These vintages for table ", px_code,
                " are not new, they will not be inserted again."))
  } else {
    series_ids <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema)
    data.frame(series_ids, published) |>
      dplyr::rename(series_id = id)
  }
}
