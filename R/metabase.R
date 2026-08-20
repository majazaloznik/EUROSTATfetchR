#' Get Eurostat metabase structural changes for a snapshot
#'
#' Returns one row per affected dataset for the given metabase snapshot,
#' classifying each change as a whole-dataset event (`dataset_added` /
#' `dataset_removed`) or a within-dataset change (`changed`) with dimension
#' changes and level (position) counts. The `time` dimension is excluded.
#'
#' Wraps the `eurostat.get_metabase_changes` database function.
#'
#' @param con Database connection object
#' @param snapshot_id Integer (or integer64) snapshot identifier
#' @param schema Character string specifying the database schema
#'
#' @return A data frame with columns `dataset`, `event`, `dim_changes`,
#'   `level_added`, `level_removed`; zero rows if the snapshot introduced no
#'   (non-time) structural changes.
#' @export
sql_get_eurostat_metabase_changes_from_snapshot <- function(con, snapshot_id,
                                                            schema = "eurostat") {
  UMARimportR::sql_function_call(
    con,
    "get_metabase_changes",
    list(p_snapshot_id = snapshot_id),
    schema)
}

