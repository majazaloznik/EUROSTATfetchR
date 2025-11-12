#' Import structure for a single dataset (categories, relationships, and table info)
#'
#' @param con Database connection object
#' @param code Character, Eurostat dataset code
#' @param source_id Integer source ID, default 7 for Eurostat
#' @param schema defautls to "platform"
#' @param all_levels logical whether to let user use dimension selector, default
#' is FALSE, which automatically selects all levels in all dimensions
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @export
EUROSTAT_import_structure <- function(con, code, source_id = 7, schema = "platform",
                                      all_levels = FALSE, keep_vintage = FALSE) {
  message("Importing structure data: ", code, " into schema ", schema)

  # get metadata from eurostat
  toc <- eurostat::get_eurostat_toc()

  # insert categories and category relationships
  ancestors <- get_dataset_ancestors(code, toc)

  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_table <- prepare_table_table(code, toc, con, schema, keep_vintage)
  insert_results$table <- UMARimportR::insert_new_table_table(con, table_table, schema)
  message("Table insert: ", insert_results$table$count, " rows")
  # prepare category and category relationships
  insert_results$category <- 0
  insert_results$category_relationship <- 0
  for (inst in unique(ancestors$instance)) {
    inst_ancestors <- ancestors |>
      dplyr::filter(instance == inst) |>
      dplyr::arrange(hierarchy)
    parent_id <- 0
    for (i in seq_len(nrow(inst_ancestors))) {
      cat_name <- inst_ancestors$title[i]
      cat_id <- category_exists_with_parent(con, cat_name, parent_id, source_id)
      if (is.null(cat_id)) {
        # Create new category
        cat_id <- get_next_category_id(con, source_id)
        cat_df <- data.frame(id = cat_id, name = cat_name, source_id = source_id)
        insert_results$category <- insert_results$category + UMARimportR::insert_new_category(con, cat_df, schema)
      }
      # ALWAYS try to insert relationship (even if category already exists)
      # ON CONFLICT DO NOTHING will handle duplicates
      rel_df <- data.frame(id = cat_id, parent_id = parent_id, source_id = source_id)
      insert_results$category_relationship <-  insert_results$category_relationship +
        UMARimportR::insert_new_category_relationship(con, rel_df, schema)
      parent_id <- cat_id
    }
  }
  message("Category insert: ", insert_results$category, " rows")
  message("Category relationship insert: ", insert_results$category_relationship, " rows")
  # prepare and insert category table table
  category_table_table <- prepare_category_table_table(code, toc, con, source_id, schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # extract dimension data
  dim_struct <- extract_dimension_structure(code)
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(code, dim_struct$dimensions, con, schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table_full <- prepare_dimension_levels_table(code, dim_struct$dimensions, con, schema)
  if(all_levels){
    dimension_levels_table <- dimension_levels_table_full |>
      dplyr::select(-dimension)} else {
        dimension_levels_table <- UMARimportR::dimension_selector(dimension_levels_table_full) |>
          dplyr::select(-dimension)}
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert series table
  series_table <-  prepare_series_table(code, con, dim_struct, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(code, con, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}
