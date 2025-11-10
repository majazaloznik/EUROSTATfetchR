#' Import structure for a single dataset (categories, relationships, and table info)
#'
#' @param con Database connection object
#' @param dataset_code Character, Eurostat dataset code
#' @param source_id Integer source ID, default 7 for Eurostat
#' @param schema defautls to "platform"
#'
#' @export
EUROSTAT_import_structure <- function(con, dataset_code, source_id = 7, schema = "platform") {
  toc <- eurostat::get_eurostat_toc()

  # insert categories and category relationships
  ancestors <- get_dataset_ancestors(dataset_code, toc)

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
        UMARimportR::insert_new_category(con, cat_df, schema)
      }

      # ALWAYS try to insert relationship (even if category already exists)
      # ON CONFLICT DO NOTHING will handle duplicates
      rel_df <- data.frame(id = cat_id, parent_id = parent_id, source_id = source_id)
      UMARimportR::insert_new_category_relationship(con, rel_df, schema)

      parent_id <- cat_id
    }
  }
}
