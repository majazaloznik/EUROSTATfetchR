devtools::install_github("majazaloznik/UMARaccessR")
devtools::install_github("majazaloznik/UMARimportR")
source("tests/testthat/helper-connection.R")

# Capture fixture for get_next_category_id with existing categories
dittodb::start_db_capturing()
con_test <- make_test_connection()
get_next_category_id(con_test, source_id = 7)
get_next_category_id(con_test, source_id = 99)
dittodb::stop_db_capturing()

# Capture fixture for category_exists_with_parent
dittodb::start_db_capturing()
con_test <- make_test_connection()
category_exists_with_parent(con_test, "Database by themes", 0, 7)
category_exists_with_parent(con_test, "Agriculture", 6, 7)
category_exists_with_parent(con_test, "Agriculture", 15, 7)
category_exists_with_parent(con_test, "Non-existent category", 0, 7)
dittodb::stop_db_capturing()


start_db_capturing()
con <- make_connection()
out <- prepare_source_table(con, schema = "platform")
stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()
EUROSTAT_import_structure(con_test, "agr_r_animal",  source_id = 7)
EUROSTAT_import_structure(con_test, "teicp000", source_id = 7)
EUROSTAT_import_structure(con_test, "teicp000", source_id = 7)
dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()
DBI::dbExecute(con_test, "set search_path to platform")
cats <- dplyr::tbl(con_test, "category") |>
  dplyr::filter(source_id == 7) |>
  dplyr::collect()
relz <- dplyr::tbl(con_test, "category_relationship") |>
  dplyr::filter(source_id == 7) |>
  dplyr::collect()
dittodb::stop_db_capturing()
