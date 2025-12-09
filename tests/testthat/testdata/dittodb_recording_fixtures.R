# devtools::install_github("majazaloznik/UMARaccessR")
# devtools::install_github("majazaloznik/UMARimportR")
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

start_db_capturing()
con_test <- make_test_connection()
toc <- get_eurostat_toc()
table_table <- prepare_table_table("agr_r_animal", toc, con_test)
stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()
toc <- eurostat::get_eurostat_toc()

# Now capture the prepare_category_table_table calls
prepare_category_table_table("agr_r_animal", toc, con_test, source_id = 7)
prepare_category_table_table("teicp000", toc, con_test, source_id = 7)
dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()

# Prepare dimension structure once to use for both functions
dim_structure_teicp <- extract_dimension_structure("teicp000")
dim_structure_agr <- extract_dimension_structure("agr_r_animal")

# Mock dim_structure for controlled testing
dim_structure_mock <- list(
  unit = tibble::tibble(level_value = c("PC", "EUR"), level_text = c("Percent", "Euro")),
  geo = tibble::tibble(level_value = c("AT", "BE", "SI"), level_text = c("Austria", "Belgium", "Slovenia"))
)

# Capture prepare_table_dimensions_table
prepare_table_dimensions_table("teicp000", dim_structure_mock, con_test)
prepare_table_dimensions_table("teicp000", con = con_test)  # Without dim_structure

# Capture prepare_dimension_levels_table
prepare_dimension_levels_table("teicp000", dim_structure_mock, con_test)
prepare_dimension_levels_table("teicp000", con = con_test)  # Without dim_structure

dittodb::stop_db_capturing()



dittodb::start_db_capturing()
con_test <- make_test_connection()

# Capture queries for mapped units
# Make sure these units are in your eurostat_unit_map first!
get_umar_unit_id("PC", con_test)
get_umar_unit_id("I15", con_test)
get_umar_unit_id("I20", con_test)
get_umar_unit_id("EUR", con_test)
get_umar_unit_id("THS", con_test)

dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()

# Explicit mappings
get_umar_unit_id("EUR", con_test)
get_umar_unit_id("THS", con_test)
get_umar_unit_id("PC", con_test)

# Pattern-based (these will call sql_get_unit_id_from_name with "percentage" or "index")
get_umar_unit_id("PC_GDP", con_test)
get_umar_unit_id("PC_POP", con_test)
get_umar_unit_id("PCH_M1", con_test)
get_umar_unit_id("I15", con_test)
get_umar_unit_id("I05", con_test)

dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con_test <- make_test_connection()

# Get table IDs
tbl_id_teicp <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
tbl_id_agr <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "agr_r_animal", "platform")

# Capture expand_to_level_codes
EUROSTATfetchR:::expand_to_level_codes(tbl_id_teicp, con_test)
EUROSTATfetchR:::expand_to_level_codes(tbl_id_agr, con_test)

# Capture expand_to_series_titles
EUROSTATfetchR:::expand_to_series_titles(tbl_id_teicp, con_test)
EUROSTATfetchR:::expand_to_series_titles(tbl_id_agr, con_test)

dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()

# Single frequency table
prepare_series_table("teicp000", con_test)

# Indicator dimension table
prepare_series_table("ext_lt_introeu27_2020", con_test)

# With pre-computed dim_structure
dim_struct_teicp <- extract_dimension_structure("teicp000")
prepare_series_table("teicp000", con_test, dim_structure = dim_struct_teicp)

# Multi-frequency table (if you have one)
prepare_series_table("avia_gooc", con_test)

dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()

# Simple table
prepare_series_levels_table("teicp000", con_test)

# Multi-dimension table
prepare_series_levels_table("agr_r_animal", con_test)

# Table with indicator dimension
prepare_series_levels_table("ext_lt_introeu27_2020", con_test)

# Also need series table queries for cross-referencing
prepare_series_table("teicp000", con_test)
prepare_series_table("agr_r_animal", con_test)

dittodb::stop_db_capturing()



dittodb::start_db_capturing()
con_test <- make_test_connection()
tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
series_ids_from_db <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con_test, "platform")

dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()
EUROSTAT_import_structure(con_test, "teiet215", source_id = 7, all_levels = TRUE)
# Then select minimal levels in the interactive prompt

dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()

prepare_series_table("teimf040", con_test)

dittodb::stop_db_capturing()



dittodb::start_db_capturing()
con_test <- make_test_connection()

prepare_vintage_table("teimf040", con_test, toc)
prepare_vintage_table("teiet215", con_test, toc)
prepare_vintage_table("agr_r_animal", con_test, toc)
dittodb::stop_db_capturing()

dittodb::start_db_capturing()
con_test <- make_test_connection()
x <- prepare_eurostat_data_for_insert("agr_r_animal", con_test, "platform")
dittodb::stop_db_capturing()


dittodb::start_db_capturing()
con_test <- make_test_connection()
x <- EUROSTAT_import_data_points("teimf040", con_test, "platform")
dittodb::stop_db_capturing()

