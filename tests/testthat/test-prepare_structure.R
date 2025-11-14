test_that("prepare source table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_source_table(con, schema = "platform")
    expect_equal(nrow(out), 1)
    expect_equal(out$id, 7)
  })
})

test_that("prepare table table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    table_table <- prepare_table_table("agr_r_animal", toc,  con_test)
    expect_equal(nrow(table_table), 1)
    expect_equal(ncol(table_table), 6)
    expect_true(all(names(table_table) %in%
                      c("name", "notes", "source_id", "url", "code", "keep_vintage")))
  })
})

test_that("prepare_category_table_table returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    # Test dataset with multiple hierarchy paths (3 parent categories)
    result <- prepare_category_table_table("agr_r_animal", toc, con_test, source_id = 7)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("category_id", "table_id", "source_id") %in% names(result)))
    expect_equal(nrow(result), 3)  # Three different hierarchy paths
    expect_equal(result$source_id, c(7, 7, 7))

    # Check that category_ids are the leaf categories
    expect_true(all(result$category_id %in% c(5, 11, 16)))  # The three leaf categories

    # Check table_id is consistent
    expect_equal(length(unique(result$table_id)), 1)
  })
})

test_that("prepare_category_table_table handles single hierarchy dataset", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    # Test dataset with single hierarchy path
    result <- prepare_category_table_table("teicp000", toc, con_test, source_id = 7)

    expect_equal(nrow(result), 1)  # Only one hierarchy path
    expect_equal(result$source_id, 7)
    expect_equal(result$category_id, 19)  # Consumer prices leaf category
  })
})

test_that("prepare_table_dimensions_table returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Pre-compute dim_structure to avoid downloading in test
    dim_structure <- list(
      unit = tibble::tibble(level_value = c("PC", "EUR"), level_text = c("Percent", "Euro")),
      geo = tibble::tibble(level_value = c("AT", "BE"), level_text = c("Austria", "Belgium"))
    )

    result <- prepare_table_dimensions_table("teicp000", dim_structure, con_test)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("table_id", "dimension", "is_time") %in% names(result)))
    expect_equal(nrow(result), 2)  # Two dimensions
    expect_equal(result$dimension, c("unit", "geo"))
    expect_true(all(result$is_time == FALSE))
  })
})

test_that("prepare_table_dimensions_table computes dim_structure if not provided", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Don't provide dim_structure, function should compute it
    result <- prepare_table_dimensions_table("teicp000", con = con_test)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_dimension_levels_table returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    dim_structure <- list(
      unit = tibble::tibble(level_value = c("PC", "EUR"), level_text = c("Percent", "Euro")),
      geo = tibble::tibble(level_value = c("AT", "BE", "SI"), level_text = c("Austria", "Belgium", "Slovenia"))
    )

    result <- prepare_dimension_levels_table("teicp000", dim_structure, con_test)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("tab_dim_id", "level_value", "level_text") %in% names(result)))
    expect_equal(nrow(result), 5)  # 2 units + 3 geos

    # Check tab_dim_id is present and valid
    expect_true(all(!is.na(result$tab_dim_id)))
  })
})

test_that("prepare_dimension_levels_table computes dim_structure if not provided", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_dimension_levels_table("teicp000", con = con_test)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_series_table returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_table("teicp000", con_test)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("table_id", "name_long", "unit_id", "code", "interval_id") %in% names(result)))

    # Check data types
    expect_type(result$table_id, "double")
    expect_type(result$name_long, "character")
    expect_type(result$unit_id, "integer")
    expect_type(result$code, "character")
    expect_type(result$interval_id, "character")

    # Should have rows
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_series_table generates correct series codes", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_table("teicp000", con_test)

    # All codes should start with EUROSTAT--teicp000--
    expect_true(all(startsWith(result$code, "EUROSTAT--teicp000--")))

    # All codes should end with interval (e.g., --M or --A)
    expect_true(all(grepl("--[MAQSFD]$", result$code)))

    # Codes should have dimension values in middle
    expect_true(all(grepl("--.*--.*--[MAQSFD]$", result$code)))
  })
})

test_that("prepare_series_table handles tables with unit dimension", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Table with unit dimension
    result <- prepare_series_table("teicp000", con_test)

    # Should have unit_ids
    expect_true(all(!is.na(result$unit_id)))

    # Should successfully map units
    expect_true(all(result$unit_id > 0))
  })
})

test_that("prepare_series_table handles tables with indicator dimension", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Table with indic_et dimension (no unit dimension)
    result <- prepare_series_table("ext_lt_introeu27_2020", con_test)

    # Should have unit_ids
    expect_true(all(!is.na(result$unit_id)))

    # Should successfully map units from indicator
    expect_true(all(result$unit_id > 0))
  })
})

test_that("prepare_series_table handles single frequency tables", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_table("teicp000", con_test)

    # All series should have same interval
    expect_equal(length(unique(result$interval_id)), 1)
  })
})

test_that("prepare_series_table handles multiple frequency tables", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    result <- prepare_series_table("avia_gooc", con_test)

    # Should have multiple intervals
    expect_true(length(unique(result$interval_id)) > 1)

    # Each series code should have its specific interval at the end
    purrr::walk(seq_len(nrow(result)), function(i) {
      expected_suffix <- paste0("--", result$interval_id[i])
      expect_true(endsWith(result$code[i], expected_suffix))
    })
  })
})

test_that("prepare_series_table uses provided dim_structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Pre-compute dim_structure
    dim_struct <- extract_dimension_structure("teicp000")

    result <- prepare_series_table("teicp000", con_test, dim_structure = dim_struct)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_series_table computes dim_structure if not provided", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Don't provide dim_structure
    result <- prepare_series_table("teicp000", con_test)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_series_table respects dimension level filtering", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # This assumes dimension_selector was used in import_structure
    # to filter dimension levels before calling prepare_series_table
    # The test verifies it uses database levels, not all available levels

    result <- prepare_series_table("teicp000", con_test)

    # Get what's in database
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
    db_combos <- expand_to_level_codes(tbl_id, con_test)

    # Should have same number of series as dimension level combinations in DB
    expect_equal(nrow(result), nrow(db_combos))
  })
})

test_that("prepare_series_table creates unique series codes", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_table("teicp000", con_test)

    # All series codes should be unique
    expect_equal(nrow(result), length(unique(result$code)))
  })
})

# Alternative approach using local_mocked_bindings (testthat 3.0+)
test_that("prepare_series_table handles user input for units", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Create a counter for sequential responses
    responses <- rep("PC", 7)
    counter <- 0

    testthat::local_mocked_bindings(
      readline = function(prompt) {
        counter <<- counter + 1
        responses[counter]
      },
      .package = "base"  # Specify that readline is from base package
    )

    result <- prepare_series_table("teimf040", con_test)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_series_levels_table returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_levels_table("teicp000", con_test)

    expect_s3_class(result, "data.frame")
    expect_true(all(c("series_id", "tab_dim_id", "level_value") %in% names(result)))

    # Check data types
    expect_type(result$series_id, "double")
    expect_type(result$tab_dim_id, "double")
    expect_type(result$level_value, "character")

    # Should have rows
    expect_true(nrow(result) > 0)
  })
})

test_that("prepare_series_levels_table creates correct number of rows", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_levels_table("teicp000", con_test)

    # Get number of series and dimensions
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
    series_table <- prepare_series_table("teicp000", con_test)
    dims <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con_test, "platform")

    n_series <- nrow(series_table)
    n_dims <- nrow(dims)

    # Should have n_series * n_dims rows (one row per series per dimension)
    expect_equal(nrow(result), n_series * n_dims)
  })
})

test_that("prepare_series_levels_table has entries for all series", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_levels_table("teicp000", con_test)

    # Get all series IDs from series table
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
    series_ids_from_db <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con_test, "platform")

    # All series should be represented
    expect_true(all(unlist(series_ids_from_db) %in% result$series_id))
  })
})

test_that("prepare_series_levels_table has entries for all dimensions", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_levels_table("teicp000", con_test)

    # Get all tab_dim_ids
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
    dims <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con_test, "platform")

    # All dimensions should be represented
    expect_true(all(dims$id %in% result$tab_dim_id))
  })
})

test_that("prepare_series_levels_table has correct number of entries per series", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_levels_table("teicp000", con_test)

    # Get number of dimensions
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
    dims <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con_test, "platform")
    n_dims <- nrow(dims)

    # Each series should have exactly n_dims entries
    entries_per_series <- result |>
      dplyr::count(series_id)

    expect_true(all(entries_per_series$n == n_dims))
  })
})

test_that("prepare_series_levels_table respects dimension filtering", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # If dimension levels were filtered in import_structure,
    # series_levels should only reference those filtered levels

    result <- prepare_series_levels_table("teicp000", con_test)

    # Get dimension levels from database
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")
    db_levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con_test, "platform")

    # All level_value entries should exist in database
    result_level_combos <- result |>
      dplyr::select(tab_dim_id, level_value) |>
      dplyr::distinct()

    db_level_combos <- db_levels |>
      dplyr::select(tab_dim_id, level_value) |>
      dplyr::distinct()

    expect_true(all(
      paste(result_level_combos$tab_dim_id, result_level_combos$level_value) %in%
        paste(db_level_combos$tab_dim_id, db_level_combos$level_value)
    ))
  })
})

test_that("prepare_series_levels_table creates unique series-dimension combinations", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    result <- prepare_series_levels_table("teicp000", con_test)

    # Each (series_id, tab_dim_id) pair should be unique
    unique_combos <- result |>
      dplyr::select(series_id, tab_dim_id) |>
      dplyr::distinct()

    expect_equal(nrow(result), nrow(unique_combos))
  })
})


test_that("prepare_series_levels_table handles multi-dimension tables", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Table with multiple dimensions
    result <- prepare_series_levels_table("agr_r_animal", con_test)

    # Should have multiple tab_dim_ids represented
    expect_true(length(unique(result$tab_dim_id)) > 1)
  })
})
