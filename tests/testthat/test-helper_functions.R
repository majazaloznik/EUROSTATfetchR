
# Tests
test_that("extract_dimension_structure returns correct structure", {
  # No database needed for this one
  dim_structure <- extract_dimension_structure("teicp000")

  expect_type(dim_structure, "list")
  expect_true(all(purrr::map_lgl(dim_structure$dimensions, is.data.frame)))

  # Check each element has correct columns
  purrr::walk(dim_structure$dimensions, function(df) {
    expect_true(all(c("level_value", "level_text") %in% names(df)))
  })

  # Check no single-value dimensions included
  purrr::walk(dim_structure$dimensions, function(df) {
    expect_true(nrow(df) > 1)
  })

  # Check dimension names are sensible (not TIME_PERIOD, values, etc)
  expect_false("TIME_PERIOD" %in% names(dim_structure$dimensions))
  expect_false("values" %in% names(dim_structure$dimensions))
})

test_that("extract_dimension_structure excludes constant dimensions", {
  # Test with a dataset that might have constant dimensions
  dim_structure <- extract_dimension_structure("agr_r_animal")

  # All dimensions should have multiple values
  n_levels <- purrr::map_int(dim_structure$dimensions, nrow)
  expect_true(all(n_levels > 1))
  interval <- dim_structure$interval
  expect_true(interval == "A")

})

test_that("get_umar_unit_id maps known units correctly", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test common mappings
    unit_id_pc <- get_umar_unit_id("PC", con_test)
    expect_type(unit_id_pc, "integer")

    unit_id_index <- get_umar_unit_id("I15", con_test)
    expect_type(unit_id_index, "integer")

    unit_id_eur <- get_umar_unit_id("EUR", con_test)
    expect_type(unit_id_eur, "integer")
  })
})


test_that("extract_dimension_structure handles single frequency tables", {
  # Using a table with single frequency
  result <- extract_dimension_structure("teicp000")

  expect_type(result, "list")
  expect_true(all(c("dimensions", "interval", "unit_mapping") %in% names(result)))

  # Should have a single interval
  expect_type(result$interval, "character")
  expect_length(result$interval, 1)

  # Dimensions should be a named list of dataframes
  expect_type(result$dimensions, "list")
  purrr::walk(result$dimensions, function(df) {
    expect_s3_class(df, "data.frame")
    expect_true(all(c("level_value", "level_text") %in% names(df)))
  })
})

test_that("extract_dimension_structure handles multiple frequencies", {
  # Need to find a table with multiple frequencies - this is a placeholder
  # # Replace "multi_freq_table" with actual Eurostat code that has M, Q, A
  # skip_if_not(exists("multi_freq_table"), "No multi-frequency table available for testing")

  result <- extract_dimension_structure("avia_gooc")

  # Interval should be NULL
  expect_null(result$interval)

  # unit_mapping should have interval column
  expect_true("interval" %in% names(result$unit_mapping))

  # freq should be a dimension
  expect_true("freq" %in% names(result$dimensions))
})

test_that("extract_dimension_structure handles unit dimension with variation", {
  # Table with unit dimension that varies
  result <- extract_dimension_structure("teicp000")

  expect_false(is.null(result$unit_mapping))
  expect_true("unit" %in% names(result$unit_mapping))

  # unit should be in dimensions too
  expect_true("unit" %in% names(result$dimensions))
})

test_that("extract_dimension_structure handles indicator dimension with variation", {
  # Table with indic_et dimension (like ext_lt_introeu27_2020)
  result <- extract_dimension_structure("ext_lt_introeu27_2020")

  expect_false(is.null(result$unit_mapping))
  expect_true("unit" %in% names(result$unit_mapping))

  # indic_et should be in dimensions
  expect_true("indic_et" %in% names(result$dimensions))
})

test_that("extract_dimension_structure handles constant unit dimension", {
  # Need a table where unit has only one value

  result <- extract_dimension_structure("teilm120")

  # Should still have unit_mapping
  expect_false(is.null(result$unit_mapping))

  # unit should NOT be in dimensions (only one value)
  expect_false("unit" %in% names(result$dimensions))
})

test_that("extract_dimension_structure handles no unit-like dimensions", {
  # Need a table with no unit/indic columns
  result <- extract_dimension_structure("teimf040")

  # unit_mapping should be NULL
  expect_null(result$unit_mapping)
})


test_that("extract_dimension_structure excludes TIME_PERIOD, values, and constant freq", {
  result <- extract_dimension_structure("teicp000")

  # These should not be in dimensions
  expect_false("TIME_PERIOD" %in% names(result$dimensions))
  expect_false("values" %in% names(result$dimensions))
  expect_false("freq" %in% names(result$dimensions))  # Single freq case
})


test_that("get_umar_unit_id fails with helpful message for unmapped units", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    expect_error(
      get_umar_unit_id("UNKNOWN_UNIT_XYZ", con_test),
      "not mapped.*Add to data-raw/eurostat_unit_map.R")
  })
})

test_that("get_umar_unit_id handles multiple units with same mapping", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Different Eurostat codes mapping to same UMAR unit
    unit_id_1 <- get_umar_unit_id("I20", con_test)
    unit_id_2 <- get_umar_unit_id("I15", con_test)

    # Should return same UMAR unit ID (both map to "index")
    expect_equal(unit_id_1, unit_id_2)
  })
})
test_that("get_umar_unit_id maps explicitly defined units", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test units that are in eurostat_unit_map
    unit_id <- get_umar_unit_id("EUR", con_test)
    expect_type(unit_id, "integer")

    unit_id <- get_umar_unit_id("THS", con_test)
    expect_type(unit_id, "integer")
  })
})

test_that("get_umar_unit_id auto-maps PC_ pattern to percentage", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test PC_ pattern
    expect_message(
      unit_id <- get_umar_unit_id("PC_GDP_PC", con_test),
      "Auto-mapped 'PC_GDP_PC' - > 'percentage'"
    )
    expect_type(unit_id, "integer")

    expect_message(
      unit_id <- get_umar_unit_id("PC_POP", con_test),
      "PC_POP.*percentage"
    )
  })
})

test_that("get_umar_unit_id auto-maps PCH_ pattern to percentage", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test PCH_ pattern
    expect_message(
      unit_id <- get_umar_unit_id("PCH_M1", con_test),
      "Auto-mapped 'PCH_M1' - > 'percentage'"
    )
    expect_type(unit_id, "integer")
  })
})

test_that("get_umar_unit_id auto-maps I## pattern to index", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test I## pattern (I followed by exactly 2 digits)
    expect_message(
      unit_id <- get_umar_unit_id("I19", con_test),
      "Auto-mapped 'I19' - > 'index'"
    )
    expect_type(unit_id, "integer")

    expect_message(
      unit_id <- get_umar_unit_id("I05", con_test),
      "I05.*index"
    )
  })
})

test_that("get_umar_unit_id does not auto-map I with non-2-digit patterns", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # I with 1 digit should not match
    expect_error(
      get_umar_unit_id("I5", con_test),
      "not mapped"
    )

    # I with 3 digits should not match
    expect_error(
      get_umar_unit_id("I150", con_test),
      "not mapped"
    )

    # I with letters should not match
    expect_error(
      get_umar_unit_id("IND", con_test),
      "not mapped"
    )
  })
})

test_that("get_umar_unit_id errors on unmapped units", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    expect_error(
      get_umar_unit_id("TOTALLY_UNKNOWN_UNIT", con_test),
      "not mapped.*Add to data-raw/eurostat_unit_map.R"
    )
  })
})

test_that("get_umar_unit_id prefers explicit mapping over pattern matching", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # If PC is explicitly in eurostat_unit_map, it should not show auto-map message
    # Assuming "PC" is in your map
    expect_silent(
      unit_id <- get_umar_unit_id("PC", con_test)
    )
  })
})

test_that("expand_to_level_codes returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Get table ID for a known table
    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")

    result <- expand_to_level_codes(tbl_id, con_test)

    expect_s3_class(result, "data.frame")

    # Should have columns named Var1, Var2, etc.
    expect_true(all(grepl("^Var\\d+$", names(result))))

    # Should have rows (all combinations of dimension levels)
    expect_true(nrow(result) > 0)

    # All values should be character (level codes)
    expect_true(all(purrr::map_lgl(result, is.character)))
  })
})

test_that("expand_to_level_codes creates all combinations", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")

    result <- expand_to_level_codes(tbl_id, con_test)

    # Number of rows should equal product of unique values per dimension
    # Get dimensions to verify
    levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con_test, "platform")
    expected_rows <- levels |>
      dplyr::group_by(tab_dim_id) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::pull(n) |>
      prod()

    expect_equal(nrow(result), expected_rows)
  })
})

test_that("expand_to_series_titles returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")

    result <- expand_to_series_titles(tbl_id, con_test)

    expect_s3_class(result, "data.frame")

    # Should have single column called name_long
    expect_equal(ncol(result), 1)
    expect_equal(names(result), "name_long")

    # Should have rows
    expect_true(nrow(result) > 0)

    # All values should be character
    expect_type(result$name_long, "character")
  })
})

test_that("expand_to_series_titles concatenates with correct separator", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")

    result <- expand_to_series_titles(tbl_id, con_test)

    # All titles should contain the separator " -- "
    # (except possibly single-dimension tables)
    levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con_test, "platform")
    n_dimensions <- length(unique(levels$tab_dim_id))

    if (n_dimensions > 1) {
      expect_true(all(grepl(" -- ", result$name_long)))
    }
  })
})

test_that("expand_to_series_titles has same row count as expand_to_level_codes", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")

    codes <- expand_to_level_codes(tbl_id, con_test)
    titles <- expand_to_series_titles(tbl_id, con_test)

    # Should have same number of rows (same combinations)
    expect_equal(nrow(codes), nrow(titles))
  })
})

test_that("expand_to_series_titles uses level_text not level_value", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con_test, "teicp000", "platform")

    result <- expand_to_series_titles(tbl_id, con_test)

    # Titles should be human-readable text, not codes
    # Check that at least one title doesn't look like a code (has spaces or long words)
    expect_true(any(grepl("[a-z]{4,}", result$name_long, ignore.case = TRUE)))
  })
})

test_that("format_period_id handles annual intervals", {
  dates <- as.Date(c("2023-01-15", "2024-06-30", "2025-12-31"))

  result <- format_period_id(dates, "A")

  expect_equal(result, c("2023", "2024", "2025"))
  expect_type(result, "character")
})

test_that("format_period_id handles monthly intervals", {
  dates <- as.Date(c("2023-01-15", "2023-02-28", "2023-12-31"))

  result <- format_period_id(dates, "M")

  expect_equal(result, c("2023M01", "2023M02", "2023M12"))
  expect_type(result, "character")
})

test_that("format_period_id handles quarterly intervals", {
  dates <- as.Date(c("2023-01-15", "2023-04-01", "2023-07-15", "2023-10-31"))

  result <- format_period_id(dates, "Q")

  expect_equal(result, c("2023Q1", "2023Q2", "2023Q3", "2023Q4"))
  expect_type(result, "character")
})

test_that("format_period_id correctly assigns quarters", {
  # Test all months to ensure correct quarter assignment
  dates <- as.Date(paste0("2023-", sprintf("%02d", 1:12), "-15"))

  result <- format_period_id(dates, "Q")

  expected <- c("2023Q1", "2023Q1", "2023Q1",  # Jan-Mar
                "2023Q2", "2023Q2", "2023Q2",  # Apr-Jun
                "2023Q3", "2023Q3", "2023Q3",  # Jul-Sep
                "2023Q4", "2023Q4", "2023Q4")  # Oct-Dec

  expect_equal(result, expected)
})

test_that("format_period_id handles single date", {
  date <- as.Date("2023-06-15")

  expect_equal(format_period_id(date, "A"), "2023")
  expect_equal(format_period_id(date, "M"), "2023M06")
  expect_equal(format_period_id(date, "Q"), "2023Q2")
})

test_that("format_period_id handles character date input", {
  # Function converts to Date, so should handle character
  dates <- c("2023-01-15", "2023-12-31")

  result_m <- format_period_id(dates, "M")
  result_q <- format_period_id(dates, "Q")
  result_a <- format_period_id(dates, "A")

  expect_equal(result_m, c("2023M01", "2023M12"))
  expect_equal(result_q, c("2023Q1", "2023Q4"))
  expect_equal(result_a, c("2023", "2023"))
})

test_that("format_period_id preserves vector length", {
  dates <- as.Date(c("2023-01-01", "2023-06-15", "2024-12-31"))

  expect_length(format_period_id(dates, "A"), 3)
  expect_length(format_period_id(dates, "M"), 3)
  expect_length(format_period_id(dates, "Q"), 3)
})

test_that("format_period_id handles edge case months for quarters", {
  # Test first and last day of each quarter
  dates <- as.Date(c("2023-01-01", "2023-03-31",  # Q1
                     "2023-04-01", "2023-06-30",  # Q2
                     "2023-07-01", "2023-09-30",  # Q3
                     "2023-10-01", "2023-12-31")) # Q4

  result <- format_period_id(dates, "Q")

  expected <- c("2023Q1", "2023Q1",
                "2023Q2", "2023Q2",
                "2023Q3", "2023Q3",
                "2023Q4", "2023Q4")

  expect_equal(result, expected)
})

test_that("format_period_id handles leap year February", {
  dates <- as.Date(c("2024-02-29", "2023-02-28"))

  result <- format_period_id(dates, "M")

  expect_equal(result, c("2024M02", "2023M02"))
})

test_that("format_period_id errors on unsupported interval", {
  date <- as.Date("2023-01-15")

  expect_error(
    format_period_id(date, "D"),
    "Unsupported interval: D"
  )

  expect_error(
    format_period_id(date, "W"),
    "Unsupported interval: W"
  )

  expect_error(
    format_period_id(date, "X"),
    "Unsupported interval"
  )
})

test_that("format_period_id handles empty input", {
  dates <- as.Date(character(0))

  expect_equal(format_period_id(dates, "A"), character(0))
  expect_equal(format_period_id(dates, "M"), character(0))
  expect_equal(format_period_id(dates, "Q"), character(0))
})

test_that("format_period_id pads months with leading zeros", {
  dates <- as.Date(c("2023-01-01", "2023-09-01"))

  result <- format_period_id(dates, "M")

  # Should have leading zero for single-digit months
  expect_equal(result, c("2023M01", "2023M09"))
  expect_true(all(nchar(result) == 7))  # YYYYMNN format
})

test_that("format_period_id handles different years correctly", {
  dates <- as.Date(c("2020-06-15", "2021-06-15", "2022-06-15", "2023-06-15"))

  result_m <- format_period_id(dates, "M")
  result_q <- format_period_id(dates, "Q")
  result_a <- format_period_id(dates, "A")

  expect_equal(result_m, c("2020M06", "2021M06", "2022M06", "2023M06"))
  expect_equal(result_q, c("2020Q2", "2021Q2", "2022Q2", "2023Q2"))
  expect_equal(result_a, c("2020", "2021", "2022", "2023"))
})

test_that("get_eurostat_calendar works", {
  x <- get_eurostat_calendar("2025-11-01", "2025-12-01")
expect_equal(dim(x), c(160,12))
})
