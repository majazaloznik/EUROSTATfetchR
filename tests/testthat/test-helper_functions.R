
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



# Tests
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



