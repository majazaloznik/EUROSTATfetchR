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
    toc <- eurostat::get_eurostat_toc()
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
    toc <- eurostat::get_eurostat_toc()

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
    toc <- eurostat::get_eurostat_toc()

    # Test dataset with single hierarchy path
    result <- prepare_category_table_table("teicp000", toc, con_test, source_id = 7)

    expect_equal(nrow(result), 1)  # Only one hierarchy path
    expect_equal(result$source_id, 7)
    expect_equal(result$category_id, 19)  # Consumer prices leaf category
  })
})

# Tests
test_that("extract_dimension_structure returns correct structure", {
  # No database needed for this one
  dim_structure <- extract_dimension_structure("teicp000")

  expect_type(dim_structure, "list")
  expect_true(all(purrr::map_lgl(dim_structure, is.data.frame)))

  # Check each element has correct columns
  purrr::walk(dim_structure, function(df) {
    expect_true(all(c("level_value", "level_text") %in% names(df)))
  })

  # Check no single-value dimensions included
  purrr::walk(dim_structure, function(df) {
    expect_true(nrow(df) > 1)
  })

  # Check dimension names are sensible (not TIME_PERIOD, values, etc)
  expect_false("TIME_PERIOD" %in% names(dim_structure))
  expect_false("values" %in% names(dim_structure))
})

test_that("extract_dimension_structure excludes constant dimensions", {
  # Test with a dataset that might have constant dimensions
  dim_structure <- extract_dimension_structure("agr_r_animal")

  # All dimensions should have multiple values
  n_levels <- purrr::map_int(dim_structure, nrow)
  expect_true(all(n_levels > 1))
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
