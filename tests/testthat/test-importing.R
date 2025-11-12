
test_that("EUROSTAT_import_structure returns correct structure", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Match what we recorded
    result <- EUROSTAT_import_structure(con_test, "teiet215", all_levels = TRUE)

    expect_type(result, "list")
    expect_true(all(c("table", "category", "category_relationship", "category_table",
                      "table_dimensions", "dimension_levels", "series", "series_levels") %in% names(result)))
  })
})
