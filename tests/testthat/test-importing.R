
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


test_that("EUROSTAT_import_data_points works correctly", {
  with_mock_db({
    con <- make_test_connection()
    result <- EUROSTAT_import_data_points("teimf040", con, schema = "platform")
    expect_true(all(names(result) == c("vintages", "data")))
    expect_true(all(names(result$data) == c("periods_inserted", "datapoints_inserted",
                                            "flags_inserted")))
    expect_true(result$data$datapoints_inserted == 12)
  })
})


test_that("EUROSTAT_import_data_points works correctly", {
  with_mock_db({
    con <- make_test_connection()
    expect_message(
      result <- EUROSTAT_import_data_points("teina200", con, schema = "platform"),
      "These vintages for table teina200 are not new"
    )
  })
})


