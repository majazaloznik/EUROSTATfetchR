

# ---------------------------------------------------------------------------
# test
# ---------------------------------------------------------------------------
test_that("sql_get_eurostat_metabase_changes_from_snapshot works correctly", {
  with_mock_db({
    con <- make_test_connection()
    result <- sql_get_eurostat_metabase_changes_from_snapshot(
      con, 16, schema = "eurostat")

    # shape
    expect_s3_class(result, "data.frame")
    expect_named(result,
                 c("dataset", "event", "dim_changes", "level_added", "level_removed"))

    # every event is one of the three known types
    expect_true(all(result$event %in%
                      c("changed", "dataset_added", "dataset_removed")))

    # no 'time' dimension leaks into dim_changes
    expect_false(any(grepl("time:", result$dim_changes, fixed = TRUE),
                     na.rm = TRUE))
  })
  DBI::dbDisconnect(con)
})

