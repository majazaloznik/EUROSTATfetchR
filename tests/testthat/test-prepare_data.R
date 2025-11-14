test_that("prepare_vintage_table works", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    result <- prepare_vintage_table("agr_r_animal", con_test, toc)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) == 30)
    expect_true(all(names(result) == c("series_id", "published")))
  })
})

test_that("prepare_eurostat_data_for_insert works", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    x <- prepare_eurostat_data_for_insert("agr_r_animal", con_test, "platform")
    expect_true(is.list(x))
    expect_true(length(x) == 4)
    expect_true(all(names(x) == c("data", "table_id", "dimension_ids", "dimension_names")))
    expect_true(all(names(x$data) == c("interval_id", "animals", "unit", "geo",
                                       "flag", "TIME_PERIOD",
                                       "value", "time")))
    expect_true(is.character(x$data$animals))
    expect_true(all(is.na(x$data$flag)))
  })
})

