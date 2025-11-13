test_that("prepare_vintage_table works", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    toc <- eurostat::get_eurostat_toc()
    result <- prepare_vintage_table("agr_r_animal", con_test, toc)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) == 30)
    expect_true(all(names(result) == c("series_id", "published")))
  })
})
