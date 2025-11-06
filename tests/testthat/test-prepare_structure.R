test_that("prepare source table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_source_table(con, schema = "platform")
    expect_equal(nrow(out), 1)
    expect_equal(out$id, 7)
  })
})
