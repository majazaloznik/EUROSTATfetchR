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


test_that("prepare_category_table_table fails for non-existent dataset", {
  con_test <- make_test_connection()
  toc <- eurostat::get_eurostat_toc()

  expect_error(
    prepare_category_table_table("fake_dataset", toc, con_test, source_id = 7),
    "Dataset not found in TOC"
  )
})
