# Test for get_next_category_id
test_that("get_next_category_id returns correct next ID", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test when categories exist for source
    next_id <- get_next_category_id(con_test, source_id = 7)
    expect_type(next_id, "double")
    expect_equal(next_id, 20)  # Based on your current max of 19

    # Test when no categories exist for source
    next_id_new <- get_next_category_id(con_test, source_id = 99)
    expect_equal(next_id_new, 0)  # Should start at 0 for new source
  })
})

# Test for get_dataset_ancestors
test_that("get_dataset_ancestors extracts correct hierarchy", {
  # This one doesn't need database mocking, just TOC data
  toc <- eurostat::get_eurostat_toc()
  ancestors <- get_dataset_ancestors("agr_r_animal", toc)

  expect_s3_class(ancestors, "data.frame")
  expect_true(all(c("title", "hierarchy", "parent_title", "instance") %in% names(ancestors)))
  expect_equal(nrow(ancestors), 17)  # Based on your example

  # Check first row is root
  expect_equal(ancestors$parent_title[1], "Eurostat")
  expect_equal(ancestors$hierarchy[1], 0)

  # Check multiple instances exist
  expect_equal(length(unique(ancestors$instance)), 3)

  # Test single instance dataset
  ancestors_single <- get_dataset_ancestors("teicp000", toc)
  expect_equal(nrow(ancestors_single), 4)
  expect_equal(length(unique(ancestors_single$instance)), 1)
})

test_that("get_dataset_ancestors fails for non-existent dataset", {
  toc <- eurostat::get_eurostat_toc()
  expect_error(
    get_dataset_ancestors("fake_dataset_code", toc),
    "Dataset not found in TOC"
  )
})

# Test for category_exists_with_parent
test_that("category_exists_with_parent finds existing categories", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test root category (parent_id = 0)
    cat_id <- category_exists_with_parent(con_test, "Database by themes", 0, 7)
    expect_equal(cat_id, 1)

    # Test non-root category with specific parent
    cat_id <- category_exists_with_parent(con_test, "Agriculture", 6, 7)
    expect_equal(cat_id, 7)

    # Test category with different parent (should find different ID)
    cat_id <- category_exists_with_parent(con_test, "Agriculture", 15, 7)
    expect_equal(cat_id, 16)

    # Test non-existent category
    cat_id <- category_exists_with_parent(con_test, "Non-existent category", 0, 7)
    expect_null(cat_id)
  })
})
