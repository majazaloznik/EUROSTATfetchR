test_that("EUROSTAT_import_structure imports categories and relationships correctly", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    EUROSTAT_import_structure(con_test, "agr_r_animal", source_id = 7)
    EUROSTAT_import_structure(con_test, "teicp000", source_id = 7)
    # Should not error on re-import (idempotent)
    expect_silent(EUROSTAT_import_structure(con_test, "teicp000", source_id = 7))
    cats <- dplyr::tbl(con_test, "category") |>
      dplyr::filter(source_id == 7) |>
      dplyr::collect()
    expect_true(nrow(cats) == 20)
    relz <- dplyr::tbl(con_test, "category_relationship") |>
      dplyr::filter(source_id == 7) |>
      dplyr::collect()
    expect_true(nrow(relz) == 19)
    expect_true(relz |> dplyr::filter(parent_id == 0) |>  nrow() == 2)
  })
})

test_that("EUROSTAT_import_structure fails gracefully for invalid dataset", {
  toc <- eurostat::get_eurostat_toc()
  con_test <- make_test_connection()
  expect_error(EUROSTAT_import_structure(con_test, "fake_dataset", source_id = 7),
    "Dataset not found in TOC")
})
