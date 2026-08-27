# Test for get_next_category_id
test_that("get_next_category_id returns correct next ID", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    # Test when categories exist for source
    next_id <- get_next_category_id(con_test, source_id = 7)
    expect_type(next_id, "double")
    expect_equal(next_id, 74)  # Based on your current max of 73

    # Test when no categories exist for source
    next_id_new <- get_next_category_id(con_test, source_id = 99)
    expect_equal(next_id_new, 0)  # Should start at 0 for new source
  })
})

# Test for get_dataset_ancestors
test_that("get_dataset_ancestors extracts correct hierarchy", {
  # This one doesn't need database mocking, just TOC data
  toc <- eurostat::get_eurostat_toc()
  ancestors <- get_dataset_ancestors("apro_mt_ls_r", toc)

  expect_s3_class(ancestors, "data.frame")
  expect_true(all(c("title", "hierarchy", "parent_title", "instance") %in% names(ancestors)))
  expect_equal(nrow(ancestors), 16)  # Based on your example

  # Check first row is root
  expect_equal(ancestors$parent_title[1], "Eurostat")
  expect_equal(ancestors$hierarchy[1], 0)

  # Check multiple instances exist
  expect_equal(length(unique(ancestors$instance)), 3)

  # Test single instance dataset
  ancestors_single <- get_dataset_ancestors("teiis550", toc)
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
    expect_equal(cat_id, 74)

    # Test non-existent category
    cat_id <- category_exists_with_parent(con_test, "Non-existent category", 0, 7)
    expect_null(cat_id)
  })
})

# ---------------------------------------------------------------------------
# REFACTOR NEEDED FIRST
# ---------------------------------------------------------------------------
# fetch_toc() currently does fetch + parse together, so it can't be tested
# against a local fixture without hitting the network. Split the pure parsing
# into parse_toc(doc), which takes a parsed xml document and returns the same
# list (nodes, edges, creation_date). fetch_toc() then just downloads, hashes,
# reads the xml, and calls parse_toc(). Only parse_toc is unit-tested here;
# the network fetch in fetch_toc stays untested (it's a thin download + sha).
#
#   parse_toc <- function(doc) {
#     ns   <- c(nt = "urn:eu.europa.ec.eurostat.navtree")
#     tree <- xml2::xml_root(doc)
#     ...  (everything from creation_date down to building nodes/edges) ...
#     list(nodes = nodes, edges = edges, creation_date = creation_date)
#   }
#
#   fetch_toc <- function(url = "...") {
#     tmp <- tempfile(fileext = ".xml")
#     httr2::request(url) |> httr2::req_perform(path = tmp)
#     sha <- digest::digest(file = tmp, algo = "sha256")
#     res <- parse_toc(xml2::read_xml(tmp))
#     c(res, list(sha = sha))
#   }
# ---------------------------------------------------------------------------

test_that("parse_toc builds the correct node set", {
  doc <- xml2::read_xml(testthat::test_path("fixtures", "toc_fixture.xml"))
  res <- parse_toc(doc)

  # 6 distinct nodes: economy, ei_bcs, teieuro_bs, popul (folders/branches),
  # ei_bssi_m_r2, demo_x (datasets), teibs020 (table) — teibs020 deduped to one
  expect_equal(nrow(res$nodes), 7)
  expect_false(any(duplicated(res$nodes$code)))

  # types assigned correctly
  types <- setNames(res$nodes$type, res$nodes$code)
  expect_equal(unname(types["economy"]),      "folder")
  expect_equal(unname(types["ei_bcs"]),        "folder")
  expect_equal(unname(types["ei_bssi_m_r2"]),  "dataset")
  expect_equal(unname(types["teibs020"]),      "table")

  # english title chosen
  titles <- setNames(res$nodes$title, res$nodes$code)
  expect_equal(unname(titles["ei_bssi_m_r2"]), "Sentiment indicators monthly")

  # missing english title -> NA, not the fr/de fallback
  expect_true(is.na(titles["demo_x"]))
})

test_that("parse_toc captures the edge structure including multi-parent", {
  doc <- xml2::read_xml(testthat::test_path("fixtures", "toc_fixture.xml"))
  res <- parse_toc(doc)
  # edges: economy->ei_bcs, economy->teieuro_bs, ei_bcs->ei_bssi_m_r2,
  #        ei_bssi_m_r2->teibs020, teieuro_bs->teibs020, popul->demo_x = 6
  expect_equal(nrow(res$edges), 6)

  # multi-parent: teibs020 has two parents
  parents_of_teibs <- res$edges$parent_code[res$edges$child_code == "teibs020"]
  expect_setequal(parents_of_teibs, c("ei_bssi_m_r2", "teieuro_bs"))

  # table nested under dataset: the ei_bssi_m_r2 -> teibs020 edge exists
  expect_true(any(res$edges$parent_code == "ei_bssi_m_r2" &
                    res$edges$child_code  == "teibs020"))

  # top-level branches (economy, popul) have no parent edge
  expect_false("economy" %in% res$edges$child_code)
  expect_false("popul"   %in% res$edges$child_code)
})

test_that("parse_toc parses the creationDate attribute", {
  doc <- xml2::read_xml(testthat::test_path("fixtures", "toc_fixture.xml"))
  res <- parse_toc(doc)

  expect_s3_class(res$creation_date, "POSIXct")
  expect_equal(format(res$creation_date, "%Y-%m-%d %H:%M", tz = "UTC"),
               "2026-08-27 00:11")
})
