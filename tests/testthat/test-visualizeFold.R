

test_that("visualize fold success", {
  accession = "P00520"

  mol <- visualize_prediction(accession)

  testthat::expect_type(mol, "list")

  testthat::expect_type(mol$x$id, "character")
  testthat::expect_type(mol$x$api, "list")
})



