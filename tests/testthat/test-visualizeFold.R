

test_that("visualize fold success", {
  accession = "P00520"

  mol <- visualize_prediction(qualifier = accession)

  testthat::expect_type(mol, "list")

  testthat::expect_type(mol$x$id, "character")
  testthat::expect_type(mol$x$api, "list")
})



test_that("visualize fold failure", {
  accession = "garbage"

  testthat::expect_error(
    visualize_prediction(qualifier = accession),
    "Invalid accession. Qualifier must be a valid Uniprot accession."
  )

})
