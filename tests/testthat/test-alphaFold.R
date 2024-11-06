

test_that("prediction request success", {
  accession = "P00520"

  prediction <- pull_prediction(qualifier = accession)

  testthat::expect_type(prediction, "list")

  testthat::expect_identical(prediction[[1]]$entryId, "AF-P00520-F1")
  testthat::expect_identical(prediction[[1]]$gene, "Abl1")
  testthat::expect_identical(prediction[[1]]$uniprotEnd, 1123L)
  testthat::expect_identical(
    prediction[[1]]$cifUrl,
    "https://alphafold.ebi.ac.uk/files/AF-P00520-F1-model_v4.cif"
  )
  testthat::expect_identical(
    prediction[[1]]$pdbUrl,
    "https://alphafold.ebi.ac.uk/files/AF-P00520-F1-model_v4.pdb"
  )
  testthat::expect_identical(
    prediction[[1]]$paeImageUrl,
    "https://alphafold.ebi.ac.uk/files/AF-P00520-F1-predicted_aligned_error_v4.png"
  )

})

test_that("prediction request failure", {
  accession = "garbage"

  testthat::expect_error(
    pull_prediction(qualifier = accession),
    "Invalid accession. Qualifier must be a valid Uniprot accession."
  )

})

test_that("summary request success", {
  accession = "P00520"

  summary <- pull_summary(qualifier = accession)

  testthat::expect_type(summary, "list")
  testthat::expect_type(summary$uniprot_entry, "list")
  testthat::expect_type(summary$structures, "list")

  testthat::expect_identical(summary$uniprot_entry$ac, accession)
  testthat::expect_identical(summary$uniprot_entry$id, "ABL1_MOUSE")
  testthat::expect_identical(summary$uniprot_entry$sequence_length, 1123L)
  testthat::expect_identical(
    summary$structures[[1]]$summary$model_url,
    "https://alphafold.ebi.ac.uk/files/AF-P00520-F1-model_v4.cif"
  )
  testthat::expect_identical(
    summary$structures[[1]]$summary$confidence_avg_local_score,
    63.73
  )

})

test_that("summary request failure", {
  accession = "garbage"

  testthat::expect_error(
    pull_summary(qualifier = accession),
    "Invalid accession. Qualifier must be a valid Uniprot accession."
  )

})


