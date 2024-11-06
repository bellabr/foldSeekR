ALPHAFOLD = "https://alphafold.ebi.ac.uk/api/"


#' Pulls the AlphaFold protein structure prediction information.
#'
#' @param qualifier A Uniprot Accession number (numeric or string).
#'
#' @returns The JSON body returned as a list whereby prediction[[1]] contains
#' the object for the predicted protein architecture. Notably, prediction[[1]]
#' contains entries for entryId, gene, pdbUrl, cifUrl, paeImageUrl, and
#' uniprotSequence.
#'
#' @examples
#' # Example 1
#' pull_prediction(qualifier = "P00520")
#'
#' @export
#' @import httr2
pull_prediction <- function(qualifier) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("`prediction()` requires package 'httr2'")
  }

  url <- paste(ALPHAFOLD, "prediction/", qualifier, sep="")
  resp <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)
    return(body)
  } else {
    stop("Invalid accession. Qualifier must be a valid Uniprot accession.")
  }
}


#' Pulls the AlphaFold protein structure summary information.
#'
#' @param qualifier A Uniprot Accession number (numeric or string).
#'
#' @returns The JSON body returned as a list whereby summary contains two
#' headers: $structures and $uniprot_entry. These headers contain information
#' about the structure produced (including urls on where to find them) and
#' information about the Uniprot accession entry respectively.
#'
#' @examples
#' # Example 1
#' pull_summary(qualifier = "P00520")
#'
#' @export
#' @import httr2
pull_summary <- function(qualifier) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("`prediction()` requires packages 'httr2'")
  }

  url <- paste(ALPHAFOLD, "uniprot/summary/", qualifier, ".json", sep="")
  resp <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)
    return(body)
  } else {
    stop("Invalid accession. Qualifier must be a valid Uniprot accession.")
  }
}

