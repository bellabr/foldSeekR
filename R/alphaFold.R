
ALPHAFOLD_API = "https://alphafold.ebi.ac.uk/api"


#' Pulls the AlphaFold protein structure prediction information.
#'
#' @param accession A Uniprot Accession string.
#'
#' @returns The JSON body returned as a list whereby prediction[[1]] contains
#' the object for the predicted protein architecture. Notably, prediction[[1]]
#' contains entries for entryId, gene, pdbUrl, cifUrl, paeImageUrl, and
#' uniprotSequence.
#'
#' @examples
#' # Example 1
#' pull_prediction("P00520")
#'
#' @export
#' @import httr2
#' @import jsonlite
pull_prediction <- function(accession) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("`prediction()` requires package 'httr2'")
  }

  API = file.path(ALPHAFOLD_API, "prediction", accession)
  resp <- httr2::request(API) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)[[1]]
    return(body)
  } else {
    stop("Invalid accession. Accession must be a valid Uniprot accession.")
  }
}


#' Pulls the AlphaFold protein structure summary information.
#'
#' @param accession A Uniprot Accession string.
#'
#' @returns The JSON body returned as a list whereby summary contains two
#' headers: $structures and $uniprot_entry. These headers contain information
#' about the structure produced (including urls on where to find them) and
#' information about the Uniprot accession entry respectively.
#'
#' @examples
#' # Example 1
#' pull_summary("P00520")
#'
#' @export
#' @import httr2
#' @import jsonlite
pull_summary <- function(accession) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("`prediction()` requires packages 'httr2'")
  }

  url <- paste(ALPHAFOLD_API, "/uniprot/summary/", accession, ".json", sep="")
  resp <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)
    # body <- jsonlite::fromJSON(resp_body)
    return(body)
  } else {
    stop("Invalid accession. Accession must be a valid Uniprot accession.")
  }
}


#' Pulls the URL for the protein.
#'
#' Pulls the AlphaFold protein structure prediction URL for the protein
#' associated with the Uniprot Accession.
#'
#' @param accession A Uniprot Accession string.
#' @param url_type The file type for the file associated with the url.
#'
#' @returns The url for the predicted protein architecture file of the
#' corresponding file type.
#'
#' @examples
#' # Example 1
#' pull_url("P00520", "pdb")
#'
#' # Example 2
#' pull_url("P00520", "cif")
#'
#' @export
#' @import httr2
#' @import jsonlite
pull_url <- function(accession, url_type) {
  if (!(url_type %in% c("pdb", "cif", "bcif"))) {
    stop("URL type may only be one of: pdb, cif, or bcif")
  }

  API = file.path(ALPHAFOLD_API, "prediction", accession)

  resp <- httr2::request(API) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)

    if (url_type == "pdb") {
      return(body[[1]]$pdbUrl)
    } else if (url_type == "cif") {
      return(body[[1]]$cifUrl)
    } else {
      return(body[[1]]$bcifUrl)
    }
  } else {
    return(invisible(NULL))
  }
}

# [END]
