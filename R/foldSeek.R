
FOLDSEEK_API = "https://search.foldseek.com/api"


#' Displays databases available in FoldSeek.
#'
#' Displays the available databases for the FoldSeek query.
#'
#' @returns A list with headers name, version, and path corresponding to each
#' available database's name, version, and path for the search query.
#'
#' @examples
#' # Example 1
#' available_databases()
#'
#' @export
#' @import httr2
#' @import jsonlite
available_databases <- function() {
  API = file.path(FOLDSEEK_API, "databases")

  # format request
  req <- httr2::request(API) %>%
    httr2::req_headers(
      "Accept" = "*/*"
    )
  # perform request
  resp <- req %>%
    httr2::req_error(is_error = \(resp) FALSE) %>%
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    # pull json
    resp_body <- httr2::resp_body_string(resp)
    body <- jsonlite::fromJSON(resp_body)$databases
    return(body[1:3])
  } else {
    stop("API request failed.")
  }
}


#' Submits a job to FoldSeek.
#'
#' Submits a job to the FoldSeek Search Server via API against all available
#' databases.
#'
#' @param filepath A filepath to a CIF or PDB file, or a string containing the
#' information in said file.
#'
#' @returns Ticket ID corresponding to submitted job.
#'
#' @examples
#' # Example 1
#' ticket("./data/test.cif")
#'
#' @export
#' @import curl
#' @import httr2
#' @import jsonlite
ticket <- function(filepath) {
  # if (!requireNamespace("httr2", quietly = TRUE) ||
  #     !requireNamespace("jsonlite", quietly = TRUE) ||
  #     !requireNamespace("curl", quietly = TRUE)) {
  #   stop("`ticket()` requires packages 'curl', 'httr2', 'jsonlite")
  # }

  databases <- foldSeekR::available_databases()$path

  # TODO: filepath or string contents ?
  filename = basename(filepath)

  if (!(file.exists(filepath))) {
    stop("File does not exist. Check your filetype/filepath.")
  }

  API = file.path(FOLDSEEK_API, "ticket")

  # format request
  req <- httr2::request(API) %>%
    httr2::req_headers(
      "Content-Type" = "multipart/form-data",
      "Accept" = "*/*"
    ) %>%
    httr2::req_body_multipart(
      "q" = curl::form_file(
        path = filepath,
        name = basename(filepath)
      ),
      "database[]" = databases[1],
      "database[]" = databases[2],
      "database[]" = databases[3],
      "database[]" = databases[4],
      "database[]" = databases[5],
      "database[]" = databases[6],
      "database[]" = databases[7],
      "database[]" = databases[8],
      "database[]" = databases[9],
      "mode" = "3diaa"
    )

  # perform request
  resp <- req %>%
    httr2::req_error(is_error = \(resp) FALSE) %>%
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    # pull json
    resp_body <- httr2::resp_body_string(resp)
    body <- jsonlite::fromJSON(resp_body)$id

    return(body)
  } else {
    stop("Request failed.")
  }
}


#' Gets status of job.
#'
#' Returns status of the job for the specified ticket ID.
#'
#' @param ticket_id A ticket ID provided by the job submitted.
#'
#' @returns Status update corresponding to the ticket (ie. "PENDING", "ERROR",
#' or "COMPLETE").
#'
#' @examples
#' # Example 1
#' status("G04AJT4Nl6Pk-o8VzLYklMyOKADAIZwUJvpIyA")
#'
#' @export
#' @import httr2
#' @import jsonlite
status <- function(ticket_id) {
  API = file.path(FOLDSEEK_API, "ticket", ticket_id)

  # format request
  req <- httr2::request(API) %>%
    httr2::req_headers(
      "Accept" = "*/*"
    )
  # perform request
  resp <- req %>%
    httr2::req_error(is_error = \(resp) FALSE) %>%
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    # pull json
    resp_body <- httr2::resp_body_string(resp)
    body <- jsonlite::fromJSON(resp_body)$status
    return(body)
  } else {
    stop("API request failed.")
  }
}


#' Runs a FoldSeek job.
#'
#' Queries the given CIF, mmCIF or PDB file via FoldSeek and returns
#' the job result for closest similarity in structure. This handles the ticket
#' and status portion.
#'
#' @param filepath A  filepath to a valid CIF, mmCIF, or PDB file of a
#' protein of interest.
#'
#' @returns A job data frame of the format
#'    ```
#'    $queries
#'      $header : <target protein header>
#'      $sequence : <target protein sequence>
#'    $mode : <search mode>
#'    $results :
#'      $db : <databases>
#'      $alignments : <hits for each database>
#'        $query : <job>
#'        $target : <hit protein name>
#'        $seqId : <sequence identity>
#'        $alnLength : <alignment length>
#'        $missmatches : <number of mismatches>
#'        $gapsopened : <number of gaps>
#'        $qStartPos : <>
#'        $qEndPos : <>
#'        $dbStartPos : <>
#'        $dbEndPos : <>
#'        $prob : <>
#'        $eval : <>
#'        $score : <>
#'        $qLen : <>
#'        $aAln : <alignment q score>
#'        $tCa : <>
#'        $tSeq : <>
#'        $taxId : <taxonomy id>
#'        $taxName : <taxonomy name>```
#'
#' @examples
#' # Example 1
#' foldseek("./data/test.cif")
#'
#' @export
#' @import httr2
#' @import jsonlite
foldseek <- function(filepath) {

  # datapath <- file.path("./data/", accession)
  # if (!(file.exists(datapath))) {
  #   pdb_url <- pull_url(accession, "pdb")
  #   bfc <- BiocFileCache(file.path("./data/", accession), ask=FALSE)
  #   filepath <- BiocFileCache::bfcrpath(bfc, accession, pdb_url)
  # } else {
  #   filepath <- accession
  # }

  ticket_id <- ticket(filepath)
  while(TRUE) {
    if (status(ticket_id) == "COMPLETE") {
      break
    }
    cat("Pending...")
  }

  API = file.path(FOLDSEEK_API, "result", ticket_id, 1)

  req <- httr2::request(API) %>%
    httr2::req_headers(
      "Accept" = "*/*"
    )
  result <- req %>%
    httr2::req_error(is_error = \(resp) FALSE) %>%
    httr2::req_perform()

  if (httr2::resp_status(result) < 400L) {
    # pull json
    resp_body <- httr2::resp_body_string(result)
    body <- jsonlite::fromJSON(resp_body)
    return(body)
  } else {
    stop("Job failed.")
  }
}

# [END]
