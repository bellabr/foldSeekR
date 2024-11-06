
FOLDSEEK = "https://search.foldseek.com/api/ticket/"
MMSEQS = "https://search.mmseqs.com/api/"


# curl -X POST -F q="./tests/testthat/Q40674.fasta" -F 'mode=3diaa' -F 'database[]=afdb50' -F 'database[]=afdb-swissprot' -F 'database[]=afdb-proteome' -F 'database[]=bfmd' -F 'database[]=cath50' -F 'database[]=mgnify_esm30' -F 'database[]=pdb100' -F 'database[]=gmgcl_id' https://search.foldseek.com/api/ticket

ticket <- function(filepath,
                   databases = c('afdb50',
                                 'afdb-swissprot',
                                 'afdb-proteome',
                                 'bfmd',
                                 'cath50',
                                 'mgnify_esm30',
                                 'pdb100',
                                 'gmgcl_id')) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`ticket()` requires packages 'httr2', 'jsonlite")
  }
  dbs_vec <- paste("database[]=", databases, sep="")
  dbs_str <- paste(dbs_vec, collapse = "' -F '")

  url <- paste("curl -X POST -F q=", filepath, " -F 'mode=3diaa' -F '",
               dbs_str, "' ", FOLDSEEK)


  # body <- c(
  #   "q" = filepath,
  #   "database[]" = as.list(databases),
  #   "mode" = "3diaa",
  #   "email" = "nil"
  # )
  # body_json <- jsonlite::toJSON(body)
  # print(body_json)

  filesplit = strsplit(filepath, split = "/")
  filename = tail(filesplit[[1]], n=1)

  req <- httr2::request(FOLDSEEK) %>%
    httr2::req_headers(
      "Content-Type" = "multipart/form-data",
      "Accept" = "*/*"
    ) %>%
    httr2::req_body_multipart(
      "q" = curl::form_file(
        path = filepath,
        type = "fasta",
        name = filename
      ),
      "database[]" = curl::form_data(databases),
      "mode" = "3diaa",
      "email" = "nil"
    )
  fasta = ">FASTA\nMPKIIEAIYENGVFKPLQKVDLKEGE\n"
  req <- httr2::request(FOLDSEEK) %>%
    httr2::req_headers(
      "Content-Type" = "application/x-www-form-urlencoded",
      "Accept" = "*/*"
    ) %>%
    httr2::req_body_multipart(
      "q" = fasta,
      "database[]" = curl::form_data(databases),
      "mode" = "3diaa",
      "email" = "nil"
    )

  # print(req)
  resp <- req %>% httr2::req_perform()
  return(resp)

  resp <- req %>%
    httr2::req_error(is_error = \(resp) FALSE) %>%
    httr2::req_perform()

  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)
    return(body)
  } else {
    stop("Job submission failed. Check your filetype, filepath, and/or databases.")
  }






  h <- curl::new_handle()
  curl::handle_setform(h,
                       q = curl::form_file(filepath),
                       mode = '3diaa',
                       database = curl::form_data(serialize(databases, NULL), "list")
  )
  curl::handle_setheaders(h, "Content-Type" = "multipart/form-data")
  req <- curl::curl_fetch_memory(FOLDSEEK, handle = h)

  parse_headers(req$headers)
  cat(rawToChar(req$content))
  str(req)



  if (httr2::resp_status(resp) < 400L) {
    body <- httr2::resp_body_json(resp)
    return(body)
  } else {
    stop("POST command failed. Check your filepath and database args.")
  }

}




#' Queries the given Uniprot accession or FASTA file via FoldSeek and returns
#' the top predictions for closest similarity in structure.
#'
#' @param target A FASTA string or FASTA filepath, or a valid Uniprot Accession
#' number (numeric or string).
#' @param k The top k number of entries most similar to the file are returned.
#'
#' @returns The k-JSON bodies for each of the resulting outputs from the query.
#'
#' @examples
#' # Example 1
#' foldseek(target = "./test.fasta", k = 3)
#'
#' # Example 2
#' foldseek(target = "P00520", k = 5)
#'
#' @import httr2
#' @import BiocFileCache
foldseek <- function(target, k) {

  # while ticket is not complete
  # wait until result fetch goes through
  # requires ticket to be completed above



}

