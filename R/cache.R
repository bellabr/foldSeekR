
#' Caches top hits for a job.
#'
#' Saves the top hits to cache for visualization. Currently, the only database
#' supported due to the constraints of varying fragile database architectures
#' is AlphaFold Database Proteome.
#'
#' @param job A job run via the foldseek() function.
#' @param k Number of top hits to save for visualization.
#' @param accession The Uniprot Accession associated with the storage location.
#'
#' @returns .
#'
#' @examples
#' # Example 1
#' job <- foldseek("./path/to/test.cif")
#' cache_top_hits(job, 3, "P00520")
#'
#' @export
#' @import BiocFileCache
cache_top_hits <- function(job, k, accession) {

  database = "afdb-proteome"

  available = job$results$db
  db_idx = match(database, available)

  # if (missing(database)) {
  #   database = job$results$db[2]
  #   db_idx = 2
  # } else {
  #   available = job$results$db
  #   db_idx = match(database, available)
  # }

  header <- job$queries$header
  seq <- job$queries$sequence

  alignments <- job$results$alignments[[db_idx]][[1]]
  # top_k <- alignments[c(1:k),]

  bfc <- BiocFileCache(accession)

  cache = list()

  ctr = 0
  while(TRUE) {
    split <- strsplit(alignments[i,]$target, split="-")[[1]]
    if (!(length(split) <= 1)) {
      if (nchar(split[1]) <= 6) {
        access <- split[2]
      } else {
        access <- split[1]
      }
    } else {
      split <- strsplit(alignments[i,]$target, split="_")[[1]]
      if (!(length(split) <= 1)) {
        access <- split[1]
      }
    }

    # skip original target
    if (access != accession) {
      pdb_url <- foldSeekR::pull_url(access, "pdb")
      if (!is.null(pdb_url)) {
        pdb_file <- BiocFileCache::bfcrpath(bfc,
                                            rnames = access,
                                            fpath = pdb_url)
        cache <- append(cache, pdb_file)
        cat("Cached", access, "successfully.\n")
        ctr = ctr + 1
      } else {
        cat("No PDB available for", access, ".\n")
      }
      if (ctr >= k || i > nrow(alignments)) {
        break
      }
    }
    i = i + 1
  }
  return(cache)
}


#' Clears the cache for an accession.
#'
#' Clears the cache for the Uniprot accession in which the FoldSeek similarity
#' search was conducted on.
#'
#' @param accession The Uniprot Accession associated with the storage location.
#'
#' @examples
#' # Example 1
#' clear_cache("P00520")
#'
#' @export
#' @import BiocFileCache
clear_cache <- function(accession) {
  bfc <- BiocFileCache(accession)
  BiocFileCache::removebfc(bfc, ask=FALSE)
  return(invisible(NULL))
}

# [END]
