
#' Visualizes a Prediction
#'
#' Produces a visual of the AlphaFold prediction for this Uniprot accession.
#'
#' @param qualifier A Uniprot Accession number (numeric or string).
#'
#' @returns A r3dmol object which displays itself in the Viewer tab showing the
#' protein of interest structure spinning. Different types of helices, and folds
#' are coloured separately. The 3D-representation is interactable with the cursor.
#'
#' @examples
#' # Example 1
#' visualize_prediction(qualifier = "P00520")
#'
#' @export
#' @import bio3d
#' @import r3dmol
#' @import BiocFileCache
visualize_prediction <- function(qualifier) {
  if (!requireNamespace("bio3d", quietly = TRUE) ||
      !requireNamespace("r3dmol", quietly = TRUE) ||
      !requireNamespace("BiocFileCache", quietly = TRUE) ) {
    stop("`visualize_prediction()` requires package 'bio3d', 'r3dmol', 'BiocFileCache'")
  }

  prediction <- pull_prediction(qualifier)
  cache <- BiocFileCache()

  pdb_url <- prediction[[1]]$pdbUrl
  pdb_file <- BiocFileCache::bfcrpath(cache, rnames = basename(pdb_url), fpath = pdb_url)
  pdb <- bio3d::read.pdb(pdb_file)

  mol <- r3dmol::r3dmol() %>%
    r3dmol::m_add_model(data = r3dmol::m_bio3d(pdb),
                        format = "pdb") %>%
    r3dmol::m_zoom_to() %>%
    r3dmol::m_set_style(
      style = r3dmol::m_style_cartoon(color = "#00cc96")
    ) %>%
    r3dmol::m_set_style(
      sel = r3dmol::m_sel(ss = "s"),
      style = r3dmol::m_style_cartoon(color = "#636efa", arrows = TRUE)
    ) %>%
    r3dmol::m_set_style(
      sel = r3dmol::m_sel(ss = "h"),
      style = r3dmol::m_style_cartoon(color = "#ff7f0e")
    ) %>%
    r3dmol::m_spin()
  return(mol)
}


#' Produces a visual of the AlphaFold prediction and the closest FoldSeek
#' predictions for this Uniprot accession.
#'
#' @param qualifier A Uniprot Accession number (numeric or string).
#'
#' @examples
#' # Example 1
#' visualize_foldseeks(qualifier = "P00520")
#'
#' @import bio3d
#' @import r3dmol
#' @import BiocFileCache
visualize_foldseeks <- function(qualifier) {
  if (!requireNamespace("bio3d", quietly = TRUE) ||
      !requireNamespace("r3dmol", quietly = TRUE) ||
      !requireNamespace("BiocFileCache", quietly = TRUE) ) {
    stop("`visualize_prediction()` requires package 'bio3d', 'r3dmol', 'BiocFileCache'")
  }

  closest_three <- fold_seek()

  prediction <- pull_prediction(qualifier)

  pdb_url <- prediction[[1]]$pdbUrl
  pdb_file <- BiocFileCache::bfcrpath(cache, rnames = basename(pdb_url), fpath = pdb_url)
  pdb <- bio3d::read.pdb(pdb_file)

  pred_mol <- r3dmol::r3dmol() %>%
    r3dmol::m_add_model(data = r3dmol::m_bio3d(pdb),
                        format = "pdb") %>%
    r3dmol::m_zoom_to() %>%
    r3dmol::m_set_style(
      style = r3dmol::m_style_cartoon(color = "#00cc96")
    ) %>%
    r3dmol::m_set_style(
      sel = r3dmol::m_sel(ss = "s"),
      style = r3dmol::m_style_cartoon(color = "#636efa", arrows = TRUE)
    ) %>%
    r3dmol::m_set_style(
      sel = r3dmol::m_sel(ss = "h"),
      style = r3dmol::m_style_cartoon(color = "#ff7f0e")
    ) %>%
    r3dmol::m_spin()



  m_grid(
    viewer = list(m1, m2, m3, m4),
    rows = 2,
    cols = 2,
    control_all = TRUE,
    viewer_config = m_viewer_spec(
      backgroundColor = "lightblue"
    )
  )

}


