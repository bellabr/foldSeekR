

#' Visualizes a Prediction.
#'
#' Produces a visual of the AlphaFold prediction for this Uniprot accession.
#'
#' @param accession A Uniprot Accession number (numeric or string).
#'
#' @returns A r3dmol object which displays itself in the Viewer tab showing the
#' protein of interest structure spinning. Different types of helices, and folds
#' are coloured separately. The 3D-representation is interactable with the cursor.
#'
#' @examples
#' # Example 1
#' visualize_prediction("P00520")
#'
#' @export
#' @import bio3d
#' @import r3dmol
#' @import BiocFileCache
visualize_prediction <- function(accession) {

  bfc <- BiocFileCache(accession)

  pdb_url <- pull_url(accession, "pdb")
  pdb_file <- BiocFileCache::bfcrpath(bfc,
                                      rnames = basename(pdb_url),
                                      fpath = pdb_url)
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


#' Visualizes FoldSeek Predictions.
#'
#' Produces a visual of the AlphaFold prediction and the closest FoldSeek
#' predictions for this accession.
#'
#' @param accession A Uniprot Accession number (numeric or string).
#'
#' @examples
#' # Example 1
#' visualize_foldseeks("P00520")
#'
#' @import bio3d
#' @import r3dmol
#' @import BiocFileCache
visualize_foldseeks <- function(accession, cache) {

  # pull files from cache
  bfc <- BiocFileCache(accession)

  pdb_url <- pull_url(accession, "pdb")
  pdb_file <- BiocFileCache::bfcrpath(bfc,
                                      rnames = accession,
                                      fpath = pdb_url)
  acc_pdb <- bio3d::read.pdb(pdb_file)

  acc_mol <- r3dmol::r3dmol() %>%
    r3dmol::m_add_model(data = r3dmol::m_bio3d(acc_pdb),
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

  mols <- list(acc_mol)
  for (file in cache) {
    pdb <- bio3d::read.pdb(file)

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

    mols <- append(mols, list(mol))
  }

  grid_size = ceiling(log2(k + 1))

  grid <- m_grid(
    viewer = mols,
    rows = grid_size,
    cols = grid_size,
    control_all = TRUE,
    viewer_config = m_viewer_spec(
      backgroundColor = "lightblue"
    )
  )
  return(grid)
}

# [END]
