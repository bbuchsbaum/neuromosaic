#' Plot Orthographic Brain Slices at a Cluster Peak
#'
#' Renders three orthogonal brain slices (axial, coronal, sagittal) from
#' the stat map, centred on the cluster peak MNI coordinate with crosshairs.
#'
#' @param stat_map A `NeuroVol` containing the statistical map.
#' @param peak_mni Numeric vector of length 3 giving the MNI (x, y, z)
#'   coordinate of the cluster peak.
#' @param cmap Color map name passed to [neuroim2::plot_ortho()].
#'   Default `"inferno"`.
#' @param crosshair Logical; draw crosshairs at the peak? Default `TRUE`.
#' @param title Optional character title printed above the slices.
#'
#' @return A named list of `ggplot` objects for the axial, coronal, and
#'   sagittal views produced by [neuroim2::plot_ortho()].
#' @export
plot_cluster_slices <- function(stat_map,
                                peak_mni,
                                cmap = "inferno",
                                crosshair = TRUE,
                                title = NULL) {
  assertthat::assert_that(
    methods::is(stat_map, "NeuroVol"),
    msg = "'stat_map' must be a NeuroVol."
  )
  assertthat::assert_that(
    is.numeric(peak_mni) && length(peak_mni) == 3,
    msg = "'peak_mni' must be a numeric vector of length 3."
  )

  p <- neuroim2::plot_ortho(
    vol       = stat_map,
    coord     = matrix(peak_mni, nrow = 1),
    unit      = "mm",
    cmap      = cmap,
    crosshair = crosshair
  )

  if (!is.null(title) && is.list(p)) {
    p <- lapply(p, function(view) {
      if (inherits(view, "gg")) {
        view + ggplot2::labs(subtitle = title)
      } else {
        view
      }
    })
  }

  p
}


#' Generate Brain Slice Plots for All Clusters
#'
#' @param stat_map A `NeuroVol` containing the statistical map.
#' @param cluster_table Enriched cluster table with `peak_mni_x`,
#'   `peak_mni_y`, `peak_mni_z` columns.
#' @param cmap Color map name. Default `"inferno"`.
#' @param crosshair Logical; draw crosshairs? Default `TRUE`.
#'
#' @return Named list of orthographic-view lists, one per `cluster_id`.
#' @export
plot_all_cluster_slices <- function(stat_map, cluster_table,
                                    cmap = "inferno", crosshair = TRUE) {
  if (nrow(cluster_table) == 0) return(list())

  slices <- stats::setNames(
    vector("list", nrow(cluster_table)),
    cluster_table$cluster_id
  )

  for (i in seq_len(nrow(cluster_table))) {
    row <- cluster_table[i, , drop = FALSE]
    peak <- c(row$peak_mni_x, row$peak_mni_y, row$peak_mni_z)
    label <- cluster_display_label(row)

    slices[[row$cluster_id]] <- tryCatch(
      plot_cluster_slices(
        stat_map  = stat_map,
        peak_mni  = peak,
        cmap      = cmap,
        crosshair = crosshair,
        title     = label
      ),
      error = function(e) {
        warning("Brain slice plot failed for ", row$cluster_id, ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  slices
}
