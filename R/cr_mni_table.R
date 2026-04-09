#' Enrich Cluster Table with MNI Coordinates and Atlas Labels
#'
#' Converts peak voxel-grid coordinates to MNI world coordinates and adds
#' atlas region labels via [neuroatlas::query_point()].
#'
#' @param cluster_table Tibble from [build_cluster_explorer_data()] with columns
#'   `cluster_id`, `peak_x`, `peak_y`, `peak_z`, `max_stat`, `n_voxels`, etc.
#' @param stat_map The `NeuroVol` used for clustering (provides the voxel-to-MNI
#'   affine via its `NeuroSpace`).
#' @param atlas A neuroatlas `atlas` object for region labeling.
#' @param max_clusters Maximum number of clusters to keep (sorted by
#'   `abs(max_stat)` descending). Default 20.
#'
#' @return The enriched tibble with additional columns: `peak_mni_x`,
#'   `peak_mni_y`, `peak_mni_z`, `atlas_label`, `hemisphere`, `network`.
#' @export
enrich_cluster_table <- function(cluster_table, stat_map, atlas,
                                 max_clusters = 20L) {
  if (nrow(cluster_table) == 0) {
    cluster_table$peak_mni_x <- numeric(0)
    cluster_table$peak_mni_y <- numeric(0)
    cluster_table$peak_mni_z <- numeric(0)
    cluster_table$atlas_label <- character(0)
    cluster_table$hemisphere <- character(0)
    cluster_table$network <- character(0)
    return(cluster_table)
  }

  # Sort by absolute stat magnitude and cap
  cluster_table <- cluster_table[
    order(abs(cluster_table$max_stat), decreasing = TRUE), ]
  if (nrow(cluster_table) > max_clusters) {
    cluster_table <- cluster_table[seq_len(max_clusters), ]
  }

  # Convert voxel grid to MNI world coordinates
  vol_space <- neuroim2::space(stat_map)
  peak_grid <- as.matrix(cluster_table[, c("peak_x", "peak_y", "peak_z")])
  mni_coords <- neuroim2::grid_to_coord(vol_space, peak_grid)

  cluster_table$peak_mni_x <- round(mni_coords[, 1], 1)
  cluster_table$peak_mni_y <- round(mni_coords[, 2], 1)
  cluster_table$peak_mni_z <- round(mni_coords[, 3], 1)

  # Look up atlas labels at MNI coordinates
  labels <- tryCatch(
    neuroatlas::query_point(mni_coords, atlas),
    error = function(e) {
      warning("Atlas lookup failed: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  if (!is.null(labels) && nrow(labels) > 0) {
    cluster_table$atlas_label <- labels$label
    cluster_table$hemisphere <- if ("hemi" %in% names(labels)) {
      labels$hemi
    } else {
      NA_character_
    }
    cluster_table$network <- if ("network" %in% names(labels)) {
      labels$network
    } else {
      NA_character_
    }
  } else {
    # Fallback: use atlas_label_primary from annotation if available
    cluster_table$atlas_label <- cluster_table$atlas_label_primary %||%
      NA_character_
    cluster_table$hemisphere <- NA_character_
    cluster_table$network <- NA_character_
  }

  cluster_table
}


#' Format MNI Coordinate Table
#'
#' Produces a publication-ready table of cluster peak coordinates with atlas
#' annotations using `gt`, `flextable`, or `kableExtra`.
#'
#' @param cluster_table Enriched cluster table from [enrich_cluster_table()].
#' @param cluster_parcels Optional cluster-parcel overlap tibble from
#'   [build_cluster_explorer_data()].
#' @param style Table rendering backend: `"gt"` (default), `"flextable"`, or
#'   `"kable"`.
#' @param top_parcels Number of top overlapping parcels to show per cluster.
#'   Default 3.
#'
#' @return A `gt_tbl`, `flextable`, or `kable` object ready for printing or
#'   embedding in reports.
#' @export
format_mni_table <- function(cluster_table,
                             cluster_parcels = NULL,
                             style = c("gt", "flextable", "kable"),
                             top_parcels = 3L) {
  style <- match.arg(style)

  if (nrow(cluster_table) == 0) {
    return(NULL)
  }

  # Build display table
  display <- tibble::tibble(
    Cluster = cluster_table$cluster_id,
    Sign = cluster_table$sign,
    `N Voxels` = cluster_table$n_voxels,
    `X (MNI)` = cluster_table$peak_mni_x,
    `Y (MNI)` = cluster_table$peak_mni_y,
    `Z (MNI)` = cluster_table$peak_mni_z,
    `Peak Stat` = round(cluster_table$max_stat, 2),
    Region = cluster_table$atlas_label,
    Hemisphere = cluster_table$hemisphere,
    Network = cluster_table$network
  )

  # Add mean signal if available
  if ("mean_signal" %in% names(cluster_table)) {
    display$`Mean Signal` <- round(cluster_table$mean_signal, 3)
  }
  if ("sd_signal" %in% names(cluster_table)) {
    display$`SD Signal` <- round(cluster_table$sd_signal, 3)
  }

  # Add top parcel overlaps if available
  if (!is.null(cluster_parcels) && nrow(cluster_parcels) > 0) {
    top_labels <- cluster_parcels |>
      dplyr::group_by(.data$cluster_id) |>
      dplyr::slice_max(order_by = .data$frac, n = top_parcels,
                       with_ties = FALSE) |>
      dplyr::summarise(
        `Top Parcels` = paste0(
          .data$parcel_label, " (", round(.data$frac * 100), "%)",
          collapse = ", "
        ),
        .groups = "drop"
      )
    display <- dplyr::left_join(display, top_labels,
                                by = c("Cluster" = "cluster_id"))
  }

  switch(style,
    gt = .format_gt(display),
    flextable = .format_flextable(display),
    kable = .format_kable(display)
  )
}


# -- gt backend ----------------------------------------------------------------

.format_gt <- function(display) {
  tbl <- gt::gt(display) |>
    gt::tab_header(
      title = "Cluster Peak Coordinates",
      subtitle = "MNI coordinates with atlas annotations"
    ) |>
    gt::fmt_number(columns = c("X (MNI)", "Y (MNI)", "Z (MNI)"), decimals = 1) |>
    gt::fmt_number(columns = "Peak Stat", decimals = 2) |>
    gt::cols_align(align = "center",
                   columns = c("Sign", "N Voxels", "X (MNI)", "Y (MNI)",
                               "Z (MNI)", "Peak Stat", "Hemisphere")) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = "Region")
    ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(16),
      heading.subtitle.font.size = gt::px(12),
      column_labels.font.weight = "bold"
    )

  # Color-code sign column
  tbl <- tbl |>
    gt::tab_style(
      style = gt::cell_fill(color = "#E8F5E9"),
      locations = gt::cells_body(
        columns = "Sign",
        rows = Sign == "positive"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#FFEBEE"),
      locations = gt::cells_body(
        columns = "Sign",
        rows = Sign == "negative"
      )
    )

  tbl
}


# -- flextable backend ---------------------------------------------------------

.format_flextable <- function(display) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' required for style = 'flextable'.", call. = FALSE)
  }
  ft <- flextable::flextable(display)
  ft <- flextable::set_header_labels(ft, values = names(display))
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::autofit(ft)
  ft <- flextable::set_caption(ft, caption = "Cluster Peak Coordinates")
  ft
}


# -- kable backend -------------------------------------------------------------

.format_kable <- function(display) {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    return(knitr::kable(display, format = "pipe",
                        caption = "Cluster Peak Coordinates"))
  }
  kableExtra::kbl(display, caption = "Cluster Peak Coordinates") |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 12
    )
}
