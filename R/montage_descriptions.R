# Descriptions use applied panel metadata. Missing metadata is not evidence
# that a default projection, palette or slice-selection rule was used.
.montage_report_colour_text <- function(row, meta = list()) {
  if (!is.list(meta)) meta <- list()
  field <- function(name) .profile_row_character(row, 1L, name)
  quantity <- meta$legend_title %||% field("legend_title") %||%
    field("effective_profile_label") %||% field("quantity") %||% "Values"
  units <- meta$units %||% field("effective_units") %||% field("units")
  limits <- meta$limits
  if (is.null(limits) && all(c("effective_lower", "effective_upper") %in% names(row))) {
    limits <- c(row$effective_lower[[1L]], row$effective_upper[[1L]])
  }
  if (is.null(limits) && length(meta$cap) == 1L && is.finite(meta$cap)) {
    limits <- if (isTRUE(meta$signed)) c(-meta$cap, meta$cap) else c(0, meta$cap)
  }
  palette <- meta$palette %||% field("effective_palette_family")
  scale <- paste0(quantity, if (!is.null(units) && nzchar(units)) paste0(" (", units, ")"),
    if (length(limits) == 2L && all(is.finite(limits))) {
      paste0("; colour scale ", paste(vapply(limits, .nm_format_tick, ""), collapse = " to "))
    })
  # Only the known signed report palette guarantees these colour directions.
  semantics <- if (identical(palette, "nm-signed") &&
                   isTRUE(meta$signed %||% row$effective_signed[[1L]])) {
    "positive values in warm colours, negative values in cool colours"
  } else if (length(palette) == 1L && !is.na(palette) && nzchar(palette)) {
    paste0(palette, " palette")
  } else if (length(palette) > 1L) {
    "custom colour palette"
  }
  paste(c(scale, semantics), collapse = "; ")
}

.montage_report_slice_plane <- function(labels) {
  axis <- if (length(labels)) substr(trimws(labels[[1L]]), 1L, 1L) else ""
  switch(axis, x = "Sagittal", y = "Coronal", z = "Axial", "Volume")
}

.montage_report_surface_method <- function(row, panel, fallback = NULL) {
  meta <- panel[["surface"]]
  if (!is.list(meta)) meta <- list()
  method <- meta$method
  if (is.list(method)) return(method)
  # Older/precomputed reports may still identify a direct parcel input.
  if ("parcel_values" %in% names(row) &&
      !.missing_column_values(row$parcel_values)[[1L]]) {
    return(list(source = "parcel_values"))
  }
  if (is.list(fallback)) {
    fallback$source <- fallback$source %||% "volume_projection"
    return(fallback)
  }
  list(source = "unknown")
}

.montage_surface_mapping_text <- function(method, detailed = FALSE) {
  source <- method$source %||% "unknown"
  if (identical(source, "parcel_values")) return("parcel values mapped directly to the cortical surface")
  if (identical(source, "precomputed")) return("precomputed vertex values mapped to the cortical surface")
  if (!identical(source, "volume_projection")) return("surface mapping details not recorded")
  text <- "volume values projected to the cortical surface"
  if (!detailed) return(text)
  depth <- method$depth
  if (identical(method$sampling, "thickness") && length(depth) && all(is.finite(depth))) {
    text <- paste0(text, " at ", length(depth), " depth", if (length(depth) != 1L) "s",
      " from ", .nm_format_tick(100 * min(depth)), "% to ",
      .nm_format_tick(100 * max(depth)), "% of cortical thickness")
  } else if (!is.null(method$sampling)) {
    text <- paste0(text, " using ", method$sampling, " sampling")
  }
  if (!is.null(method$interpolation)) {
    text <- paste0(text, " (", if (identical(method$interpolation, "linear")) "trilinear" else
      method$interpolation, " interpolation)")
  }
  if (!is.null(method$aggregate)) text <- paste0(text, "; aggregation: ", method$aggregate)
  if (!is.null(method$fun)) text <- paste0(text, "; projection function: ", method$fun)
  text
}

.montage_report_surface_text <- function(row, panel, detailed = FALSE, fallback = NULL) {
  meta <- panel[["surface"]]
  if (!is.list(meta)) meta <- list()
  method <- .montage_report_surface_method(row, panel, fallback)
  views <- meta$views
  hemis <- meta$hemis
  extent <- .montage_report_pick(row, "effective_min_cluster_size", "min_cluster_size")
  projected <- identical(method$source, "volume_projection")
  extent_text <- if (detailed && projected && length(extent) == 1L &&
                     is.finite(extent) && extent > 1) {
    paste0(" Voxels of clusters smaller than ", extent,
      " voxels are removed before projection; the remaining volume values are sampled before thresholding.")
  } else if (detailed && projected) {
    " Volume values are sampled before thresholding."
  }
  paste0("Cortical surface",
    if (length(views)) paste0(", ", .montage_report_and(views), " views"),
    if (length(hemis)) paste0(" of the ", .montage_report_and(hemis), " hemisphere",
      if (length(hemis) != 1L) "s"),
    "; ", .montage_surface_mapping_text(method, detailed),
    if (detailed && !is.null(method$space) && !is.na(method$space)) paste0("; mesh: the ", method$space,
      " cortical surface", if (is.finite(method$vertices %||% NA)) paste0(" (", .montage_report_int(method$vertices), " vertices per hemisphere)")),
    if (detailed && !is.null(method$surface) && !is.na(method$surface)) paste0("; drawn on the ", method$surface, " surface"),
    if (detailed && isTRUE(method$medial_wall)) " with the medial wall masked",
    ".", extent_text, " Subcortical and cerebellar effects are not shown.")
}
