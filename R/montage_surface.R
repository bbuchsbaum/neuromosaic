#' Render a Statistic Surface Montage
#'
#' Projects a thresholded statistic volume onto a surface atlas using the
#' existing `ce_overlay.R` projection layer and writes a surface montage PNG.
#' The projection layer reuses `.overlay_geom_cache`; this function does not add
#' a second asset cache.
#'
#' @param stat Statistic `NeuroVol` or file path.
#' @param surfatlas Surface atlas with `lh_atlas`/`rh_atlas` geometry.
#' @param output_file PNG output path.
#' @param threshold Positive numeric overlay threshold.
#' @param tail Tail mode.
#' @param signed Logical; use symmetric overlay limits?
#' @param cap Optional shared cap for surface overlay colors.
#' @param views Surface views passed to `neuroatlas::plot_brain()`.
#' @param hemis Hemispheres passed to `neuroatlas::plot_brain()`.
#' @param surface Surface geometry name for `plot_brain()`.
#' @param surface_space,density_override,resolution_override Surface asset
#'   overrides passed through the shared projection layer.
#' @param fun,sampling Projection controls passed to `neurosurf::vol_to_surf()`.
#' @param overlay_alpha Surface overlay alpha.
#' @param palette Base parcel palette.
#' @param overlay_palette Overlay palette.
#' @param width,height,res PNG device settings.
#' @param title,subtitle,caption Plot annotations.
#' @param plot_fun Advanced/testing hook. Defaults to `neuroatlas::plot_brain`.
#' @param projection Advanced/testing hook for a precomputed projection payload.
#' @param empty Action when no suprathreshold voxels are present.
#'
#' @return A `surf_montage_result` list containing the PNG path, projection,
#'   diagnostics, and render metadata.
#' @export
surf_montage <- function(stat,
                         surfatlas,
                         output_file,
                         threshold,
                         tail = c("two_sided", "positive", "negative"),
                         signed = TRUE,
                         cap = NULL,
                         views = c("lateral", "medial"),
                         hemis = c("left", "right"),
                         surface = "inflated",
                         surface_space = "fsLR-32k",
                         density_override = NULL,
                         resolution_override = NULL,
                         fun = c("avg", "nn", "mode"),
                         sampling = c("midpoint", "normal_line", "thickness"),
                         overlay_alpha = 0.45,
                         palette = "cork",
                         overlay_palette = if (isTRUE(signed)) "vik" else "inferno",
                         width = 1400,
                         height = 900,
                         res = 144,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         plot_fun = NULL,
                         projection = NULL,
                         empty = c("error", "warning")) {
  tail <- match.arg(tail)
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)
  empty <- match.arg(empty)

  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0) {
    stop("'threshold' must be a positive number.", call. = FALSE)
  }
  if (!is.logical(signed) || length(signed) != 1L || is.na(signed)) {
    stop("'signed' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(cap) && (!is.numeric(cap) || length(cap) != 1L ||
                        !is.finite(cap) || cap <= 0)) {
    stop("'cap' must be NULL or a positive number.", call. = FALSE)
  }

  stat <- .load_overlay_neurovol(stat, "stat")
  stat_arr <- as.array(stat)
  supra <- .suprathreshold_mask(as.numeric(stat_arr), threshold = threshold,
                                tail = tail)
  n_supra <- sum(supra, na.rm = TRUE)
  if (n_supra == 0L) {
    msg <- paste0(
      "No finite suprathreshold voxels for threshold ", threshold,
      " and tail '", tail, "'."
    )
    if (identical(empty, "error")) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  supra_arr <- array(supra, dim = dim(stat_arr))
  display_arr <- stat_arr
  display_arr[!supra_arr] <- NA_real_
  cap <- cap %||% .stat_montage_default_cap(display_arr, signed = signed)
  display_arr <- .clip_montage_overlay(display_arr, cap = cap, signed = signed)
  display_vol <- neuroim2::NeuroVol(display_arr, space = neuroim2::space(stat))

  projection <- projection %||% .project_cluster_overlay(
    cluster_vol = display_vol,
    surfatlas = surfatlas,
    space_override = surface_space,
    density_override = density_override,
    resolution_override = resolution_override,
    fun = fun,
    sampling = sampling
  )
  diagnostics <- .overlay_projection_diagnostics(
    cluster_vol = display_vol,
    projection = projection,
    threshold = threshold,
    sampling = sampling,
    fun = fun
  )
  overlay <- .clip_surface_overlay(projection$overlay, cap = cap, signed = signed)

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  plot_fun <- plot_fun %||% neuroatlas::plot_brain
  vals <- .surface_base_values(surfatlas)
  overlay_lim <- if (isTRUE(signed)) c(-cap, cap) else c(0, cap)

  grDevices::png(filename = output_file, width = width, height = height, res = res)
  tryCatch({
    p <- plot_fun(
      surfatlas = surfatlas,
      vals = vals,
      views = views,
      hemis = hemis,
      surface = surface,
      palette = palette,
      lim = c(0, 0),
      interactive = FALSE,
      overlay = overlay,
      overlay_threshold = max(abs(threshold), .Machine$double.eps),
      overlay_alpha = overlay_alpha,
      overlay_palette = overlay_palette,
      overlay_lim = overlay_lim,
      colorbar = TRUE,
      colorbar_title = "Statistic",
      title = title,
      subtitle = subtitle,
      caption = caption
    )
    print(p)
  }, finally = grDevices::dev.off())

  structure(
    list(
      image = normalizePath(output_file, mustWork = FALSE),
      overlay = overlay,
      diagnostics = diagnostics,
      threshold = threshold,
      tail = tail,
      signed = signed,
      cap = cap,
      n_suprathreshold = n_supra,
      surface_space = projection$meta$surface_space %||% surface_space,
      views = views,
      hemis = hemis
    ),
    class = "surf_montage_result"
  )
}

.surface_base_values <- function(surfatlas) {
  ids <- suppressWarnings(as.integer(surfatlas$ids))
  ids <- ids[is.finite(ids)]
  if (length(ids) == 0L) {
    return(numeric(0))
  }
  stats::setNames(rep(0, length(ids)), ids)
}

.clip_surface_overlay <- function(overlay, cap, signed) {
  lapply(overlay, function(values) {
    if (is.null(values) || !is.finite(cap) || cap <= 0) {
      return(values)
    }
    .clip_montage_overlay(as.numeric(values), cap = cap, signed = signed)
  })
}
