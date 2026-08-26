#' Render a Statistic Surface Montage
#'
#' Renders a statistic volume as a surface overlay by passing the volume to
#' [neuroatlas::plot_brain()], which performs volume-to-surface projection
#' against the supplied surface atlas. Alternatively, pass parcel-valued
#' statistics in `vals` to render directly with `plot_brain(vals = )` without
#' a volume-to-surface projection. The older precomputed projection hook is
#' retained only for compatibility and testing, and is deprecated.
#'
#' @param stat Statistic `NeuroVol` or file path. Leave `NULL` when using
#'   parcel-valued `vals`.
#' @param surfatlas Surface atlas with `lh_atlas`/`rh_atlas` geometry.
#' @param output_file PNG or PDF output path. PDF embeds the rasterized cortical
#'   panels while retaining vector text, orientation marks, and legend.
#' @param threshold Positive numeric overlay threshold, or `NULL`/`NA` for a
#'   continuous map.
#' @param tail Tail mode.
#' @param signed Logical; use symmetric overlay limits?
#' @param cap Optional shared cap for surface overlay colors.
#' @param limits Optional finite increasing display limits.
#' @param support_mask Optional logical/numeric mask in source-value order.
#' @param views Surface views passed to `neuroatlas::plot_brain()`.
#' @param hemis Hemispheres passed to `neuroatlas::plot_brain()`.
#' @param surface Surface geometry name for `plot_brain()`.
#' @param surface_space Surface-space metadata used when `surfatlas` does not
#'   define `surface_space`. The default rendering path uses the atlas geometry
#'   resolved by [neuroatlas::plot_brain()].
#' @param density_override,resolution_override Deprecated manual-projection
#'   controls. They are ignored by the default `plot_brain()` path.
#' @param fun,sampling Projection controls forwarded to
#'   [neuroatlas::plot_brain()] as `overlay_fun` and `overlay_sampling`.
#'   Continuous publication output defaults to five samples through cortical
#'   thickness rather than the historical Gaussian-KNN midpoint proxy.
#' @param interpolation Voxel interpolation for continuous statistics:
#'   `"linear"` (trilinear publication default), `"nearest"`, or the historical
#'   `"legacy"` KNN contract.
#' @param aggregate Aggregation across cortical-depth samples. The publication
#'   default is `"mean"`; categorical `"mode"` remains distinct and is invalid
#'   with linear interpolation.
#' @param depth Explicit white-to-pial fractions. Defaults to five fractions
#'   from 0.1 through 0.9.
#' @param surface_smooth_fwhm Optional tangential smoothing in millimetres.
#'   Zero disables it and is the publication default.
#' @param cortex_mask Optional explicit lh/rh cortex-domain mask forwarded to
#'   the publication renderer. If NULL, neuroatlas resolves atlas provenance
#'   without treating parcel label zero as a universal medial wall.
#' @param cortex_mask_source Provenance label for an explicit mask.
#' @param anatomy_metric Optional lh/rh sulcal-depth or curvature metric.
#' @param anatomy_metric_source Provenance label for an explicit anatomy metric.
#' @param medial_wall Medial-wall display policy.
#' @param camera Strict canonical or slightly oblique presentation camera.
#' @param orientation_labels Draw anterior/posterior marks.
#' @param overlay_alpha Surface overlay alpha. The publication default leaves
#'   a small amount of anatomical context visible beneath the statistic.
#' @param palette Base parcel palette.
#' @param overlay_palette Overlay palette.
#' @param width,height,res PNG device settings. Defaults target a 10 by 6.25
#'   inch, 300-dpi publication raster.
#' @param render_device Output backend: \code{"auto"} selects Cairo PDF for a
#'   `.pdf` path, otherwise prefers \pkg{ragg} when it
#'   is installed and otherwise uses the base PNG device; \code{"ragg"}
#'   requires \pkg{ragg}; \code{"png"} forces the base device; and
#'   \code{"pdf"} forces Cairo PDF.
#' @param title,subtitle,caption Plot annotations.
#' @param plot_fun Advanced/testing hook. Defaults to `neuroatlas::plot_brain`.
#' @param projection Deprecated advanced/testing hook for a precomputed
#'   projection payload.
#' @param empty Action when no suprathreshold voxels are present.
#' @param vals Optional numeric vector of per-parcel statistic values. Named
#'   vectors are matched to `surfatlas$ids`; unnamed vectors must be in
#'   `surfatlas$ids` order.
#'
#' @return A `surf_montage_result` list containing the PNG path, projection,
#'   diagnostics, and render metadata.
#' @export
surf_montage <- function(stat = NULL,
                         surfatlas,
                         output_file,
                         threshold,
                         tail = c("two_sided", "positive", "negative"),
                         signed = TRUE,
                         cap = NULL,
                         limits = NULL,
                         support_mask = NULL,
                         views = c("lateral", "medial"),
                         hemis = c("left", "right"),
                         surface = "inflated",
                         surface_space = "fsLR-32k",
                         density_override = NULL,
                         resolution_override = NULL,
                         fun = c("avg", "nn", "mode"),
                         sampling = c("thickness", "midpoint", "normal_line"),
                         interpolation = c("linear", "nearest", "legacy"),
                         aggregate = c("mean", "closest", "mode"),
                         depth = NULL,
                         surface_smooth_fwhm = 0,
                         cortex_mask = NULL,
                         cortex_mask_source = NULL,
                         anatomy_metric = NULL,
                         anatomy_metric_source = NULL,
                         medial_wall = c("shade", "mask", "outline"),
                         camera = c("canonical", "presentation"),
                         orientation_labels = TRUE,
                         overlay_alpha = 0.85,
                         palette = "cork",
                         overlay_palette = if (isTRUE(signed)) "vik" else "lajolla",
                         width = 3000,
                         height = 1875,
                         res = 300,
                         render_device = c("auto", "ragg", "png", "pdf"),
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         plot_fun = NULL,
                         projection = NULL,
                         empty = c("error", "warning"),
                         vals = NULL) {
  tail <- match.arg(tail)
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)
  interpolation <- match.arg(interpolation)
  aggregate <- match.arg(aggregate)
  medial_wall <- match.arg(medial_wall)
  camera <- match.arg(camera)
  empty <- match.arg(empty)
  render_device <- match.arg(render_device)
  if (identical(interpolation, "linear") && identical(aggregate, "mode")) {
    stop("aggregate = 'mode' is invalid with linear interpolation.",
         call. = FALSE)
  }
  if (!is.numeric(surface_smooth_fwhm) ||
      length(surface_smooth_fwhm) != 1L ||
      !is.finite(surface_smooth_fwhm) || surface_smooth_fwhm < 0) {
    stop("'surface_smooth_fwhm' must be a non-negative numeric scalar.",
         call. = FALSE)
  }
  projection_depth <- if (identical(sampling, "thickness") &&
                          identical(interpolation, "linear") &&
                          is.null(depth)) {
    seq(0.1, 0.9, length.out = 5L)
  } else {
    depth
  }

  has_stat <- !is.null(stat)
  has_vals <- !is.null(vals)
  if (has_stat && has_vals) {
    stop("Supply exactly one of 'stat' or 'vals', not both.", call. = FALSE)
  }
  if (!has_stat && !has_vals) {
    stop("Supply either 'stat' or parcel-valued 'vals'.", call. = FALSE)
  }
  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.", call. = FALSE)
  }
  continuous <- is.null(threshold) ||
    (length(threshold) == 1L && is.na(threshold))
  if (!continuous && (!is.numeric(threshold) || length(threshold) != 1L ||
                      !is.finite(threshold) || threshold <= 0)) {
    stop("'threshold' must be NULL/NA or a positive number.", call. = FALSE)
  }
  if (!is.logical(signed) || length(signed) != 1L || is.na(signed)) {
    stop("'signed' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(cap) && (!is.numeric(cap) || length(cap) != 1L ||
                        !is.finite(cap) || cap <= 0)) {
    stop("'cap' must be NULL or a positive number.", call. = FALSE)
  }
  limits <- .validate_montage_limits(limits)

  if (has_vals && !is.null(projection)) {
    stop("'projection' is only supported with volumetric 'stat'.", call. = FALSE)
  }

  values <- if (has_vals) {
    .surface_parcel_values(vals, surfatlas)
  } else {
    stat <- .load_overlay_neurovol(stat, "stat")
    as.numeric(as.array(stat))
  }
  support <- .normalize_montage_support_mask(support_mask, length(values))
  supra <- if (continuous) {
    is.finite(values)
  } else {
    .suprathreshold_mask(values, threshold = threshold, tail = tail)
  }
  supra <- supra & support
  n_supra <- sum(supra, na.rm = TRUE)
  if (n_supra == 0L) {
    msg <- if (continuous) {
      paste0("No finite display ", if (has_vals) "parcels" else "voxels",
             " for the continuous overlay.")
    } else {
      paste0(
        "No finite suprathreshold ",
        if (has_vals) "parcels" else "voxels",
        " for threshold ", threshold,
        " and tail '", tail, "'."
      )
    }
    if (identical(empty, "error")) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  if (has_vals) {
    display_vals <- values
    display_vals[!supra] <- NA_real_
    if (is.null(limits)) {
      cap <- cap %||% .stat_montage_default_cap(display_vals, signed = signed)
      limits <- if (isTRUE(signed)) c(-cap, cap) else c(0, cap)
    } else {
      cap <- max(abs(limits))
    }
    display_vals <- .clip_montage_limits(display_vals, limits)
    plot_vals <- display_vals
    overlay_payload <- NULL
    result_overlay <- NULL
    surface_space_out <- surfatlas$surface_space %||% surface_space
    diagnostics <- list(
      projection = "parcel_values",
      surface_space = surface_space_out,
      n_parcels = length(values),
      n_suprathreshold_parcels = n_supra
    )
  } else {
    stat_arr <- as.array(stat)
    supra_arr <- array(supra, dim = dim(stat_arr))
    display_arr <- stat_arr
    display_arr[!supra_arr] <- NA_real_
    if (is.null(limits)) {
      cap <- cap %||% .stat_montage_default_cap(display_arr, signed = signed)
      limits <- if (isTRUE(signed)) c(-cap, cap) else c(0, cap)
    } else {
      cap <- max(abs(limits))
    }
    display_arr <- .clip_montage_limits(display_arr, limits)
    display_vol <- neuroim2::NeuroVol(display_arr, space = neuroim2::space(stat))

    # Volume -> surface projection. By default delegate to neuroatlas::plot_brain,
    # which resolves the anatomical geometry that matches the atlas's own surface
    # space and handles the vol_to_surf sampling. The historical in-package
    # projection (.project_cluster_overlay) forced an fsLR-32k geometry; on an
    # fsaverage atlas the vertex counts disagreed and every vertex collapsed to
    # NA, so the panel rendered blank regardless of map intensity (issue #5). The
    # `projection` argument keeps the manual path available as a testing hook.
    if (!is.null(density_override) || !is.null(resolution_override)) {
      .warn_manual_surface_projection_deprecated(
        "Manual surface density/resolution controls"
      )
    }

    if (is.null(projection)) {
      # Hand plot_brain the continuous statistic and let it project, then
      # threshold the projected vertex values (overlay_threshold) and clamp the
      # color scale (overlay_lim). Projecting continuous values and thresholding
      # on the surface gives focal clusters; projecting a pre-masked volume
      # over-dilates because every column touching a suprathreshold voxel lights.
      # plot_brain has no tail channel and gates on |value|, so for a one-sided
      # tail we must drop the wrong-signed voxels here or they would render.
      overlay_arr <- stat_arr
      overlay_arr[!support] <- NA_real_
      if (!continuous && identical(tail, "positive")) {
        overlay_arr[stat_arr < 0] <- NA_real_
      } else if (!continuous && identical(tail, "negative")) {
        overlay_arr[stat_arr > 0] <- NA_real_
      }
      overlay_payload <- neuroim2::NeuroVol(
        overlay_arr, space = neuroim2::space(stat)
      )
      result_overlay <- NULL
      surface_space_out <- surfatlas$surface_space %||% surface_space
      diagnostics <- list(
        projection = "plot_brain",
        projection_fun = fun,
        projection_sampling = sampling,
        projection_interpolation = interpolation,
        projection_aggregate = aggregate,
        projection_depth = projection_depth,
        surface_smooth_fwhm = surface_smooth_fwhm,
        surface_space = surface_space_out,
        cluster_voxels_nonzero = n_supra
      )
    } else {
      .warn_manual_surface_projection_deprecated(
        "`projection` precomputed surface overlay hook"
      )
      diagnostics <- .overlay_projection_diagnostics(
        cluster_vol = display_vol,
        projection = projection,
        threshold = if (continuous) 0 else threshold,
        sampling = sampling,
        fun = fun
      )
      overlay_payload <- .clip_surface_overlay_limits(
        projection$overlay, limits = limits
      )
      result_overlay <- overlay_payload
      surface_space_out <- projection$meta$surface_space %||% surface_space
    }
    plot_vals <- .surface_base_values(surfatlas)
  }

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  plot_fun <- plot_fun %||% neuroatlas::plot_brain
  # Match the color limits to the rendered sign: symmetric for signed maps, and
  # for unsigned maps the half-range that holds the retained tail so negative
  # one-sided effects are not clamped to a [0, cap] scale.
  overlay_lim <- limits

  render_device_used <- .open_surface_montage_device(
    filename = output_file,
    width = width,
    height = height,
    res = res,
    device = render_device,
    background = "#FBFBF8"
  )
  plot_anatomy_provenance <- NULL
  plot_backend <- if (has_vals) "ggplot" else "cpu_barycentric"
  tryCatch({
    plot_args <- list(
      surfatlas = surfatlas,
      vals = plot_vals,
      views = views,
      hemis = hemis,
      surface = surface,
      palette = if (has_vals) overlay_palette else palette,
      lim = if (has_vals) overlay_lim else c(0, 0),
      interactive = FALSE,
      colorbar = TRUE,
      colorbar_title = "Statistic",
      title = title,
      subtitle = subtitle,
      caption = caption
    )
    if (!has_vals) {
      plot_args <- c(plot_args, list(
        style = "stat_publication",
        static_backend = "cpu",
        colorbar_source = "overlay",
        overlay_title = "Statistic",
        overlay = overlay_payload,
        overlay_threshold = if (continuous) NULL else {
          max(abs(threshold), .Machine$double.eps)
        },
        overlay_alpha = overlay_alpha,
        overlay_palette = overlay_palette,
        overlay_lim = overlay_lim,
        overlay_fun = fun,
        overlay_sampling = sampling,
        overlay_interpolation = interpolation,
        overlay_aggregate = aggregate,
        overlay_depth = projection_depth,
        overlay_surface_smooth_fwhm = surface_smooth_fwhm,
        cortex_mask = cortex_mask,
        cortex_mask_source = cortex_mask_source,
        anatomy_metric = anatomy_metric,
        anatomy_metric_source = anatomy_metric_source,
        medial_wall = medial_wall,
        camera = camera,
        orientation_labels = orientation_labels
      ))
    }
    p <- do.call(plot_fun, plot_args)
    plot_anatomy_provenance <- attr(p, "plot_brain_anatomy")
    plot_backend <- attr(p, "plot_brain_backend") %||% plot_backend
    print(p)
  }, finally = grDevices::dev.off())

  structure(
    list(
      image = normalizePath(output_file, mustWork = FALSE),
      overlay = result_overlay,
      diagnostics = diagnostics,
      threshold = if (continuous) NA_real_ else threshold,
      display_mode = if (continuous) "continuous" else "thresholded",
      tail = tail,
      signed = signed,
      cap = cap,
      limits = limits,
      n_suprathreshold = n_supra,
      surface_space = surface_space_out,
      vals = if (has_vals) plot_vals else NULL,
      views = views,
      hemis = hemis,
      render = list(
        device = render_device_used,
        width = width,
        height = height,
        res = res,
        background = "#FBFBF8",
        style = if (has_vals) "parcel_values" else "stat_publication",
        backend = plot_backend,
        colorbar_source = if (has_vals) "base" else "overlay",
        palette = if (has_vals) overlay_palette else palette,
        overlay_palette = if (has_vals) NULL else overlay_palette,
        limits = overlay_lim,
        projection = list(
          interpolation = interpolation,
          sampling = sampling,
          aggregate = aggregate,
          depth = projection_depth,
          surface_smooth_fwhm = surface_smooth_fwhm
        ),
        anatomy = plot_anatomy_provenance,
        medial_wall = medial_wall,
        camera = camera,
        orientation_labels = orientation_labels
      )
    ),
    class = "surf_montage_result"
  )
}

.open_surface_montage_device <- function(filename, width, height, res,
                                         device = c("auto", "ragg", "png",
                                                    "pdf"),
                                         background = "white") {
  device <- match.arg(device)
  is_pdf <- identical(tolower(tools::file_ext(filename)), "pdf")
  if (identical(device, "pdf") || (identical(device, "auto") && is_pdf)) {
    grDevices::cairo_pdf(
      filename = filename,
      width = width / res,
      height = height / res,
      bg = background,
      onefile = FALSE
    )
    return("cairo_pdf")
  }
  if (is_pdf) {
    stop("A '.pdf' output_file requires render_device = 'auto' or 'pdf'.",
         call. = FALSE)
  }
  use_ragg <- identical(device, "ragg") ||
    (identical(device, "auto") && requireNamespace("ragg", quietly = TRUE))

  if (identical(device, "ragg") &&
      !requireNamespace("ragg", quietly = TRUE)) {
    stop(
      "render_device = 'ragg' requires the optional 'ragg' package.",
      call. = FALSE
    )
  }

  if (use_ragg) {
    ragg::agg_png(
      filename = filename,
      width = width,
      height = height,
      units = "px",
      res = res,
      background = background
    )
    return("ragg")
  }

  grDevices::png(
    filename = filename,
    width = width,
    height = height,
    res = res,
    bg = background
  )
  "png"
}

.surface_parcel_values <- function(vals, surfatlas) {
  values <- .coerce_parcel_value_vector(vals, "vals")
  ids <- suppressWarnings(as.integer(surfatlas$ids))
  ids <- ids[is.finite(ids)]
  id_names <- as.character(ids)
  value_names <- names(values)
  has_names <- !is.null(value_names) && any(nzchar(trimws(value_names)))

  if (has_names) {
    if (any(is.na(value_names) | !nzchar(trimws(value_names)))) {
      stop("All named 'vals' entries must have non-empty parcel ids.",
           call. = FALSE)
    }
    if (anyDuplicated(value_names)) {
      stop("'vals' names must be unique parcel ids.", call. = FALSE)
    }
    if (length(id_names) == 0L) {
      return(values)
    }
    unknown <- setdiff(value_names, id_names)
    if (length(unknown) > 0L) {
      stop(
        "'vals' contains parcel id(s) not present in 'surfatlas$ids': ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
    out <- stats::setNames(rep(NA_real_, length(id_names)), id_names)
    out[match(value_names, id_names)] <- values
    return(out)
  }

  if (length(id_names) > 0L && length(values) != length(id_names)) {
    stop(
      "Unnamed 'vals' must have length ", length(id_names),
      " to match 'surfatlas$ids'.",
      call. = FALSE
    )
  }
  if (length(id_names) > 0L) {
    names(values) <- id_names
  }
  values
}

.coerce_parcel_value_vector <- function(values, field, map_id = NULL) {
  if (!is.numeric(values)) {
    where <- if (!is.null(map_id)) paste0(" for map_id '", map_id, "'") else ""
    stop("'", field, "' must be a numeric vector", where, ".", call. = FALSE)
  }
  out <- as.numeric(values)
  names(out) <- names(values)
  if (length(out) == 0L) {
    where <- if (!is.null(map_id)) paste0(" for map_id '", map_id, "'") else ""
    stop("'", field, "' must not be empty", where, ".", call. = FALSE)
  }
  out
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

.clip_surface_overlay_limits <- function(overlay, limits) {
  lapply(overlay, function(values) {
    if (is.null(values)) return(values)
    .clip_montage_limits(as.numeric(values), limits)
  })
}
