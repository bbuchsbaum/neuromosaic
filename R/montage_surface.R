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
#' @param appearance Publication shading or FreeSurfer-style cortical contrast
#'   for vertex overlays. The latter defaults to opaque heat colors.
#' @param anatomy_style Continuous or binary folding contrast. NULL uses the
#'   appearance default. These options apply to the CPU vertex renderer.
#' @param anatomy_midpoint,anatomy_invert Anatomical boundary and polarity;
#'   see [neurosurf::normalize_surface_anatomy()].
#' @param medial_wall Medial-wall display policy.
#' @param camera Strict canonical or slightly oblique presentation camera.
#'   Honoured by volumetric `stat` maps and by the default CPU parcel
#'   renderer; `parcel_backend = "ggplot"` always renders canonically and
#'   warns on `"presentation"`. `medial_wall` and `orientation_labels` apply
#'   only to `stat` maps.
#' @param orientation_labels Draw anterior/posterior marks.
#' @param overlay_alpha Surface overlay alpha. The publication default leaves
#'   a small amount of anatomical context visible beneath the statistic.
#' @param palette Base parcel palette.
#' @param overlay_palette Overlay palette. `"nm-signed"` (the default for
#'   signed maps) is the report's threshold-saturated diverging palette; it is
#'   drawn opaque above threshold with no near-threshold fade.
#' @param min_cluster_size Optional minimum cluster extent in voxels for a
#'   thresholded volumetric `stat`; smaller suprathreshold clusters
#'   (sign-separated, using `connectivity`) are removed before projection.
#' @param connectivity Voxel connectivity for `min_cluster_size`.
#' @param key `"report"` (default) draws a compact horizontal colour key
#'   below the views with rounded ticks and the hidden sub-threshold band
#'   hatched; `"plot"` keeps `plot_brain()`'s own colour bar. Parcel-valued
#'   maps always use `plot_brain()`'s colour bar.
#' @param layout `"strip"` (default, with `key = "report"` and a volumetric
#'   `stat`) composes the four views as one row (left lateral, left medial,
#'   right medial, right lateral) with small view labels and A/P marks derived
#'   from the camera geometry, at the report figure width (`width`/`height`/
#'   `res` are then chosen by the strip, 1760 px at 144 dpi). `"grid"` keeps
#'   `plot_brain()`'s 2 x 2 arrangement.
#' @param width,height,res PNG device settings. Defaults target a 10 by 6.25
#'   inch, 300-dpi publication raster.
#' @param render_device Output backend: \code{"auto"} selects Cairo PDF for a
#'   `.pdf` path, otherwise prefers \pkg{ragg} when it
#'   is installed and otherwise uses the base PNG device; \code{"ragg"}
#'   requires \pkg{ragg}; \code{"png"} forces the base device; and
#'   \code{"pdf"} forces Cairo PDF.
#' @param title,subtitle,caption Plot annotations.
#' @param legend_title Quantity name for static and interactive legends,
#'   separate from the panel `title`. `NULL` defaults to `"Statistic"`.
#' @param units Optional units appended to the colorbar quantity label.
#' @param plot_fun Advanced/testing hook. Defaults to `neuroatlas::plot_brain`.
#' @param projection Deprecated advanced/testing hook for a precomputed
#'   projection payload.
#' @param empty Action when no suprathreshold voxels are present.
#' @param parcel_backend Renderer for parcel-valued `vals`. `"cpu"` (default)
#'   uses the deferred parcel renderer in [neuroatlas::plot_brain()]: smooth
#'   antialiased parcel edges, a sulcal-depth underlay, and an automatic
#'   panel layout. `"ggplot"` keeps the historical polygon renderer.
#' @param parcel_style Optional `neurosurf::surface_parcel_style()` settings
#'   for the CPU parcel renderer.
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
                         overlay_palette = if (isTRUE(signed)) "nm-signed" else "lajolla",
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
                         vals = NULL,
                         legend_title = NULL,
                         units = NULL,
                         appearance = c("publication", "freesurfer"),
                         anatomy_style = NULL,
                         anatomy_midpoint = NULL,
                         anatomy_invert = FALSE,
                         parcel_backend = c("cpu", "ggplot"),
                         parcel_style = NULL,
                         min_cluster_size = NULL,
                         connectivity = c("18-connect", "26-connect",
                                          "6-connect"),
                         key = c("report", "plot"),
                         layout = c("strip", "grid")) {
  legend <- .montage_legend(legend_title, units)
  layout <- match.arg(layout)
  connectivity <- match.arg(connectivity)
  key <- match.arg(key)
  tail <- match.arg(tail)
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)
  interpolation <- match.arg(interpolation)
  aggregate <- match.arg(aggregate)
  medial_wall <- match.arg(medial_wall)
  camera <- match.arg(camera)
  empty <- match.arg(empty)
  render_device <- match.arg(render_device)
  appearance <- match.arg(appearance)
  if (is.null(anatomy_style)) {
    anatomy_style <- if (appearance == "freesurfer") "binary" else "publication"
  }
  anatomy_style <- match.arg(anatomy_style, c("publication", "continuous", "binary"))
  if (appearance == "freesurfer") {
    if (missing(overlay_alpha)) overlay_alpha <- 1
    if (missing(overlay_palette)) overlay_palette <- neurosurf::surface_heat_colors(signed)
  }
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

  parcel_backend <- match.arg(parcel_backend)
  parcel_cpu <- has_vals && identical(parcel_backend, "cpu")
  if (has_vals && !parcel_cpu && identical(camera, "presentation")) {
    warning(
      "camera = 'presentation' is ignored by parcel_backend = 'ggplot', ",
      "which always uses canonical projections.",
      call. = FALSE
    )
  }
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
  if (!continuous && has_stat) {
    # Same display extent as the cluster table and the volume figure. The
    # empty policy above concerns the threshold, not the extent.
    extent_excluded <- .montage_extent_excluded(
      values, dim(stat), supra,
      min_cluster_size = min_cluster_size, connectivity = connectivity
    )
    supra <- supra & !extent_excluded
    support <- support & !extent_excluded
  }

  surface_projection_meta <- NULL
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
      # Project here with plot_brain()'s own projector and the same settings,
      # and hand it the vertex values: the drawn surface is unchanged, and the
      # key's cap triangles can then come from the values actually drawn.
      # (A caller's own plot_fun still receives the volume.)
      projected <- if (is.null(plot_fun)) {
        .nm_project_surface_overlay(
          overlay_payload, surfatlas, fun = fun, sampling = sampling,
          interpolation = interpolation, aggregate = aggregate,
          depth = projection_depth, surface_smooth_fwhm = surface_smooth_fwhm
        )
      }
      if (!is.null(projected)) {
        overlay_payload <- projected$overlay
        surface_projection_meta <- projected$meta
      }
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
  plot_fun_arg <- plot_fun
  plot_fun <- plot_fun %||% neuroatlas::plot_brain
  # Match the color limits to the rendered sign: symmetric for signed maps, and
  # for unsigned maps the half-range that holds the retained tail so negative
  # one-sided effects are not clamped to a [0, cap] scale.
  overlay_lim <- limits
  nm_palette <- .nm_is_palette(overlay_palette)
  palette_name <- overlay_palette
  if (nm_palette) {
    overlay_palette <- if (has_vals || continuous) {
      nm_palette_colors(
        limits, threshold = if (continuous) 0 else threshold,
        palette = overlay_palette
      )
    } else {
      # neurosurf's CPU rasterizer already stretches each half of the palette
      # over [threshold, cap], so hand it the ramp stops themselves.
      .nm_threshold_stops(overlay_palette, tail)
    }
  }
  report_key <- identical(key, "report") && !has_vals
  montage_bg <- .nm_canvas

  plot_anatomy_provenance <- NULL
  plot_backend <- if (!has_vals) "cpu_barycentric" else if (parcel_cpu) {
    "cpu_deferred_parcels"
  } else {
    "ggplot"
  }
  # The report's 1 x 4 strip (left lateral, left medial, right medial, right
  # lateral) is composed here from plot_brain()'s per-view rasters, which are
  # rendered on a chroma-key ground so they can be cut out and placed on the
  # light or the dark canvas, with this package's own type and A/P marks.
  strip_mode <- identical(layout, "strip") && report_key && has_stat &&
    is.null(plot_fun_arg) && !identical(tools::file_ext(output_file), "pdf")
  build_plot <- function(strip) {
    plot_args <- list(
      surfatlas = surfatlas,
      vals = plot_vals,
      views = views,
      hemis = hemis,
      surface = surface,
      palette = if (has_vals) overlay_palette else palette,
      lim = if (has_vals) overlay_lim else c(0, 0),
      interactive = FALSE,
      colorbar = !report_key,
      colorbar_title = legend$text,
      title = if (strip) NULL else title,
      subtitle = if (strip) NULL else subtitle,
      caption = if (strip) NULL else caption
    )
    if (parcel_cpu) {
      # Deferred CPU parcel renderer: smooth parcel edges, sulcal underlay,
      # and a layout sized to the device. Sub-threshold parcels are already
      # NA; the threshold only greys the matching band of the colorbar.
      plot_args <- c(plot_args, list(
        static_backend = "cpu",
        camera = camera,
        bg = montage_bg,
        vals_threshold = if (continuous) NULL else threshold,
        parcel_style = parcel_style
      ))
    } else if (has_vals) {
      # Match the stat path's presentation layout, shaded cortex backdrop, and
      # page colour without its stat_publication preset, which would zero the
      # parcel fill alpha.
      plot_args <- c(plot_args, list(
        panel_layout = "presentation",
        background = TRUE,
        bg = montage_bg
      ))
    } else {
      plot_args <- c(plot_args, list(
        style = "stat_publication",
        static_backend = "cpu",
        bg = if (strip) .nm_chroma_key else montage_bg,
        colorbar_source = "overlay",
        overlay_title = legend$text,
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
        anatomy_style = anatomy_style,
        anatomy_midpoint = anatomy_midpoint,
        anatomy_invert = anatomy_invert,
        anatomy_range = if (appearance == "freesurfer") c(0.25, 0.75) else c(0.72, 0.90),
        overlay_alpha_ramp = if (appearance == "freesurfer" || nm_palette) {
          0
        } else {
          NULL
        },
        medial_wall = medial_wall,
        camera = camera,
        # The strip draws its own A/P marks from the camera geometry (older
        # neuroatlas releases swap A/P on medial views).
        orientation_labels = if (strip) FALSE else orientation_labels
      ))
      if (strip) {
        plot_args$render_width <- 1000L
        plot_args$render_height <- 625L
      }
    }
    do.call(plot_fun, plot_args)
  }
  # Cap triangles mark values beyond the cap that are actually drawn: the
  # projected cortical vertex values when available (surface peaks are lower
  # than the volume's), else the displayed volume voxels.
  drawn <- .nm_surface_drawn_values(
    overlay_payload, surfatlas, hemis = hemis, cortex_mask = cortex_mask,
    threshold = if (continuous) 0 else threshold
  )
  cap_source <- if (is.null(drawn)) values[supra] else drawn
  key_args <- list(
    palette = if (.nm_is_palette(palette_name)) palette_name else overlay_palette,
    limits = overlay_lim,
    threshold = if (continuous) NA_real_ else threshold,
    signed = signed, tail = tail, continuous = continuous,
    title = legend$text,
    over_low = any(cap_source < overlay_lim[[1L]], na.rm = TRUE),
    over_high = any(cap_source > overlay_lim[[2L]], na.rm = TRUE),
    cap_source = if (is.null(drawn)) "volume" else "surface",
    # Surface values are the volume sampled through the cortical ribbon and
    # averaged, so surface peaks read lower than the volume's.
    note = if (has_stat && length(projection_depth) > 1L) {
      "values averaged across cortical depth"
    }
  )

  strip <- NULL
  if (strip_mode) {
    p <- build_plot(TRUE)
    strip <- .nm_surface_strip_parts(p, key = key_args)
    if (is.null(strip)) {
      strip_mode <- FALSE
    } else {
      plot_anatomy_provenance <- .nm_with_projection_meta(
        attr(p, "plot_brain_anatomy"), surface_projection_meta
      )
      plot_backend <- attr(p, "plot_brain_backend") %||% plot_backend
      plain <- identical(render_device, "png") ||
        !requireNamespace("ragg", quietly = TRUE)
      .nm_font_env$force_plain <- plain
      spec <- tryCatch(.nm_surface_strip_layout(strip),
                       finally = .nm_font_env$force_plain <- FALSE)
      width <- spec$width
      height <- spec$height
      res <- spec$res
      render_device_used <- .open_surface_montage_device(
        filename = output_file, width = width, height = height, res = res,
        device = render_device, background = montage_bg
      )
      tryCatch({
        grid::grid.newpage()
        grid::grid.draw(spec$grob)
      }, finally = grDevices::dev.off())
    }
  }

  if (!strip_mode) {
    render_device_used <- .open_surface_montage_device(
      filename = output_file,
      width = width,
      height = height,
      res = res,
      device = render_device,
      background = montage_bg
    )
    tryCatch({
      p <- build_plot(FALSE)
      plot_anatomy_provenance <- .nm_with_projection_meta(
        attr(p, "plot_brain_anatomy"), surface_projection_meta
      )
      plot_backend <- attr(p, "plot_brain_backend") %||% plot_backend
      if (report_key) {
        .surf_montage_draw_with_key(
          p, res = res, height = height, palette = palette_name,
          colours = overlay_palette, limits = overlay_lim,
          threshold = key_args$threshold,
          signed = signed, tail = tail, continuous = continuous,
          title = legend$text,
          over_low = key_args$over_low,
          over_high = key_args$over_high
        )
      } else {
        print(p)
      }
    }, finally = grDevices::dev.off())
  }

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
        legend_title = legend$title,
        units = legend$units,
        device = render_device_used,
        width = width,
        height = height,
        res = res,
        background = montage_bg,
        panel_layout = if (has_vals && !parcel_cpu) "presentation" else NULL,
        style = if (has_vals) "parcel_values" else "stat_publication",
        parcel_backend = if (has_vals) parcel_backend else NULL,
        backend = plot_backend,
        colorbar_source = if (has_vals) "base" else "overlay",
        palette = if (has_vals) overlay_palette else palette,
        overlay_palette = if (has_vals) NULL else palette_name,
        limits = overlay_lim,
        projection = list(
          interpolation = interpolation,
          sampling = sampling,
          aggregate = aggregate,
          depth = projection_depth,
          surface_smooth_fwhm = surface_smooth_fwhm
        ),
        anatomy = plot_anatomy_provenance,
        # The parcel ggplot path cannot honour the CPU-only camera,
        # medial-wall, and orientation-label controls; record what was applied.
        medial_wall = if (has_vals) NA_character_ else medial_wall,
        camera = if (has_vals && !parcel_cpu) "canonical" else camera,
        orientation_labels = if (has_vals) FALSE else orientation_labels,
        layout = if (strip_mode) "strip" else "grid"
      ),
      strip = strip
    ),
    class = "surf_montage_result"
  )
}

# Volume -> vertex projection with neuroatlas's own plot_brain() projector
# (internal; looked up at run time so older neuroatlas releases simply fall
# back to letting plot_brain() project). Returns list(overlay = lh/rh values,
# meta) or NULL.
.nm_project_surface_overlay <- function(vol, surfatlas, fun, sampling,
                                        interpolation, aggregate, depth,
                                        surface_smooth_fwhm) {
  projector <- tryCatch(
    utils::getFromNamespace(".project_cluster_overlay", "neuroatlas"),
    error = function(e) NULL
  )
  if (!is.function(projector)) return(NULL)
  formals_ok <- all(c("cluster_vol", "surfatlas", "fun", "sampling",
                      "interpolation", "aggregate", "depth",
                      "surface_smooth_fwhm") %in% names(formals(projector)))
  if (!formals_ok) return(NULL)
  out <- tryCatch(
    suppressWarnings(projector(
      cluster_vol = vol, surfatlas = surfatlas, fun = fun,
      sampling = sampling, interpolation = interpolation,
      aggregate = aggregate, depth = depth,
      surface_smooth_fwhm = surface_smooth_fwhm
    )),
    error = function(e) NULL
  )
  if (is.null(out) || !is.list(out$overlay) ||
      !all(c("lh", "rh") %in% names(out$overlay))) {
    return(NULL)
  }
  out
}

# Vertex values the static surface draws: projected values on the requested
# hemispheres, inside the cortex domain (medial wall excluded), at or above
# the threshold. NULL when `overlay` is not a projected lh/rh list.
.nm_surface_drawn_values <- function(overlay, surfatlas, hemis,
                                     cortex_mask = NULL, threshold = 0) {
  if (!is.list(overlay) || inherits(overlay, "NeuroVol")) return(NULL)
  vals <- lapply(hemis, function(h) {
    hk <- if (h %in% c("left", "lh")) "lh" else "rh"
    v <- as.numeric(overlay[[hk]] %||% overlay[[h]])
    if (!length(v)) return(numeric())
    mask <- .nm_hemi_value(cortex_mask, hk) %||%
      .nm_hemi_value(surfatlas$cortex_mask, hk)
    if (is.logical(mask) && length(mask) == length(v)) v[!mask] <- NA_real_
    v
  })
  v <- unlist(vals, use.names = FALSE)
  v[is.finite(v) & abs(v) >= threshold]
}

# plot_brain() records its projection in the anatomy provenance only when it
# projects itself; restore it when the projection was done here.
.nm_with_projection_meta <- function(provenance, meta) {
  if (is.null(meta) || !is.list(provenance) ||
      !is.null(provenance$projection)) {
    return(provenance)
  }
  provenance$projection <- meta
  provenance
}

.nm_hemi_value <- function(x, hk) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) return(x)
  x[[hk]] %||% x[[if (hk == "lh") "left" else "right"]]
}

# Draw the plot_brain() views above a compact horizontal colour key (the
# 2 x 2 "grid" layout).
.surf_montage_draw_with_key <- function(p, res, height, palette, colours,
                                        limits, threshold, signed, tail,
                                        continuous, title, over_low = FALSE,
                                        over_high = FALSE) {
  key_px <- 120
  px <- function(v) grid::unit(v * 72 / res, "bigpts")
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = .nm_canvas, col = NA))
  print(p, newpage = FALSE, vp = grid::viewport(
    y = grid::unit(1, "npc"), height = grid::unit(1, "npc") - px(key_px),
    just = c("centre", "top")
  ))
  key_palette <- if (.nm_is_palette(palette)) palette else colours
  key <- .nm_with_device_fonts(.nm_colour_key_grob(
    key_palette, limits, threshold, signed, tail, continuous,
    over_low = over_low, over_high = over_high,
    title = title, scale = 144 / res
  ))
  geo <- .nm_key_geometry()
  key$vp <- grid::viewport(
    x = grid::unit(geo$x0, "npc"), y = px(key_px * 0.75),
    width = grid::unit(geo$w, "npc"), height = px(60),
    just = c("left", "top")
  )
  grid::grid.draw(key)
  invisible(NULL)
}

# Static surface strip ------------------------------------------------------

# Ground colour for the cut-out renders: pure green never occurs in the grey
# cortex, its outline, or the nm palettes, so it can be keyed out exactly.
.nm_chroma_key <- "#00FF00"

# RGBA raster with the chroma-key ground made transparent. Anti-aliased
# silhouette pixels (within a few pixels of the ground) are un-mixed: for a
# grey-ish foreground, the green excess G - max(R, B) is the key's share.
.nm_unkey_raster <- function(raster) {
  raster <- as.matrix(raster)
  nr <- nrow(raster)
  nc <- ncol(raster)
  m <- grDevices::col2rgb(raster) / 255
  r <- m[1L, ]
  g <- m[2L, ]
  b <- m[3L, ]
  pure <- matrix(r < 0.03 & b < 0.03 & g > 0.97, nr, nc)
  near <- pure
  for (i in seq_len(3L)) {
    near <- near | .nm_shift(near, 1, 0) | .nm_shift(near, -1, 0) |
      .nm_shift(near, 0, 1) | .nm_shift(near, 0, -1)
  }
  excess <- pmax(g - pmax(r, b), 0)
  alpha <- rep(1, length(r))
  fringe <- as.vector(near & !pure) & excess > 0
  alpha[fringe] <- pmax(0, 1 - excess[fringe])
  alpha[as.vector(pure)] <- 0
  a <- pmax(alpha, 1e-6)
  g2 <- ifelse(alpha > 0, (g - (1 - alpha)) / a, 0)
  r2 <- ifelse(alpha > 0, r / a, 0)
  b2 <- ifelse(alpha > 0, b / a, 0)
  out <- grDevices::rgb(pmin(pmax(r2, 0), 1), pmin(pmax(g2, 0), 1),
                        pmin(pmax(b2, 0), 1), alpha)
  out <- matrix(out, nr, nc)
  keep <- matrix(alpha > 0.02, nr, nc)
  rows <- which(rowSums(keep) > 0)
  cols <- which(colSums(keep) > 0)
  if (!length(rows) || !length(cols)) return(out)
  out[seq(min(rows), max(rows)), seq(min(cols), max(cols)), drop = FALSE]
}

# Screen side ("left"/"right") on which the anterior pole appears for a
# lateral or medial view, from the CPU renderer's camera: lateral views look
# from outside the hemisphere (left: from -x, anterior on screen-left), medial
# views from the midline (left: from +x, anterior on screen-right).
.nm_surface_anterior_side <- function(hemi, view) {
  left <- hemi %in% c("left", "lh", "L")
  toward_x <- switch(
    view,
    lateral = if (left) -1 else 1,
    medial = if (left) 1 else -1,
    stop("No anterior side for view '", view, "'.", call. = FALSE)
  )
  # screen-right = up (0,0,1) x toward (toward_x,0,0) = (0, toward_x, 0):
  # anterior (+y) is on screen-right when toward_x > 0.
  if (toward_x > 0) "right" else "left"
}

# Cut-out per-view rasters (and their hemisphere/view) from a plot_brain()
# CPU figure rendered on the chroma-key ground. NULL when the figure does not
# carry exactly the expected per-view raster panels.
.nm_surface_strip_parts <- function(p, key) {
  plots <- if (inherits(p, "patchwork")) {
    c(p$patches$plots, list(p))
  } else {
    list(p)
  }
  cameras <- attr(p, "plot_brain_anatomy")$camera %||% list()
  panels <- list()
  for (q in plots) {
    if (!inherits(q, "ggplot")) return(NULL)
    title <- tryCatch(q$labels$title, error = function(e) NULL)
    ras <- NULL
    for (layer in q$layers) {
      candidate <- tryCatch(layer$geom_params$raster, error = function(e) NULL)
      if (!is.null(candidate)) {
        ras <- candidate
        break
      }
    }
    cam <- if (!is.null(title)) cameras[[title]] else NULL
    if (is.null(ras) || is.null(cam) ||
        !cam$view %in% c("lateral", "medial")) {
      return(NULL)
    }
    hemi <- if (cam$hemi %in% c("lh", "left")) "left" else "right"
    panels[[length(panels) + 1L]] <- list(
      raster = .nm_unkey_raster(ras), hemi = hemi, view = cam$view,
      anterior = .nm_surface_anterior_side(hemi, cam$view)
    )
  }
  if (!length(panels)) return(NULL)
  order_key <- vapply(panels, function(x) {
    match(paste(x$hemi, x$view), c("left lateral", "left medial",
                                   "right medial", "right lateral"))
  }, numeric(1))
  list(panels = panels[order(order_key)], key = key)
}

# Lay the cut-out views out as the report's surface figure: one row (wide) or
# lateral-over-medial 2 x 2 (narrow), small view labels, A/P marks under each
# view's ends, and the colour key at the volume figure's key position.
.nm_surface_strip_layout <- function(parts, width_px = NULL, res = 144,
                                     theme = "light",
                                     variant = c("wide", "narrow")) {
  variant <- match.arg(variant)
  narrow <- identical(variant, "narrow")
  th <- .nm_figure_theme(theme)
  width_px <- width_px %||% if (narrow) 800 else 1760
  panels <- parts$panels
  n <- length(panels)
  if (narrow) {
    grid_order <- c("left lateral", "right lateral", "left medial",
                    "right medial")
    idx <- match(grid_order, vapply(panels, function(x) {
      paste(x$hemi, x$view)
    }, character(1)))
    idx <- c(idx[!is.na(idx)], setdiff(seq_len(n), idx))
    panels <- panels[idx]
  }
  ncol <- if (narrow) min(2L, n) else n
  nrow <- ceiling(n / ncol)
  fs <- if (narrow) 14 else 11
  pad <- if (narrow) 18 else 28
  gutter <- if (narrow) 14 else 22
  label_h <- fs * 2 * 1.45
  # The orientation mark rides in the label row, so no extra row is needed.
  mark_h <- fs * 2 * 0.35
  block <- .nm_key_block(fs, narrow)
  key_h <- block$height
  px <- function(v) grid::unit(v * 72 / res, "bigpts")

  dims <- t(vapply(panels, function(x) dim(x$raster), numeric(2)))
  cell_w <- (width_px - 2 * pad - (ncol - 1) * gutter) / ncol
  # One scale for every view so hemispheres keep their relative size.
  scale <- min(cell_w / dims[, 2L])
  cell_h <- max(dims[, 1L]) * scale
  height_px <- ceiling(
    pad + nrow * (label_h + cell_h + mark_h) + (nrow - 1) * gutter + key_h
  )
  label_of <- function(x) {
    paste0(if (x$hemi == "left") "Left " else "Right ", x$view)
  }
  kids <- list(grid::rectGrob(gp = grid::gpar(fill = th$canvas, col = NA)))
  marks <- list()
  for (i in seq_len(n)) {
    x <- panels[[i]]
    row <- (i - 1L) %/% ncol
    col <- (i - 1L) %% ncol
    w <- dims[i, 2L] * scale
    h <- dims[i, 1L] * scale
    cx <- pad + col * (cell_w + gutter) + cell_w / 2
    x0 <- cx - w / 2
    y_top <- pad + row * (label_h + cell_h + mark_h + gutter)
    img_top <- y_top + label_h + (cell_h - h)
    kids[[length(kids) + 1L]] <- grid::rasterGrob(
      x$raster, x = px(x0), y = grid::unit(1, "npc") - px(img_top),
      width = px(w), height = px(h), just = c("left", "top"),
      interpolate = TRUE
    )
    label_y <- grid::unit(1, "npc") - px(y_top + label_h - fs * 0.6)
    kids[[length(kids) + 1L]] <- grid::textGrob(
      label_of(x), x = px(x0 + 2), y = label_y,
      just = c("left", "bottom"), gp = .nm_gp_text(fs, th$label)
    )
    # One quiet orientation mark per view, in its label row: an arrow that
    # points to the screen side of the anterior pole ("<- A" / "A ->"), set
    # after the view name, so no letters pair up across the gutters.
    a_left <- identical(x$anterior, "left")
    mark <- if (a_left) "\u2190 A" else "A \u2192"
    mark_x0 <- x0 + 2 + .nm_text_width_px(label_of(x), fs, res) + fs * 2 * 0.8
    kids[[length(kids) + 1L]] <- grid::textGrob(
      mark, x = px(mark_x0), y = label_y, just = c("left", "bottom"),
      gp = .nm_gp_text(fs, th$muted)
    )
    marks[[i]] <- list(
      hemi = x$hemi, view = x$view, anterior = x$anterior, glyph = mark,
      mark_x = mark_x0,
      # Screen x of the panel end the arrow points to (the anterior pole).
      a_x = if (a_left) x0 else x0 + w, panel_x0 = x0, panel_w = w
    )
  }
  k <- parts$key
  geo <- .nm_key_geometry(narrow, title = k$title, fontsize = fs,
                          width_px = width_px, res = res)
  key <- .nm_colour_key_grob(
    k$palette, k$limits, k$threshold, k$signed, k$tail, k$continuous,
    over_low = k$over_low, over_high = k$over_high, title = k$title,
    note = k$note, fontsize = fs, theme = th, note_below = TRUE,
    title_above = geo$title_above
  )
  key_top <- height_px - key_h + block$above
  key$vp <- grid::viewport(
    x = px(width_px * geo$x0), y = grid::unit(1, "npc") - px(key_top),
    width = px(width_px * geo$w), height = px(fs * 2 * 3),
    just = c("left", "top")
  )
  kids[[length(kids) + 1L]] <- key
  list(grob = grid::gTree(children = do.call(grid::gList, kids)),
       width = width_px, height = height_px, res = res, canvas = th$canvas,
       theme = th$name, variant = variant, marks = marks)
}

# Write a strip variant (theme x width) to `path`.
.nm_write_surface_strip <- function(parts, path, theme = "light",
                                    variant = "wide") {
  spec <- .nm_surface_strip_layout(parts, theme = theme, variant = variant)
  .nm_write_figure(spec, path)
  invisible(spec)
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
