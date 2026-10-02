#' Render a Statistic Volume Montage
#'
#' Builds a thresholded or continuous volume montage using
#' `neuroim2::plot_overlay()` after
#' running [prepare_overlay()]. The function hard-errors by default when no
#' finite suprathreshold voxels are present, avoiding silently blank figures.
#'
#' @param bg Background `NeuroVol` or file path.
#' @param stat Statistic `NeuroVol` or file path.
#' @param threshold Positive numeric overlay threshold, or `NULL`/`NA` to show
#'   finite values continuously without significance thresholding.
#' @param tail Tail mode: `"two_sided"`, `"positive"`, or `"negative"`.
#' @param signed Logical; use symmetric signed color limits and a diverging
#'   palette?
#' @param cap Optional shared color cap. When supplied, overlay values are
#'   clipped to this cap before plotting.
#' @param limits Optional finite increasing display limits. This is the
#'   preferred scale contract for continuous maps; `cap` remains as a backward-
#'   compatible shorthand for zero-centered or nonnegative scales.
#' @param support_mask Optional logical/numeric mask in statistic-map order.
#'   Values outside the mask are hidden. This supports explicitly requested
#'   primary-map masking without making it the auxiliary-map default.
#' @param title,subtitle,caption Text passed to `neuroim2::plot_overlay()`.
#' @param initial_world_coord Optional finite `(x, y, z)` world coordinate used
#'   as the interactive view's initial crosshair. When omitted, the shared
#'   display specification uses a tail-aware peak and then a field-of-view
#'   centre fallback. It does not change the static slice selection.
#' @param zlevels,along,ncol Slice selection and layout. When `zlevels` is
#'   `NULL`, up to 12 evenly spaced slices are chosen across the central mass
#'   (5th to 95th percentile) of the displayed voxels. `ncol = NULL` lays the
#'   report figure out in rows of six.
#' @param min_cluster_size Optional minimum cluster extent in voxels. When
#'   supplied for a thresholded map, suprathreshold clusters smaller than this
#'   (sign-separated, using `connectivity`) are not drawn, matching the
#'   cluster table.
#' @param connectivity Voxel connectivity for `min_cluster_size`.
#' @param figure `"report"` (default) draws the report's figure of record: a
#'   cropped slice grid labelled in world millimetres with L/R markers, the
#'   threshold-saturated palette, opaque suprathreshold colour (at least 0.92
#'   opacity), and a compact horizontal colour key with the hidden
#'   sub-threshold band hatched. `"classic"` keeps the
#'   `neuroim2::plot_overlay()` montage.
#' @param bg_cmap,ov_cmap Background and overlay color maps. `"nm-signed"`
#'   (the default for signed maps) is the report's threshold-saturated
#'   diverging palette; see [nm_palette_colors()].
#' @param ov_alpha Overlay alpha.
#' @param ov_alpha_mode How overlay opacity tracks magnitude. `"binary"` gives
#'   every suprathreshold voxel full `ov_alpha`; `"ramp"` ramps alpha linearly
#'   from the threshold to the color cap; `"proportional"` sets alpha to
#'   `|v| / cap`; `"soft"` uses a nonlinear self-tuning curve. Modes unknown to
#'   the installed `neuroim2` fall back to its proportional alpha.
#' @param alpha_gamma Optional exponent for `ov_alpha_mode = "soft"`, forwarded
#'   to `neuroim2::plot_overlay()`. `NULL` (default) lets `neuroim2` auto-tune
#'   it; larger values push more of the low-value range toward transparency.
#' @param style Requested plot style. `"report"` is used when supported by the
#'   installed `neuroim2`; otherwise it falls back to `"light"`.
#' @param on_mismatch Passed to [prepare_overlay()].
#' @param empty Action when no suprathreshold voxels are present.
#' @param legend_title Quantity name for the colorbar, separate from the panel
#'   `title`. `NULL` defaults to `"Statistic"`.
#' @param units Optional units appended to the colorbar quantity label.
#' @param draw Passed to `neuroim2::plot_overlay()`.
#'
#' @return A `stat_montage_result` list containing the plot object/list and
#'   render metadata.
#' @export
stat_montage <- function(bg,
                         stat,
                         threshold,
                         tail = c("two_sided", "positive", "negative"),
                         signed = TRUE,
                         cap = NULL,
                         limits = NULL,
                         support_mask = NULL,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         initial_world_coord = NULL,
                         zlevels = NULL,
                         along = 3L,
                         ncol = NULL,
                         bg_cmap = "grays",
                         ov_cmap = if (isTRUE(signed)) "nm-signed" else "inferno",
                         ov_alpha = 0.7,
                         ov_alpha_mode = c("soft", "binary", "proportional",
                                           "ramp"),
                         alpha_gamma = NULL,
                         style = "report",
                         on_mismatch = c("error", "restamp"),
                         empty = c("error", "warning"),
                         draw = TRUE,
                         legend_title = NULL,
                         units = NULL,
                         min_cluster_size = NULL,
                         connectivity = c("18-connect", "26-connect",
                                          "6-connect"),
                         figure = c("report", "classic")) {
  tail <- match.arg(tail)
  connectivity <- match.arg(connectivity)
  figure <- match.arg(figure)
  ov_alpha_mode <- match.arg(ov_alpha_mode)
  on_mismatch <- match.arg(on_mismatch)
  empty <- match.arg(empty)
  spec <- .prepare_montage_volume_display_spec(
    bg = bg,
    stat = stat,
    threshold = threshold,
    tail = tail,
    signed = signed,
    cap = cap,
    limits = limits,
    support_mask = support_mask,
    initial_world_coord = initial_world_coord,
    zlevels = zlevels,
    along = along,
    bg_cmap = bg_cmap,
    ov_cmap = ov_cmap,
    ov_alpha = ov_alpha,
    ov_alpha_mode = ov_alpha_mode,
    alpha_gamma = alpha_gamma,
    on_mismatch = on_mismatch,
    empty = empty,
    min_cluster_size = min_cluster_size,
    connectivity = connectivity
  )

  legend <- .montage_legend(legend_title, units)
  spec$legend_title <- legend$title
  spec["units"] <- list(legend$units)
  spec$units_explicit <- !missing(units)
  plot_style <- .plot_overlay_style(style)

  plot <- if (identical(figure, "report")) {
    .stat_montage_report_figure(spec, legend = legend, ncol = ncol,
                                along = along)
  } else {
    .stat_montage_classic_plot(
      spec, legend = legend, along = along, ncol = ncol %||% 3L,
      title = title, subtitle = subtitle, caption = caption,
      draw = FALSE, plot_style = plot_style, alpha_gamma = alpha_gamma
    )
  }
  if (isTRUE(draw)) print(plot)

  structure(
    list(
      plot = plot,
      background = spec$background,
      overlay = spec$overlay,
      threshold = spec$threshold,
      display_mode = spec$display_mode,
      tail = spec$tail,
      signed = spec$signed,
      cap = spec$cap,
      limits = spec$limits,
      zlevels = spec$zlevels,
      slice_labels = if (inherits(plot, "nm_volume_figure_spec")) {
        plot$labels
      } else {
        NULL
      },
      figure = figure,
      initial_world_coord = spec$initial_world_coord,
      n_suprathreshold = spec$n_suprathreshold %||% spec$n_display_voxels,
      n_display_voxels = spec$n_display_voxels,
      n_extent_excluded = spec$n_extent_excluded,
      style = plot_style,
      requested_style = style,
      alpha_mode = spec$alpha_mode,
      display_spec = spec,
      overlay_action = spec$overlay_action
    ),
    class = "stat_montage_result"
  )
}

.stat_montage_classic_plot <- function(spec, legend, along, ncol, title,
                                       subtitle, caption, draw, plot_style,
                                       alpha_gamma) {
  palette <- spec$palette_value
  if (.nm_is_palette(palette)) {
    palette <- nm_palette_colors(
      spec$limits,
      threshold = if (is.finite(spec$threshold)) spec$threshold else 0,
      palette = palette
    )
  }
  overlay_args <- list(
    bgvol = spec$background,
    overlay = spec$overlay,
    zlevels = spec$zlevels,
    along = along,
    bg_cmap = spec$bg_cmap,
    ov_cmap = palette,
    bg_range = "robust",
    ov_range = if (identical(spec$display_mode, "continuous")) {
      spec$limits
    } else {
      "data"
    },
    ov_thresh = if (identical(spec$display_mode, "continuous")) {
      0
    } else {
      spec$threshold
    },
    ov_alpha = spec$alpha,
    ov_alpha_mode = spec$alpha_mode,
    ov_symmetric = isTRUE(spec$signed),
    ov_cap = if (isTRUE(spec$signed)) max(abs(spec$limits)) else NULL,
    ncol = ncol,
    title = title,
    subtitle = subtitle,
    caption = caption,
    draw = draw,
    style = plot_style,
    cbar_title = legend$text
  )
  # alpha_gamma is newer than the oldest neuroim2 we support; only forward it
  # when the installed plot_overlay accepts it, so the call stays portable.
  if (!is.null(alpha_gamma) &&
      "alpha_gamma" %in% names(formals(neuroim2::plot_overlay))) {
    overlay_args$alpha_gamma <- alpha_gamma
  }
  do.call(neuroim2::plot_overlay, overlay_args)
}

# The report's figure of record (see R/montage_figure.R).
.stat_montage_report_figure <- function(spec, legend, ncol, along) {
  continuous <- identical(spec$display_mode, "continuous")
  values <- as.numeric(as.array(spec$source))
  if (continuous) {
    values[!spec$display_mask] <- NA_real_
  } else {
    values[!spec$display_mask | !is.finite(values)] <- 0
  }
  note <- .stat_montage_figure_note(spec, legend)
  .nm_volume_figure(
    background = spec$background,
    stat_values = values,
    zlevels = spec$zlevels,
    along = along,
    threshold = spec$threshold,
    limits = spec$limits,
    palette = spec$palette_value,
    signed = isTRUE(spec$signed),
    tail = spec$tail,
    continuous = continuous,
    legend_text = legend$text,
    note = note,
    ncol = ncol,
    alpha = max(spec$alpha, 0.92)
  )
}

.stat_montage_figure_note <- function(spec, legend) {
  if (identical(spec$display_mode, "continuous")) return(NULL)
  thr <- .nm_format_tick(spec$threshold)
  shown <- switch(
    spec$tail,
    positive = paste0("threshold ", thr),
    negative = paste0("threshold ", .nm_minus, thr),
    paste0("threshold \u00b1", thr)
  )
  k <- spec$min_cluster_size
  if (!is.null(k) && is.finite(k) && k > 1) {
    shown <- paste0(
      shown, " \u00b7 clusters \u2265 ", k, " voxels (",
      sub("-connect$", "-connected", spec$connectivity), ")"
    )
  }
  shown
}

.validate_montage_limits <- function(limits) {
  if (is.null(limits)) return(NULL)
  if (!is.numeric(limits) || length(limits) != 2L || anyNA(limits) ||
      any(!is.finite(limits)) || limits[[1L]] >= limits[[2L]]) {
    stop("'limits' must be NULL or a finite increasing numeric pair.",
         call. = FALSE)
  }
  as.numeric(limits)
}

.normalize_montage_support_mask <- function(mask, n) {
  if (is.null(mask)) return(rep(TRUE, n))
  if (methods::is(mask, "NeuroVol")) mask <- as.numeric(mask)
  if (length(mask) != n) {
    stop("'support_mask' must have one value per statistic value.",
         call. = FALSE)
  }
  if (is.logical(mask)) {
    mask[is.na(mask)] <- FALSE
    return(mask)
  }
  mask <- suppressWarnings(as.numeric(mask))
  is.finite(mask) & mask != 0
}

.clip_montage_limits <- function(values, limits) {
  values[values < limits[[1L]]] <- limits[[1L]]
  values[values > limits[[2L]]] <- limits[[2L]]
  values
}

.stat_montage_default_cap <- function(values, signed) {
  finite <- values[is.finite(values)]
  if (length(finite) == 0L) {
    # No finite overlay values (e.g. an empty/all-zero map rendered with
    # empty = "warning"). Return a benign positive cap so the overlay color
    # limits stay finite and the base panel renders, rather than NA which would
    # poison overlay_lim and reject the cosmetic (empty) scale downstream.
    return(1)
  }
  if (isTRUE(signed)) {
    return(max(abs(finite), na.rm = TRUE))
  }
  max(finite, na.rm = TRUE)
}

.clip_montage_overlay <- function(values, cap, signed) {
  if (!is.finite(cap) || cap <= 0) {
    return(values)
  }
  if (isTRUE(signed)) {
    values[values > cap] <- cap
    values[values < -cap] <- -cap
  } else {
    values[values > cap] <- cap
  }
  values
}

.plot_overlay_style <- function(style) {
  choices <- tryCatch(
    eval(formals(neuroim2::plot_overlay)$style),
    error = function(e) c("light", "dark")
  )
  if (style %in% choices) {
    return(style)
  }
  if (identical(style, "report") && "light" %in% choices) {
    return("light")
  }
  stop(
    "'style' must be one of: ",
    paste(unique(c(choices, "report")), collapse = ", "),
    call. = FALSE
  )
}

.plot_overlay_alpha_mode <- function(alpha_mode) {
  choices <- tryCatch(
    eval(formals(neuroim2::plot_overlay)$ov_alpha_mode),
    error = function(e) c("binary", "proportional")
  )
  if (alpha_mode %in% choices) {
    return(alpha_mode)
  }
  # `soft`/`ramp` are newer neuroim2 modes; degrade to the proportional ramp
  # that every supported plot_overlay provides.
  if (alpha_mode %in% c("soft", "ramp") && "proportional" %in% choices) {
    return("proportional")
  }
  stop(
    "'ov_alpha_mode' must be one of: ",
    paste(unique(c(choices, "soft", "ramp")), collapse = ", "),
    call. = FALSE
  )
}

.stat_montage_zlevels <- function(supra_arr, along, max_slices = 12L) {
  along <- as.integer(along)
  if (length(along) != 1L || is.na(along) || along < 1L || along > 3L) {
    return(NULL)
  }
  .stat_montage_central_zlevels(supra_arr, along, max_slices = max_slices)
}
