#' Render a Statistic Volume Montage
#'
#' Builds a thresholded volume montage using `neuroim2::plot_overlay()` after
#' running [prepare_overlay()]. The function hard-errors by default when no
#' finite suprathreshold voxels are present, avoiding silently blank figures.
#'
#' @param bg Background `NeuroVol` or file path.
#' @param stat Statistic `NeuroVol` or file path.
#' @param threshold Positive numeric overlay threshold.
#' @param tail Tail mode: `"two_sided"`, `"positive"`, or `"negative"`.
#' @param signed Logical; use symmetric signed color limits and a diverging
#'   palette?
#' @param cap Optional shared color cap. When supplied, overlay values are
#'   clipped to this cap before plotting.
#' @param title,subtitle,caption Text passed to `neuroim2::plot_overlay()`.
#' @param zlevels,along,ncol Slice selection arguments passed through.
#' @param bg_cmap,ov_cmap Background and overlay color maps.
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
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         zlevels = NULL,
                         along = 3L,
                         ncol = 3L,
                         bg_cmap = "grays",
                         ov_cmap = if (isTRUE(signed)) "blue-red" else "inferno",
                         ov_alpha = 0.7,
                         ov_alpha_mode = c("soft", "binary", "proportional",
                                           "ramp"),
                         alpha_gamma = NULL,
                         style = "report",
                         on_mismatch = c("error", "restamp"),
                         empty = c("error", "warning"),
                         draw = TRUE) {
  tail <- match.arg(tail)
  ov_alpha_mode <- match.arg(ov_alpha_mode)
  on_mismatch <- match.arg(on_mismatch)
  empty <- match.arg(empty)

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
  if (!is.null(alpha_gamma) && (!is.numeric(alpha_gamma) ||
                                length(alpha_gamma) != 1L ||
                                !is.finite(alpha_gamma) || alpha_gamma <= 0)) {
    stop("'alpha_gamma' must be NULL or a positive number.", call. = FALSE)
  }

  aligned <- prepare_overlay(bg, stat, on_mismatch = on_mismatch)
  stat_arr <- as.array(aligned$stat)
  stat_values <- as.numeric(stat_arr)
  supra <- .suprathreshold_mask(stat_values, threshold = threshold, tail = tail)
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

  display_arr <- stat_arr
  supra_arr <- array(supra, dim = dim(display_arr))
  display_arr[!supra_arr] <- NA_real_
  cap <- cap %||% .stat_montage_default_cap(display_arr, signed = signed)
  display_arr <- .clip_montage_overlay(display_arr, cap = cap, signed = signed)
  display_stat <- neuroim2::NeuroVol(display_arr, space = neuroim2::space(aligned$stat))
  plot_zlevels <- zlevels %||% .stat_montage_zlevels(supra_arr, along = along)

  plot_style <- .plot_overlay_style(style)
  alpha_mode <- .plot_overlay_alpha_mode(ov_alpha_mode)

  overlay_args <- list(
    bgvol = aligned$background,
    overlay = display_stat,
    zlevels = plot_zlevels,
    along = along,
    bg_cmap = bg_cmap,
    ov_cmap = ov_cmap,
    bg_range = "robust",
    ov_range = "data",
    ov_thresh = threshold,
    ov_alpha = ov_alpha,
    ov_alpha_mode = alpha_mode,
    ov_symmetric = isTRUE(signed),
    ncol = ncol,
    title = title,
    subtitle = subtitle,
    caption = caption,
    draw = draw,
    style = plot_style
  )
  # alpha_gamma is newer than the oldest neuroim2 we support; only forward it
  # when the installed plot_overlay accepts it, so the call stays portable.
  if (!is.null(alpha_gamma) &&
      "alpha_gamma" %in% names(formals(neuroim2::plot_overlay))) {
    overlay_args$alpha_gamma <- alpha_gamma
  }
  plot <- do.call(neuroim2::plot_overlay, overlay_args)

  structure(
    list(
      plot = plot,
      background = aligned$background,
      overlay = display_stat,
      threshold = threshold,
      tail = tail,
      signed = signed,
      cap = cap,
      zlevels = plot_zlevels,
      n_suprathreshold = n_supra,
      style = plot_style,
      requested_style = style,
      alpha_mode = alpha_mode,
      overlay_action = aligned$action
    ),
    class = "stat_montage_result"
  )
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

.stat_montage_zlevels <- function(supra_arr, along, max_slices = 9L) {
  along <- as.integer(along)
  if (length(along) != 1L || is.na(along) || along < 1L || along > 3L) {
    return(NULL)
  }
  dims <- dim(supra_arr)
  has_supra <- vapply(seq_len(dims[[along]]), function(index) {
    if (along == 1L) {
      any(supra_arr[index, , ], na.rm = TRUE)
    } else if (along == 2L) {
      any(supra_arr[, index, ], na.rm = TRUE)
    } else {
      any(supra_arr[, , index], na.rm = TRUE)
    }
  }, logical(1))
  z <- which(has_supra)
  if (length(z) == 0L) {
    # No suprathreshold slices (e.g. an empty contrast rendered with
    # empty = "warning"): fall back to evenly spaced slices so the base montage
    # still renders rather than failing on an empty slice set.
    n <- dims[[along]]
    return(unique(round(seq(1, n, length.out = min(max_slices, n)))))
  }
  if (length(z) <= max_slices) {
    return(z)
  }
  unique(round(seq(min(z), max(z), length.out = max_slices)))
}
