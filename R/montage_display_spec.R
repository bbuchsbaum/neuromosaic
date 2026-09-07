# Shared static/interactive volume display resolution.

.prepare_montage_volume_display_spec <- function(bg,
                                                 stat,
                                                 threshold,
                                                 tail,
                                                 signed,
                                                 cap,
                                                 limits,
                                                 support_mask,
                                                 initial_world_coord,
                                                 zlevels,
                                                 along,
                                                 bg_cmap,
                                                 ov_cmap,
                                                 ov_alpha,
                                                 ov_alpha_mode,
                                                 alpha_gamma,
                                                 on_mismatch,
                                                 empty) {
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
  if (!is.numeric(ov_alpha) || length(ov_alpha) != 1L || is.na(ov_alpha) ||
      !is.finite(ov_alpha) || ov_alpha < 0 || ov_alpha > 1) {
    stop("'ov_alpha' must be a finite number between zero and one.",
         call. = FALSE)
  }
  if (!is.null(alpha_gamma) && (!is.numeric(alpha_gamma) ||
                                length(alpha_gamma) != 1L ||
                                !is.finite(alpha_gamma) || alpha_gamma <= 0)) {
    stop("'alpha_gamma' must be NULL or a positive number.", call. = FALSE)
  }
  if (!is.null(initial_world_coord) &&
      (!is.numeric(initial_world_coord) ||
       length(initial_world_coord) != 3L ||
       anyNA(initial_world_coord) ||
       any(!is.finite(initial_world_coord)))) {
    stop(
      "'initial_world_coord' must be NULL or a finite numeric (x, y, z) triplet.",
      call. = FALSE
    )
  }

  aligned <- prepare_overlay(bg, stat, on_mismatch = on_mismatch)
  stat_arr <- as.array(aligned$stat)
  stat_values <- as.numeric(stat_arr)
  support <- .normalize_montage_support_mask(support_mask, length(stat_values))
  display_mask <- if (continuous) {
    is.finite(stat_values)
  } else {
    .suprathreshold_mask(stat_values, threshold = threshold, tail = tail)
  }
  display_mask <- display_mask & support
  n_display <- sum(display_mask, na.rm = TRUE)
  if (n_display == 0L) {
    msg <- if (continuous) {
      "No finite display voxels for the continuous overlay."
    } else {
      paste0(
        "No finite suprathreshold voxels for threshold ", threshold,
        " and tail '", tail, "'."
      )
    }
    if (identical(empty, "error")) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  display_arr <- stat_arr
  display_arr[!array(display_mask, dim = dim(display_arr))] <- NA_real_
  if (is.null(limits)) {
    cap <- cap %||% .stat_montage_default_cap(display_arr, signed = signed)
    limits <- if (isTRUE(signed)) c(-cap, cap) else c(0, cap)
  } else {
    cap <- max(abs(limits))
  }
  display_arr <- .clip_montage_limits(display_arr, limits)
  display_stat <- neuroim2::NeuroVol(
    display_arr, space = neuroim2::space(aligned$stat)
  )
  plot_zlevels <- zlevels %||% .stat_montage_zlevels(
    array(display_mask, dim = dim(stat_arr)), along = along
  )
  alpha_mode <- .plot_overlay_alpha_mode(ov_alpha_mode)
  initial_world_coord <- as.numeric(initial_world_coord %||%
    .montage_volume_initial_world_coord(
      aligned$stat,
      display_mask = display_mask,
      tail = tail,
      signed = signed
    ))

  structure(
    list(
      background = aligned$background,
      source = aligned$stat,
      overlay = display_stat,
      display_mask = display_mask,
      support_mask = support,
      threshold = if (continuous) NA_real_ else as.numeric(threshold),
      display_mode = if (continuous) "continuous" else "thresholded",
      tail = tail,
      signed = signed,
      scale = if (isTRUE(signed)) "diverging" else "sequential",
      center = if (isTRUE(signed)) 0 else NULL,
      cap = cap,
      limits = limits,
      zlevels = plot_zlevels,
      zlevel_bookmarks = plot_zlevels,
      initial_world_coord = initial_world_coord,
      n_display_voxels = n_display,
      bg_cmap = bg_cmap,
      palette = .montage_volume_palette_identity(ov_cmap),
      palette_value = ov_cmap,
      alpha = as.numeric(ov_alpha),
      alpha_mode = alpha_mode,
      alpha_gamma = alpha_gamma,
      overlay_action = aligned$action
    ),
    class = "montage_volume_display_spec"
  )
}

.montage_volume_display_metadata <- function(spec,
                                             row,
                                             support,
                                             initial_world_coord = NULL) {
  if (!inherits(spec, "montage_volume_display_spec")) {
    stop("'spec' must be a montage_volume_display_spec.", call. = FALSE)
  }
  required <- c(
    "analysis_id", "map_id", "quantity", "label", "effective_scale",
    "effective_support", "effective_units"
  )
  missing <- setdiff(required, names(row))
  if (length(missing)) {
    stop(
      "Resolved manifest row is missing display field(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  legend <- if (!is.null(spec$legend_title)) {
    .montage_legend(
      spec$legend_title,
      if (isTRUE(spec$units_explicit)) spec$units else {
        spec$units %||% .montage_row_legend(row)$units
      }
    )
  } else {
    .montage_row_legend(row)
  }
  units <- legend$units
  selector_label <- if ("selector_label" %in% names(row)) {
    value <- as.character(row$selector_label[[1L]])
    if (is.na(value) || !nzchar(value)) NULL else value
  } else {
    NULL
  }
  world_coord <- initial_world_coord %||% spec$initial_world_coord
  resolved_support <- .normalize_montage_support_mask(
    support, length(spec$support_mask)
  )
  support_name <- as.character(row$effective_support[[1L]])
  if (!identical(resolved_support, spec$support_mask)) {
    support_name <- "custom"
  }

  structure(
    list(
      analysis_id = as.character(row$analysis_id[[1L]]),
      map_id = as.character(row$map_id[[1L]]),
      quantity = as.character(row$quantity[[1L]]),
      label = as.character(row$label[[1L]]),
      selector_label = selector_label,
      legend_title = legend$title,
      units = units,
      display_mode = spec$display_mode,
      threshold = spec$threshold,
      tail = spec$tail,
      signed = spec$signed,
      scale = spec$scale,
      center = spec$center,
      cap = spec$cap,
      limits = spec$limits,
      palette = spec$palette,
      alpha = spec$alpha,
      alpha_mode = spec$alpha_mode,
      alpha_gamma = spec$alpha_gamma,
      support = support_name,
      support_mask = spec$support_mask,
      zlevels = spec$zlevels,
      zlevel_bookmarks = spec$zlevel_bookmarks,
      initial_world_coord = as.numeric(world_coord),
      n_suprathreshold = spec$n_display_voxels,
      n_display_voxels = spec$n_display_voxels,
      status = if (spec$n_display_voxels == 0L) "empty" else "ready",
      style = NULL,
      overlay_action = spec$overlay_action
    ),
    class = c("montage_volume_display_metadata", "list")
  )
}

.montage_share_group_volume_views <- function(manifest, panels) {
  groups <- .montage_analysis_groups(manifest)
  for (group in groups) {
    primary <- panels[[group$primary_map_id]][["volume"]]
    if (!inherits(primary, "montage_volume_display_metadata")) next
    for (map_id in group$map_ids) {
      volume <- panels[[map_id]][["volume"]]
      if (!inherits(volume, "montage_volume_display_metadata")) next
      volume$initial_world_coord <- primary$initial_world_coord
      volume$zlevels <- primary$zlevels
      volume$zlevel_bookmarks <- primary$zlevel_bookmarks
      panels[[map_id]][["volume"]] <- volume
    }
  }
  panels
}

.montage_volume_initial_world_coord <- function(stat,
                                                display_mask,
                                                tail,
                                                signed) {
  values <- as.numeric(stat)
  eligible <- which(display_mask & is.finite(values))
  if (length(eligible)) {
    candidate_values <- values[eligible]
    offset <- if (identical(tail, "positive")) {
      which.max(candidate_values)
    } else if (identical(tail, "negative")) {
      which.min(candidate_values)
    } else if (isTRUE(signed)) {
      which.max(abs(candidate_values))
    } else {
      which.max(candidate_values)
    }
    # neuroim2's single-index `index_to_coord()` path currently drops the
    # one-row grid matrix. Keep the conversion explicit so this contract also
    # works for a single selected peak.
    grid <- neuroim2::index_to_grid(stat, as.integer(eligible[[offset]]))
    coord <- neuroim2::grid_to_coord(stat, as.matrix(grid))
  } else {
    coord <- neuroim2::grid_to_coord(
      stat,
      matrix(as.numeric((dim(stat)[seq_len(3L)] + 1) / 2), nrow = 1L)
    )
  }
  as.numeric(coord[1L, seq_len(3L), drop = TRUE])
}

.montage_volume_palette_identity <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    return(as.character(x))
  }
  paste0("custom:", paste(class(x), collapse = "/"))
}
