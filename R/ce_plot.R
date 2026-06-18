.cluster_explorer_demo_inputs <- function(n_time = 48L) {
  dims <- c(12L, 12L, 12L)
  sp3 <- neuroim2::NeuroSpace(
    dim = dims,
    spacing = c(2, 2, 2),
    origin = c(-12, -12, -12)
  )

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[2:4, 2:4, 2:4] <- 1L
  atlas_arr[8:10, 2:4, 2:4] <- 2L
  atlas_arr[2:4, 8:10, 2:4] <- 3L
  atlas_arr[8:10, 8:10, 2:4] <- 4L
  atlas_arr[2:4, 5:7, 8:10] <- 5L
  atlas_arr[8:10, 5:7, 8:10] <- 6L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas <- list(
    name = "demo_atlas",
    atlas = atlas_vol,
    ids = 1:6,
    labels = c("Frontal-A", "Frontal-B", "Parietal-A",
               "Parietal-B", "Temporal-A", "Temporal-B"),
    orig_labels = c("Frontal-A", "Frontal-B", "Parietal-A",
                    "Parietal-B", "Temporal-A", "Temporal-B"),
    hemi = c("left", "right", "left", "right", "left", "right"),
    roi_metadata = tibble::tibble(
      id = 1:6,
      label = c("Frontal-A", "Frontal-B", "Parietal-A",
                "Parietal-B", "Temporal-A", "Temporal-B"),
      hemi = c("left", "right", "left", "right", "left", "right")
    )
  )
  class(atlas) <- c("demo", "atlas")

  stat_arr <- array(0, dim = dims)
  stat_arr[2:4, 2:4, 2:4] <- 4.8
  stat_arr[8:10, 5:7, 8:10] <- -5.1
  stat_arr[6, 6, 6] <- 8.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = sp3)

  sp4 <- neuroim2::NeuroSpace(
    dim = c(dims, n_time),
    spacing = c(2, 2, 2),
    origin = c(-12, -12, -12)
  )

  data_arr <- array(0, dim = c(dims, n_time))
  p_mask <- atlas_arr == 1L
  n_mask <- atlas_arr == 6L
  bg_mask <- atlas_arr %in% c(2L, 3L, 4L, 5L)

  t_idx <- seq_len(n_time)
  p_sig <- 0.5 + 0.7 * sin(t_idx / 5)
  n_sig <- 1.2 + 0.55 * cos(t_idx / 7)
  bg_sig <- 0.2 + 0.15 * sin(t_idx / 9)

  for (t in t_idx) {
    vol_t <- data_arr[,,, t]
    vol_t[p_mask] <- p_sig[t]
    vol_t[n_mask] <- n_sig[t]
    vol_t[bg_mask] <- bg_sig[t]
    data_arr[,,, t] <- vol_t
  }

  data_source <- neuroim2::NeuroVec(data_arr, sp4)

  sample_table <- tibble::tibble(
    sample_id = sprintf("sample_%03d", t_idx),
    time = t_idx,
    run = factor(ifelse(t_idx <= n_time / 2, "run1", "run2")),
    condition = factor(rep(c("A", "B", "C"), length.out = n_time))
  )

  design <- tibble::tibble(
    task_load = as.numeric(scale(t_idx)),
    cue = factor(rep(c("cue", "probe"), length.out = n_time))
  )

  surfatlas <- list(
    name = "demo_surfatlas",
    ids = atlas$ids,
    labels = atlas$labels,
    hemi = atlas$hemi
  )
  class(surfatlas) <- c("demo_surfatlas", "surfatlas", "atlas")

  list(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    surfatlas = surfatlas,
    sample_table = sample_table,
    design = design
  )
}

.fallback_brain_plot <- function(surfatlas,
                                 vals = NULL,
                                 palette = "vik",
                                 lim = NULL,
                                 interactive = TRUE,
                                 title = "Parcel Layout (Fallback)") {
  ids <- as.integer(surfatlas$ids)
  if (length(ids) == 0) {
    ids <- seq_len(if (!is.null(vals)) length(vals) else 12L)
  }

  labels <- as.character(surfatlas$labels)
  if (length(labels) != length(ids)) {
    labels <- paste0("Parcel ", ids)
  }

  if (is.null(vals)) {
    value <- rep(0, length(ids))
  } else {
    v <- as.numeric(vals)
    if (length(v) != length(ids)) {
      value <- rep(NA_real_, length(ids))
      names(value) <- ids
      common <- intersect(as.character(ids), names(vals))
      value[common] <- as.numeric(vals[common])
    } else {
      value <- v
    }
  }

  n <- length(ids)
  ncol <- max(1L, ceiling(sqrt(n)))
  nrow <- ceiling(n / ncol)
  grid_idx <- seq_len(n)
  cx <- ((grid_idx - 1L) %% ncol) + 1L
  cy <- nrow - ((grid_idx - 1L) %/% ncol)

  dat <- tibble::tibble(
    parcel_id = ids,
    label = labels,
    value = value,
    x = cx,
    y = cy,
    data_id = as.character(ids),
    tooltip = paste0(labels, "\nValue: ", signif(value, 4))
  )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y))
  if (isTRUE(interactive)) {
    p <- p + ggiraph::geom_tile_interactive(
      ggplot2::aes(fill = value, tooltip = tooltip, data_id = data_id),
      width = 0.94,
      height = 0.94,
      colour = "#ffffff",
      linewidth = 0.4
    )
  } else {
    p <- p + ggplot2::geom_tile(
      ggplot2::aes(fill = value),
      width = 0.94,
      height = 0.94,
      colour = "#ffffff",
      linewidth = 0.4
    )
  }

  if (n <= 30) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = parcel_id),
      size = 2.8,
      colour = "#111827"
    )
  }

  p <- p +
    scico::scale_fill_scico(
      palette = palette,
      limits = lim,
      oob = scales::squish,
      na.value = "#f3f4f6"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, fill = "Value") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "#6b7280", size = 11),
      legend.position = "none"
    )

  if (!isTRUE(interactive)) {
    return(p)
  }

  ggiraph::girafe(ggobj = p)
}

# Shared brain-plot builder used by both the interactive render and the PNG
# download handler to avoid duplicating val/lim computation and the
# neuroatlas::plot_brain fallback chain.
.make_brain_plot <- function(surfatlas, cluster_parcels, scope_ids,
                              display_mode, use_surface_plot,
                              overlay_vals, overlay_threshold, overlay_alpha,
                              brain_views, brain_hemis, palette,
                              interactive,
                              overlay_fun = "avg",
                              overlay_sampling = "midpoint") {
  surf_ids <- as.integer(surfatlas$ids)
  vals <- .parcel_values_from_clusters(
    cluster_parcels = cluster_parcels,
    atlas_ids = surf_ids,
    selected_cluster_ids = scope_ids,
    mode = display_mode
  )

  lim <- NULL
  finite_vals <- vals[is.finite(vals)]
  if (length(finite_vals) > 0) {
    lim_max <- max(abs(finite_vals))
    lim <- c(-lim_max, lim_max)
  }

  g <- .fallback_brain_plot(
    surfatlas = surfatlas, vals = vals, palette = palette,
    lim = lim, interactive = interactive
  )

  if (isTRUE(use_surface_plot)) {
    plot_args <- list(
      surfatlas = surfatlas, vals = vals,
      views = brain_views, hemis = brain_hemis,
      palette = palette, lim = lim,
      overlay = overlay_vals,
      overlay_threshold = max(abs(overlay_threshold), .Machine$double.eps),
      overlay_alpha = overlay_alpha,
      overlay_palette = palette,
      interactive = interactive
    )
    # When `overlay_vals` is a NeuroVol, plot_brain projects it with the atlas's
    # own geometry; forward the sampling controls so they are honored.
    if (methods::is(overlay_vals, "NeuroVol")) {
      plot_args$overlay_fun <- overlay_fun
      plot_args$overlay_sampling <- overlay_sampling
    }
    if (interactive) plot_args$data_id_mode <- "polygon"
    g <- tryCatch(
      do.call(neuroatlas::plot_brain, plot_args),
      error = function(e) {
        .fallback_brain_plot(
          surfatlas = surfatlas, vals = vals, palette = palette,
          lim = lim, interactive = interactive,
          title = paste0("Parcel Layout (Fallback): ", conditionMessage(e))
        )
      }
    )
  }

  g
}

.format_plot_value <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x))
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, usetz = TRUE))
  }
  if (is.numeric(x)) {
    return(as.character(signif(x, 4)))
  }
  as.character(x)
}

.build_design_plot <- function(data, x_var, collapse_vars = NULL,
                               group_var = NULL,
                               facet_var = NULL,
                               interactive = FALSE) {
  collapse_vars <- collapse_vars[collapse_vars %in% names(data)]
  collapse_vars <- setdiff(collapse_vars, c("cluster_id", "signal"))
  if (is.null(group_var) || !nzchar(group_var) || !group_var %in% names(data)) {
    group_var <- NULL
  }
  if (is.null(facet_var) || !nzchar(facet_var) || !facet_var %in% names(data)) {
    facet_var <- NULL
  }

  plot_data <- data
  if (length(collapse_vars) > 0) {
    group_vars <- unique(c("cluster_id", x_var, collapse_vars, group_var,
                           facet_var))
    plot_data <- plot_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(signal = mean(signal, na.rm = TRUE), .groups = "drop")
  }

  color_var <- if (is.null(group_var)) "cluster_id" else group_var
  color_label <- if (is.null(group_var)) "Cluster" else group_var
  plot_data$.plot_group_ <- factor(as.character(plot_data[[color_var]]))
  if (!is.null(facet_var)) {
    plot_data$.facet_panel_ <- factor(as.character(plot_data[[facet_var]]))
  }

  # Build tooltip column for interactive mode
  if (isTRUE(interactive)) {
    tooltip_lines <- list(
      paste0("cluster: ", plot_data$cluster_id),
      paste0(x_var, ": ", .format_plot_value(plot_data[[x_var]])),
      paste0("signal: ", .format_plot_value(plot_data$signal))
    )
    if (!is.null(group_var)) {
      tooltip_lines <- append(
        tooltip_lines,
        list(paste0(group_var, ": ", .format_plot_value(plot_data[[group_var]]))),
        after = 1L
      )
    }
    if (!is.null(facet_var)) {
      tooltip_lines <- append(
        tooltip_lines,
        list(paste0(facet_var, ": ", .format_plot_value(plot_data[[facet_var]])))
      )
    }
    plot_data$tooltip_ <- paste0(
      tooltip_lines[[1L]], "\n",
      tooltip_lines[[2L]], "\n",
      tooltip_lines[[3L]],
      if (length(tooltip_lines) > 3L) {
        paste0("\n", tooltip_lines[[4L]])
      } else {
        ""
      },
      if (length(tooltip_lines) > 4L) {
        paste0("\n", tooltip_lines[[5L]])
      } else {
        ""
      }
    )
  }

  x <- plot_data[[x_var]]
  xtype <- infer_design_var_type(x)

  if (xtype == "continuous") {
    if (identical(x_var, ".sample_index") ||
        inherits(x, "Date") || inherits(x, "POSIXt")) {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[x_var]],
                     y = signal,
                     color = .data$.plot_group_,
                     group = .data$.plot_group_)
      ) +
        ggplot2::geom_line(alpha = 0.7)
      if (isTRUE(interactive)) {
        p <- p + ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = tooltip_),
          size = 1.5, alpha = 0.8
        )
      } else {
        p <- p + ggplot2::geom_point(size = 1.5, alpha = 0.8)
      }
    } else {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[x_var]],
                     y = signal,
                     color = .data$.plot_group_)
      )
      if (isTRUE(interactive)) {
        p <- p + ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = tooltip_),
          alpha = 0.7
        )
      } else {
        p <- p + ggplot2::geom_point(alpha = 0.7)
      }
      if (is.null(group_var)) {
        p <- p + ggplot2::geom_smooth(se = FALSE, method = "loess")
      } else {
        p <- p + ggplot2::geom_smooth(
          ggplot2::aes(group = .data$.plot_group_),
          se = FALSE,
          method = "lm"
        )
      }
    }
  } else {
    if (is.null(group_var)) {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = factor(.data[[x_var]]),
                     y = signal,
                     color = .data$.plot_group_)
      ) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.25)
    } else {
      plot_data$.box_group_ <- interaction(
        plot_data[[x_var]],
        plot_data$.plot_group_,
        drop = TRUE
      )
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = factor(.data[[x_var]]),
                     y = signal,
                     color = .data$.plot_group_)
      ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(group = .data$.box_group_),
          outlier.shape = NA,
          alpha = 0.2,
          position = ggplot2::position_dodge(width = 0.75)
        )
    }
    if (isTRUE(interactive)) {
      jitter_mapping <- ggplot2::aes(tooltip = tooltip_)
      if (is.null(group_var)) {
        p <- p + ggiraph::geom_jitter_interactive(
          jitter_mapping,
          width = 0.15, alpha = 0.75, size = 1.5
        )
      } else {
        p <- p + ggiraph::geom_jitter_interactive(
          jitter_mapping,
          position = ggplot2::position_jitterdodge(
            jitter.width = 0.15,
            dodge.width = 0.75
          ),
          alpha = 0.75,
          size = 1.5
        )
      }
    } else {
      if (is.null(group_var)) {
        p <- p + ggplot2::geom_jitter(width = 0.15, alpha = 0.75, size = 1.5)
      } else {
        p <- p + ggplot2::geom_jitter(
          position = ggplot2::position_jitterdodge(
            jitter.width = 0.15,
            dodge.width = 0.75
          ),
          alpha = 0.75,
          size = 1.5
        )
      }
    }
  }

  if (!is.null(facet_var) && length(unique(plot_data$cluster_id)) > 1) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(.data$cluster_id),
      cols = ggplot2::vars(.data$.facet_panel_),
      scales = "free_y"
    )
  } else if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(~ .facet_panel_, scales = "free_y")
  } else if (length(unique(plot_data$cluster_id)) > 1) {
    p <- p + ggplot2::facet_wrap(~ cluster_id, scales = "free_y")
  }

  p <- p +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#f3f4f6", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.4),
      axis.title = ggplot2::element_text(color = "#374151"),
      axis.text = ggplot2::element_text(color = "#111827"),
      strip.background = ggplot2::element_rect(fill = "#f9fafb", color = "#e5e7eb"),
      strip.text = ggplot2::element_text(color = "#111827", face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = "#374151"),
      legend.text = ggplot2::element_text(color = "#111827")
    ) +
    ggplot2::labs(
      x = x_var,
      y = "Cluster Signal",
      color = color_label
    )

  if (isTRUE(interactive)) {
    return(ggiraph::girafe(ggobj = p))
  }
  p
}

.empty_plot <- function(label, subtitle = NULL) {
  p <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0.1, label = label, size = 5,
                      color = "#374151") +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
  if (!is.null(subtitle)) {
    p <- p + ggplot2::annotate("text", x = 0, y = -0.15, label = subtitle,
                               size = 3.5, color = "#9ca3af")
  }
  p
}
