#' Plot Cluster Time Course
#'
#' Generates a ggplot2 figure showing time-course data for a single cluster,
#' grouped and faceted according to an R formula. Data is already-fitted
#' (betas/contrasts); the formula controls grouping and layout, not model
#' fitting.
#'
#' @param data Tibble of extracted time-course data for one cluster, with a
#'   `value` column and design columns matching the formula terms.
#' @param formula A formula describing the plot layout (not a statistical
#'   model). The LHS names the value column (default `value`). RHS terms
#'   control aesthetics -- the operators `+` and `*` are equivalent since
#'   terms are extracted with `all.vars()`:
#'   - `value ~ time` — single line, x = time, y = mean(value)
#'   - `value ~ condition * time` — color by condition, x = time
#'   - `value ~ cond1 * cond2 * time` — color by cond2, facet by cond1, x = time
#'
#'   The last RHS term becomes the x-axis (unless a column named `"time"`
#'   exists, which takes priority). Earlier terms map to color and facet.
#' @param cluster_label Character label for the plot title (e.g.,
#'   `"Cluster 1: L DefaultA (89 vox)"`).
#' @param ci_level Confidence interval coverage for error ribbons. Default 0.95.
#' @param palette Optional named character vector of colors, or a palette
#'   function.
#' @param time_var Character name of the time variable. If `NULL`, inferred from
#'   the formula (the last RHS term, or a column named `"time"`).
#' @param point_size Size of individual data points (0 to hide). Default 0.
#' @param default_color Color used when there is only one group. Default
#'   `"steelblue"`.
#'
#' @return A `ggplot` object.
#' @export
plot_cluster_timecourse <- function(data,
                                    formula,
                                    cluster_label = NULL,
                                    ci_level = 0.95,
                                    palette = NULL,
                                    time_var = NULL,
                                    point_size = 0,
                                    default_color = "steelblue") {
  assertthat::assert_that(inherits(formula, "formula"),
                          msg = "'formula' must be a formula object.")
  assertthat::assert_that(ci_level > 0 && ci_level < 1,
                          msg = "'ci_level' must be between 0 and 1.")

  parsed <- .parse_tc_formula(formula, time_var)

  value_col <- parsed$value_col
  x_var     <- parsed$x_var
  color_var <- parsed$color_var
  facet_var <- parsed$facet_var
  group_vars <- parsed$group_vars

  # Convert character grouping columns to factors for proper legend ordering
  for (v in c(color_var, facet_var)) {
    if (!is.null(v) && v %in% names(data) && is.character(data[[v]])) {
      data[[v]] <- factor(data[[v]])
    }
  }

  agg <- .aggregate_timecourse(data, value_col, group_vars, ci_level)

  p <- ggplot2::ggplot(agg, ggplot2::aes(
    x = .data[[x_var]],
    y = .data$mean_value
  ))

  if (!is.null(color_var)) {
    p <- p +
      ggplot2::aes(color = .data[[color_var]],
                   fill = .data[[color_var]],
                   group = .data[[color_var]]) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
        alpha = 0.2, color = NA
      ) +
      ggplot2::geom_line(linewidth = 0.8)
  } else {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
        alpha = 0.2, fill = default_color
      ) +
      ggplot2::geom_line(linewidth = 0.8, color = default_color)
  }

  if (point_size > 0) {
    if (!is.null(color_var)) {
      p <- p + ggplot2::geom_point(size = point_size)
    } else {
      p <- p + ggplot2::geom_point(size = point_size, color = default_color)
    }
  }

  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))
  }

  # Palette (compute once)
  if (!is.null(palette) && !is.null(color_var)) {
    pal_values <- if (is.function(palette)) {
      palette(length(unique(agg[[color_var]])))
    } else {
      palette
    }
    p <- p +
      ggplot2::scale_color_manual(values = pal_values) +
      ggplot2::scale_fill_manual(values = pal_values)
  }

  p <- p +
    ggplot2::labs(
      title = cluster_label,
      x = x_var,
      y = paste0("Mean ", value_col),
      color = color_var,
      fill = color_var
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )

  p
}


# -- Formula parsing -----------------------------------------------------------

.parse_tc_formula <- function(formula, time_var = NULL) {
  lhs <- rlang::f_lhs(formula)
  value_col <- if (!is.null(lhs)) as.character(lhs) else "value"

  rhs_terms <- all.vars(rlang::f_rhs(formula))

  if (!is.null(time_var)) {
    x_var <- time_var
  } else if ("time" %in% rhs_terms) {
    x_var <- "time"
  } else {
    x_var <- rhs_terms[length(rhs_terms)]
  }

  non_time <- setdiff(rhs_terms, x_var)
  color_var <- if (length(non_time) >= 1) non_time[length(non_time)] else NULL
  facet_var <- if (length(non_time) >= 2) non_time[1] else NULL

  list(
    value_col = value_col,
    x_var = x_var,
    color_var = color_var,
    facet_var = facet_var,
    group_vars = c(x_var, non_time)
  )
}


# -- Aggregation ---------------------------------------------------------------

.aggregate_timecourse <- function(data, value_col, group_vars, ci_level = 0.95) {
  z <- stats::qnorm(1 - (1 - ci_level) / 2)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      mean_value = mean(.data[[value_col]], na.rm = TRUE),
      se_value = stats::sd(.data[[value_col]], na.rm = TRUE) /
        sqrt(dplyr::n()),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ci_lower = .data$mean_value - z * .data$se_value,
      ci_upper = .data$mean_value + z * .data$se_value
    )
}


# -- Shared label builder ------------------------------------------------------

#' Build a Display Label for a Cluster
#'
#' Constructs a human-readable label like `"pos_1: left FrontalA (89 vox)"`
#' from a single row of an enriched cluster table.
#'
#' @param row A one-row data frame (or tibble) with columns `cluster_id`,
#'   `hemisphere`, `atlas_label`, and `n_voxels`.
#' @return A character string.
#' @keywords internal
#' @export
cluster_display_label <- function(row) {
  cid <- row$cluster_id[1]
  hemi <- row$hemisphere[1]
  label <- row$atlas_label[1]
  nvox <- row$n_voxels[1]

  paste0(
    cid, ": ",
    if (!is.na(hemi)) paste0(hemi, " ") else "",
    if (!is.na(label)) label else "Unknown",
    " (", nvox, " vox)"
  )
}


#' Plot All Cluster Time Courses
#'
#' Convenience wrapper that generates one plot per cluster for a single formula.
#'
#' @param tc_data Full time-course tibble (all clusters) from
#'   [extract_cluster_data()].
#' @param formula Formula for plot layout (see [plot_cluster_timecourse()]).
#' @param cluster_table Enriched cluster table from [enrich_cluster_table()]
#'   (used for labels).
#' @param ci_level Confidence interval level.
#' @param palette Color palette.
#'
#' @return Named list of ggplot objects, one per cluster_id.
#' @export
plot_all_clusters <- function(tc_data, formula, cluster_table,
                              ci_level = 0.95, palette = NULL) {
  if (nrow(tc_data) == 0) return(list())

  # Pre-split for O(1) per-cluster lookup
  tc_split <- split(tc_data, tc_data$cluster_id)
  ct_split <- split(cluster_table, cluster_table$cluster_id)

  cluster_ids <- intersect(names(tc_split), cluster_table$cluster_id)
  plots <- stats::setNames(vector("list", length(cluster_ids)), cluster_ids)

  for (cid in cluster_ids) {
    row <- ct_split[[cid]]
    label <- if (!is.null(row) && nrow(row) > 0) {
      cluster_display_label(row)
    } else {
      cid
    }

    plots[[cid]] <- plot_cluster_timecourse(
      data = tc_split[[cid]],
      formula = formula,
      cluster_label = label,
      ci_level = ci_level,
      palette = palette
    )
  }

  plots
}
