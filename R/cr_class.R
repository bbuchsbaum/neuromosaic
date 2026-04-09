# -- cluster_report_result S3 class --------------------------------------------

#' Construct a Cluster Report Result
#'
#' @param cluster_table Enriched cluster table with MNI coords and labels.
#' @param cluster_parcels Cluster-parcel overlap tibble.
#' @param time_courses Extracted time-course tibble.
#' @param plots Named list (by formula name) of named lists (by cluster_id)
#'   of ggplot objects.
#' @param mni_table Formatted gt/flextable/kable object.
#' @param report_path Path to rendered report, or `NULL`.
#' @param params List of pipeline parameters (threshold, tail, etc.).
#' @return A `cluster_report_result` object.
#' @keywords internal
#' @export
new_cluster_report_result <- function(cluster_table,
                                      cluster_parcels,
                                      time_courses,
                                      plots,
                                      brain_slices = NULL,
                                      mni_table,
                                      report_path,
                                      params) {
  structure(
    list(
      cluster_table   = cluster_table,
      cluster_parcels = cluster_parcels,
      time_courses    = time_courses,
      plots           = plots,
      brain_slices    = brain_slices,
      mni_table       = mni_table,
      report_path     = report_path,
      params          = params
    ),
    class = "cluster_report_result"
  )
}


#' Print a Cluster Report Result
#'
#' @param x A `cluster_report_result` object.
#' @param ... Ignored.
#' @export
print.cluster_report_result <- function(x, ...) {
  n <- nrow(x$cluster_table)
  cat(sprintf("Cluster Report: %d cluster%s\n", n, if (n != 1) "s" else ""))
  cat(sprintf("  Threshold: %s | Tail: %s | Min size: %s\n",
              x$params$threshold, x$params$tail,
              x$params$min_cluster_size))
  if (!is.null(x$params$atlas_name)) {
    cat(sprintf("  Atlas: %s\n", x$params$atlas_name))
  }
  if (identical(x$params$report_mode, "table_only")) {
    cat("  Mode: table-only\n")
  }
  if (!is.null(x$report_path)) {
    cat(sprintf("  Report: %s\n", x$report_path))
  }
  if (n > 0) {
    cat("\nTop clusters:\n")
    cols <- intersect(
      c("cluster_id", "n_voxels", "peak_mni_x", "peak_mni_y", "peak_mni_z",
        "max_stat", "atlas_label", "hemisphere"),
      names(x$cluster_table)
    )
    print(utils::head(x$cluster_table[, cols, drop = FALSE], 6))
  }
  if (length(x$plots) > 0) {
    fnames <- names(x$plots)
    n_plots <- sum(vapply(x$plots, length, integer(1)))
    cat(sprintf("\nPlots: %d across %d formula%s (%s)\n",
                n_plots, length(fnames),
                if (length(fnames) != 1) "s" else "",
                paste(fnames, collapse = ", ")))
  }
  invisible(x)
}


#' Summarize a Cluster Report Result
#'
#' @param object A `cluster_report_result` object.
#' @param ... Ignored.
#' @export
summary.cluster_report_result <- function(object, ...) {
  cat("Cluster Report Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat("Parameters:\n")
  cat(sprintf("  Threshold:        %s\n", object$params$threshold))
  cat(sprintf("  Tail:             %s\n", object$params$tail))
  cat(sprintf("  Min cluster size: %s\n", object$params$min_cluster_size))
  cat(sprintf("  Max clusters:     %s\n", object$params$max_clusters))
  cat(sprintf("  Atlas:            %s\n",
              object$params$atlas_name %||% "Unknown"))
  if (!is.null(object$params$report_mode)) {
    cat(sprintf("  Mode:             %s\n", object$params$report_mode))
  }
  cat("\n")

  ct <- object$cluster_table
  n <- nrow(ct)
  cat(sprintf("Clusters: %d\n", n))

  if (n > 0) {
    n_pos <- sum(ct$sign == "positive", na.rm = TRUE)
    n_neg <- sum(ct$sign == "negative", na.rm = TRUE)
    cat(sprintf("  Positive: %d | Negative: %d\n", n_pos, n_neg))
    cat(sprintf("  Total voxels: %d\n", sum(ct$n_voxels)))
    cat(sprintf("  Peak stat range: [%.2f, %.2f]\n",
                min(ct$max_stat), max(ct$max_stat)))
    cat("\n")

    # Per-cluster summary
    for (i in seq_len(n)) {
      row <- ct[i, , drop = FALSE]
      label <- cluster_display_label(row)
      cat(sprintf("  %s  |  stat=%.2f  MNI=(%s, %s, %s)\n",
                  label, row$max_stat[1],
                  row$peak_mni_x[1], row$peak_mni_y[1], row$peak_mni_z[1]))
    }
  }

  if (!is.null(object$cluster_parcels) && nrow(object$cluster_parcels) > 0) {
    cat(sprintf("\nParcel overlaps: %d entries across %d clusters\n",
                nrow(object$cluster_parcels),
                length(unique(object$cluster_parcels$cluster_id))))
  }

  if (length(object$plots) > 0) {
    cat(sprintf("\nFormulas: %s\n",
                paste(names(object$plots), collapse = ", ")))
  }

  invisible(object)
}


#' Plot a Cluster Report Result
#'
#' @param x A `cluster_report_result` object.
#' @param which Integer index or character cluster_id selecting which cluster
#'   to plot. Default `1L` (the first cluster).
#' @param formula_name Name of the formula to plot. If `NULL`, uses the first.
#' @param ... Passed to print method of the ggplot.
#' @return The ggplot object (invisibly).
#' @export
plot.cluster_report_result <- function(x, which = 1L, formula_name = NULL, ...) {
  if (length(x$plots) == 0) {
    message("No plots available.")
    return(invisible(NULL))
  }

  fname <- formula_name %||% names(x$plots)[1]
  if (!fname %in% names(x$plots)) {
    stop(sprintf("Formula '%s' not found. Available: %s",
                 fname, paste(names(x$plots), collapse = ", ")),
         call. = FALSE)
  }

  formula_plots <- x$plots[[fname]]
  if (length(formula_plots) == 0) {
    message("No plots for formula '", fname, "'.")
    return(invisible(NULL))
  }

  if (is.character(which)) {
    cid <- which
  } else {
    cid <- names(formula_plots)[min(which, length(formula_plots))]
  }

  if (!cid %in% names(formula_plots)) {
    stop(sprintf("Cluster '%s' not found. Available: %s",
                 cid, paste(names(formula_plots), collapse = ", ")),
         call. = FALSE)
  }

  p <- formula_plots[[cid]]
  print(p)
  invisible(p)
}


#' Export Cluster Report Results to CSV
#'
#' Write cluster tables and time-course data as CSV files suitable for
#' supplementary materials.
#'
#' @param x A `cluster_report_result` object.
#' @param dir Directory to write into. Default `"."`.
#' @param prefix Filename prefix. Default `"cluster_report"`.
#' @param ... Ignored.
#' @return Invisibly, a character vector of written file paths.
#' @export
export_csv <- function(x, ...) UseMethod("export_csv")

#' @rdname export_csv
#' @export
export_csv.cluster_report_result <- function(x,
                                             dir = ".",
                                             prefix = "cluster_report",
                                             ...) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  paths <- character()

  # Cluster table
  p <- file.path(dir, paste0(prefix, "_clusters.csv"))
  utils::write.csv(x$cluster_table, p, row.names = FALSE)
  paths <- c(paths, p)

  # Parcel overlaps
  if (!is.null(x$cluster_parcels) && nrow(x$cluster_parcels) > 0) {
    p <- file.path(dir, paste0(prefix, "_parcels.csv"))
    utils::write.csv(x$cluster_parcels, p, row.names = FALSE)
    paths <- c(paths, p)
  }

  # Time courses
  if (!is.null(x$time_courses) && nrow(x$time_courses) > 0) {
    p <- file.path(dir, paste0(prefix, "_timecourses.csv"))
    utils::write.csv(x$time_courses, p, row.names = FALSE)
    paths <- c(paths, p)
  }

  message("Exported ", length(paths), " CSV file(s) to ", normalizePath(dir))
  invisible(paths)
}
