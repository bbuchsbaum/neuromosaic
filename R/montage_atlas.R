#' Build a Montage Peak Table
#'
#' Builds the cluster peak table used by montage reports by reusing the
#' single-map cluster-report annotation path: cluster extraction, atlas overlap,
#' MNI coordinate conversion, and `neuroatlas::query_point()` peak labels.
#'
#' @param stat Statistic `NeuroVol` or file path.
#' @param atlas Atlas object passed to [enrich_cluster_table()].
#' @param threshold Positive numeric threshold.
#' @param tail Tail mode.
#' @param connectivity Voxel connectivity for cluster extraction.
#' @param min_cluster_size Minimum cluster size in voxels.
#' @param max_clusters Maximum number of clusters to retain.
#' @param map_id Optional panel map id to add to the returned table.
#'
#' @return A tibble/data frame of enriched cluster peaks.
#' @export
montage_peak_table <- function(stat,
                               atlas,
                               threshold,
                               tail = c("two_sided", "positive", "negative"),
                               connectivity = c("18-connect", "26-connect",
                                                "6-connect"),
                               min_cluster_size = 10L,
                               max_clusters = 20L,
                               map_id = NULL) {
  tail <- match.arg(tail)
  connectivity <- match.arg(connectivity)
  if (is.null(atlas)) {
    stop("'atlas' is required for montage peak annotation.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0) {
    stop("'threshold' must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(min_cluster_size) || length(min_cluster_size) != 1L ||
      !is.finite(min_cluster_size) || min_cluster_size < 1 ||
      min_cluster_size != floor(min_cluster_size)) {
    stop("'min_cluster_size' must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(max_clusters) || length(max_clusters) != 1L ||
      !is.finite(max_clusters) || max_clusters < 1 ||
      max_clusters != floor(max_clusters)) {
    stop("'max_clusters' must be a positive integer.", call. = FALSE)
  }

  stat <- .load_overlay_neurovol(stat, "stat")
  cluster_data <- .build_cluster_report_table_data(
    atlas = atlas,
    stat_map = stat,
    threshold = threshold,
    min_cluster_size = as.integer(min_cluster_size),
    connectivity = connectivity,
    tail = tail
  )
  out <- enrich_cluster_table(
    cluster_table = cluster_data$cluster_table,
    stat_map = stat,
    atlas = atlas,
    max_clusters = as.integer(max_clusters)
  )
  if (!is.null(map_id)) {
    out$map_id <- as.character(map_id)
    out <- out[, c("map_id", setdiff(names(out), "map_id")), drop = FALSE]
  }
  out
}

.render_montage_peak_panels <- function(manifest,
                                        atlas,
                                        panels,
                                        max_clusters = 20L) {
  if (is.null(atlas)) {
    stop("'atlas' is required when render_peaks = TRUE.", call. = FALSE)
  }
  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    peaks <- montage_peak_table(
      stat = stat_maps[[i]],
      atlas = atlas,
      threshold = manifest$effective_threshold[[i]],
      tail = manifest$effective_tail[[i]],
      connectivity = manifest$effective_connectivity[[i]],
      min_cluster_size = manifest$effective_min_cluster_size[[i]],
      max_clusters = max_clusters,
      map_id = map_id
    )
    panels[[map_id]]$peak_table <- peaks
    if (is.null(panels[[map_id]]$table)) {
      panels[[map_id]]$table <- peaks
    }
    panels[[map_id]]$peaks <- list(
      n_clusters = nrow(peaks),
      threshold = manifest$effective_threshold[[i]],
      tail = manifest$effective_tail[[i]],
      connectivity = manifest$effective_connectivity[[i]],
      min_cluster_size = manifest$effective_min_cluster_size[[i]]
    )
  }

  panels
}

.montage_qc_summary <- function(manifest) {
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    row <- manifest[i, , drop = FALSE]
    subjects <- .montage_subject_vector(.montage_row_value(row, "subjects"))
    dropped_subjects <- .montage_subject_vector(.montage_first_row_value(
      row,
      c("dropped_subjects", "excluded_subjects", "drop_subjects")
    ))

    effective_n <- .montage_numeric_row_value(row, "n")
    source_n <- .montage_first_numeric_row_value(
      row,
      c("source_n", "input_n", "total_n", "n_total", "subjects_n")
    )
    if (is.na(source_n) && length(subjects) > 0L) {
      source_n <- length(subjects)
    }
    dropped_n <- .montage_first_numeric_row_value(
      row,
      c("dropped_n", "excluded_n", "drop_n")
    )
    if (is.na(dropped_n) && !is.na(source_n) && !is.na(effective_n)) {
      dropped_n <- max(source_n - effective_n, 0)
    }
    if (is.na(dropped_n) && length(dropped_subjects) > 0L) {
      dropped_n <- length(dropped_subjects)
    }

    has_reported_n <- !is.na(effective_n) || !is.na(source_n) || !is.na(dropped_n)
    has_dropped <- (!is.na(dropped_n) && dropped_n > 0) ||
      length(dropped_subjects) > 0L

    data.frame(
      map_id = as.character(row$map_id[[1]]),
      label = if ("label" %in% names(row)) as.character(row$label[[1]]) else NA_character_,
      effective_n = effective_n,
      source_n = source_n,
      dropped_n = dropped_n,
      subjects = paste(subjects, collapse = ", "),
      dropped_subjects = paste(dropped_subjects, collapse = ", "),
      has_dropped_subjects = has_dropped,
      qc_status = if (!has_reported_n) {
        "not_reported"
      } else if (has_dropped) {
        "dropped_subjects"
      } else {
        "ok"
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.attach_montage_qc <- function(panels, qc) {
  if (is.null(qc) || nrow(qc) == 0L) {
    return(panels)
  }
  for (i in seq_len(nrow(qc))) {
    map_id <- as.character(qc$map_id[[i]])
    if (!map_id %in% names(panels)) {
      next
    }
    panels[[map_id]]$qc <- qc[i, , drop = FALSE]
  }
  panels
}

.montage_row_value <- function(row, field) {
  if (!field %in% names(row)) {
    return(NULL)
  }
  row[[field]][[1]]
}

.montage_first_row_value <- function(row, fields) {
  for (field in fields) {
    value <- .montage_row_value(row, field)
    if (!.montage_is_missing_value(value)) {
      return(value)
    }
  }
  NULL
}

.montage_numeric_row_value <- function(row, field) {
  value <- .montage_row_value(row, field)
  if (.montage_is_missing_value(value)) {
    return(NA_real_)
  }
  out <- suppressWarnings(as.numeric(value[[1]]))
  if (length(out) != 1L || !is.finite(out)) {
    return(NA_real_)
  }
  out
}

.montage_first_numeric_row_value <- function(row, fields) {
  for (field in fields) {
    value <- .montage_numeric_row_value(row, field)
    if (!is.na(value)) {
      return(value)
    }
  }
  NA_real_
}

.montage_subject_vector <- function(x) {
  if (.montage_is_missing_value(x)) {
    return(character())
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(character())
  }
  pieces <- unlist(strsplit(x, "[,;|]+", perl = TRUE), use.names = FALSE)
  pieces <- trimws(pieces)
  pieces[nzchar(pieces)]
}

.montage_is_missing_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(TRUE)
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  length(x) == 0L || all(is.na(x)) ||
    all(!nzchar(trimws(as.character(x))))
}
