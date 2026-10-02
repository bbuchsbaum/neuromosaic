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
#' @param label_radius Search radius in millimetres for peaks that fall
#'   outside every atlas region: the nearest labelled voxel within this radius
#'   names the peak, and the table records the distance
#'   (`label_distance_mm`) and `label_source = "nearest"`. `0` disables the
#'   search.
#' @param cerebellum Logical; when the atlas has no cerebellar regions, label
#'   cerebellar peaks from `cerebellum_reference` (FreeSurfer labels 8 and
#'   47). `FALSE` never consults a reference.
#' @param cerebellum_reference Cerebellar reference segmentation used when
#'   `cerebellum = TRUE` and the atlas has no cerebellum: `NULL` or `FALSE`
#'   (none; nothing is fetched), `TRUE` or `"hcp"` (the HCP grayordinate
#'   subcortical segmentation, TemplateFlow `tpl-MNI152NLin6Asym` `atlas-HCP`
#'   `dseg`, 2 mm, fetched once per session), a `NeuroVol`, or a path to a
#'   label volume in the map's space. Defaults to the global option
#'   `neuromosaic.cerebellum_reference`, so an explicit argument always wins.
#'   Without a reference, or when it cannot be obtained, cerebellar peaks keep
#'   the nearest-region or "outside atlas" label. Peaks labelled from the
#'   reference have `label_source = "cerebellum"`.
#'
#' @details Clusters are numbered within sign by peak strength: `P1` is the
#'   positive cluster with the largest peak value, `N1` the negative cluster
#'   with the most negative peak, and so on (ties go to the larger cluster).
#'   Numbering never changes a value, coordinate or cluster extent. The rows
#'   returned are the `max_clusters` strongest by |peak|, except that each sign
#'   with a surviving cluster keeps at least its strongest one; they are
#'   ordered by |peak|. The complete (untruncated) labelled cluster list is
#'   attached as `attr(x, "all_clusters")`. `hemisphere` follows the peak's
#'   x coordinate (`"left"`, `"right"`, or `"midline"` when |x| < 2 mm), the
#'   same rule for labelled and unlabelled peaks.
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
                               map_id = NULL,
                               label_radius = 4,
                               cerebellum = TRUE,
                               cerebellum_reference = getOption(
                                 "neuromosaic.cerebellum_reference", NULL
                               )) {
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

  if (!is.numeric(label_radius) || length(label_radius) != 1L ||
      !is.finite(label_radius) || label_radius < 0) {
    stop("'label_radius' must be a non-negative number.", call. = FALSE)
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
  all_clusters <- .montage_sign_cluster_ids(cluster_data$cluster_table)
  selected <- .montage_select_peak_rows(all_clusters, as.integer(max_clusters))
  # Label every surviving cluster (the downloadable list is untruncated), then
  # report the selected rows in the same |peak| order as before.
  enriched <- enrich_cluster_table(
    cluster_table = all_clusters,
    stat_map = stat,
    atlas = atlas,
    max_clusters = max(nrow(all_clusters), 1L)
  )
  enriched <- .montage_add_atlas_networks(enriched, atlas)
  enriched <- .montage_fill_peak_labels(
    enriched, atlas, radius = label_radius, cerebellum = isTRUE(cerebellum),
    reference = cerebellum_reference
  )
  enriched <- .montage_peak_hemisphere(enriched)
  if (!is.null(map_id) && nrow(enriched) > 0L) {
    enriched$map_id <- as.character(map_id)
    enriched <- enriched[, c("map_id", setdiff(names(enriched), "map_id")),
                         drop = FALSE]
  }
  keep_ids <- as.character(all_clusters$cluster_id[selected])
  out <- enriched[match(keep_ids, as.character(enriched$cluster_id)), , drop = FALSE]
  out <- out[order(abs(out$max_stat), decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out <- .montage_peak_table_totals(out, all_clusters)
  attr(out, "cerebellum_source") <- attr(enriched, "cerebellum_source")
  attr(out, "all_clusters") <- enriched
  out
}

# Cluster IDs numbered within sign by peak strength: P1 is the positive
# cluster with the largest peak, N1 the negative cluster with the most negative
# peak (ties: larger cluster first, then extraction order). So "P1" in the
# table, the CSV, the glance card and the viewer's starting point is always the
# strongest positive effect. Row order and every other column are unchanged.
.montage_sign_cluster_ids <- function(clusters) {
  if (!is.data.frame(clusters) || nrow(clusters) == 0L ||
      !all(c("cluster_id", "sign") %in% names(clusters))) {
    return(clusters)
  }
  sign <- as.character(clusters$sign)
  id_num <- suppressWarnings(as.numeric(gsub("\\D+", "", clusters$cluster_id)))
  strength <- if ("max_stat" %in% names(clusters)) {
    abs(suppressWarnings(as.numeric(clusters$max_stat)))
  } else {
    rep(NA_real_, nrow(clusters))
  }
  size <- if ("n_voxels" %in% names(clusters)) {
    suppressWarnings(as.numeric(clusters$n_voxels))
  } else {
    rep(NA_real_, nrow(clusters))
  }
  out <- as.character(clusters$cluster_id)
  for (s in c("positive", "negative")) {
    hit <- which(sign == s)
    if (!length(hit)) next
    ord <- order(-strength[hit], -size[hit], id_num[hit], seq_along(hit),
                 na.last = TRUE)
    rank <- integer(length(hit))
    rank[ord] <- seq_along(hit)
    out[hit] <- paste0(if (s == "positive") "P" else "N", rank)
  }
  clusters$cluster_id <- out
  clusters
}

# Peaks the exact atlas lookup leaves unlabelled: a cerebellar label when the
# peak lies in the cerebellum (and the atlas has none), otherwise the nearest
# labelled atlas voxel within `radius` mm. Adds `label_source` ("atlas",
# "cerebellum", "nearest" or NA) and `label_distance_mm`. Never invents a
# label: anything without a labelled voxel within `radius` stays NA.
.montage_fill_peak_labels <- function(out, atlas, radius = 4, cerebellum = TRUE,
                                      reference = getOption(
                                        "neuromosaic.cerebellum_reference", NULL
                                      )) {
  if (!is.data.frame(out) || nrow(out) == 0L ||
      !all(c("peak_mni_x", "peak_mni_y", "peak_mni_z") %in% names(out))) {
    if (is.data.frame(out) && nrow(out) == 0L) {
      out$label_source <- character(0)
      out$label_distance_mm <- numeric(0)
    }
    return(out)
  }
  label <- if ("atlas_label" %in% names(out)) as.character(out$atlas_label) else
    rep(NA_character_, nrow(out))
  for (col in c("hemisphere", "network")) {
    if (!col %in% names(out)) out[[col]] <- NA_character_
    out[[col]] <- as.character(out[[col]])
  }
  have <- !is.na(label) & nzchar(trimws(label))
  source <- ifelse(have, "atlas", NA_character_)
  distance <- ifelse(have, 0, NA_real_)
  missing <- which(!have)
  if (length(missing)) {
    coords <- cbind(out$peak_mni_x, out$peak_mni_y, out$peak_mni_z)[missing, , drop = FALSE]
    ids <- suppressWarnings(as.integer(atlas$ids))
    labels <- as.character(atlas$labels)
    hemi <- if (length(atlas$hemi) == length(ids)) as.character(atlas$hemi) else
      rep(NA_character_, length(ids))
    near <- if (radius > 0 && !is.null(atlas$atlas)) {
      .montage_nearest_labels(atlas$atlas, coords, radius)
    } else {
      NULL
    }
    cere_vol <- if (isTRUE(cerebellum) && !.montage_atlas_has_cerebellum(atlas)) {
      .montage_cerebellum_reference(reference)
    }
    cere <- if (!is.null(cere_vol)) {
      .montage_nearest_labels(cere_vol, coords, radius, keep = c(8L, 47L))
    }
    for (k in seq_along(missing)) {
      r <- missing[[k]]
      d_atlas <- if (!is.null(near)) near$distance[[k]] else NA_real_
      d_cere <- if (!is.null(cere)) cere$distance[[k]] else NA_real_
      use_cere <- is.finite(d_cere) && (d_cere == 0 || !is.finite(d_atlas) || d_cere < d_atlas)
      if (use_cere) {
        out$atlas_label[[r]] <- "Cerebellum"
        out$hemisphere[[r]] <- if (cere$id[[k]] == 8L) "left" else "right"
        out$network[[r]] <- NA_character_
        source[[r]] <- if (d_cere == 0) "cerebellum" else "nearest"
        distance[[r]] <- d_cere
      } else if (is.finite(d_atlas)) {
        at <- match(near$id[[k]], ids)
        if (is.na(at)) next
        out$atlas_label[[r]] <- labels[[at]]
        out$hemisphere[[r]] <- hemi[[at]]
        out$network[[r]] <- .montage_atlas_network_at(atlas, at)
        source[[r]] <- "nearest"
        distance[[r]] <- d_atlas
      }
    }
  }
  out$label_source <- source
  out$label_distance_mm <- distance
  if (exists("cere_vol", inherits = FALSE) && !is.null(cere_vol) &&
      any(source %in% c("cerebellum", "nearest") & out$atlas_label %in% "Cerebellum")) {
    attr(out, "cerebellum_source") <- attr(cere_vol, "nm_source") %||%
      "cerebellar segmentation"
  }
  out
}

# One hemisphere rule for every view of a peak (table, CSV, glance card,
# viewer cluster list): the side of the peak's x coordinate, "midline" when
# |x| < 2 mm (the midsagittal voxel column of a 2 mm grid). Rows without a
# finite x keep the atlas hemisphere.
.montage_midline_mm <- 2
.montage_hemisphere_from_x <- function(x, fallback = rep(NA_character_, length(x))) {
  x <- suppressWarnings(as.numeric(x))
  out <- as.character(fallback)
  ok <- is.finite(x)
  out[ok] <- ifelse(abs(x[ok]) < .montage_midline_mm, "midline",
                    ifelse(x[ok] < 0, "left", "right"))
  out
}
.montage_peak_hemisphere <- function(out) {
  if (!is.data.frame(out) || !nrow(out) || !"peak_mni_x" %in% names(out)) return(out)
  fallback <- if ("hemisphere" %in% names(out)) out$hemisphere else NA_character_
  out$hemisphere <- .montage_hemisphere_from_x(out$peak_mni_x, fallback)
  out
}

# Nearest non-zero label of `vol` within `radius` mm of each world coordinate
# (exact voxel first: distance 0). Returns list(id, distance); NA when none.
.montage_nearest_labels <- function(vol, coords, radius, keep = NULL) {
  n <- nrow(coords)
  ids <- rep(NA_integer_, n)
  dist <- rep(NA_real_, n)
  sp <- neuroim2::space(vol)
  dims <- dim(vol)[1:3]
  spacing <- as.numeric(neuroim2::spacing(sp))[1:3]
  if (any(!is.finite(spacing)) || any(spacing <= 0)) spacing <- rep(1, 3)
  reach <- as.integer(ceiling(max(radius, 0) / spacing)) + 1L
  offsets <- as.matrix(expand.grid(
    seq.int(-reach[1], reach[1]), seq.int(-reach[2], reach[2]),
    seq.int(-reach[3], reach[3])
  ))
  centre <- round(neuroim2::coord_to_grid(sp, coords))
  if (is.null(dim(centre))) centre <- matrix(centre, nrow = 1L)
  for (i in seq_len(n)) {
    if (any(!is.finite(coords[i, ]))) next
    cand <- sweep(offsets, 2L, centre[i, ], "+")
    inside <- cand[, 1] >= 1 & cand[, 1] <= dims[1] & cand[, 2] >= 1 &
      cand[, 2] <= dims[2] & cand[, 3] >= 1 & cand[, 3] <= dims[3]
    cand <- cand[inside, , drop = FALSE]
    if (!nrow(cand)) next
    vals <- as.integer(vol[cand])
    hit <- !is.na(vals) & vals != 0L
    if (!is.null(keep)) hit <- hit & vals %in% keep
    if (!any(hit)) next
    world <- neuroim2::grid_to_coord(sp, cand[hit, , drop = FALSE])
    if (is.null(dim(world))) world <- matrix(world, nrow = 1L)
    d <- sqrt(rowSums((world - matrix(coords[i, ], nrow(world), 3L, byrow = TRUE))^2))
    best <- which(d <= radius + 1e-6 | d < 1e-6)
    if (!length(best)) next
    j <- best[which.min(d[best])]
    ids[[i]] <- vals[hit][[j]]
    # A peak inside a labelled voxel is distance 0 whatever the voxel centre.
    centre_val <- as.integer(vol[matrix(centre[i, ], nrow = 1L)])
    dist[[i]] <- if (!is.na(centre_val) && identical(centre_val, ids[[i]])) 0 else round(d[[j]], 1)
  }
  list(id = ids, distance = dist)
}

# Network of atlas region `at` (index into atlas$ids): the atlas column, else
# parsed from the full Schaefer name ("RH_Cont_PFCl_4" -> "Cont").
.montage_atlas_network_at <- function(atlas, at) {
  n <- length(atlas$ids)
  net <- if (length(atlas$network) == n) as.character(atlas$network)[[at]] else NA_character_
  if (is.na(net) || !nzchar(net)) {
    full <- if (length(atlas$orig_labels) == n) as.character(atlas$orig_labels)[[at]] else NA_character_
    net <- .montage_schaefer_network(full)
  }
  net
}

.montage_atlas_has_cerebellum <- function(atlas) {
  any(grepl("cerebell", c(as.character(atlas$labels), as.character(atlas$orig_labels)),
            ignore.case = TRUE))
}

.montage_cerebellum_cache <- new.env(parent = emptyenv())

# The human-readable source of the HCP reference, stated in the report.
.montage_hcp_cerebellum_source <- paste(
  "HCP grayordinate subcortical segmentation",
  "(TemplateFlow tpl-MNI152NLin6Asym atlas-HCP dseg, 2 mm)"
)

# Resolve a cerebellar reference segmentation (FreeSurfer numbering: 8 = left,
# 47 = right cerebellum). `ref` may be NULL/FALSE (none), TRUE or "hcp" (the
# HCP grayordinate subcortical segmentation, MNI152NLin6Asym 2 mm, fetched once
# from TemplateFlow), a NeuroVol (used as-is) or a path to a label volume.
# The default falls back to the global option. Unset means no reference:
# rendering never touches the network unless the caller opted in. The result
# carries attr "nm_source", the description the report states.
.montage_cerebellum_reference <- function(
    ref = getOption("neuromosaic.cerebellum_reference", NULL)) {
  if (is.null(ref) || isFALSE(ref)) return(NULL)
  if (methods::is(ref, "NeuroVol")) {
    if (is.null(attr(ref, "nm_source"))) {
      attr(ref, "nm_source") <- "user-supplied cerebellar segmentation"
    }
    return(ref)
  }
  hcp <- isTRUE(ref) ||
    (is.character(ref) && length(ref) == 1L && identical(tolower(ref), "hcp"))
  if (!hcp) {
    if (!is.character(ref) || length(ref) != 1L || is.na(ref)) {
      stop("'cerebellum_reference' must be NULL, FALSE, TRUE, \"hcp\", ",
           "a NeuroVol, or a file path.", call. = FALSE)
    }
    if (!file.exists(ref)) {
      warning("Cerebellar reference '", ref, "' not found; cerebellar peaks ",
              "keep their nearest-region or 'outside atlas' label.",
              call. = FALSE)
      return(NULL)
    }
    vol <- neuroim2::read_vol(ref)
    attr(vol, "nm_source") <- basename(ref)
    return(vol)
  }
  if (exists("vol", envir = .montage_cerebellum_cache, inherits = FALSE)) {
    return(get("vol", envir = .montage_cerebellum_cache))
  }
  vol <- tryCatch({
    path <- suppressMessages(as.character(neuroatlas::get_template(
      space = "MNI152NLin6Asym", atlas = "HCP", suffix = "dseg",
      resolution = 2, desc = NA, path_only = TRUE
    )))[[1L]]
    if (is.null(path) || !file.exists(path)) NULL else neuroim2::read_vol(path)
  }, error = function(e) NULL)
  if (!is.null(vol)) attr(vol, "nm_source") <- .montage_hcp_cerebellum_source
  assign("vol", vol, envir = .montage_cerebellum_cache)
  vol
}

# Rows of the full cluster table to report: the `max_rows` strongest by
# |peak|, except that every sign with a surviving cluster keeps at least its
# strongest cluster (it replaces the weakest row of an over-represented sign).
# Returned in |peak| order, so the selection never reorders or alters values.
.montage_select_peak_rows <- function(clusters, max_rows) {
  n <- if (is.data.frame(clusters)) nrow(clusters) else 0L
  if (n == 0L) return(integer(0))
  ord <- order(abs(clusters$max_stat), decreasing = TRUE)
  keep <- utils::head(ord, max_rows)
  if (!"sign" %in% names(clusters) || length(keep) >= n) return(keep)
  sign <- as.character(clusters$sign)
  for (s in unique(sign[ord])) {
    if (any(sign[keep] == s)) next
    counts <- table(sign[keep])
    donors <- which(sign[keep] %in% names(counts)[counts > 1L])
    if (length(donors) == 0L) next
    keep[[max(donors)]] <- ord[sign[ord] == s][[1L]]
  }
  keep[order(abs(clusters$max_stat[keep]), decreasing = TRUE)]
}

# Totals of the complete (untruncated) cluster table, attached to the
# reported rows so captions can say "12 of 22 clusters shown".
.montage_peak_table_totals <- function(out, clusters) {
  n_total <- if (is.data.frame(clusters)) nrow(clusters) else 0L
  sign <- if (n_total > 0L && "sign" %in% names(clusters)) {
    as.character(clusters$sign)
  } else {
    character(n_total)
  }
  voxels <- if (n_total > 0L && "n_voxels" %in% names(clusters)) {
    as.numeric(clusters$n_voxels)
  } else {
    numeric(n_total)
  }
  attr(out, "n_total") <- as.integer(n_total)
  attr(out, "n_by_sign") <- c(
    positive = sum(sign == "positive"),
    negative = sum(sign == "negative")
  )
  attr(out, "n_voxels_total") <- sum(voxels, na.rm = TRUE)
  attr(out, "n_voxels_by_sign") <- c(
    positive = sum(voxels[sign == "positive"], na.rm = TRUE),
    negative = sum(voxels[sign == "negative"], na.rm = TRUE)
  )
  out
}

# Fill `network` from the atlas when the peak lookup left it empty (merged
# atlases drop the per-region network column but keep the full Schaefer
# names, e.g. "RH_Default_Temp_3"). Values and coordinates are untouched.
.montage_add_atlas_networks <- function(out, atlas) {
  if (!is.data.frame(out) || nrow(out) == 0L ||
      !all(c("peak_mni_x", "peak_mni_y", "peak_mni_z") %in% names(out))) {
    return(out)
  }
  network <- if ("network" %in% names(out)) as.character(out$network) else
    rep(NA_character_, nrow(out))
  missing <- is.na(network) | !nzchar(network)
  if (!any(missing)) return(out)
  hits <- tryCatch(
    neuroatlas::query_point(
      cbind(out$peak_mni_x, out$peak_mni_y, out$peak_mni_z), atlas
    ),
    error = function(e) NULL
  )
  if (is.null(hits) || nrow(hits) != nrow(out) || !"id" %in% names(hits)) {
    return(out)
  }
  ids <- suppressWarnings(as.integer(atlas$ids))
  at <- match(as.integer(hits$id), ids)
  atlas_network <- if (length(atlas$network) == length(ids)) {
    as.character(atlas$network)[at]
  } else {
    rep(NA_character_, nrow(out))
  }
  full <- if (length(atlas$orig_labels) == length(ids)) {
    as.character(atlas$orig_labels)[at]
  } else {
    rep(NA_character_, nrow(out))
  }
  parsed <- .montage_schaefer_network(full)
  fill <- ifelse(!is.na(atlas_network) & nzchar(atlas_network),
                 atlas_network, parsed)
  network[missing] <- fill[missing]
  out$network <- network
  out
}

# "RH_Default_Temp_3" -> "Default"; NA for names without a network field.
.montage_schaefer_network <- function(full_label) {
  full_label <- as.character(full_label)
  m <- regmatches(full_label,
                  regexec("^(?:LH|RH)_([A-Za-z]+)_.+$", full_label, perl = TRUE))
  vapply(m, function(x) if (length(x) == 2L) x[[2L]] else NA_character_, "")
}

.render_montage_peak_panels <- function(manifest,
                                        atlas,
                                        panels,
                                        max_clusters = 20L,
                                        cerebellum_reference = getOption(
                                          "neuromosaic.cerebellum_reference",
                                          NULL
                                        )) {
  if (is.null(atlas)) {
    stop("'atlas' is required when render_peaks = TRUE.", call. = FALSE)
  }
  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })
  support_masks <- .montage_render_support_masks(manifest, stat_maps)

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    if (!identical(manifest$effective_display_mode[[i]], "thresholded")) {
      panels[[map_id]]$peaks <- list(
        n_clusters = NA_integer_,
        threshold = NA_real_,
        note = "Peak tables are not generated for continuous map variants."
      )
      next
    }
    stat <- stat_maps[[i]]
    if (!is.null(support_masks[[i]])) {
      values <- as.array(stat)
      mask <- array(support_masks[[i]], dim = dim(values))
      values[!mask] <- NA_real_
      stat <- neuroim2::NeuroVol(values, space = neuroim2::space(stat))
    }
    peaks <- montage_peak_table(
      stat = stat,
      atlas = atlas,
      threshold = manifest$effective_threshold[[i]],
      tail = manifest$effective_tail[[i]],
      connectivity = manifest$effective_connectivity[[i]],
      min_cluster_size = manifest$effective_min_cluster_size[[i]],
      max_clusters = max_clusters,
      map_id = map_id,
      cerebellum_reference = cerebellum_reference
    )
    panels[[map_id]]$peak_table <- peaks
    if (is.null(panels[[map_id]]$table)) {
      # Parcel maps report surviving parcels by atlas name: voxel clusters of
      # a parcel-valued volume merge neighbouring parcels and split others.
      parcel_values <- .montage_row_parcel_values(manifest, i)
      panels[[map_id]]$table <- if (!is.null(parcel_values)) {
        .montage_parcel_peak_table(
          parcel_values, atlas,
          threshold = manifest$effective_threshold[[i]],
          tail = manifest$effective_tail[[i]],
          max_rows = max_clusters
        )
      } else {
        peaks
      }
    }
    panels[[map_id]]$peaks <- list(
      n_clusters = attr(peaks, "n_total") %||% nrow(peaks),
      n_shown = nrow(peaks),
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

.montage_row_parcel_values <- function(manifest, i) {
  if (!"parcel_values" %in% names(manifest)) return(NULL)
  values <- manifest$parcel_values[[i]]
  if (is.null(values) || !length(values) || is.null(names(values))) return(NULL)
  values
}

# Surviving parcels of a parcel-valued map, strongest first, labelled from the
# atlas. `n_total` records how many survived before truncation to `max_rows`.
.montage_parcel_peak_table <- function(values, atlas, threshold, tail,
                                       max_rows = 20L) {
  ids <- suppressWarnings(as.integer(names(values)))
  values <- as.numeric(values)
  keep <- is.finite(values) & !is.na(ids)
  if (is.finite(threshold)) {
    keep <- keep & switch(
      as.character(tail),
      positive = values >= threshold,
      negative = values <= -threshold,
      abs(values) >= threshold
    )
  }
  atlas_ids <- suppressWarnings(as.integer(atlas$ids))
  pick <- function(field) {
    x <- atlas[[field]]
    if (is.null(x) || length(x) != length(atlas_ids)) {
      return(rep(NA_character_, sum(keep)))
    }
    as.character(x[match(ids[keep], atlas_ids)])
  }
  out <- data.frame(
    parcel_id = ids[keep],
    parcel = pick("labels"),
    hemisphere = pick("hemi"),
    network = pick("network"),
    value = values[keep],
    stringsAsFactors = FALSE
  )
  out <- out[order(-abs(out$value)), , drop = FALSE]
  n_total <- nrow(out)
  out <- utils::head(out, max_rows)
  rownames(out) <- NULL
  attr(out, "n_total") <- n_total
  class(out) <- c("montage_parcel_table", class(out))
  out
}

# ---- Label provenance -------------------------------------------------------

# "Schaefer-200-7networks::ASEG" -> c("Schaefer-200 (7 networks)", "ASEG").
.montage_atlas_display_names <- function(atlas) {
  name <- as.character(atlas$name %||% atlas$metadata$identity$name %||% "")[[1L]]
  if (is.na(name) || !nzchar(name)) return(character(0))
  parts <- trimws(strsplit(name, "::", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  parts <- sub("^Schaefer-(\\d+)-(\\d+)networks$", "Schaefer-\\1 (\\2 networks)",
               parts, ignore.case = TRUE)
  parts[toupper(parts) == "ASEG"] <- "FreeSurfer ASEG"
  parts
}

# Citations the atlas carries (neuroatlas metadata): "Schaefer et al., 2018"
# with a DOI where known.
.montage_atlas_citations <- function(atlas) {
  cit <- tryCatch(as.data.frame(atlas$metadata$citations), error = function(e) NULL)
  if (!is.data.frame(cit) || !nrow(cit) || !"author" %in% names(cit)) {
    return(list())
  }
  lapply(seq_len(nrow(cit)), function(i) {
    author <- as.character(cit$author[[i]])
    surname <- sub("^.*\\s(\\S+)$", "\\1", sub("\\s+and others$", "", author))
    etal <- grepl("and others|,| and ", author)
    list(
      short = paste0(surname, if (etal) " et al." else "", ", ",
                     as.character(cit$year[[i]] %||% "")),
      title = as.character(cit$title[[i]] %||% NA),
      journal = as.character(cit$journal[[i]] %||% NA),
      doi = as.character(cit$doi[[i]] %||% NA)
    )
  })
}

# What the report must say about where peak labels came from: atlas names
# and citations, the nearest-region radius, and the cerebellar reference when
# it was consulted (with how many peaks it named). NULL without an atlas.
.montage_labelling_summary <- function(atlas, panels, reference = NULL,
                                       radius = 4) {
  if (is.null(atlas)) return(NULL)
  tables <- lapply(panels, function(p) {
    t <- if (is.list(p)) p[["peak_table"]] else NULL
    all <- attr(t, "all_clusters")
    if (is.data.frame(all)) all else t
  })
  tables <- Filter(is.data.frame, tables)
  col <- function(t, name) if (name %in% names(t)) as.character(t[[name]]) else
    rep(NA_character_, nrow(t))
  source <- unlist(lapply(tables, col, "label_source"), use.names = FALSE)
  label <- unlist(lapply(tables, col, "atlas_label"), use.names = FALSE)
  cere_used <- !is.null(reference) && !.montage_atlas_has_cerebellum(atlas)
  n_cere <- if (cere_used) {
    sum(source %in% c("cerebellum", "nearest") & label %in% "Cerebellum")
  } else {
    0L
  }
  hcp <- identical(attr(reference, "nm_source"), .montage_hcp_cerebellum_source)
  list(
    atlases = .montage_atlas_display_names(atlas),
    atlas_id = as.character(atlas$name %||% NA)[[1L]],
    citations = .montage_atlas_citations(atlas),
    radius = radius,
    n_peaks = length(source),
    n_nearest = sum(source %in% "nearest" & !label %in% "Cerebellum"),
    n_unlabelled = sum(is.na(source)),
    cerebellum = if (cere_used) {
      list(
        source = attr(reference, "nm_source") %||% "cerebellar segmentation",
        short = if (hcp) "HCP grayordinate segmentation (TemplateFlow)" else
          attr(reference, "nm_source") %||% "cerebellar segmentation",
        hcp = hcp,
        n = n_cere,
        citations = if (hcp) list(
          list(short = "Glasser et al., 2013",
               title = "The minimal preprocessing pipelines for the Human Connectome Project",
               journal = "NeuroImage", doi = "10.1016/j.neuroimage.2013.04.127"),
          list(short = "Ciric et al., 2022",
               title = "TemplateFlow: FAIR-sharing of multi-scale, multi-species brain models",
               journal = "Nature Methods", doi = "10.1038/s41592-022-01681-2")
        ) else list()
      )
    }
  )
}
