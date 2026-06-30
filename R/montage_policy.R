.montage_default_threshold <- function(stat_kind, df, p, tail) {
  stat_kind <- tolower(as.character(stat_kind))
  tail <- as.character(tail)
  p <- as.numeric(p)

  out <- rep(NA_real_, length(stat_kind))
  for (i in seq_along(stat_kind)) {
    if (!is.finite(p[[i]]) || p[[i]] <= 0 || p[[i]] >= 1) {
      stop("'p' must be between 0 and 1.", call. = FALSE)
    }
    upper_prob <- if (identical(tail[[i]], "two_sided")) {
      1 - p[[i]] / 2
    } else {
      1 - p[[i]]
    }

    if (identical(stat_kind[[i]], "z")) {
      out[[i]] <- stats::qnorm(upper_prob)
    } else if (identical(stat_kind[[i]], "t")) {
      if (is.na(df[[i]]) || !is.finite(df[[i]]) || df[[i]] <= 0) {
        stop(
          "Rows with stat_kind 't' require positive finite df for threshold derivation.",
          call. = FALSE
        )
      }
      out[[i]] <- stats::qt(upper_prob, df = df[[i]])
    } else {
      stop(
        "Cannot derive a threshold for stat_kind '", stat_kind[[i]],
        "'. Supply an explicit threshold or custom policy function.",
        call. = FALSE
      )
    }
  }

  out
}

#' Define a Montage Report Policy
#'
#' Creates the declarative policy used by montage reports. Threshold behavior is
#' function-valued: callers may pass a custom function for TFCE, FDR, or other
#' study-specific rules without changing the render engine.
#'
#' @param p Default p-value used when a manifest row has no `p` or `threshold`.
#' @param q Optional Benjamini-Hochberg FDR q-value. When supplied, rows without
#'   an explicit `threshold` use the map's finite statistic values to derive a
#'   q-controlled critical statistic. A manifest `q` column can override this per
#'   row. Currently supported for `stat_kind` `"z"` and `"t"`.
#' @param threshold Optional numeric threshold or function. A function receives
#'   `stat_kind`, `df`, `p`, and `tail` vectors (one element per map) and must
#'   return a numeric threshold per map, expressed in the statistic's own units
#'   (e.g. a critical t or z value); the engine keeps voxels with `|stat|`
#'   at or above it. A manifest `threshold` column overrides this per row.
#' @param tail Default cluster tail policy.
#' @param connectivity Default cluster connectivity policy.
#' @param min_cluster_size Default minimum cluster size.
#' @param cap_within Character vector of manifest columns that share color caps.
#' @param cap Optional fixed color cap (magnitude). When supplied, every cap
#'   group uses this value and the robust quantile default is ignored. `NULL`
#'   (default) derives a cap per group from the data.
#' @param cap_quantile Quantile of the suprathreshold `|stat|` distribution used
#'   for the data-driven cap when `cap` is `NULL`. Defaults to `0.99`, which is
#'   robust to a few extreme voxels (the raw maximum washes the rest of a strong
#'   map out under proportional/soft alpha).
#' @param cap_floor Optional lower bound applied to the data-driven cap so maps
#'   with a narrow suprathreshold range still span a usable color scale. `NULL`
#'   (default) applies no floor.
#' @param layout Character vector of manifest columns used for nested sections.
#' @param layout_fun Optional custom layout function for non-nested layouts.
#'
#' @details
#' # FDR / q-value thresholding
#'
#' `q` performs Benjamini-Hochberg FDR thresholding per map over finite statistic
#' values. The resolved threshold is written to `effective_threshold`, so the
#' same cutoff is used for volume panels, surface panels, and peak tables.
#' Explicit manifest `threshold` values still win for those rows. Maps with no
#' FDR survivors receive a finite threshold above ordinary observed values and
#' therefore render as empty panels under `render_montage_report(empty =
#' "warning")`.
#'
#' @return A `montage_policy` list.
#' @examples
#' policy <- montage_policy(p = 0.001, tail = "two_sided")
#'
#' # Fixed study-wide rule via a custom function (no map data needed):
#' z_at <- function(stat_kind, df, p, tail) rep(stats::qnorm(0.999), length(p))
#' montage_policy(threshold = z_at)
#'
#' \dontrun{
#' render_montage_report(
#'   manifest,
#'   "report.html",
#'   policy = montage_policy(q = 0.05),
#'   surfatlas = atlas
#' )
#' }
#' @export
montage_policy <- function(p = 0.005,
                           q = NULL,
                           threshold = NULL,
                           tail = c("two_sided", "positive", "negative"),
                           connectivity = c("18-connect", "26-connect",
                                            "6-connect"),
                           min_cluster_size = 10L,
                           cap_within = character(),
                           cap = NULL,
                           cap_quantile = 0.99,
                           cap_floor = NULL,
                           layout = character(),
                           layout_fun = NULL) {
  tail <- match.arg(tail)
  connectivity <- match.arg(connectivity)

  if (!is.numeric(p) || length(p) != 1L || !is.finite(p) ||
      p <= 0 || p >= 1) {
    stop("'p' must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.null(q) && (!is.numeric(q) || length(q) != 1L || !is.finite(q) ||
                      q <= 0 || q >= 1)) {
    stop("'q' must be NULL or a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.null(q) && !is.null(threshold)) {
    stop("'q' cannot be combined with a policy-level 'threshold'.", call. = FALSE)
  }
  if (!is.numeric(min_cluster_size) || length(min_cluster_size) != 1L ||
      !is.finite(min_cluster_size) || min_cluster_size < 1 ||
      min_cluster_size != floor(min_cluster_size)) {
    stop("'min_cluster_size' must be a positive integer.", call. = FALSE)
  }
  if (!is.character(cap_within)) {
    stop("'cap_within' must be a character vector.", call. = FALSE)
  }
  if (!is.null(cap) && (!is.numeric(cap) || length(cap) != 1L ||
                        !is.finite(cap) || cap <= 0)) {
    stop("'cap' must be NULL or a positive number.", call. = FALSE)
  }
  if (!is.numeric(cap_quantile) || length(cap_quantile) != 1L ||
      !is.finite(cap_quantile) || cap_quantile <= 0 || cap_quantile > 1) {
    stop("'cap_quantile' must be a single number in (0, 1].", call. = FALSE)
  }
  if (!is.null(cap_floor) && (!is.numeric(cap_floor) || length(cap_floor) != 1L ||
                              !is.finite(cap_floor) || cap_floor <= 0)) {
    stop("'cap_floor' must be NULL or a positive number.", call. = FALSE)
  }
  if (!is.character(layout)) {
    stop("'layout' must be a character vector.", call. = FALSE)
  }
  if (!is.null(layout_fun) && !is.function(layout_fun)) {
    stop("'layout_fun' must be NULL or a function.", call. = FALSE)
  }

  threshold_fun <- if (is.null(threshold)) {
    .montage_default_threshold
  } else if (is.function(threshold)) {
    threshold
  } else if (is.numeric(threshold) && length(threshold) == 1L &&
             is.finite(threshold) && threshold > 0) {
    force(threshold)
    function(stat_kind, df, p, tail) rep(threshold, length(stat_kind))
  } else {
    stop("'threshold' must be NULL, a positive number, or a function.",
         call. = FALSE)
  }

  structure(
    list(
      p = p,
      q = q,
      threshold = threshold,
      threshold_fun = threshold_fun,
      tail = tail,
      connectivity = connectivity,
      min_cluster_size = as.integer(min_cluster_size),
      cap_within = cap_within,
      cap = cap,
      cap_quantile = cap_quantile,
      cap_floor = cap_floor,
      layout = layout,
      layout_fun = layout_fun
    ),
    class = "montage_policy"
  )
}

#' Resolve Montage Policy Defaults Against a Manifest
#'
#' Applies a [montage_policy()] to a render manifest, preserving per-row manifest
#' overrides where present.
#'
#' @param manifest A render manifest data frame.
#' @param policy A `montage_policy` object.
#' @param empty Action when overlay QC finds a map with no suprathreshold
#'   voxels: `"error"` (default) or `"warning"`. Forwarded to
#'   [validate_manifest()]; only relevant when a `stat_map` list-column triggers
#'   overlay checks.
#' @param stat_maps Optional list of statistic maps used to resolve FDR `q`
#'   thresholds. Required when `policy$q` or a manifest `q` column applies to
#'   any row without an explicit `threshold`.
#'
#' @return The manifest with `effective_threshold`, `effective_tail`,
#'   `effective_connectivity`, `effective_min_cluster_size`, and `cap_key`
#'   columns added.
#' @export
resolve_montage_policy <- function(manifest, policy = montage_policy(),
                                   empty = c("error", "warning"),
                                   stat_maps = NULL) {
  if (!inherits(policy, "montage_policy")) {
    stop("'policy' must be created by montage_policy().", call. = FALSE)
  }
  empty <- match.arg(empty)
  manifest <- validate_manifest(manifest, check_files = FALSE, empty = empty)
  .check_policy_columns(manifest, policy$cap_within, "cap_within")
  .check_policy_columns(manifest, policy$layout, "layout")

  n <- nrow(manifest)
  tail <- .policy_column_or_default(manifest, "tail", policy$tail)
  connectivity <- .policy_column_or_default(
    manifest,
    "connectivity",
    policy$connectivity
  )
  min_cluster_size <- .policy_column_or_default(
    manifest,
    "min_cluster_size",
    policy$min_cluster_size
  )
  p <- .policy_column_or_default(manifest, "p", policy$p)
  q <- .policy_column_or_default(manifest, "q", policy$q %||% NA_real_)
  df <- if ("df" %in% names(manifest)) manifest$df else rep(NA_real_, n)

  threshold <- rep(NA_real_, n)
  has_threshold <- if ("threshold" %in% names(manifest)) {
    !is.na(manifest$threshold)
  } else {
    rep(FALSE, n)
  }
  threshold[has_threshold] <- manifest$threshold[has_threshold]
  if (any(!has_threshold)) {
    threshold[!has_threshold] <- policy$threshold_fun(
      stat_kind = manifest$stat_kind[!has_threshold],
      df = df[!has_threshold],
      p = p[!has_threshold],
      tail = tail[!has_threshold]
    )
  }

  fdr_rows <- !has_threshold & !is.na(q)
  if (any(fdr_rows)) {
    threshold[fdr_rows] <- .montage_fdr_thresholds(
      manifest = manifest,
      stat_maps = stat_maps,
      rows = which(fdr_rows),
      q = q,
      tail = tail
    )
  }

  manifest$effective_threshold <- threshold
  manifest$effective_q <- q
  manifest$effective_tail <- tail
  manifest$effective_connectivity <- connectivity
  manifest$effective_min_cluster_size <- as.integer(min_cluster_size)
  manifest$cap_key <- .policy_cap_key(manifest, policy$cap_within)
  attr(manifest, "montage_policy") <- policy
  manifest
}

.montage_policy_uses_fdr <- function(manifest, policy) {
  if (!is.null(policy$q)) {
    return(TRUE)
  }
  "q" %in% names(manifest) && any(!.missing_numeric(manifest$q))
}

.montage_fdr_thresholds <- function(manifest, stat_maps, rows, q, tail) {
  if (is.null(stat_maps)) {
    stop(
      "FDR q thresholding requires statistic map values. Supply `stat_maps` ",
      "to resolve_montage_policy() or render through render_montage_report().",
      call. = FALSE
    )
  }
  if (!is.list(stat_maps) || length(stat_maps) != nrow(manifest)) {
    stop("'stat_maps' must be a list with one entry per manifest row.", call. = FALSE)
  }

  vapply(rows, function(i) {
    .montage_fdr_threshold_one(
      values = as.numeric(stat_maps[[i]]),
      stat_kind = manifest$stat_kind[[i]],
      df = if ("df" %in% names(manifest)) manifest$df[[i]] else NA_real_,
      q = q[[i]],
      tail = tail[[i]],
      map_id = manifest$map_id[[i]]
    )
  }, numeric(1))
}

.montage_fdr_threshold_one <- function(values, stat_kind, df, q, tail, map_id) {
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q <= 0 || q >= 1) {
    stop("'q' must be between 0 and 1 for map_id '", map_id, "'.", call. = FALSE)
  }

  stat_kind <- tolower(as.character(stat_kind))
  if (!stat_kind %in% c("z", "t")) {
    stop(
      "FDR q thresholding currently supports stat_kind 'z' and 't' only; ",
      "supply an explicit threshold for map_id '", map_id, "'.",
      call. = FALSE
    )
  }
  if (identical(stat_kind, "t") &&
      (is.na(df) || !is.finite(df) || df <= 0)) {
    stop(
      "FDR q thresholding for stat_kind 't' requires positive finite df for ",
      "map_id '", map_id, "'.",
      call. = FALSE
    )
  }

  finite <- is.finite(values)
  values <- values[finite]
  if (length(values) == 0L) {
    return(.Machine$double.xmax)
  }

  pvals <- .montage_stat_p_values(values, stat_kind = stat_kind, df = df,
                                  tail = tail)
  keep <- is.finite(pvals)
  pvals <- pvals[keep]
  values <- values[keep]
  if (length(pvals) == 0L) {
    return(.Machine$double.xmax)
  }

  ord <- order(pvals)
  p_sorted <- pvals[ord]
  ok <- p_sorted <= (seq_along(p_sorted) / length(p_sorted)) * q
  if (!any(ok)) {
    return(.Machine$double.xmax)
  }

  pcrit <- p_sorted[max(which(ok))]
  threshold <- .montage_p_to_stat_threshold(
    p = pcrit, stat_kind = stat_kind, df = df, tail = tail
  )
  if (is.finite(threshold) && threshold > 0) {
    return(threshold)
  }

  accepted <- pvals <= pcrit
  .montage_observed_threshold(values[accepted], tail = tail)
}

.montage_stat_p_values <- function(values, stat_kind, df, tail) {
  p <- if (identical(stat_kind, "z")) {
    if (identical(tail, "positive")) {
      stats::pnorm(values, lower.tail = FALSE)
    } else if (identical(tail, "negative")) {
      stats::pnorm(values)
    } else {
      2 * stats::pnorm(abs(values), lower.tail = FALSE)
    }
  } else if (identical(tail, "positive")) {
    stats::pt(values, df = df, lower.tail = FALSE)
  } else if (identical(tail, "negative")) {
    stats::pt(values, df = df)
  } else {
    2 * stats::pt(abs(values), df = df, lower.tail = FALSE)
  }
  pmax(0, pmin(1, p))
}

.montage_p_to_stat_threshold <- function(p, stat_kind, df, tail) {
  upper_prob <- if (identical(tail, "two_sided")) 1 - p / 2 else 1 - p
  if (identical(stat_kind, "z")) {
    return(stats::qnorm(upper_prob))
  }
  stats::qt(upper_prob, df = df)
}

.montage_observed_threshold <- function(values, tail) {
  if (length(values) == 0L) {
    return(.Machine$double.xmax)
  }
  threshold <- if (identical(tail, "positive")) {
    min(values[values > 0], na.rm = TRUE)
  } else {
    min(abs(values), na.rm = TRUE)
  }
  if (is.finite(threshold) && threshold > 0) threshold else .Machine$double.xmax
}

.policy_column_or_default <- function(manifest, field, default) {
  n <- nrow(manifest)
  if (!field %in% names(manifest)) {
    return(rep(default, n))
  }

  values <- manifest[[field]]
  missing <- is.na(values)
  values[missing] <- default
  values
}

.check_policy_columns <- function(manifest, fields, label) {
  missing <- setdiff(fields, names(manifest))
  if (length(missing) > 0L) {
    stop(
      "Policy ", label, " column(s) not found in manifest: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.policy_cap_key <- function(manifest, cap_within) {
  if (length(cap_within) == 0L) {
    return(as.character(manifest$map_id))
  }
  apply(manifest[, cap_within, drop = FALSE], 1L, function(row) {
    paste(as.character(row), collapse = "/")
  })
}
