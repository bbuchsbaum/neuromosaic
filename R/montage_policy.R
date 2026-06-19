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
#' @param threshold Optional numeric threshold or function. A function receives
#'   `stat_kind`, `df`, `p`, and `tail` vectors (one element per map) and must
#'   return a numeric threshold per map, expressed in the statistic's own units
#'   (e.g. a critical t or z value); the engine keeps voxels with `|stat|`
#'   at or above it. A manifest `threshold` column overrides this per row. The
#'   function sees only those scalar policy fields, not the map values, so
#'   data-dependent corrections such as FDR must be precomputed per map and
#'   supplied through the manifest `threshold` column (see Details).
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
#' There is no built-in `q =` option: the built-in p-to-threshold rule
#' (`stat_kind` `"z"`/`"t"`) is uncorrected. FDR is data-dependent — the cutoff
#' depends on the whole p-value distribution of a map — and the policy
#' `threshold` function only sees `(stat_kind, df, p, tail)`, not the map
#' values, so it cannot compute it. The supported pattern is to precompute the
#' FDR-critical statistic value for each map upstream and put it in the manifest
#' `threshold` column, which overrides the policy for that row. This is cleaner
#' than zeroing non-significant voxels in the volume and forcing a tiny
#' `threshold` to bypass the policy. See the example below.
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
#' # FDR (q < 0.05): precompute a per-map critical |t| from each map's p-values
#' # and supply it through the manifest `threshold` column.
#' bh_threshold <- function(pvals, df, q = 0.05) {
#'   pvals <- sort(pvals[is.finite(pvals)])
#'   m <- length(pvals)
#'   ok <- pvals <= (seq_len(m) / m) * q
#'   # No survivors: a finite threshold above any statistic, so the map renders
#'   # as an empty panel under render_montage_report(empty = "warning"). Avoid
#'   # Inf, which the finite-positive threshold check would reject.
#'   if (!any(ok)) return(.Machine$double.xmax)
#'   stats::qt(1 - pvals[max(which(ok))] / 2, df = df)  # critical |t|
#' }
#' manifest$threshold <- mapply(bh_threshold, map_pvalues, manifest$df, q = 0.05)
#' render_montage_report(manifest, "report.html", surfatlas = atlas)
#' }
#' @export
montage_policy <- function(p = 0.005,
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
#'
#' @return The manifest with `effective_threshold`, `effective_tail`,
#'   `effective_connectivity`, `effective_min_cluster_size`, and `cap_key`
#'   columns added.
#' @export
resolve_montage_policy <- function(manifest, policy = montage_policy(),
                                   empty = c("error", "warning")) {
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

  manifest$effective_threshold <- threshold
  manifest$effective_tail <- tail
  manifest$effective_connectivity <- connectivity
  manifest$effective_min_cluster_size <- as.integer(min_cluster_size)
  manifest$cap_key <- .policy_cap_key(manifest, policy$cap_within)
  attr(manifest, "montage_policy") <- policy
  manifest
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
