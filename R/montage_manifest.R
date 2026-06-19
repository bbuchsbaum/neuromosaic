# Render-manifest fields shared by the programmatic API and CLI.
.montage_manifest_required <- c("map_id", "label", "stat_kind", "signed")
.montage_manifest_stat_kinds <- c("t", "z", "beta", "cope")
.montage_manifest_tails <- c("two_sided", "positive", "negative")
.montage_manifest_connectivity <- c("26-connect", "18-connect", "6-connect")

#' Render Manifest Schema
#'
#' Returns the formal schema for multi-map montage render manifests. A render
#' manifest has one row per statistical map and is distinct from the
#' per-observation `nftab`/`--design` manifest used by the existing cluster
#' report path.
#'
#' @return A data frame with columns `field`, `required`, `type`, and `role`.
#' @export
montage_manifest_schema <- function() {
  data.frame(
    field = c(
      "map_id", "path", "recipe", "space", "template", "mask",
      "stat_kind", "df", "units", "signed",
      "p", "threshold", "tail", "connectivity", "min_cluster_size",
      "level", "label", "description", "n", "subjects"
    ),
    required = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      TRUE, FALSE, FALSE, TRUE,
      FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, FALSE, FALSE, FALSE
    ),
    type = c(
      "character", "character", "function/list", "character", "character",
      "character",
      "character", "numeric", "character", "logical",
      "numeric", "numeric", "character", "character", "integer",
      "character", "character", "character", "integer", "character/list"
    ),
    role = c(
      "stable join key, cache key, figure anchor, and table key",
      "path to a renderable map",
      "deferred map recipe when no path is available",
      "declared map space",
      "template/background identity",
      "optional analysis mask",
      "statistic family such as t, z, beta, or cope",
      "degrees of freedom for p-to-threshold conversion",
      "colorbar units",
      "whether the statistic has positive and negative semantics",
      "per-map p-value override",
      "per-map numeric threshold override",
      "cluster tail policy",
      "cluster connectivity policy",
      "minimum cluster size policy",
      "subject/group or other report level",
      "human-facing panel title",
      "markdown panel description",
      "effective sample size",
      "subject identifiers or a compact subject summary"
    ),
    stringsAsFactors = FALSE
  )
}

#' Validate a Montage Render Manifest
#'
#' Validates the storage-agnostic render manifest consumed by the montage report
#' engine. Structural checks always run. Overlay checks run when `load_maps` is
#' `TRUE`, when a `stat_map` list-column is present, or when `check_overlays` is
#' explicitly set to `TRUE`.
#'
#' @param manifest A data frame with one row per renderable statistical map.
#' @param background Optional background `NeuroVol`, `NeuroSpace`, or path used
#'   to enforce the grid-reconciliation invariant when overlay checks are run.
#' @param load_maps Logical; read map files from the `path` column and run
#'   non-empty overlay checks.
#' @param check_files Logical; require non-missing `path` values to exist.
#' @param check_overlays Logical; run map-level QC checks. Defaults to `TRUE`
#'   when `load_maps = TRUE` or a `stat_map` list-column is present.
#' @param default_p Default p-value used to derive thresholds when a row has no
#'   explicit `threshold` or `p`.
#' @param default_tail Default tail used when the manifest omits `tail`.
#' @param empty Action taken during overlay QC when a map has no suprathreshold
#'   voxels: `"error"` (default) aborts; `"warning"` warns and continues so a
#'   single empty contrast does not fail the whole manifest. Only relevant when
#'   overlay checks run (see `check_overlays`).
#'
#' @return The validated manifest as a data frame, with simple logical and
#'   numeric policy columns normalized where present.
#' @export
validate_manifest <- function(manifest,
                              background = NULL,
                              load_maps = FALSE,
                              check_files = TRUE,
                              check_overlays = load_maps ||
                                "stat_map" %in% names(manifest),
                              default_p = 0.005,
                              default_tail = c("two_sided", "positive",
                                               "negative"),
                              empty = c("error", "warning")) {
  default_tail <- match.arg(default_tail)
  empty <- match.arg(empty)
  if (!is.data.frame(manifest)) {
    stop("'manifest' must be a data frame.", call. = FALSE)
  }
  if (nrow(manifest) == 0L) {
    stop("'manifest' must contain at least one row.", call. = FALSE)
  }

  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  .validate_manifest_required_columns(manifest)
  manifest <- .normalize_manifest_policy_columns(manifest)
  .validate_manifest_identity(manifest)
  .validate_manifest_map_sources(manifest, check_files = check_files)
  .validate_manifest_semantics(manifest)
  .validate_manifest_policy(manifest)
  .validate_manifest_metadata(manifest)

  if (isTRUE(check_overlays)) {
    .validate_manifest_overlays(
      manifest = manifest,
      background = background,
      load_maps = load_maps,
      default_p = default_p,
      default_tail = default_tail,
      empty = empty
    )
  }

  manifest
}

.validate_manifest_required_columns <- function(manifest) {
  missing <- setdiff(.montage_manifest_required, names(manifest))
  if (length(missing) > 0L) {
    stop(
      "Render manifest is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.normalize_manifest_policy_columns <- function(manifest) {
  if ("stat_kind" %in% names(manifest)) {
    manifest$stat_kind <- tolower(trimws(as.character(manifest$stat_kind)))
  }
  if ("tail" %in% names(manifest)) {
    manifest$tail <- trimws(as.character(manifest$tail))
  }
  if ("connectivity" %in% names(manifest)) {
    manifest$connectivity <- trimws(as.character(manifest$connectivity))
  }
  manifest$signed <- .coerce_manifest_logical(manifest$signed, "signed")

  for (field in c("df", "p", "threshold", "min_cluster_size", "n")) {
    if (field %in% names(manifest)) {
      manifest[[field]] <- .coerce_manifest_numeric(manifest[[field]], field)
    }
  }

  manifest
}

.validate_manifest_identity <- function(manifest) {
  ids <- trimws(as.character(manifest$map_id))
  bad_ids <- .missing_character(ids)
  if (any(bad_ids)) {
    stop(
      "Render manifest column 'map_id' must be non-empty for every row.",
      call. = FALSE
    )
  }
  dup <- duplicated(ids)
  if (any(dup)) {
    stop(
      "Render manifest column 'map_id' must be unique; duplicate value(s): ",
      paste(unique(ids[dup]), collapse = ", "),
      call. = FALSE
    )
  }

  labels <- trimws(as.character(manifest$label))
  if (any(.missing_character(labels))) {
    stop(
      "Every render manifest row must have a non-empty 'label'.",
      call. = FALSE
    )
  }
}

.validate_manifest_map_sources <- function(manifest, check_files) {
  has_path <- if ("path" %in% names(manifest)) {
    !.missing_character(as.character(manifest$path))
  } else {
    rep(FALSE, nrow(manifest))
  }
  has_recipe <- if ("recipe" %in% names(manifest)) {
    !.missing_column_values(manifest$recipe)
  } else {
    rep(FALSE, nrow(manifest))
  }
  has_stat_map <- if ("stat_map" %in% names(manifest)) {
    !.missing_column_values(manifest$stat_map)
  } else {
    rep(FALSE, nrow(manifest))
  }

  if (any(!has_path & !has_recipe & !has_stat_map)) {
    rows <- which(!has_path & !has_recipe & !has_stat_map)
    stop(
      "Each render manifest row must define 'path' or 'recipe' ",
      "(or an in-memory 'stat_map' for validation). Missing row(s): ",
      paste(rows, collapse = ", "),
      call. = FALSE
    )
  }

  if (isTRUE(check_files) && any(has_path)) {
    missing_paths <- has_path & !file.exists(as.character(manifest$path))
    if (any(missing_paths)) {
      stop(
        "Render manifest path(s) do not exist for map_id: ",
        paste(manifest$map_id[missing_paths], collapse = ", "),
        call. = FALSE
      )
    }
  }
}

.validate_manifest_semantics <- function(manifest) {
  stat_kind <- tolower(trimws(as.character(manifest$stat_kind)))
  bad <- !stat_kind %in% .montage_manifest_stat_kinds
  if (any(bad)) {
    stop(
      "Unsupported 'stat_kind' for map_id: ",
      paste(manifest$map_id[bad], collapse = ", "),
      ". Supported values are: ",
      paste(.montage_manifest_stat_kinds, collapse = ", "),
      call. = FALSE
    )
  }

  has_threshold <- if ("threshold" %in% names(manifest)) {
    !.missing_numeric(manifest$threshold)
  } else {
    rep(FALSE, nrow(manifest))
  }
  needs_df <- stat_kind == "t" & !has_threshold
  has_df <- if ("df" %in% names(manifest)) {
    !.missing_numeric(manifest$df)
  } else {
    rep(FALSE, nrow(manifest))
  }
  if (any(needs_df & !has_df)) {
    stop(
      "Rows with stat_kind 't' require 'df' unless an explicit 'threshold' ",
      "is supplied. Missing df for map_id: ",
      paste(manifest$map_id[needs_df & !has_df], collapse = ", "),
      call. = FALSE
    )
  }
}

.validate_manifest_policy <- function(manifest) {
  if ("p" %in% names(manifest)) {
    bad <- !.missing_numeric(manifest$p) &
      (!is.finite(manifest$p) | manifest$p <= 0 | manifest$p >= 1)
    if (any(bad)) {
      stop(
        "Manifest column 'p' must be between 0 and 1 for map_id: ",
        paste(manifest$map_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }

  if ("threshold" %in% names(manifest)) {
    bad <- !.missing_numeric(manifest$threshold) &
      (!is.finite(manifest$threshold) | manifest$threshold <= 0)
    if (any(bad)) {
      stop(
        "Manifest column 'threshold' must be positive for map_id: ",
        paste(manifest$map_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }

  if ("tail" %in% names(manifest)) {
    tails <- trimws(as.character(manifest$tail))
    bad <- !.missing_character(tails) & !tails %in% .montage_manifest_tails
    if (any(bad)) {
      stop(
        "Manifest column 'tail' must be one of ",
        paste(.montage_manifest_tails, collapse = ", "),
        " for map_id: ",
        paste(manifest$map_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }

  if ("connectivity" %in% names(manifest)) {
    conn <- trimws(as.character(manifest$connectivity))
    bad <- !.missing_character(conn) &
      !conn %in% .montage_manifest_connectivity
    if (any(bad)) {
      stop(
        "Manifest column 'connectivity' must be one of ",
        paste(.montage_manifest_connectivity, collapse = ", "),
        " for map_id: ",
        paste(manifest$map_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }

  if ("min_cluster_size" %in% names(manifest)) {
    bad <- !.missing_numeric(manifest$min_cluster_size) &
      (!is.finite(manifest$min_cluster_size) |
        manifest$min_cluster_size < 1 |
        manifest$min_cluster_size != floor(manifest$min_cluster_size))
    if (any(bad)) {
      stop(
        "Manifest column 'min_cluster_size' must be a positive integer for ",
        "map_id: ",
        paste(manifest$map_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }
}

.validate_manifest_metadata <- function(manifest) {
  for (field in c("space", "template", "units", "level")) {
    if (field %in% names(manifest)) {
      values <- trimws(as.character(manifest[[field]]))
      bad <- !is.na(values) & !nzchar(values)
      if (any(bad)) {
        stop(
          "Manifest column '", field,
          "' must not contain blank strings for map_id: ",
          paste(manifest$map_id[bad], collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  if ("n" %in% names(manifest)) {
    bad <- !.missing_numeric(manifest$n) &
      (!is.finite(manifest$n) | manifest$n < 0 |
        manifest$n != floor(manifest$n))
    if (any(bad)) {
      stop(
        "Manifest column 'n' must be a non-negative integer for map_id: ",
        paste(manifest$map_id[bad], collapse = ", "),
        call. = FALSE
      )
    }
  }
}

.validate_manifest_overlays <- function(manifest,
                                        background,
                                        load_maps,
                                        default_p,
                                        default_tail,
                                        empty = "error") {
  background_space <- .montage_background_space(background)

  for (i in seq_len(nrow(manifest))) {
    stat_map <- .manifest_row_stat_map(manifest, i, load_maps = load_maps)
    if (!methods::is(stat_map, "NeuroVol")) {
      stop(
        "Overlay QC requires a NeuroVol for map_id '", manifest$map_id[[i]],
        "'. Supply a path with load_maps=TRUE or a 'stat_map' list-column.",
        call. = FALSE
      )
    }

    if (!is.null(background_space)) {
      stat_space <- neuroim2::space(stat_map)
      if (!.same_neuro_space(background_space, stat_space)) {
        stop(
          "Grid mismatch between background and stat map for map_id '",
          manifest$map_id[[i]], "'.",
          call. = FALSE
        )
      }
    }

    threshold <- .manifest_row_threshold(
      manifest = manifest,
      row = i,
      default_p = default_p,
      default_tail = default_tail
    )
    tail <- .manifest_row_tail(manifest, i, default_tail)
    values <- as.numeric(stat_map)
    supra <- .suprathreshold_mask(values, threshold = threshold, tail = tail)

    if (!any(supra, na.rm = TRUE)) {
      msg <- paste0(
        "No finite suprathreshold voxels for map_id '",
        manifest$map_id[[i]], "'."
      )
      if (identical(empty, "warning")) {
        warning(msg, call. = FALSE)
      } else {
        stop(msg, call. = FALSE)
      }
    }
  }

  TRUE
}

.manifest_row_stat_map <- function(manifest, row, load_maps) {
  if ("stat_map" %in% names(manifest) &&
      !.missing_column_values(manifest$stat_map)[[row]]) {
    col <- manifest$stat_map
    return(if (is.list(col)) col[[row]] else col[row])
  }

  if ("path" %in% names(manifest) &&
      !.missing_character(as.character(manifest$path))[[row]]) {
    if (!isTRUE(load_maps)) {
      stop(
        "Overlay QC for path-backed manifests requires load_maps=TRUE.",
        call. = FALSE
      )
    }
    return(neuroim2::read_vol(as.character(manifest$path[[row]])))
  }

  if ("recipe" %in% names(manifest) &&
      !.missing_column_values(manifest$recipe)[[row]]) {
    return(.montage_evaluate_recipe(
      manifest$recipe[[row]],
      manifest[row, , drop = FALSE]
    ))
  }

  NULL
}

.manifest_row_threshold <- function(manifest, row, default_p, default_tail) {
  if ("threshold" %in% names(manifest) &&
      !.missing_numeric(manifest$threshold)[[row]]) {
    return(abs(manifest$threshold[[row]]))
  }

  p_value <- default_p
  if ("p" %in% names(manifest) && !.missing_numeric(manifest$p)[[row]]) {
    p_value <- manifest$p[[row]]
  }
  if (!is.numeric(p_value) || length(p_value) != 1L ||
      !is.finite(p_value) || p_value <= 0 || p_value >= 1) {
    stop("'default_p' must be a single number between 0 and 1.", call. = FALSE)
  }

  tail <- .manifest_row_tail(manifest, row, default_tail)
  stat_kind <- tolower(as.character(manifest$stat_kind[[row]]))
  upper_prob <- if (identical(tail, "two_sided")) 1 - p_value / 2 else 1 - p_value

  if (identical(stat_kind, "z")) {
    return(stats::qnorm(upper_prob))
  }

  if (identical(stat_kind, "t")) {
    df <- manifest$df[[row]]
    return(stats::qt(upper_prob, df = df))
  }

  stop(
    "Overlay QC cannot derive a threshold for stat_kind '", stat_kind,
    "' without an explicit 'threshold' for map_id '", manifest$map_id[[row]],
    "'.",
    call. = FALSE
  )
}

.manifest_row_tail <- function(manifest, row, default_tail) {
  if ("tail" %in% names(manifest) &&
      !.missing_character(as.character(manifest$tail))[[row]]) {
    return(as.character(manifest$tail[[row]]))
  }
  default_tail
}

.suprathreshold_mask <- function(values, threshold, tail) {
  finite <- is.finite(values)
  if (identical(tail, "positive")) {
    return(finite & values >= threshold)
  }
  if (identical(tail, "negative")) {
    return(finite & values <= -threshold)
  }
  finite & abs(values) >= threshold
}

.montage_background_space <- function(background) {
  if (is.null(background)) {
    return(NULL)
  }
  if (is.character(background) && length(background) == 1L) {
    background <- neuroim2::read_vol(background)
  }
  if (methods::is(background, "NeuroVol")) {
    return(neuroim2::space(background))
  }
  if (methods::is(background, "NeuroSpace")) {
    return(background)
  }
  stop(
    "'background' must be NULL, a path, a NeuroVol, or a NeuroSpace.",
    call. = FALSE
  )
}

.same_neuro_space <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
  x_sig <- .neuro_space_signature(x)
  y_sig <- .neuro_space_signature(y)

  identical(x_sig$dim, y_sig$dim) &&
    isTRUE(all.equal(x_sig$spacing, y_sig$spacing, tolerance = tolerance)) &&
    isTRUE(all.equal(x_sig$origin, y_sig$origin, tolerance = tolerance)) &&
    isTRUE(all.equal(x_sig$trans, y_sig$trans, tolerance = tolerance))
}

.neuro_space_signature <- function(space) {
  list(
    dim = dim(space),
    spacing = neuroim2::spacing(space),
    origin = neuroim2::origin(space),
    trans = if ("trans" %in% methods::slotNames(space)) {
      as.numeric(methods::slot(space, "trans"))
    } else {
      numeric(0)
    }
  )
}

.coerce_manifest_logical <- function(x, field) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    bad <- !is.na(x) & !x %in% c(0, 1)
    if (!any(bad)) {
      return(as.logical(x))
    }
  }
  values <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(values))
  out[values %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[values %in% c("false", "f", "0", "no", "n")] <- FALSE
  bad <- is.na(out) & !is.na(values)
  if (any(bad)) {
    stop(
      "Manifest column '", field,
      "' must be logical for row(s): ",
      paste(which(bad), collapse = ", "),
      call. = FALSE
    )
  }
  out
}

.coerce_manifest_numeric <- function(x, field) {
  if (is.list(x) && !is.data.frame(x)) {
    stop(
      "Manifest column '", field, "' must be numeric, not a list.",
      call. = FALSE
    )
  }
  out <- suppressWarnings(as.numeric(x))
  bad <- is.na(out) & !.missing_column_values(x)
  if (any(bad)) {
    stop(
      "Manifest column '", field,
      "' must be numeric for row(s): ",
      paste(which(bad), collapse = ", "),
      call. = FALSE
    )
  }
  out
}

.missing_column_values <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    return(vapply(x, function(value) {
      is.null(value) ||
        (length(value) == 1L && is.atomic(value) && is.na(value)) ||
        (is.character(value) && length(value) == 1L && !nzchar(trimws(value)))
    }, logical(1)))
  }
  if (is.character(x) || is.factor(x)) {
    return(.missing_character(as.character(x)))
  }
  is.na(x)
}

.missing_character <- function(x) {
  is.na(x) | !nzchar(trimws(x))
}

.missing_numeric <- function(x) {
  is.na(x)
}
