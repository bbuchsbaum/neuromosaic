# Render-manifest fields shared by the programmatic API and CLI.
.montage_manifest_required <- c("map_id", "label")
.montage_manifest_stat_kinds <- c("t", "z", "beta", "cope")
.montage_manifest_roles <- c("primary", "auxiliary")
.montage_manifest_distributions <- c("z", "t")
.montage_manifest_builtin_quantities <- c(
  "test_statistic", "estimate", "standard_error", "variance",
  "probability", "proportion", "r_squared", "diagnostic"
)
.montage_manifest_tails <- c("two_sided", "positive", "negative")
.montage_manifest_connectivity <- c("26-connect", "18-connect", "6-connect")

#' Render Manifest Schema
#'
#' Returns the formal schema for multi-map montage render manifests. A render
#' manifest has one row per map variant and is distinct from the
#' per-observation `nftab`/`--design` manifest used by the existing cluster
#' report path.
#'
#' @return A data frame with columns `field`, `required`, `type`, and `role`.
#' @export
montage_manifest_schema <- function() {
  data.frame(
    field = c(
      "map_id", "path", "recipe", "space", "template", "mask",
      "parcel_values", "analysis_id", "analysis_label", "role",
      "quantity", "distribution", "stat_kind", "df", "units", "signed",
      "selector_label", "display_order", "profile",
      "display_mode", "scale", "center", "lower", "upper",
      "palette_family", "alpha_mode", "support",
      "p", "q", "threshold", "tail", "connectivity", "min_cluster_size",
      "level", "label", "description", "n", "subjects"
    ),
    required = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE,
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, FALSE, FALSE, FALSE
    ),
    type = c(
      "character", "character", "function/list", "character", "character",
      "character",
      "numeric/list", "character", "character", "character",
      "character", "character", "character", "numeric", "character", "logical",
      "character", "numeric", "character",
      "character", "character", "numeric", "numeric", "numeric",
      "character", "character", "character",
      "numeric", "numeric", "numeric", "character", "character", "integer",
      "character", "character", "character", "integer", "character/list"
    ),
    role = c(
      "stable join key, cache key, figure anchor, and table key",
      "path to a renderable map",
      "deferred map recipe when no path is available",
      "declared map space",
      "template/background identity",
      "optional analysis mask",
      "per-parcel statistic vector for direct surface rendering",
      "stable identifier shared by associated map variants",
      "human-facing title for an analysis group",
      "primary or auxiliary role within an analysis group",
      "generic scientific quantity or a caller-defined namespaced value",
      "sampling distribution for a test statistic, currently z or t",
      "legacy statistic family; accepted for compatibility",
      "degrees of freedom for p-to-threshold conversion",
      "colorbar units",
      "whether the statistic has positive and negative semantics",
      "short label used by the map selector",
      "ordering of variants within an analysis group",
      "optional profile id used to resolve display semantics",
      "explicit thresholded or continuous display-mode override",
      "explicit diverging or sequential color-scale override",
      "center of an explicit diverging color scale",
      "explicit lower display limit",
      "explicit upper display limit",
      "explicit renderer palette family or palette name",
      "explicit overlay opacity mode",
      "analysis-wide or primary-map display support",
      "per-map p-value override",
      "per-map Benjamini-Hochberg FDR q-value override",
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
#' `TRUE`, when a non-missing `stat_map` or `parcel_values` list-column is
#' present, or when `check_overlays` is explicitly set to `TRUE`.
#'
#' @param manifest A data frame with one row per renderable statistical map.
#' @param background Optional background `NeuroVol`, `NeuroSpace`, or path used
#'   to enforce the grid-reconciliation invariant when overlay checks are run.
#' @param load_maps Logical; read map files from the `path` column and run
#'   non-empty overlay checks.
#' @param check_files Logical; require non-missing `path` values to exist.
#' @param check_overlays Logical; run map-level QC checks. Defaults to `TRUE`
#'   when `load_maps = TRUE` or a non-missing `stat_map`/`parcel_values`
#'   list-column is present.
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
                                .manifest_has_nonmissing_column(manifest, "stat_map") ||
                                .manifest_has_nonmissing_column(manifest, "parcel_values"),
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
  manifest <- .normalize_montage_semantics(manifest)
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

.manifest_has_nonmissing_column <- function(manifest, field) {
  if (is.null(names(manifest)) || !field %in% names(manifest)) {
    return(FALSE)
  }
  any(!.missing_column_values(manifest[[field]]))
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
  if (!"quantity" %in% names(manifest) &&
      !"stat_kind" %in% names(manifest)) {
    stop(
      "Render manifest must define 'quantity' (preferred) or legacy ",
      "'stat_kind'.",
      call. = FALSE
    )
  }
}

.normalize_montage_semantics <- function(manifest) {
  n <- nrow(manifest)
  legacy_kind <- if ("stat_kind" %in% names(manifest)) {
    tolower(trimws(as.character(manifest$stat_kind)))
  } else {
    rep(NA_character_, n)
  }
  legacy_kind[.missing_character(legacy_kind)] <- NA_character_
  bad_legacy <- !is.na(legacy_kind) &
    !legacy_kind %in% .montage_manifest_stat_kinds
  if (any(bad_legacy)) {
    stop(
      "Unsupported 'stat_kind' for map_id: ",
      paste(manifest$map_id[bad_legacy], collapse = ", "),
      ". Supported legacy values are: ",
      paste(.montage_manifest_stat_kinds, collapse = ", "),
      call. = FALSE
    )
  }

  legacy_quantity <- rep(NA_character_, n)
  legacy_quantity[legacy_kind %in% c("z", "t")] <- "test_statistic"
  legacy_quantity[legacy_kind %in% c("beta", "cope")] <- "estimate"
  quantity <- if ("quantity" %in% names(manifest)) {
    tolower(trimws(as.character(manifest$quantity)))
  } else {
    legacy_quantity
  }
  quantity <- .normalize_montage_quantity(quantity)
  quantity[.missing_character(quantity)] <- legacy_quantity[.missing_character(quantity)]
  if (any(.missing_character(quantity))) {
    stop(
      "Every render manifest row must define a non-empty 'quantity' or a ",
      "recognized legacy 'stat_kind'. Missing for map_id: ",
      paste(manifest$map_id[.missing_character(quantity)], collapse = ", "),
      call. = FALSE
    )
  }
  conflict <- !is.na(legacy_quantity) & quantity != legacy_quantity
  if (any(conflict)) {
    stop(
      "Manifest 'quantity' conflicts with legacy 'stat_kind' for map_id: ",
      paste(manifest$map_id[conflict], collapse = ", "),
      call. = FALSE
    )
  }

  distribution <- if ("distribution" %in% names(manifest)) {
    tolower(trimws(as.character(manifest$distribution)))
  } else {
    rep(NA_character_, n)
  }
  distribution[.missing_character(distribution)] <- NA_character_
  legacy_distribution <- legacy_kind
  legacy_distribution[!legacy_distribution %in% c("z", "t")] <- NA_character_
  fill_distribution <- is.na(distribution) & !is.na(legacy_distribution)
  distribution[fill_distribution] <- legacy_distribution[fill_distribution]
  distribution_conflict <- !is.na(legacy_distribution) &
    !is.na(distribution) & distribution != legacy_distribution
  if (any(distribution_conflict)) {
    stop(
      "Manifest 'distribution' conflicts with legacy 'stat_kind' for map_id: ",
      paste(manifest$map_id[distribution_conflict], collapse = ", "),
      call. = FALSE
    )
  }

  analysis_id <- if ("analysis_id" %in% names(manifest)) {
    trimws(as.character(manifest$analysis_id))
  } else {
    as.character(manifest$map_id)
  }
  if (any(.missing_character(analysis_id))) {
    stop("Manifest column 'analysis_id' must be non-empty.", call. = FALSE)
  }

  role <- if ("role" %in% names(manifest)) {
    tolower(trimws(as.character(manifest$role)))
  } else {
    rep(NA_character_, n)
  }
  role[.missing_character(role)] <- NA_character_
  for (id in unique(analysis_id)) {
    rows <- which(analysis_id == id)
    present <- role[rows][!is.na(role[rows])]
    if (length(present) == 0L) {
      candidates <- rows[quantity[rows] == "test_statistic"]
      if (length(rows) == 1L) {
        role[rows] <- "primary"
      } else if (length(candidates) == 1L) {
        role[rows] <- "auxiliary"
        role[candidates] <- "primary"
      }
    } else if (sum(present == "primary") == 1L) {
      missing_rows <- rows[is.na(role[rows])]
      role[missing_rows] <- "auxiliary"
    }
  }

  signed <- if ("signed" %in% names(manifest)) {
    .coerce_manifest_logical(manifest$signed, "signed")
  } else {
    rep(NA, n)
  }
  signed_default <- rep(NA, n)
  signed_default[quantity %in% c("test_statistic", "estimate")] <- TRUE
  signed_default[quantity %in% c(
    "standard_error", "variance", "probability", "proportion", "r_squared"
  )] <- FALSE
  signed[is.na(signed)] <- signed_default[is.na(signed)]

  manifest$analysis_id <- analysis_id
  manifest$role <- role
  manifest$quantity <- quantity
  manifest$distribution <- distribution
  manifest$stat_kind <- legacy_kind
  test_rows <- quantity == "test_statistic" & is.na(manifest$stat_kind)
  manifest$stat_kind[test_rows] <- distribution[test_rows]
  manifest$signed <- signed
  if (!"selector_label" %in% names(manifest)) {
    manifest$selector_label <- as.character(manifest$label)
  } else {
    selector_label <- trimws(as.character(manifest$selector_label))
    missing_selector <- .missing_character(selector_label)
    selector_label[missing_selector] <- as.character(
      manifest$label[missing_selector]
    )
    manifest$selector_label <- selector_label
  }
  if (!"display_order" %in% names(manifest)) {
    manifest$display_order <- stats::ave(
      seq_len(n), analysis_id, FUN = seq_along
    )
  } else {
    manifest$display_order <- suppressWarnings(as.numeric(manifest$display_order))
  }
  manifest
}

.normalize_montage_quantity <- function(quantity) {
  aliases <- c(
    se = "standard_error",
    stderr = "standard_error",
    `standard-error` = "standard_error",
    var = "variance",
    probability_map = "probability",
    prop = "proportion",
    r2 = "r_squared",
    `r-squared` = "r_squared",
    statistic = "test_statistic",
    teststat = "test_statistic",
    `test-statistic` = "test_statistic"
  )
  hit <- match(quantity, names(aliases))
  quantity[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  quantity
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

  for (field in c(
    "df", "p", "q", "threshold", "min_cluster_size", "n",
    "center", "lower", "upper"
  )) {
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
  has_parcel_values <- if ("parcel_values" %in% names(manifest)) {
    !.missing_column_values(manifest$parcel_values)
  } else {
    rep(FALSE, nrow(manifest))
  }

  if (any(!has_path & !has_recipe & !has_stat_map & !has_parcel_values)) {
    rows <- which(!has_path & !has_recipe & !has_stat_map & !has_parcel_values)
    stop(
      "Each render manifest row must define 'path' or 'recipe' ",
      "(or an in-memory 'stat_map'/'parcel_values' source). Missing row(s): ",
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
  quantity <- trimws(as.character(manifest$quantity))
  if (any(.missing_character(quantity))) {
    stop(
      "Manifest column 'quantity' must be non-empty for every map.",
      call. = FALSE
    )
  }

  distribution <- trimws(as.character(manifest$distribution))
  distribution[.missing_character(distribution)] <- NA_character_
  bad_distribution <- !is.na(distribution) &
    !distribution %in% .montage_manifest_distributions
  if (any(bad_distribution)) {
    stop(
      "Manifest column 'distribution' must be one of ",
      paste(.montage_manifest_distributions, collapse = ", "),
      " for map_id: ",
      paste(manifest$map_id[bad_distribution], collapse = ", "),
      call. = FALSE
    )
  }
  missing_distribution <- quantity == "test_statistic" & is.na(distribution)
  if (any(missing_distribution)) {
    stop(
      "Rows with quantity 'test_statistic' require 'distribution' for map_id: ",
      paste(manifest$map_id[missing_distribution], collapse = ", "),
      call. = FALSE
    )
  }

  role <- trimws(as.character(manifest$role))
  bad_role <- is.na(role) | !role %in% .montage_manifest_roles
  if (any(bad_role)) {
    stop(
      "Each analysis group must have exactly one primary map; role is ",
      "missing or invalid for map_id: ",
      paste(manifest$map_id[bad_role], collapse = ", "),
      call. = FALSE
    )
  }
  primary_counts <- vapply(
    split(role, manifest$analysis_id),
    function(x) sum(x == "primary"),
    integer(1)
  )
  if (any(primary_counts != 1L)) {
    stop(
      "Each analysis group must have exactly one primary map. Invalid ",
      "analysis_id: ",
      paste(names(primary_counts)[primary_counts != 1L], collapse = ", "),
      call. = FALSE
    )
  }
  if ("analysis_label" %in% names(manifest)) {
    labels_by_group <- split(
      trimws(as.character(manifest$analysis_label)), manifest$analysis_id
    )
    inconsistent <- vapply(labels_by_group, function(x) {
      length(unique(x[!.missing_character(x)])) > 1L
    }, logical(1))
    if (any(inconsistent)) {
      stop(
        "Manifest 'analysis_label' must be consistent within analysis_id: ",
        paste(names(inconsistent)[inconsistent], collapse = ", "),
        call. = FALSE
      )
    }
  }

  has_threshold <- if ("threshold" %in% names(manifest)) {
    !.missing_numeric(manifest$threshold)
  } else {
    rep(FALSE, nrow(manifest))
  }
  needs_df <- !is.na(distribution) & distribution == "t" & !has_threshold
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

  if ("display_order" %in% names(manifest)) {
    display_order <- suppressWarnings(as.numeric(manifest$display_order))
    bad <- !is.finite(display_order)
    if (any(bad)) {
      stop("Manifest column 'display_order' must be finite.", call. = FALSE)
    }
    manifest$display_order <- display_order
  }

  profile_choices <- list(
    display_mode = c("thresholded", "continuous"),
    scale = c("diverging", "sequential"),
    support = c("analysis", "primary")
  )
  for (field in names(profile_choices)) {
    if (!field %in% names(manifest)) next
    values <- trimws(as.character(manifest[[field]]))
    present <- !.missing_character(values)
    bad <- present & !values %in% profile_choices[[field]]
    if (any(bad)) {
      stop(
        "Manifest column '", field, "' must be one of ",
        paste(profile_choices[[field]], collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  for (field in intersect(c("center", "lower", "upper"), names(manifest))) {
    present <- !.missing_numeric(manifest[[field]])
    if (any(present & !is.finite(manifest[[field]]))) {
      stop("Manifest column '", field, "' must be finite.", call. = FALSE)
    }
  }
  if (all(c("lower", "upper") %in% names(manifest))) {
    complete <- !.missing_numeric(manifest$lower) &
      !.missing_numeric(manifest$upper)
    if (any(complete & manifest$lower >= manifest$upper)) {
      stop("Manifest display limits must be increasing.", call. = FALSE)
    }
  }
}

.montage_analysis_groups <- function(manifest) {
  ids <- unique(as.character(manifest$analysis_id))
  stats::setNames(lapply(ids, function(id) {
    rows <- which(manifest$analysis_id == id)
    order_key <- if ("display_order" %in% names(manifest)) {
      as.numeric(manifest$display_order[rows])
    } else {
      seq_along(rows)
    }
    primary <- manifest$role[rows] == "primary"
    rows <- rows[order(!primary, order_key, rows)]
    primary_row <- rows[manifest$role[rows] == "primary"][[1L]]
    group_label <- if ("analysis_label" %in% names(manifest) &&
                       !.missing_character(as.character(
                         manifest$analysis_label[[primary_row]]
                       ))) {
      as.character(manifest$analysis_label[[primary_row]])
    } else {
      as.character(manifest$label[[primary_row]])
    }
    list(
      analysis_id = id,
      analysis_label = group_label,
      primary_map_id = as.character(manifest$map_id[[primary_row]]),
      map_ids = as.character(manifest$map_id[rows]),
      rows = rows
    )
  }), ids)
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

  if ("q" %in% names(manifest)) {
    bad <- !.missing_numeric(manifest$q) &
      (!is.finite(manifest$q) | manifest$q <= 0 | manifest$q >= 1)
    if (any(bad)) {
      stop(
        "Manifest column 'q' must be between 0 and 1 for map_id: ",
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
  sources <- vector("list", nrow(manifest))

  for (i in seq_len(nrow(manifest))) {
    parcel_values <- .manifest_row_parcel_values(manifest, i, required = FALSE)
    if (!is.null(parcel_values)) {
      values <- parcel_values
      sources[[i]] <- parcel_values
    } else {
      stat_map <- .manifest_row_stat_map(manifest, i, load_maps = load_maps)
      if (!methods::is(stat_map, "NeuroVol")) {
        stop(
          "Overlay QC requires a NeuroVol or parcel values for map_id '",
          manifest$map_id[[i]], "'. Supply a path with load_maps=TRUE, ",
          "a 'stat_map' list-column, or a 'parcel_values' list-column.",
          call. = FALSE
        )
      }
      values <- as.numeric(stat_map)
      sources[[i]] <- stat_map
    }

    if (is.null(parcel_values) && !is.null(background_space)) {
      stat_space <- neuroim2::space(stat_map)
      if (!.same_neuro_space(background_space, stat_space)) {
        stop(
          "Grid mismatch between background and stat map for map_id '",
          manifest$map_id[[i]], "'.",
          call. = FALSE
        )
      }
    }

    display_mode <- .manifest_row_display_mode(manifest, i)
    if (identical(display_mode, "thresholded")) {
      threshold <- .manifest_row_threshold(
        manifest = manifest,
        row = i,
        default_p = default_p,
        default_tail = default_tail
      )
      tail <- .manifest_row_tail(manifest, i, default_tail)
      supra <- .suprathreshold_mask(values, threshold = threshold, tail = tail)
    } else {
      supra <- is.finite(values)
    }

    if (!any(supra, na.rm = TRUE)) {
      msg <- paste0(
        "No finite ",
        if (identical(display_mode, "thresholded")) "suprathreshold " else "display ",
        if (is.null(parcel_values)) "voxels" else "parcels",
        " for map_id '",
        manifest$map_id[[i]], "'."
      )
      if (identical(empty, "warning")) {
        warning(msg, call. = FALSE)
      } else {
        stop(msg, call. = FALSE)
      }
    }
  }

  .validate_montage_group_sources(manifest, sources)

  TRUE
}

.validate_montage_group_sources <- function(manifest, sources) {
  groups <- .montage_analysis_groups(manifest)
  for (group in groups) {
    if (length(group$rows) < 2L) next

    primary_row <- match(group$primary_map_id, manifest$map_id)
    primary <- sources[[primary_row]]
    primary_is_volume <- methods::is(primary, "NeuroVol")

    for (row in setdiff(group$rows, primary_row)) {
      candidate <- sources[[row]]
      candidate_is_volume <- methods::is(candidate, "NeuroVol")
      incompatible <- primary_is_volume != candidate_is_volume
      if (!incompatible && primary_is_volume) {
        incompatible <- !.same_neuro_space(
          neuroim2::space(primary), neuroim2::space(candidate)
        )
      } else if (!incompatible) {
        incompatible <- length(primary) != length(candidate)
      }

      if (incompatible) {
        stop(
          "Map variants in analysis_id '", group$analysis_id,
          "' must use the same spatial representation and geometry. ",
          "Map '", manifest$map_id[[row]], "' is incompatible with primary ",
          "map '", group$primary_map_id, "'.",
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

.manifest_row_display_mode <- function(manifest, row) {
  if ("effective_display_mode" %in% names(manifest) &&
      !.missing_character(as.character(manifest$effective_display_mode))[[row]]) {
    return(as.character(manifest$effective_display_mode[[row]]))
  }
  if ("display_mode" %in% names(manifest) &&
      !.missing_character(as.character(manifest$display_mode))[[row]]) {
    return(as.character(manifest$display_mode[[row]]))
  }
  if (identical(as.character(manifest$quantity[[row]]), "test_statistic")) {
    "thresholded"
  } else {
    "continuous"
  }
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

.manifest_row_parcel_values <- function(manifest, row, required = TRUE) {
  if (!"parcel_values" %in% names(manifest) ||
      .missing_column_values(manifest$parcel_values)[[row]]) {
    if (isTRUE(required)) {
      stop(
        "Missing 'parcel_values' for map_id '", manifest$map_id[[row]], "'.",
        call. = FALSE
      )
    }
    return(NULL)
  }
  col <- manifest$parcel_values
  values <- if (is.list(col)) col[[row]] else col[row]
  .coerce_parcel_value_vector(
    values,
    "parcel_values",
    map_id = manifest$map_id[[row]]
  )
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

.same_neuro_space <- function(x, y,
                              tolerance = .montage_geometry_tolerance) {
  x_sig <- .neuro_space_signature(x)
  y_sig <- .neuro_space_signature(y)

  identical(x_sig$dim, y_sig$dim) &&
    identical(x_sig$orientation, y_sig$orientation) &&
    .montage_geometry_numeric_equal(x_sig$spacing, y_sig$spacing, tolerance) &&
    .montage_geometry_numeric_equal(x_sig$origin, y_sig$origin, tolerance) &&
    .montage_geometry_numeric_equal(x_sig$trans, y_sig$trans, tolerance)
}

.neuro_space_signature <- function(space) {
  list(
    dim = dim(space),
    spacing = neuroim2::spacing(space),
    origin = neuroim2::origin(space),
    orientation = .montage_space_orientation(space),
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
