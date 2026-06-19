#' Build a Montage Render Manifest
#'
#' Builds the storage-agnostic render manifest consumed by montage reports.
#' Sources may be CSV/TSV tables, data frames, explicit path vectors, or a glob
#' pattern. Path-based sources parse BIDS-style filename entities such as
#' `contrast-faces_model-m1_stat-z.nii.gz`.
#'
#' @details
#' For an explicit `source` data frame (or CSV/TSV) you control every column,
#' including the required `map_id`, `stat_kind`, `signed`, and `label`; this is
#' the recommended route for non-BIDS filenames where token parsing is
#' unreliable. See [montage_manifest_schema()] for the full column contract.
#'
#' For path/`pattern` discovery, `map_id` is derived from the full filename
#' stem so maps differing only by a *bare* (non `key-value`) token stay
#' distinct; a warning is emitted if any derived `map_id` still collides.
#' Key-value BIDS entities become columns, and a `stat-` entity (`t`, `z`,
#' `beta`, or `cope`)
#' (or a bare token naming the statistic) populates `stat_kind` and a default
#' `signed = TRUE`. Other required fields, notably `label`, still come from a
#' `labeller`, `overrides`, or an explicit `source`.
#'
#' @param source A data frame, CSV/TSV path, path vector, or `NULL` when using
#'   `pattern`.
#' @param pattern Optional glob pattern resolved under `root`.
#' @param root Root directory for `pattern` and relative path outputs.
#' @param overrides Optional named list/vector or data frame. Named lists apply
#'   columns to every row. Data frames override by `map_id` when present, else
#'   by `path`.
#' @param labeller Optional labeller passed to [apply_montage_labeller()].
#' @param validate Logical; run [validate_manifest()] after building?
#' @param check_files Logical; passed to [validate_manifest()] and
#'   [apply_montage_labeller()].
#'
#' @return A render manifest data frame.
#' @examples
#' # Minimal explicit manifest. Required columns: map_id, stat_kind, signed
#' # (a logical), and label; df is also needed for t-stat p->threshold.
#' manifest <- build_manifest(
#'   source = data.frame(
#'     map_id = c("faces_gt_houses", "houses_gt_faces"),
#'     path = c("faces.nii.gz", "houses.nii.gz"),
#'     stat_kind = "t",
#'     df = 24,
#'     signed = TRUE,
#'     label = c("Faces > Houses", "Houses > Faces"),
#'     stringsAsFactors = FALSE
#'   ),
#'   check_files = FALSE
#' )
#' montage_manifest_schema() # the full column contract
#' @export
build_manifest <- function(source = NULL,
                           pattern = NULL,
                           root = ".",
                           overrides = NULL,
                           labeller = NULL,
                           validate = TRUE,
                           check_files = TRUE) {
  manifest <- .build_manifest_source(source, pattern = pattern, root = root)
  manifest <- .apply_manifest_overrides(manifest, overrides)

  if (!is.null(labeller)) {
    manifest <- apply_montage_labeller(
      manifest,
      labeller = labeller,
      check_files = check_files
    )
  } else if (isTRUE(validate)) {
    manifest <- validate_manifest(manifest, check_files = check_files)
  }

  manifest
}

.build_manifest_source <- function(source, pattern, root) {
  if (is.data.frame(source)) {
    return(as.data.frame(source, stringsAsFactors = FALSE))
  }

  if (!is.null(pattern)) {
    root <- normalizePath(root, mustWork = TRUE)
    hits <- Sys.glob(file.path(root, pattern))
    if (length(hits) == 0L) {
      stop("No files matched manifest pattern: ", pattern, call. = FALSE)
    }
    return(.manifest_from_paths(hits, root = root))
  }

  if (is.null(source)) {
    stop("Provide 'source' or 'pattern'.", call. = FALSE)
  }

  if (is.character(source) && length(source) == 1L &&
      file.exists(source) &&
      tolower(tools::file_ext(source)) %in% c("csv", "tsv", "txt")) {
    return(.read_manifest_table(source))
  }

  if (is.character(source)) {
    return(.manifest_from_paths(source, root = root))
  }

  stop(
    "'source' must be a data frame, CSV/TSV path, path vector, or NULL with pattern.",
    call. = FALSE
  )
}

.read_manifest_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("tsv", "txt")) {
    return(utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  stop("Manifest tables must be CSV or TSV.", call. = FALSE)
}

.montage_stat_entities <- c("t", "z", "beta", "cope")

.manifest_from_paths <- function(paths, root = ".") {
  paths <- normalizePath(paths, mustWork = TRUE)
  parsed <- lapply(paths, .parse_bids_filename)
  entities <- lapply(parsed, `[[`, "entities")
  fields <- unique(unlist(lapply(entities, names), use.names = FALSE))

  out <- data.frame(
    path = paths,
    stringsAsFactors = FALSE
  )
  for (field in fields) {
    out[[field]] <- vapply(entities, function(x) x[[field]] %||% NA_character_,
                           character(1))
  }
  if (!"map_id" %in% names(out)) {
    # Derive map_id from the full filename stem (not just the key-value
    # entities) so maps that differ only by a bare token keep distinct keys.
    out$map_id <- vapply(parsed, `[[`, character(1), "stem")
    .warn_duplicate_map_ids(out$map_id)
  }

  out <- .derive_manifest_stat_fields(out, parsed)
  out
}

# Warn (don't silently collide) when filename parsing yields duplicate map_ids;
# map_id is the schema's stable join/cache/figure/table key.
.warn_duplicate_map_ids <- function(map_id) {
  dups <- unique(map_id[duplicated(map_id)])
  if (length(dups) > 0L) {
    warning(
      "Duplicate map_id derived from filenames: ",
      paste(dups, collapse = ", "),
      ". map_id is the stable join/cache/figure key; distinct maps must not ",
      "share one. Supply an explicit `source`/`overrides` table with unique ",
      "map_id values, or rename the files.",
      call. = FALSE
    )
  }
}

# A `stat-{t,z,beta,cope}` entity (or a bare token naming the statistic, e.g.
# `..._z_g.nii.gz`) already encodes the required `stat_kind`/`signed` fields.
# Derive them when absent so pattern-only discovery can satisfy validate = TRUE.
.derive_manifest_stat_fields <- function(out, parsed) {
  if ("stat_kind" %in% names(out)) {
    return(out)
  }
  kinds <- vapply(seq_along(parsed), function(i) {
    stat_entity <- if ("stat" %in% names(out)) {
      tolower(trimws(as.character(out$stat[[i]])))
    } else {
      NA_character_
    }
    if (!is.na(stat_entity) && stat_entity %in% .montage_stat_entities) {
      return(stat_entity)
    }
    tokens <- tolower(parsed[[i]]$tokens)
    hit <- tokens[tokens %in% .montage_stat_entities]
    if (length(hit) > 0L) hit[[1]] else NA_character_
  }, character(1))

  if (all(is.na(kinds))) {
    return(out)
  }
  out$stat_kind <- kinds
  # All four statistic families carry a sign; default signed = TRUE where the
  # kind was derived. Never clobber an explicit `signed` value that filename
  # parsing already captured (e.g. a `signed-false` entity): only fill rows
  # that are still missing one.
  if ("signed" %in% names(out)) {
    fill <- is.na(out$signed) & !is.na(kinds)
    out$signed[fill] <- TRUE
  } else {
    out$signed <- ifelse(is.na(kinds), NA, TRUE)
  }
  out
}

.parse_bids_filename <- function(path) {
  stem <- basename(path)
  stem <- sub("\\.nii\\.gz$", "", stem, ignore.case = TRUE)
  stem <- sub("\\.[^.]+$", "", stem)
  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]

  entities <- list()
  tokens <- character(0)
  for (part in parts) {
    if (!nzchar(part)) {
      next
    }
    if (grepl("-", part, fixed = TRUE)) {
      split <- strsplit(part, "-", fixed = TRUE)[[1]]
      key <- make.names(split[[1]], unique = FALSE)
      value <- paste(split[-1], collapse = "-")
      if (nzchar(key) && nzchar(value)) {
        entities[[key]] <- value
        next
      }
    }
    tokens <- c(tokens, part)
  }
  list(stem = stem, entities = entities, tokens = tokens)
}

.apply_manifest_overrides <- function(manifest, overrides) {
  if (is.null(overrides)) {
    return(manifest)
  }
  if (is.data.frame(overrides)) {
    return(.apply_manifest_override_table(manifest, overrides))
  }
  if (is.list(overrides) || is.atomic(overrides)) {
    return(.apply_manifest_override_list(manifest, overrides))
  }
  stop("'overrides' must be NULL, a named list/vector, or a data frame.",
       call. = FALSE)
}

.apply_manifest_override_list <- function(manifest, overrides) {
  if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
    stop("Named overrides are required.", call. = FALSE)
  }
  n <- nrow(manifest)
  for (field in names(overrides)) {
    value <- overrides[[field]]
    if (length(value) == 1L) {
      manifest[[field]] <- rep(value, n)
    } else if (length(value) == n) {
      manifest[[field]] <- value
    } else {
      stop(
        "Override '", field, "' must have length 1 or nrow(manifest).",
        call. = FALSE
      )
    }
  }
  manifest
}

.apply_manifest_override_table <- function(manifest, overrides) {
  overrides <- as.data.frame(overrides, stringsAsFactors = FALSE)
  key <- if ("map_id" %in% names(overrides) && "map_id" %in% names(manifest)) {
    "map_id"
  } else if ("path" %in% names(overrides) && "path" %in% names(manifest)) {
    "path"
  } else {
    stop("Override tables must share a 'map_id' or 'path' column with the manifest.",
         call. = FALSE)
  }
  if (anyDuplicated(overrides[[key]])) {
    stop("Override table key values must be unique.", call. = FALSE)
  }

  idx <- match(manifest[[key]], overrides[[key]])
  if (identical(key, "map_id") && nrow(overrides) > 0L && all(is.na(idx))) {
    warning(
      "Override table keyed by 'map_id' matched no manifest rows; the keys ",
      "may be stale (path discovery derives map_id from the full filename ",
      "stem). No overrides were applied.",
      call. = FALSE
    )
  }
  fields <- setdiff(names(overrides), key)
  for (field in fields) {
    if (!field %in% names(manifest)) {
      manifest[[field]] <- NA
    }
    hit <- !is.na(idx)
    values <- overrides[[field]][idx[hit]]
    keep <- !is.na(values)
    row_ids <- which(hit)[keep]
    manifest[[field]][row_ids] <- values[keep]
  }
  manifest
}
