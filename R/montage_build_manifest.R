#' Build a Montage Render Manifest
#'
#' Builds the storage-agnostic render manifest consumed by montage reports.
#' Sources may be CSV/TSV tables, data frames, explicit path vectors, or a glob
#' pattern. Path-based sources parse BIDS-style filename entities such as
#' `contrast-faces_model-m1_stat-z.nii.gz`.
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

.manifest_from_paths <- function(paths, root = ".") {
  paths <- normalizePath(paths, mustWork = TRUE)
  entities <- lapply(paths, .parse_bids_entities)
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
    out$map_id <- vapply(seq_along(entities), function(i) {
      .manifest_map_id(entities[[i]], paths[[i]])
    }, character(1))
  }

  out
}

.parse_bids_entities <- function(path) {
  stem <- basename(path)
  stem <- sub("\\.nii\\.gz$", "", stem, ignore.case = TRUE)
  stem <- sub("\\.[^.]+$", "", stem)
  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]

  entities <- list()
  for (part in parts) {
    if (!grepl("-", part, fixed = TRUE)) {
      next
    }
    split <- strsplit(part, "-", fixed = TRUE)[[1]]
    key <- make.names(split[[1]], unique = FALSE)
    value <- paste(split[-1], collapse = "-")
    if (nzchar(key) && nzchar(value)) {
      entities[[key]] <- value
    }
  }
  entities
}

.manifest_map_id <- function(entities, path) {
  if (length(entities) == 0L) {
    stem <- basename(path)
    stem <- sub("\\.nii\\.gz$", "", stem, ignore.case = TRUE)
    return(tools::file_path_sans_ext(stem))
  }
  paste(paste(names(entities), unlist(entities, use.names = FALSE), sep = "-"),
        collapse = "_")
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
