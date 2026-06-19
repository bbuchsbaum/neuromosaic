#' Apply a Montage Manifest Labeller
#'
#' Applies the PRD labeller contract to a render manifest. A function labeller
#' is called once per row with an entity list and must return `title` (or
#' `label`), plus optional `short`, `description`, and `legend_semantics`.
#' A data-frame labeller is joined by `map_id` and may contain the same fields.
#'
#' @param manifest A render manifest data frame.
#' @param labeller `NULL`, a function, or a data frame keyed by `map_id`.
#' @param entity_cols Optional character vector of manifest columns passed to a
#'   function labeller. Defaults to all columns in the row.
#' @param check_files Logical; passed to [validate_manifest()] after labelling.
#' @param empty Action when overlay QC finds a map with no suprathreshold
#'   voxels: `"error"` (default) or `"warning"`. Forwarded to
#'   [validate_manifest()] so the labeller path honors the same empty-map policy
#'   as the rest of the report pipeline.
#'
#' @return The labelled and validated manifest.
#' @export
apply_montage_labeller <- function(manifest,
                                   labeller = NULL,
                                   entity_cols = NULL,
                                   check_files = FALSE,
                                   empty = c("error", "warning")) {
  if (!is.data.frame(manifest)) {
    stop("'manifest' must be a data frame.", call. = FALSE)
  }
  empty <- match.arg(empty)
  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)

  if (is.null(labeller)) {
    return(validate_manifest(manifest, check_files = check_files, empty = empty))
  }
  if (is.function(labeller)) {
    manifest <- .apply_function_labeller(manifest, labeller, entity_cols)
  } else if (is.data.frame(labeller)) {
    manifest <- .apply_table_labeller(manifest, labeller)
  } else {
    stop("'labeller' must be NULL, a function, or a data frame.",
         call. = FALSE)
  }

  validate_manifest(manifest, check_files = check_files, empty = empty)
}

.apply_function_labeller <- function(manifest, labeller, entity_cols) {
  if (is.null(entity_cols)) {
    entity_cols <- names(manifest)
  }
  missing_cols <- setdiff(entity_cols, names(manifest))
  if (length(missing_cols) > 0L) {
    stop(
      "'entity_cols' not found in manifest: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(manifest))) {
    entities <- as.list(manifest[i, entity_cols, drop = FALSE])
    labelled <- .normalize_labeller_output(labeller(entities), row = i)
    manifest <- .assign_labeller_fields(manifest, i, labelled)
  }

  manifest
}

.apply_table_labeller <- function(manifest, labeller) {
  labels <- as.data.frame(labeller, stringsAsFactors = FALSE)
  if (!"map_id" %in% names(labels)) {
    stop("A data-frame labeller must contain a 'map_id' column.",
         call. = FALSE)
  }
  if (anyDuplicated(labels$map_id)) {
    stop("A data-frame labeller must have unique 'map_id' values.",
         call. = FALSE)
  }

  fields <- intersect(
    c("title", "label", "short", "description", "legend_semantics"),
    names(labels)
  )
  if (length(fields) == 0L) {
    stop(
      "A data-frame labeller must contain at least one label field.",
      call. = FALSE
    )
  }

  idx <- match(manifest$map_id, labels$map_id)
  for (i in seq_len(nrow(manifest))) {
    if (is.na(idx[[i]])) {
      next
    }
    labelled <- .normalize_labeller_output(labels[idx[[i]], fields, drop = FALSE],
                                           row = i)
    manifest <- .assign_labeller_fields(manifest, i, labelled)
  }

  manifest
}

.normalize_labeller_output <- function(x, row) {
  if (is.data.frame(x)) {
    if (nrow(x) != 1L) {
      stop(
        "Labeller output for row ", row, " must have exactly one row.",
        call. = FALSE
      )
    }
    x <- as.list(x[1, , drop = FALSE])
  }
  if (!is.list(x)) {
    stop("Labeller output for row ", row, " must be a list or data frame.",
         call. = FALSE)
  }

  title <- x$title %||% x$label
  if (is.null(title) || length(title) != 1L || is.na(title) ||
      !nzchar(trimws(as.character(title)))) {
    stop(
      "Labeller output for row ", row,
      " must include a non-empty 'title' or 'label'.",
      call. = FALSE
    )
  }

  list(
    label = as.character(title),
    short = .labeller_optional_scalar(x$short),
    description = .labeller_optional_scalar(x$description),
    legend_semantics = .labeller_optional_scalar(x$legend_semantics)
  )
}

.assign_labeller_fields <- function(manifest, row, labelled) {
  for (field in names(labelled)) {
    value <- labelled[[field]]
    if (is.null(value)) {
      next
    }
    if (!field %in% names(manifest)) {
      manifest[[field]] <- NA_character_
    }
    manifest[[field]][[row]] <- value
  }
  manifest
}

.labeller_optional_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  as.character(x[[1]])
}
