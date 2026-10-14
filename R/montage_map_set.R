# Map-set value objects and raw-data helpers for the Space runtime.

#' Define a set of maps for comparison
#'
#' Membership references the report manifest's `map_id` values. Use a table
#' to pair t statistics, estimates, and standard errors for each `member_id`;
#' optional `model` and `contrast` columns default to `"model"` and `"maps"`.
#' A character vector makes each map a separate member. Set membership never
#' changes analysis groups, report figures, thresholds, or cluster tables.
#'
#' The first report integration supports `member_kind = "subject"`, one
#' model/contrast per set, and t/estimate channels. Generic map and z-statistic
#' specifications can be validated but cannot yet be rendered in Space.
#' The comparison mask is explicit, finite, binary, and nonempty. Shared
#' finite zeros are retained; missing values are not converted to zero.
#'
#' @param id Stable non-empty set identifier.
#' @param members Character map IDs or a data frame with map_id and member_id.
#' @param mask Required NeuroVol or NIfTI path defining the comparison domain.
#' @param label Optional reader-facing label.
#' @param member_kind Member vocabulary.
#' @param template Optional NeuroVol or NIfTI path on the set grid. Defaults
#'   to the report's `bg`.
#' @param atlas Optional list with `name`, `labels` (a label NeuroVol or NIfTI
#'   path), and `regions` (a data frame with integer `id` and text `label`).
#' @param covariates Optional data frame keyed by member_id. Values may be
#'   numeric, categorical, or missing. Logical columns become categorical
#'   `"TRUE"`/`"FALSE"` strings; missing values remain missing.
#' @return A montage_map_set value object.
#' @md
#' @export
montage_map_set <- function(id, members, mask, label = NULL,
                            member_kind = c("map", "subject"),
                            template = NULL, atlas = NULL, covariates = NULL) {
  id <- .montage_map_set_token(id, "id")
  member_kind <- match.arg(member_kind)
  members <- .montage_map_set_members(members)
  .montage_map_set_volume_ref(mask, "mask", TRUE)
  .montage_map_set_volume_ref(template, "template")
  atlas <- .montage_map_set_atlas(atlas)
  if (!is.null(label)) label <- .montage_map_set_character(label, "label")
  covariates <- .montage_map_set_covariates(covariates, members, member_kind)
  structure(list(id = id, members = members, mask = mask, label = label %||% id,
                 member_kind = member_kind, template = template, atlas = atlas,
                 covariates = covariates), class = "montage_map_set")
}

.montage_map_set_character <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("'", field, "' must be one non-empty character value.", call. = FALSE)
  }
  trimws(x)
}

.montage_map_set_token <- function(x, field) {
  x <- .montage_map_set_character(x, field)
  if (nchar(x, type = "bytes") > 128L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", x)) {
    stop("'", field, "' must be an ASCII token beginning with an alphanumeric character ",
         "and containing only alphanumerics, '.', '_', or '-'.", call. = FALSE)
  }
  x
}

.montage_map_set_volume_ref <- function(x, field, required = FALSE) {
  if (is.null(x)) {
    if (required) stop("'", field, "' is required.", call. = FALSE)
    return(invisible(NULL))
  }
  if (!methods::is(x, "NeuroVol") &&
      !(is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x))) {
    stop("'", field, "' must be a NeuroVol or a non-empty NIfTI path.", call. = FALSE)
  }
  invisible(x)
}

.montage_map_set_members <- function(members) {
  if (is.character(members) && !is.data.frame(members)) {
    if (!length(members) || anyNA(members) || any(!nzchar(trimws(members)))) {
      stop("Character 'members' must contain non-empty map_id values.", call. = FALSE)
    }
    out <- data.frame(map_id = trimws(members), member_id = trimws(members),
                      model = "model", contrast = "maps", label = NA_character_,
                      stringsAsFactors = FALSE)
  } else if (is.data.frame(members)) {
    required <- c("map_id", "member_id")
    missing <- setdiff(required, names(members))
    if (length(missing)) stop("Table 'members' is missing column(s): ",
                              paste(missing, collapse = ", "), ".", call. = FALSE)
    unknown <- setdiff(names(members), c(required, "model", "contrast", "label"))
    if (length(unknown)) stop("Table 'members' has unsupported column(s): ",
                              paste(unknown, collapse = ", "), ".", call. = FALSE)
    if (!nrow(members)) stop("Table 'members' must not be empty.", call. = FALSE)
    get <- function(field, default) {
      if (field %in% names(members)) trimws(as.character(members[[field]])) else default
    }
    out <- data.frame(map_id = get("map_id", NA_character_),
                      member_id = get("member_id", NA_character_),
                      model = get("model", "model"), contrast = get("contrast", "maps"),
                      label = get("label", NA_character_), stringsAsFactors = FALSE)
    out$label[is.na(out$label) | !nzchar(out$label)] <- NA_character_
  } else stop("'members' must be a character vector or data frame.", call. = FALSE)
  for (field in c("map_id", "member_id", "model", "contrast")) {
    if (anyNA(out[[field]]) || any(!nzchar(out[[field]]))) {
      stop("'members$", field, "' must be non-empty.", call. = FALSE)
    }
  }
  for (field in c("member_id", "model", "contrast")) {
    out[[field]] <- vapply(out[[field]], .montage_map_set_token, character(1),
                           field = paste0("members$", field))
  }
  if (anyDuplicated(out$map_id)) stop("A map_id occurs more than once within a map set: ",
                                       paste(unique(out$map_id[duplicated(out$map_id)]), collapse = ", "),
                                       ".", call. = FALSE)
  out
}

.montage_map_set_covariates <- function(covariates, members, member_kind) {
  if (is.null(covariates)) return(NULL)
  if (!is.data.frame(covariates) || !"member_id" %in% names(covariates)) {
    stop("'covariates' must be a data frame with a 'member_id' column.", call. = FALSE)
  }
  covariates <- as.data.frame(covariates, stringsAsFactors = FALSE)
  covariates$member_id <- vapply(as.character(covariates$member_id),
                                 .montage_map_set_token, character(1),
                                 field = "covariates$member_id")
  if (anyDuplicated(covariates$member_id) ||
      any(!covariates$member_id %in% unique(members$member_id))) {
    stop("'covariates$member_id' must be unique and identify set members.", call. = FALSE)
  }
  if (identical(member_kind, "subject") &&
      any(!covariates$member_id %in% unique(members$member_id))) {
    stop("Subject covariates must identify subjects in the set.", call. = FALSE)
  }
  for (field in setdiff(names(covariates), "member_id")) {
    value <- covariates[[field]]
    if (is.factor(value)) value <- as.character(value)
    # Space accepts categorical strings, numeric values and null, not JSON
    # booleans. Preserve logical missingness while naming the two categories.
    if (is.logical(value)) value <- ifelse(is.na(value), NA_character_,
                                         ifelse(value, "TRUE", "FALSE"))
    if (is.numeric(value) && any(!is.finite(value[!is.na(value)]))) {
      stop("Numeric covariate '", field, "' must be finite where present.", call. = FALSE)
    }
    if (is.character(value) && any(!is.na(value) & !nzchar(value))) {
      stop("Character covariate '", field, "' must be non-empty where present.", call. = FALSE)
    }
    if (!is.numeric(value) && !is.character(value)) {
      stop("Covariate '", field, "' must be numeric, character, logical, or missing.",
           call. = FALSE)
    }
    covariates[[field]] <- value
  }
  covariates
}

.montage_map_set_atlas <- function(atlas) {
  if (is.null(atlas)) return(NULL)
  if (!is.list(atlas) || is.data.frame(atlas) ||
      !all(c("name", "labels", "regions") %in% names(atlas))) {
    stop("'atlas' must be NULL or a list with name, labels, and regions.", call. = FALSE)
  }
  atlas$name <- .montage_map_set_character(atlas$name, "atlas$name")
  .montage_map_set_volume_ref(atlas$labels, "atlas$labels", TRUE)
  if (!is.data.frame(atlas$regions) || !all(c("id", "label") %in% names(atlas$regions))) {
    stop("'atlas$regions' must be a data frame with id and label columns.", call. = FALSE)
  }
  ids <- atlas$regions$id
  # Factor codes are not atlas IDs (including when a background label is 0).
  if (is.factor(ids)) ids <- as.character(ids)
  ids <- suppressWarnings(as.numeric(ids))
  labels <- trimws(as.character(atlas$regions$label))
  if (any(!is.finite(ids)) || any(ids < 0) || any(ids > .Machine$integer.max) ||
      any(ids != floor(ids)) ||
      anyDuplicated(ids) || anyNA(labels) || any(!nzchar(labels))) {
    stop("'atlas$regions' must have unique non-negative integer ids and non-empty labels.",
         call. = FALSE)
  }
  atlas$regions$id <- as.integer(ids)
  atlas$regions$label <- labels
  atlas
}

.montage_map_set_channel <- function(quantity, distribution, map_id, set_id) {
  if (identical(quantity, "test_statistic") && distribution %in% c("t", "z")) return(distribution)
  if (quantity %in% c("estimate", "standard_error")) return(quantity)
  stop("Map set '", set_id, "' cannot use map_id '", map_id,
       "': quantity/distribution does not define t, z, estimate, or standard_error.",
       call. = FALSE)
}

.montage_map_set_read_volume <- function(x, field, set_id) {
  if (is.character(x)) x <- neuroim2::read_vol(x)
  if (!methods::is(x, "NeuroVol")) {
    stop("Map set '", set_id, "' ", field, " must resolve to a NeuroVol.", call. = FALSE)
  }
  if (length(dim(x)) != 3L || any(dim(x) <= 0L)) {
    stop("Map set '", set_id, "' ", field, " must be a non-empty three-dimensional volume.",
         call. = FALSE)
  }
  x
}

.montage_map_set_source <- function(manifest, row) {
  if ("stat_map" %in% names(manifest) &&
      !.missing_column_values(manifest$stat_map)[[row]]) {
    source <- if (is.list(manifest$stat_map)) manifest$stat_map[[row]] else manifest$stat_map[[row]]
  } else if ("path" %in% names(manifest) &&
             !.missing_character(as.character(manifest$path))[[row]]) {
    source <- neuroim2::read_vol(as.character(manifest$path[[row]]))
  } else {
    stop("Map set loading requires a materialized stat_map or path for map_id '",
         manifest$map_id[[row]], "'; deferred recipes are not executed.", call. = FALSE)
  }
  .montage_map_set_read_volume(source, paste0("map_id '", manifest$map_id[[row]], "'"), "source")
}

.montage_map_set_ras_geometry <- function(volume, field, set_id) {
  geometry <- .montage_volume_geometry(volume)
  affine <- matrix(as.numeric(geometry$affine), nrow = 4L, ncol = 4L)
  linear <- affine[1:3, 1:3, drop = FALSE]
  if (any(!is.finite(affine)) || !identical(geometry$orientation, "RAS") || any(diag(linear) <= 0) ||
      any(linear[row(linear) != col(linear)] != 0) ||
      any(affine[4, ] != c(0, 0, 0, 1))) {
    stop("Map set '", set_id, "' ", field,
         " must use an axis-aligned positive-diagonal RAS+ affine.", call. = FALSE)
  }
  geometry
}

.montage_map_set_same_geometry <- function(reference, candidate, field, set_id) {
  mismatch <- .montage_geometry_mismatches(reference, candidate)
  if (length(mismatch)) stop("Map set '", set_id, "' ", field,
                             " does not match the set grid: ",
                             paste(mismatch, collapse = ", "), ".", call. = FALSE)
}

.montage_map_set_float32 <- function(values, label) {
  if (any(is.infinite(values))) {
    stop("Map-set data contain infinite values for ", label, ".", call. = FALSE)
  }
  finite <- is.finite(values)
  if (any(abs(values[finite]) > (2 - 2^-23) * 2^127)) {
    stop("Float32 conversion overflows finite values for ", label, ".", call. = FALSE)
  }
  as.numeric(values)
}
