#' Build a Montage Render Manifest from an NFTab Dataset
#'
#' Converts a subject/observation-level `neurotabs` dataset into the
#' storage-agnostic render manifest consumed by [render_montage_report()].
#' File-backed feature rows are used directly; backend-backed rows are
#' materialized to NIfTI files in `materialize_dir`.
#'
#' @param ds An `nftab` object.
#' @param data_feature Feature name containing the map to render.
#' @param path_col Optional design/observation column containing map paths. If
#'   `NULL`, the adapter tries to infer it from the feature's ref encoding.
#' @param root Optional root used to resolve relative paths. Defaults to the
#'   NFTab root when available.
#' @param materialize_dir Optional directory for backend-backed feature maps.
#'   Required only when paths cannot be read directly; defaults to a temporary
#'   directory.
#' @param map_id_cols,label_cols Observation columns used to build `map_id` and
#'   `label`. Defaults to NFTab observation axes when available.
#' @param include_design Logical; include observation/design columns in the
#'   render manifest?
#' @param stat_kind,units,signed,threshold,p,tail,connectivity,min_cluster_size
#'   Default render-manifest policy columns.
#' @param level Manifest `level` value. Defaults to `"subject"`.
#' @param validate Logical; run [validate_manifest()] before returning?
#' @param check_files Logical; require output paths to exist during validation?
#'
#' @return A render manifest data frame.
#' @export
nf_render_manifest <- function(ds,
                               data_feature,
                               path_col = NULL,
                               root = NULL,
                               materialize_dir = NULL,
                               map_id_cols = NULL,
                               label_cols = map_id_cols,
                               include_design = TRUE,
                               stat_kind = "z",
                               units = stat_kind,
                               signed = TRUE,
                               threshold = NULL,
                               p = NULL,
                               tail = "two_sided",
                               connectivity = "18-connect",
                               min_cluster_size = 10L,
                               level = "subject",
                               validate = TRUE,
                               check_files = TRUE) {
  if (!requireNamespace("neurotabs", quietly = TRUE)) {
    stop("Package 'neurotabs' is required for nf_render_manifest().",
         call. = FALSE)
  }
  if (!inherits(ds, "nftab")) {
    stop("'ds' must be an nftab object.", call. = FALSE)
  }
  if (!is.character(data_feature) || length(data_feature) != 1L ||
      !nzchar(data_feature)) {
    stop("'data_feature' must be a single non-empty string.", call. = FALSE)
  }
  if (!data_feature %in% neurotabs::nf_feature_names(ds)) {
    stop(
      "Feature '", data_feature, "' not found in ds. Available features: ",
      paste(neurotabs::nf_feature_names(ds), collapse = ", "),
      call. = FALSE
    )
  }

  design <- neurotabs::nf_design(ds)
  if (nrow(design) == 0L) {
    stop("'ds' contains no observations.", call. = FALSE)
  }
  axes <- .nf_render_axes(ds)
  map_id_cols <- .nf_render_default_cols(map_id_cols, axes, design, ds)
  label_cols <- .nf_render_default_cols(label_cols, axes, design, ds)
  .nf_render_check_cols(design, map_id_cols, "map_id_cols")
  .nf_render_check_cols(design, label_cols, "label_cols")

  root <- root %||% .nf_render_root(ds)
  inferred_path_col <- path_col %||% .nf_feature_locator_col(ds, data_feature)
  if (!is.null(path_col) && nzchar(path_col) && !path_col %in% names(design)) {
    stop("'path_col' was not found in the NFTab design: ", path_col,
         call. = FALSE)
  }
  paths <- .nf_render_feature_paths(
    ds = ds,
    data_feature = data_feature,
    design = design,
    path_col = inferred_path_col,
    root = root,
    materialize_dir = materialize_dir
  )

  out <- if (isTRUE(include_design)) {
    as.data.frame(design, stringsAsFactors = FALSE)
  } else {
    data.frame(stringsAsFactors = FALSE, row.names = seq_len(nrow(design)))
  }
  out$path <- paths
  out$map_id <- .nf_render_row_ids(design, map_id_cols)
  out$label <- .nf_render_row_labels(design, label_cols)
  out$stat_kind <- stat_kind
  out$units <- units %||% stat_kind
  out$signed <- signed
  out$tail <- tail
  out$connectivity <- connectivity
  out$min_cluster_size <- min_cluster_size
  out$level <- level
  out$n <- 1L
  if ("subject" %in% names(design)) {
    out$subjects <- as.character(design$subject)
  }
  if (!is.null(threshold)) {
    out$threshold <- threshold
  }
  if (!is.null(p)) {
    out$p <- p
  }

  out <- .nf_render_order_manifest_columns(out)
  if (isTRUE(validate)) {
    out <- validate_manifest(out, check_files = check_files)
  }
  out
}

.nf_render_axes <- function(ds) {
  axes <- tryCatch(neurotabs::nf_axes(ds), error = function(e) character())
  axes <- as.character(axes)
  axes[nzchar(axes)]
}

.nf_render_default_cols <- function(cols, axes, design, ds) {
  if (!is.null(cols)) {
    return(as.character(cols))
  }
  cols <- intersect(axes, names(design))
  if (length(cols) > 0L) {
    return(cols)
  }
  row_id <- tryCatch(ds$manifest$row_id, error = function(e) NULL)
  if (!is.null(row_id) && row_id %in% names(design)) {
    return(row_id)
  }
  names(design)[[1L]]
}

.nf_render_check_cols <- function(design, cols, label) {
  missing <- setdiff(cols, names(design))
  if (length(missing) > 0L) {
    stop(
      "'", label, "' column(s) not found in NFTab design: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.nf_render_root <- function(ds) {
  root <- tryCatch(ds$.root, error = function(e) NULL)
  if (is.null(root) || !nzchar(root)) {
    return(NULL)
  }
  normalizePath(root, mustWork = FALSE)
}

.nf_feature_locator_col <- function(ds, data_feature) {
  feat <- ds$manifest$features[[data_feature]]
  for (enc in feat$encodings) {
    if (!identical(enc$type, "ref")) {
      next
    }
    locator <- enc$binding$locator
    col <- tryCatch(locator$column, error = function(e) NULL)
    if (!is.null(col) && nzchar(col)) {
      return(col)
    }
  }
  NULL
}

.nf_render_feature_paths <- function(ds,
                                     data_feature,
                                     design,
                                     path_col,
                                     root,
                                     materialize_dir) {
  if (!is.null(path_col) && nzchar(path_col) && path_col %in% names(design)) {
    paths <- .nf_render_resolve_paths(design[[path_col]], root = root)
    if (all(file.exists(paths))) {
      return(normalizePath(paths, mustWork = TRUE))
    }
  }

  materialize_dir <- materialize_dir %||%
    tempfile(paste0("neuromosaic-", data_feature, "-render-"))
  if (!dir.exists(materialize_dir)) {
    dir.create(materialize_dir, recursive = TRUE, showWarnings = FALSE)
  }
  vapply(seq_len(nrow(design)), function(i) {
    map_id <- .safe_file_stem(.nf_render_row_ids(design[i, , drop = FALSE],
                                                names(design)[[1L]]))
    out <- file.path(materialize_dir, sprintf("%03d_%s.nii.gz", i, map_id))
    value <- neurotabs::nf_resolve(ds, row_index = i, feature = data_feature,
                                   as_array = FALSE)
    if (!methods::is(value, "NeuroVol")) {
      stop(
        "Feature '", data_feature, "' row ", i,
        " did not resolve to a NeuroVol; provide a file-backed path_col or ",
        "a backend with native NeuroVol resolution.",
        call. = FALSE
      )
    }
    neuroim2::write_vol(value, out)
    normalizePath(out, mustWork = TRUE)
  }, character(1))
}

.nf_render_resolve_paths <- function(paths, root) {
  paths <- as.character(paths)
  paths <- trimws(paths)
  if (any(!nzchar(paths) | is.na(paths))) {
    return(paths)
  }
  absolute <- grepl("^(/|[A-Za-z]:[/\\\\])", paths)
  if (!is.null(root)) {
    paths[!absolute] <- file.path(root, paths[!absolute])
  }
  paths
}

.nf_render_row_ids <- function(design, cols) {
  apply(design[, cols, drop = FALSE], 1L, function(row) {
    parts <- paste(names(row), as.character(row), sep = "-")
    .safe_file_stem(paste(parts, collapse = "_"))
  })
}

.nf_render_row_labels <- function(design, cols) {
  apply(design[, cols, drop = FALSE], 1L, function(row) {
    paste(as.character(row), collapse = " / ")
  })
}

.nf_render_order_manifest_columns <- function(manifest) {
  preferred <- c(
    "map_id", "path", "stat_kind", "units", "signed", "p", "threshold",
    "tail", "connectivity", "min_cluster_size", "level", "label",
    "description", "n", "subjects"
  )
  extra <- setdiff(names(manifest), preferred)
  manifest[, c(intersect(preferred, names(manifest)), extra), drop = FALSE]
}
