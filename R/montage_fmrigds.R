#' Build a Montage Render Manifest from an fmrigds GDS
#'
#' Converts a realised `fmrigds` GDS assay into the storage-agnostic render
#' manifest consumed by [render_montage_report()]. The selected assay is
#' materialized to NIfTI files, while GDS map metadata such as `contrast`,
#' `model`, and `variant` is carried into the manifest.
#'
#' @param gds A realised `fmrigds` GDS object.
#' @param assay Assay name to materialize.
#' @param materialize_dir Directory where assay maps are written. Defaults to a
#'   temporary directory.
#' @param map_id_cols,label_cols GDS metadata columns used to build `map_id` and
#'   `label`. Defaults prefer `contrast/model/variant`, falling back to
#'   `contrast/subject`.
#' @param analysis_id_cols Metadata columns defining associated-map groups.
#' @param quantity Generic scientific quantity, including custom namespaced
#'   values.
#' @param distribution Optional z or t distribution for test statistics.
#' @param role Role within each analysis group.
#' @param stat_kind Legacy statistic-kind compatibility field.
#' @param units,signed,threshold,p,tail,connectivity,min_cluster_size Default
#'   render-manifest policy columns.
#' @param level Manifest `level` value. Defaults to `"group"`.
#' @param validate Logical; run [validate_manifest()] before returning?
#' @param check_files Logical; require materialized paths to exist?
#'
#' @return A render manifest data frame.
#' @export
fmrigds_render_manifest <- function(gds,
                                    assay = "beta",
                                    materialize_dir = NULL,
                                    map_id_cols = NULL,
                                    label_cols = map_id_cols,
                                    analysis_id_cols = map_id_cols,
                                    stat_kind = .fmrigds_default_stat_kind(assay),
                                    quantity = NULL,
                                    distribution = NULL,
                                    role = "primary",
                                    units = assay,
                                    signed = NULL,
                                    threshold = NULL,
                                    p = NULL,
                                    tail = "two_sided",
                                    connectivity = "18-connect",
                                    min_cluster_size = 10L,
                                    level = "group",
                                    validate = TRUE,
                                    check_files = TRUE) {
  if (!requireNamespace("fmrigds", quietly = TRUE)) {
    stop("Package 'fmrigds' is required for fmrigds_render_manifest().",
         call. = FALSE)
  }
  if (!inherits(gds, "gds")) {
    stop("'gds' must be an fmrigds GDS object.", call. = FALSE)
  }
  if (!is.character(assay) || length(assay) != 1L || !nzchar(assay)) {
    stop("'assay' must be a single non-empty string.", call. = FALSE)
  }
  assay_names <- names(gds$assays)
  if (!assay %in% assay_names) {
    stop(
      "Assay '", assay, "' not found in GDS. Available assays: ",
      paste(assay_names, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(quantity) && is.null(stat_kind)) {
    quantity <- .fmrigds_default_quantity(assay)
  }

  materialize_dir <- materialize_dir %||%
    tempfile(paste0("neuromosaic-fmrigds-", assay, "-"))
  if (!dir.exists(materialize_dir)) {
    dir.create(materialize_dir, recursive = TRUE, showWarnings = FALSE)
  }

  vols <- fmrigds::as_neurovol_list(gds, assay = assay, drop_dim = FALSE)
  flat <- .fmrigds_flatten_vols(vols)
  meta <- .fmrigds_map_metadata(gds, assay = assay)
  flat <- .fmrigds_join_metadata(flat, meta)
  flat$path <- .fmrigds_write_maps(flat, materialize_dir = materialize_dir)

  map_id_cols <- .fmrigds_default_cols(map_id_cols, flat)
  map_id_cols <- .fmrigds_ensure_unique_id_cols(flat, map_id_cols)
  label_cols <- .fmrigds_default_cols(label_cols, flat)
  analysis_id_cols <- .fmrigds_default_cols(analysis_id_cols, flat)
  .nf_render_check_cols(flat, map_id_cols, "map_id_cols")
  .nf_render_check_cols(flat, label_cols, "label_cols")
  .nf_render_check_cols(flat, analysis_id_cols, "analysis_id_cols")

  out <- flat[, setdiff(names(flat), "vol"), drop = FALSE]
  out$map_id <- .nf_render_row_ids(out, map_id_cols)
  out$analysis_id <- .nf_render_row_ids(out, analysis_id_cols)
  out$role <- role
  out$label <- .nf_render_row_labels(out, label_cols)
  out$selector_label <- assay
  if (!is.null(quantity)) out$quantity <- quantity
  if (!is.null(distribution)) out$distribution <- distribution
  if (!is.null(stat_kind) && (is.null(quantity) || !missing(stat_kind))) {
    out$stat_kind <- stat_kind
  }
  out$units <- units %||% quantity %||% assay
  if (!is.null(signed)) out$signed <- signed
  out$tail <- tail
  out$connectivity <- connectivity
  out$min_cluster_size <- min_cluster_size
  out$level <- level
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

.fmrigds_default_stat_kind <- function(assay) {
  assay <- tolower(as.character(assay))
  if (assay %in% c("t", "z", "beta", "cope")) {
    return(assay)
  }
  NULL
}

.fmrigds_default_quantity <- function(assay) {
  normalized <- .normalize_montage_quantity(tolower(as.character(assay)))
  if (normalized %in% .montage_manifest_builtin_quantities) {
    return(normalized)
  }
  paste0("assay:", tolower(as.character(assay)))
}

.fmrigds_flatten_vols <- function(x, depth = 1L, keys = list()) {
  if (methods::is(x, "NeuroVol")) {
    out <- data.frame(
      subject = keys$subject %||% NA_character_,
      contrast = keys$contrast %||% NA_character_,
      stringsAsFactors = FALSE
    )
    out$vol <- list(x)
    return(out)
  }
  if (!is.list(x)) {
    stop("GDS assay extraction produced a non-list, non-NeuroVol object.",
         call. = FALSE)
  }
  nms <- names(x)
  if (is.null(nms)) {
    nms <- as.character(seq_along(x))
  }
  key_name <- c("subject", "contrast")[pmin(depth, 2L)]
  pieces <- lapply(seq_along(x), function(i) {
    .fmrigds_flatten_vols(
      x[[i]],
      depth = depth + 1L,
      keys = c(keys, stats::setNames(list(nms[[i]]), key_name))
    )
  })
  if (length(pieces) == 0L) {
    return(data.frame())
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

.fmrigds_map_metadata <- function(gds, assay) {
  tbl <- fmrigds::gds_to_tibble(
    gds,
    assays = assay,
    drop_na = TRUE,
    include_col_data = TRUE
  )
  meta_cols <- setdiff(names(tbl), c("sample", assay))
  meta <- unique(tbl[, meta_cols, drop = FALSE])
  rownames(meta) <- NULL
  as.data.frame(meta, stringsAsFactors = FALSE)
}

.fmrigds_join_metadata <- function(flat, meta) {
  key_fields <- intersect(c("subject", "contrast"), intersect(names(flat), names(meta)))
  if (length(key_fields) == 0L || nrow(meta) == 0L) {
    return(flat)
  }
  flat_key <- .fmrigds_row_key(flat, key_fields)
  meta_key <- .fmrigds_row_key(meta, key_fields)
  idx <- match(flat_key, meta_key)
  extra <- setdiff(names(meta), names(flat))
  for (field in extra) {
    flat[[field]] <- meta[[field]][idx]
  }
  flat
}

.fmrigds_row_key <- function(data, fields) {
  apply(data[, fields, drop = FALSE], 1L, function(row) {
    paste(as.character(row), collapse = "\r")
  })
}

.fmrigds_write_maps <- function(flat, materialize_dir) {
  vapply(seq_len(nrow(flat)), function(i) {
    stem <- .safe_file_stem(paste(
      stats::na.omit(as.character(flat[i, c("contrast", "subject"), drop = TRUE])),
      collapse = "_"
    ))
    path <- file.path(materialize_dir, sprintf("%03d_%s.nii.gz", i, stem))
    neuroim2::write_vol(flat$vol[[i]], path)
    normalizePath(path, mustWork = TRUE)
  }, character(1))
}

.fmrigds_default_cols <- function(cols, manifest) {
  if (!is.null(cols)) {
    return(as.character(cols))
  }
  preferred <- intersect(c("contrast", "model", "variant"), names(manifest))
  if (length(preferred) > 0L) {
    return(preferred)
  }
  fallback <- intersect(c("contrast", "subject"), names(manifest))
  if (length(fallback) > 0L) {
    return(fallback)
  }
  names(manifest)[[1L]]
}

.fmrigds_ensure_unique_id_cols <- function(manifest, cols) {
  ids <- .nf_render_row_ids(manifest, cols)
  if (!anyDuplicated(ids)) {
    return(cols)
  }
  if ("subject" %in% names(manifest) && !"subject" %in% cols) {
    cols <- c(cols, "subject")
  }
  cols
}
