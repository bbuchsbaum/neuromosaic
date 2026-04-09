#' Extract Per-Cluster Time-Course Data
#'
#' Given a data source and cluster information (from
#' [build_cluster_explorer_data()]), extract the mean signal per cluster per
#' sample and merge with design columns.
#'
#' @param data_source A data object containing the backing images.
#'   Supported types: a 4-D `NeuroVec` (neuroim2), a `list` of 3-D
#'   `NeuroVol` objects (one per sample/beta), or an `nftab` (neurotabs).
#' @param cluster_info List returned by [build_cluster_explorer_data()],
#'   must contain `cluster_voxels` (named list of Nx3 matrices) and
#'   `cluster_table`.
#' @param design Optional `data.frame` with one row per sample. Required when
#'   `data_source` is a `NeuroVec` or `list`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble with columns `cluster_id`, `.sample_index`, `value`,
#'   and all columns from `design` (or observation table for `nftab`).
#' @export
extract_cluster_data <- function(data_source, cluster_info, design = NULL, ...) {
  UseMethod("extract_cluster_data")
}

#' @rdname extract_cluster_data
#' @export
extract_cluster_data.NeuroVec <- function(data_source, cluster_info,
                                          design = NULL, ...) {
  cluster_voxels <- cluster_info$cluster_voxels
  if (length(cluster_voxels) == 0) return(.empty_tc_tibble(design))

  n_samples <- dim(data_source)[4]
  sample_tbl <- .normalize_sample_table(sample_table = design,
                                        n_samples = n_samples)

  # Delegate to existing compute engine (handles caching, coercion, fast path)
  ts_tbl <- .compute_cluster_timeseries(
    data_source = data_source,
    cluster_voxels = cluster_voxels,
    sample_table = sample_tbl,
    signal_fun = mean,
    signal_fun_args = list(na.rm = TRUE)
  )

  if (nrow(ts_tbl) == 0) return(.empty_tc_tibble(design))
  dplyr::rename(ts_tbl, value = "signal")
}

#' @rdname extract_cluster_data
#' @export
extract_cluster_data.list <- function(data_source, cluster_info,
                                      design = NULL, ...) {
  cluster_voxels <- cluster_info$cluster_voxels
  if (length(cluster_voxels) == 0) return(.empty_tc_tibble(design))

  stopifnot(
    length(data_source) > 0,
    all(vapply(data_source, function(x) methods::is(x, "NeuroVol"), logical(1)))
  )

  n_samples <- length(data_source)
  ids <- names(cluster_voxels)

  # Sample-outer / cluster-inner to avoid redundant volume access
  values <- matrix(NA_real_, nrow = n_samples, ncol = length(ids))
  colnames(values) <- ids
  for (t in seq_len(n_samples)) {
    arr <- as.array(data_source[[t]])
    for (k in seq_along(ids)) {
      vox <- cluster_voxels[[ids[k]]]
      values[t, k] <- mean(arr[vox], na.rm = TRUE)
    }
  }

  out <- vector("list", length(ids))
  for (k in seq_along(ids)) {
    out[[k]] <- tibble::tibble(
      cluster_id = ids[k],
      .sample_index = seq_len(n_samples),
      value = values[, k]
    )
  }
  ret <- dplyr::bind_rows(out)
  .merge_design(ret, design)
}

#' @rdname extract_cluster_data
#' @export
extract_cluster_data.default <- function(data_source, cluster_info,
                                         design = NULL, ...) {
  if (inherits(data_source, "nftab")) {
    if (!requireNamespace("neurotabs", quietly = TRUE)) {
      stop("Package 'neurotabs' is required for nftab data sources.",
           call. = FALSE)
    }
    return(.extract_from_nftab(data_source, cluster_info, ...))
  }

  # Fallback for NeuroVec subclasses not caught by S3

  if (methods::is(data_source, "NeuroVec")) {
    return(extract_cluster_data.NeuroVec(data_source, cluster_info, design, ...))
  }

  stop(
    "No extract_cluster_data method for class: ",
    paste(class(data_source), collapse = ", "),
    ". Supported: NeuroVec, list (of NeuroVol), nftab.",
    call. = FALSE
  )
}

.extract_from_nftab <- function(nftab_obj, cluster_info, ...) {
  cluster_voxels <- cluster_info$cluster_voxels
  design_tbl <- neurotabs::nf_design(nftab_obj)
  if (length(cluster_voxels) == 0) return(.empty_tc_tibble(design_tbl))

  n_samples <- nrow(design_tbl)
  ids <- names(cluster_voxels)

  # Sample-outer / cluster-inner: resolve each volume once, extract all clusters
  values <- matrix(NA_real_, nrow = n_samples, ncol = length(ids))
  colnames(values) <- ids
  for (t in seq_len(n_samples)) {
    vol <- neurotabs::nf_resolve(nftab_obj, row = t)
    for (k in seq_along(ids)) {
      vox <- cluster_voxels[[ids[k]]]
      vals <- if (methods::is(vol, "NeuroVol")) vol[vox] else as.numeric(vol[vox])
      values[t, k] <- mean(vals, na.rm = TRUE)
    }
  }

  out <- vector("list", length(ids))
  for (k in seq_along(ids)) {
    out[[k]] <- tibble::tibble(
      cluster_id = ids[k],
      .sample_index = seq_len(n_samples),
      value = values[, k]
    )
  }
  ret <- dplyr::bind_rows(out)
  .merge_design(ret, design_tbl)
}

# -- Helpers -------------------------------------------------------------------

.merge_design <- function(tc_tbl, design) {
  if (is.null(design) || nrow(design) == 0) return(tc_tbl)
  design_tbl <- tibble::as_tibble(design)
  if (!".sample_index" %in% names(design_tbl)) {
    design_tbl$.sample_index <- seq_len(nrow(design_tbl))
  }
  # Avoid duplicating columns already present from prefetch join
  shared <- intersect(names(tc_tbl), names(design_tbl))
  shared <- setdiff(shared, ".sample_index")
  if (length(shared) > 0) {
    design_tbl <- design_tbl[, !names(design_tbl) %in% shared, drop = FALSE]
  }
  dplyr::left_join(tc_tbl, design_tbl, by = ".sample_index")
}

.empty_tc_tibble <- function(design = NULL) {
  base <- tibble::tibble(
    cluster_id = character(0),
    .sample_index = integer(0),
    value = numeric(0)
  )

  if (is.null(design)) {
    return(base)
  }

  design_empty <- tibble::as_tibble(design)[0, , drop = FALSE]
  if (".sample_index" %in% names(design_empty)) {
    design_empty <- design_empty[, names(design_empty) != ".sample_index",
                                 drop = FALSE]
  }

  dplyr::bind_cols(base, design_empty)
}
