#' Build a Montage Render Manifest from Parcel Statistics
#'
#' Converts one parcel-level results table into the grouped render manifest
#' consumed by [render_montage_report()]. Each selected metric is aligned to the
#' atlas once, retained as named parcel values for surface rendering, and
#' expanded with [neuroatlas::parcel_volume()] for volume slices, peak tables,
#' and optional interactive volume views.
#'
#' @details
#' The parcel table remains the authoritative statistical result. The generated
#' `parcel_values` and `stat_map` list-columns are deterministic display
#' representations in atlas order and atlas space, respectively.
#'
#' When `metrics = NULL`, numeric columns that are not atlas metadata or join
#' keys are selected. Common names receive generic display semantics: z/t
#' statistics become `test_statistic`, beta/effect columns become `estimate`,
#' and standard-error or variance columns use their nonnegative built-in
#' profiles. Other names become namespaced `parcel:<metric>` quantities and use
#' the ordinary data-driven custom-metric defaults.
#'
#' @param data A data frame, tibble, or `neuroatlas::parcel_data` object with
#'   one row per parcel and one or more numeric metric columns.
#' @param atlas A `neuroatlas` atlas. A volumetric atlas is required when
#'   `include_volume = TRUE`; a surface atlas may be used for surface-only
#'   manifests.
#' @param metrics Character vector naming metric columns. `NULL` infers numeric
#'   non-key, non-atlas-metadata columns.
#' @param by Parcel-key specification passed to
#'   [neuroatlas::align_parcel_values()].
#' @param allow_partial Logical. If `FALSE` (default), every atlas parcel must
#'   be represented. If `TRUE`, missing parcels receive `NA`; unknown and
#'   duplicate keys still error.
#' @param analysis_id Stable identifier shared by the generated map variants.
#'   `NULL` derives one from the atlas name.
#' @param analysis_label Optional human-facing analysis title. `NULL` uses the
#'   atlas name followed by "parcel statistics".
#' @param primary Optional metric name to make primary. `NULL` chooses the first
#'   inferred test statistic, then falls back to the first metric.
#' @param quantity,distribution,labels,units Optional scalar, per-metric, or
#'   named per-metric overrides. Unnamed vectors must have one value per metric;
#'   named values are matched by metric name.
#' @param threshold,df,p Optional scalar, per-metric, or named numeric manifest
#'   values. Omitted values are resolved later by the ordinary montage policy.
#' @param tail,connectivity,min_cluster_size Manifest policy defaults applied to
#'   every generated row.
#' @param background Numeric scalar passed to [neuroatlas::parcel_volume()] for
#'   voxels outside the atlas. The default keeps background distinct from a
#'   valid zero-valued parcel.
#' @param include_volume Logical. If `TRUE` (default for volumetric atlases),
#'   add a `stat_map` list-column. If `FALSE`, return surface-ready parcel values
#'   without volumetric maps.
#' @param overrides Optional named list/vector or data frame passed through the
#'   same override machinery as [build_manifest()].
#' @param validate Logical. Validate the resulting manifest structure and
#'   semantics? Overlay emptiness is deferred to report rendering.
#'
#' @return A montage render manifest with one row per selected metric,
#'   `parcel_values` in exact atlas-ID order, and, when requested, `stat_map`
#'   volumes in the atlas grid.
#'
#' @examples
#' \dontrun{
#' atlas <- neuroatlas::get_schaefer_atlas(parcels = "100", networks = "7")
#' roi_results <- data.frame(
#'   id = atlas$ids,
#'   z_stat = stats::rnorm(length(atlas$ids)),
#'   beta = stats::rnorm(length(atlas$ids)),
#'   standard_error = stats::runif(length(atlas$ids), 0.05, 0.3)
#' )
#' manifest <- parcel_render_manifest(
#'   roi_results,
#'   atlas,
#'   metrics = c("z_stat", "beta", "standard_error"),
#'   analysis_id = "faces-vs-houses"
#' )
#' }
#'
#' @export
parcel_render_manifest <- function(data,
                                   atlas,
                                   metrics = NULL,
                                   by = NULL,
                                   allow_partial = FALSE,
                                   analysis_id = NULL,
                                   analysis_label = NULL,
                                   primary = NULL,
                                   quantity = NULL,
                                   distribution = NULL,
                                   labels = NULL,
                                   units = NULL,
                                   threshold = NULL,
                                   df = NULL,
                                   p = NULL,
                                   tail = "two_sided",
                                   connectivity = "18-connect",
                                   min_cluster_size = 1L,
                                   background = NA_real_,
                                   include_volume = !inherits(atlas, "surfatlas"),
                                   overrides = NULL,
                                   validate = TRUE) {
  if (!inherits(atlas, "atlas")) {
    stop("'atlas' must inherit from class 'atlas'.", call. = FALSE)
  }
  if (!is.logical(allow_partial) || length(allow_partial) != 1L ||
      is.na(allow_partial)) {
    stop("'allow_partial' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_volume) || length(include_volume) != 1L ||
      is.na(include_volume)) {
    stop("'include_volume' must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(include_volume) && inherits(atlas, "surfatlas")) {
    stop(
      "'include_volume = TRUE' requires a volumetric atlas, not a surfatlas.",
      call. = FALSE
    )
  }

  table <- .parcel_render_table(data)
  metrics <- .parcel_render_metrics(table, atlas, metrics, by)
  inferred <- lapply(metrics, .infer_parcel_metric_semantics)
  inferred_quantity <- vapply(inferred, `[[`, character(1), "quantity")
  inferred_distribution <- vapply(
    inferred,
    function(x) x$distribution %||% NA_character_,
    character(1)
  )
  inferred_label <- vapply(inferred, `[[`, character(1), "label")
  inferred_units <- vapply(inferred, `[[`, character(1), "units")

  quantity <- .parcel_metric_parameter(
    quantity, metrics, inferred_quantity, "quantity"
  )
  distribution <- .parcel_metric_parameter(
    distribution, metrics, inferred_distribution, "distribution"
  )
  labels <- .parcel_metric_parameter(labels, metrics, inferred_label, "labels")
  units <- .parcel_metric_parameter(units, metrics, inferred_units, "units")
  tail <- .parcel_metric_parameter(tail, metrics, NULL, "tail")
  connectivity <- .parcel_metric_parameter(
    connectivity, metrics, NULL, "connectivity"
  )
  min_cluster_size <- .parcel_metric_parameter(
    min_cluster_size, metrics, NULL, "min_cluster_size"
  )

  analysis_id <- analysis_id %||% .parcel_render_analysis_id(atlas)
  .parcel_render_scalar_character(analysis_id, "analysis_id")
  analysis_id <- trimws(analysis_id)
  analysis_label <- analysis_label %||% .parcel_render_analysis_label(atlas)
  .parcel_render_scalar_character(analysis_label, "analysis_label")
  analysis_label <- trimws(analysis_label)

  primary <- .parcel_render_primary(primary, metrics, quantity)
  map_ids <- paste0(
    .safe_file_stem(tolower(analysis_id)), "_",
    vapply(metrics, function(x) .safe_file_stem(tolower(x)), character(1))
  )
  if (anyDuplicated(map_ids)) {
    stop(
      "Metric names produce duplicate map_id values after sanitization.",
      call. = FALSE
    )
  }

  parcel_values <- lapply(metrics, function(metric) {
    .neuroatlas_align_parcel_values(
      atlas = atlas,
      data = data,
      value = metric,
      by = by,
      allow_partial = allow_partial
    )
  })

  out <- data.frame(
    analysis_id = rep(analysis_id, length(metrics)),
    analysis_label = rep(analysis_label, length(metrics)),
    map_id = map_ids,
    parcel_metric = metrics,
    role = ifelse(metrics == primary, "primary", "auxiliary"),
    quantity = as.character(quantity),
    distribution = as.character(distribution),
    label = as.character(labels),
    selector_label = as.character(labels),
    units = as.character(units),
    display_order = seq_along(metrics),
    tail = tail,
    connectivity = connectivity,
    min_cluster_size = min_cluster_size,
    stringsAsFactors = FALSE
  )
  out$parcel_values <- I(parcel_values)

  if (isTRUE(include_volume)) {
    out$stat_map <- I(lapply(metrics, function(metric) {
      .neuroatlas_parcel_volume(
        atlas = atlas,
        data = data,
        value = metric,
        by = by,
        allow_partial = allow_partial,
        background = background
      )
    }))
  }

  optional <- list(threshold = threshold, df = df, p = p)
  for (field in names(optional)) {
    if (!is.null(optional[[field]])) {
      out[[field]] <- .parcel_metric_parameter(
        optional[[field]], metrics, NULL, field
      )
    }
  }

  out <- .apply_manifest_overrides(out, overrides)
  if (isTRUE(validate)) {
    out <- validate_manifest(
      out,
      check_files = FALSE,
      check_overlays = FALSE
    )
  }
  out
}

.parcel_render_table <- function(data) {
  table <- if (inherits(data, "parcel_data")) data$parcels else data
  if (!is.data.frame(table)) {
    stop("'data' must be a data frame, tibble, or parcel_data object.",
         call. = FALSE)
  }
  if (nrow(table) == 0L) {
    stop("'data' must contain at least one parcel row.", call. = FALSE)
  }
  as.data.frame(table, stringsAsFactors = FALSE)
}

.parcel_render_metrics <- function(table, atlas, metrics, by) {
  if (is.null(metrics)) {
    metadata <- tryCatch(
      names(neuroatlas::roi_metadata(atlas)),
      error = function(e) character()
    )
    join_columns <- if (is.null(by)) character() else unname(as.character(by))
    candidates <- setdiff(names(table), unique(c(metadata, join_columns)))
    metrics <- candidates[vapply(table[candidates], function(x) {
      (is.numeric(x) || is.integer(x)) && is.null(dim(x))
    }, logical(1))]
  }
  if (!is.character(metrics) || length(metrics) == 0L ||
      anyNA(metrics) || any(!nzchar(trimws(metrics)))) {
    stop(
      "'metrics' must name at least one numeric parcel-value column.",
      call. = FALSE
    )
  }
  metrics <- trimws(metrics)
  if (anyDuplicated(metrics)) {
    stop("'metrics' must contain unique column names.", call. = FALSE)
  }
  missing <- setdiff(metrics, names(table))
  if (length(missing) > 0L) {
    stop(
      "Metric column(s) not found in 'data': ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  metrics
}

.infer_parcel_metric_semantics <- function(metric) {
  normalized <- gsub("[^a-z0-9]+", "_", tolower(metric))
  normalized <- gsub("^_+|_+$", "", normalized)
  compact <- gsub("_", "", normalized, fixed = TRUE)

  if (compact %in% c("z", "zstat", "zstatistic", "zscore")) {
    return(list(
      quantity = "test_statistic", distribution = "z",
      label = "Z statistic", units = "z"
    ))
  }
  if (compact %in% c("t", "tstat", "tstatistic", "tscore")) {
    return(list(
      quantity = "test_statistic", distribution = "t",
      label = "T statistic", units = "t"
    ))
  }
  if (compact %in% c(
    "beta", "effect", "effectestimate", "estimate", "coefficient",
    "coef"
  )) {
    return(list(
      quantity = "estimate", distribution = NULL,
      label = if (compact == "beta") "Beta estimate" else "Effect estimate",
      units = "estimate"
    ))
  }
  if (compact %in% c("se", "stderr", "standarderror", "standarderr")) {
    return(list(
      quantity = "standard_error", distribution = NULL,
      label = "Standard error", units = "SE"
    ))
  }
  if (compact %in% c("variance", "var")) {
    return(list(
      quantity = "variance", distribution = NULL,
      label = "Variance", units = "variance"
    ))
  }

  aliases <- c(
    probability = "probability",
    prob = "probability",
    proportion = "proportion",
    prop = "proportion",
    rsquared = "r_squared",
    r2 = "r_squared"
  )
  if (compact %in% names(aliases)) {
    quantity <- unname(aliases[[compact]])
    return(list(
      quantity = quantity,
      distribution = NULL,
      label = .parcel_render_pretty_metric(metric),
      units = quantity
    ))
  }

  list(
    quantity = paste0("parcel:", .safe_file_stem(normalized)),
    distribution = NULL,
    label = .parcel_render_pretty_metric(metric),
    units = metric
  )
}

.parcel_render_pretty_metric <- function(metric) {
  words <- gsub("[_.-]+", " ", trimws(metric))
  tools::toTitleCase(words)
}

.parcel_metric_parameter <- function(value, metrics, default, label) {
  if (is.null(value)) {
    if (is.null(default)) return(NULL)
    return(default)
  }
  if (is.list(value) && !is.data.frame(value)) {
    value <- unlist(value, recursive = FALSE, use.names = TRUE)
  }
  if (length(value) == 1L && is.null(names(value))) {
    return(rep(value, length(metrics)))
  }
  if (!is.null(names(value)) && any(nzchar(names(value)))) {
    if (anyDuplicated(names(value))) {
      stop("Named '", label, "' values must have unique metric names.",
           call. = FALSE)
    }
    missing <- setdiff(metrics, names(value))
    unknown <- setdiff(names(value), metrics)
    if (length(missing) > 0L || length(unknown) > 0L) {
      stop(
        "Named '", label, "' values must match 'metrics' exactly.",
        call. = FALSE
      )
    }
    return(unname(value[metrics]))
  }
  if (length(value) != length(metrics)) {
    stop(
      "'", label, "' must have length 1 or length(metrics).",
      call. = FALSE
    )
  }
  unname(value)
}

.parcel_render_primary <- function(primary, metrics, quantity) {
  if (is.null(primary)) {
    test_rows <- which(quantity == "test_statistic")
    return(metrics[if (length(test_rows) > 0L) test_rows[[1L]] else 1L])
  }
  .parcel_render_scalar_character(primary, "primary")
  if (!primary %in% metrics) {
    stop("'primary' must name one of the selected metrics.", call. = FALSE)
  }
  primary
}

.parcel_render_analysis_id <- function(atlas) {
  atlas_name <- .parcel_render_atlas_name(atlas, "atlas")
  paste0(.safe_file_stem(tolower(atlas_name)), "_parcels")
}

.parcel_render_analysis_label <- function(atlas) {
  paste0(.parcel_render_atlas_name(atlas, "Atlas"), " parcel statistics")
}

.parcel_render_atlas_name <- function(atlas, fallback) {
  atlas_name <- atlas$name
  if (is.null(atlas_name) || length(atlas_name) != 1L ||
      is.na(atlas_name) || !nzchar(trimws(as.character(atlas_name)))) {
    atlas_name <- class(atlas)[[1L]] %||% fallback
  }
  if (is.null(atlas_name) || length(atlas_name) != 1L ||
      is.na(atlas_name) || !nzchar(trimws(as.character(atlas_name)))) {
    atlas_name <- fallback
  }
  trimws(as.character(atlas_name))
}

.parcel_render_scalar_character <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    stop("'", label, "' must be a single non-empty string.", call. = FALSE)
  }
  invisible(TRUE)
}

.neuroatlas_align_parcel_values <- function(atlas,
                                             data,
                                             value,
                                             by,
                                             allow_partial) {
  do.call(
    neuroatlas::align_parcel_values,
    list(
      atlas = atlas,
      data = data,
      value = value,
      by = by,
      allow_partial = allow_partial
    )
  )
}

.neuroatlas_parcel_volume <- function(atlas,
                                      data,
                                      value,
                                      by,
                                      allow_partial,
                                      background) {
  if (!"parcel_volume" %in% getNamespaceExports("neuroatlas")) {
    stop(
      "Volumetric parcel reports require a neuroatlas version exporting ",
      "parcel_volume(). Update neuroatlas or set include_volume = FALSE.",
      call. = FALSE
    )
  }
  do.call(
    getExportedValue("neuroatlas", "parcel_volume"),
    list(
      atlas = atlas,
      data = data,
      value = value,
      by = by,
      allow_partial = allow_partial,
      background = background
    )
  )
}
