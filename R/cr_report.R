#' Generate a Cluster Report
#'
#' End-to-end pipeline: threshold a stat map, find clusters, annotate peaks with
#' MNI coordinates and atlas labels, extract design-aware time courses, generate
#' formula-driven plots and formatted tables, and render a PDF or HTML report.
#'
#' @param stat_map A `NeuroVol` (or file path to a NIfTI) containing the
#'   statistical map to threshold.
#' @param data_source Optional backing data for time-course extraction.
#'   Supported types:
#'   - A 4-D `NeuroVec` (one volume per sample/beta)
#'   - A `list` of 3-D `NeuroVol` objects (one per sample)
#'   - An `nftab` object (neurotabs)
#'   - `NULL`, which produces a table-only cluster report from `stat_map`
#'     without sample-level plots
#' @param atlas A neuroatlas `atlas` object (e.g.,
#'   `neuroatlas::get_schaefer_atlas(200, 7)`).
#' @param threshold Numeric threshold for cluster formation.
#' @param formulas Named list of formulas for time-course plots. Names become
#'   section headings when `data_source` is supplied. Example:
#'   `list("Main Effect" = value ~ condition * time)`.
#' @param design Optional `data.frame` with one row per sample and columns for
#'   the design variables referenced in `formulas`. Required when `data_source`
#'   is a `NeuroVec` or list; ignored for `nftab` (uses `nf_design()`); unused
#'   when `data_source = NULL`.
#' @param min_cluster_size Minimum voxels per cluster. Default 10.
#' @param connectivity Voxel connectivity for clustering. Default `"18-connect"`.
#' @param tail Tail mode: `"two_sided"`, `"positive"`, or `"negative"`.
#' @param max_clusters Maximum clusters in the report. Default 20.
#' @param output_file Path for the rendered report (`.pdf`, `.html`, or
#'   `.qmd`). Set to `NULL` to skip rendering and return data only. `.qmd`
#'   writes a Quarto source document plus a companion `.rds` data bundle.
#' @param template Path to a custom Rmd/Qmd template. `NULL` uses the bundled
#'   template matching `output_file`.
#' @param table_style Table backend: `"gt"`, `"flextable"`, or `"kable"`.
#' @param ci_level Confidence interval for time-course ribbons. Default 0.95.
#' @param palette Color palette for condition lines.
#' @param brain_slices Logical; generate orthographic brain slice images per
#'   cluster? Default `TRUE`.
#' @param brain_cmap Color map for brain slice images. Default `"inferno"`.
#' @param quiet Suppress rendering messages? Default `TRUE`.
#' @param ... Additional arguments passed to [build_cluster_explorer_data()].
#'
#' @return Invisibly, a `cluster_report_result` object (an S3 class that
#'   supports `print()`, `summary()`, `plot()`, and `export_csv()`). Components
#'   accessible via `$`:
#' \describe{
#'   \item{cluster_table}{Enriched cluster table with MNI coords and labels.}
#'   \item{cluster_parcels}{Cluster-parcel overlap tibble.}
#'   \item{time_courses}{Extracted time-course tibble.}
#'   \item{plots}{Named list (by formula name) of named lists (by cluster_id)
#'     of ggplot objects.}
#'   \item{brain_slices}{Named list of orthographic brain slice plots per
#'     cluster, or `NULL` if `brain_slices = FALSE`.}
#'   \item{mni_table}{Formatted gt/flextable/kable object.}
#'   \item{report_path}{Path to rendered report, or `NULL` if skipped.}
#' }
#'
#' @examples
#' \dontrun{
#' library(neuroatlas)
#' atlas <- get_schaefer_atlas(200, 7)
#'
#' cluster_report(
#'   stat_map = "zstat1.nii.gz",
#'   data_source = my_4d_betas,
#'   atlas = atlas,
#'   threshold = 3.1,
#'   formulas = list(
#'     "Condition x Time" = value ~ condition * time
#'   ),
#'   design = my_design,
#'   output_file = "results/cluster_report.pdf"
#' )
#'
#' # Table-only cluster report from a single stat map
#' cluster_report(
#'   stat_map = "zstat1.nii.gz",
#'   atlas = atlas,
#'   threshold = 3.1,
#'   data_source = NULL,
#'   output_file = "results/cluster_table.html"
#' )
#' }
#' @export
cluster_report <- function(stat_map,
                           data_source = NULL,
                           atlas = neuroatlas::get_schaefer_atlas(200, 7),
                           threshold = 3.5,
                           formulas = list(value ~ time),
                           design = NULL,
                           min_cluster_size = 10L,
                           connectivity = c("18-connect", "26-connect",
                                            "6-connect"),
                           tail = c("two_sided", "positive", "negative"),
                           max_clusters = 20L,
                           output_file = "cluster_report.pdf",
                           template = NULL,
                           table_style = c("gt", "flextable", "kable"),
                           ci_level = 0.95,
                           palette = NULL,
                           brain_slices = TRUE,
                           brain_cmap = "inferno",
                           quiet = TRUE,
                           ...) {
  connectivity <- match.arg(connectivity)
  tail <- match.arg(tail)
  table_style <- match.arg(table_style)
  has_data_source <- !is.null(data_source)

  # Input validation
  if (is.character(stat_map)) {
    stat_map <- neuroim2::read_vol(stat_map)
  }
  assertthat::assert_that(
    methods::is(stat_map, "NeuroVol"),
    msg = "'stat_map' must be a NeuroVol or a path to a NIfTI file."
  )
  assertthat::assert_that(
    is.numeric(threshold) && length(threshold) == 1 && threshold > 0,
    msg = "'threshold' must be a positive number."
  )
  assertthat::assert_that(
    ci_level > 0 && ci_level < 1,
    msg = "'ci_level' must be between 0 and 1."
  )

  if (!has_data_source) {
    if (!missing(formulas) && length(formulas) > 0) {
      warning(
        "Ignoring `formulas` because `data_source` is NULL.",
        call. = FALSE
      )
    }
    formulas <- list()
  } else {
    # Normalise formulas to a named list
    if (inherits(formulas, "formula")) {
      formulas <- list(formulas)
    }
    assertthat::assert_that(
      is.list(formulas) &&
        all(vapply(formulas, inherits, logical(1), "formula")),
      msg = "'formulas' must be a formula or list of formulas."
    )
    if (is.null(names(formulas))) {
      names(formulas) <- vapply(formulas, function(f) {
        paste(deparse(f), collapse = " ")
      }, character(1))
    }
  }

  # Resolve design table when sample-wise data is available
  sample_table <- if (has_data_source) .resolve_design(data_source, design) else NULL
  if (has_data_source && is.null(sample_table)) {
    stop(
      "A 'design' data.frame is required for non-'nftab' data sources ",
      "in cluster_report().",
      call. = FALSE
    )
  }

  # Step 1: Cluster extraction + optional time-series
  cluster_data <- if (has_data_source) {
    build_cluster_explorer_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      connectivity = connectivity,
      tail = tail,
      prefetch = TRUE,
      ...
    )
  } else {
    .build_cluster_report_table_data(
      atlas = atlas,
      stat_map = stat_map,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      connectivity = connectivity,
      tail = tail
    )
  }

  # Step 2: Enrich with MNI coords + atlas labels
  enriched <- enrich_cluster_table(
    cluster_table = cluster_data$cluster_table,
    stat_map = stat_map,
    atlas = atlas,
    max_clusters = max_clusters
  )

  # Filter to kept clusters
  kept_ids <- enriched$cluster_id
  cluster_parcels <- cluster_data$cluster_parcels
  if (nrow(cluster_parcels) > 0) {
    cluster_parcels <- cluster_parcels[
      cluster_parcels$cluster_id %in% kept_ids, ]
  }

  # Step 3: Reuse prefetched time-series (avoid double-extraction)
  tc_data <- cluster_data$cluster_ts
  if ("cluster_id" %in% names(tc_data)) {
    tc_data <- tc_data[tc_data$cluster_id %in% kept_ids, , drop = FALSE]
  }
  if ("signal" %in% names(tc_data) && !"value" %in% names(tc_data)) {
    tc_data <- dplyr::rename(tc_data, value = "signal")
  }
  if (!is.null(sample_table) && ".sample_index" %in% names(tc_data)) {
    tc_data <- .merge_design(tc_data, sample_table)
  }

  # Step 3b: Compute per-cluster mean/SD signal for the enriched table
  if (nrow(tc_data) > 0) {
    signal_stats <- tc_data |>
      dplyr::group_by(.data$cluster_id) |>
      dplyr::summarise(
        mean_signal = mean(.data$value, na.rm = TRUE),
        sd_signal   = stats::sd(.data$value, na.rm = TRUE),
        .groups = "drop"
      )
    enriched <- dplyr::left_join(enriched, signal_stats, by = "cluster_id")
  } else {
    enriched$mean_signal <- NA_real_
    enriched$sd_signal   <- NA_real_
  }

  # Step 4: Generate plots per formula per cluster
  all_plots <- list()
  if (length(formulas) > 0 && nrow(tc_data) > 0) {
    for (fname in names(formulas)) {
      all_plots[[fname]] <- plot_all_clusters(
        tc_data = tc_data,
        formula = formulas[[fname]],
        cluster_table = enriched,
        ci_level = ci_level,
        palette = palette
      )
    }
  }

  # Step 4b: Brain slice images per cluster
  slice_plots <- NULL
  if (isTRUE(brain_slices) && nrow(enriched) > 0) {
    slice_plots <- plot_all_cluster_slices(
      stat_map      = stat_map,
      cluster_table = enriched,
      cmap          = brain_cmap
    )
  }

  # Step 5: Format MNI table
  mni_table <- format_mni_table(
    cluster_table = enriched,
    cluster_parcels = cluster_parcels,
    style = table_style
  )

  # Step 6: Assemble and render
  atlas_name <- tryCatch(atlas$name, error = function(e) "Unknown atlas")

  report_data <- list(
    cluster_table = enriched,
    cluster_parcels = cluster_parcels,
    mni_table = mni_table,
    plots = all_plots,
    brain_slices = slice_plots,
    params = list(
      threshold = threshold,
      tail = tail,
      min_cluster_size = min_cluster_size,
      max_clusters = max_clusters,
      atlas_name = atlas_name,
      formulas = formulas,
      table_style = table_style,
      report_mode = if (has_data_source) "full" else "table_only"
    )
  )

  report_path <- NULL
  if (!is.null(output_file)) {
    report_path <- render_cluster_report(
      report_data = report_data,
      output_file = output_file,
      template = template,
      quiet = quiet
    )
  }

  result <- new_cluster_report_result(
    cluster_table   = enriched,
    cluster_parcels = cluster_parcels,
    time_courses    = tc_data,
    plots           = all_plots,
    brain_slices    = slice_plots,
    mni_table       = mni_table,
    report_path     = report_path,
    params          = report_data$params
  )
  invisible(result)
}

# Resolve design from data_source or explicit argument
.resolve_design <- function(data_source, design) {
  if (!is.null(design)) return(design)
  if (inherits(data_source, "nftab") &&
      requireNamespace("neurotabs", quietly = TRUE)) {
    return(neurotabs::nf_design(data_source))
  }
  NULL
}

.build_cluster_report_table_data <- function(atlas,
                                             stat_map,
                                             threshold,
                                             min_cluster_size,
                                             connectivity,
                                             tail) {
  aligned <- .harmonize_cluster_explorer_atlas(atlas, stat_map)
  atlas <- aligned$atlas
  if (!is.null(aligned$message)) {
    message(aligned$message)
  }
  if (!is.null(aligned$warning)) {
    warning(aligned$warning, call. = FALSE)
  }

  stat_arr <- as.array(stat_map)
  comp <- .extract_stat_clusters(
    stat_map = stat_map,
    stat_arr = stat_arr,
    threshold = threshold,
    min_cluster_size = min_cluster_size,
    connectivity = connectivity,
    tail = tail
  )
  ann <- .annotate_clusters_with_atlas(
    cluster_table = comp$cluster_table,
    cluster_voxels = comp$cluster_voxels,
    atlas = atlas,
    stat_arr = stat_arr
  )

  list(
    cluster_table = ann$cluster_table,
    cluster_parcels = ann$cluster_parcels,
    cluster_ts = tibble::tibble(),
    cluster_voxels = comp$cluster_voxels,
    cluster_index = comp$cluster_index,
    sample_table = NULL,
    prefetch_info = list(
      requested = FALSE,
      applied = FALSE,
      n_clusters = nrow(ann$cluster_table),
      total_voxels = if (nrow(ann$cluster_table) > 0) {
        sum(ann$cluster_table$n_voxels)
      } else {
        0
      },
      max_clusters = 0,
      max_voxels = 0
    )
  )
}
