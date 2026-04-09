# neurotabs adapter for neuromosaic
# Bridges nftab datasets to build_cluster_explorer_data() / cluster_explorer().

# -- Validation ---------------------------------------------------------------

.nf_validate <- function(ds, stat_map, data_feature, dots) {
  if (!requireNamespace("neurotabs", quietly = TRUE))
    stop("Package 'neurotabs' is required. Install with: ",
         "remotes::install_github('bbuchsbaum/neurotabs')", call. = FALSE)
  if (!inherits(ds, "nftab"))
    stop("'ds' must be an nftab object.", call. = FALSE)
  if (nrow(ds$observations) == 0L)
    stop("'ds' contains no observations.", call. = FALSE)
  if (!is.character(data_feature) || length(data_feature) != 1L ||
      !nzchar(data_feature))
    stop("'data_feature' must be a single non-empty string.", call. = FALSE)
  if (!data_feature %in% neurotabs::nf_feature_names(ds))
    stop("Feature '", data_feature, "' not found in ds. ",
         "Available features: ",
         paste(neurotabs::nf_feature_names(ds), collapse = ", "),
         call. = FALSE)
  if (!methods::is(stat_map, "NeuroVol"))
    stop("'stat_map' must be a NeuroVol.", call. = FALSE)
  reserved <- intersect(names(dots), c("data_source", "sample_table", "series_fun"))
  if (length(reserved))
    stop("Do not pass ", paste(reserved, collapse = ", "),
         " via '...' — the adapter manages these arguments.", call. = FALSE)
}

# -- Strategy helpers ---------------------------------------------------------

#' @keywords internal
.nf_collect_to_neurovec <- function(ds, data_feature, stat_map) {
  result <- neurotabs::nf_collect_array(ds, data_feature)
  # result$data  — plain 4D R array [x,y,z,n_obs]; neurotabs uses
  #                array(as.vector(vol), dim=dim(vol)) internally so
  #                there are no S4 dispatch surprises.
  # result$space — NeuroSpace from the first resolved volume.

  d3      <- as.integer(dim(result$data))[1:3]
  sm_dims <- as.integer(dim(stat_map))[1:3]
  if (!identical(d3, sm_dims))
    stop("Feature '", data_feature, "' dimensions (",
         paste(d3, collapse = "x"), ") do not match stat_map (",
         paste(sm_dims, collapse = "x"), ").", call. = FALSE)

  n_obs <- dim(result$data)[4L]
  sp4   <- neuroim2::NeuroSpace(
    dim     = c(as.integer(dim(result$space)), n_obs),
    spacing = neuroim2::spacing(result$space),
    origin  = neuroim2::origin(result$space)
  )
  neuroim2::NeuroVec(result$data, sp4)
}

#' @keywords internal
.nf_make_series_fun <- function(data_feature) {
  feature_name <- data_feature
  function(data_source, voxel_coords) {
    # data_source is the nftab object; nf_sample returns [n_obs x n_coords]
    # with coord_type = "voxel" (1-based, matches neuromosaic convention)
    neurotabs::nf_sample(data_source, feature_name,
                         coords     = voxel_coords,
                         coord_type = "voxel")
  }
}

# -- Public API ---------------------------------------------------------------

#' Build Cluster Explorer Data from an NFTab Dataset
#'
#' Adapter that accepts an \code{\link[neurotabs]{nftab}} object and passes
#' it to \code{\link{build_cluster_explorer_data}}, deriving
#' \code{data_source} and \code{sample_table} automatically from \code{ds}.
#'
#' @param ds An \code{nftab} object.
#' @param stat_map A \code{\link[neuroim2]{NeuroVol}} representing the
#'   group-level statistic map.  Must be provided separately — it is not
#'   stored per-row in the NFTab.
#' @param data_feature Character.  Name of the per-row volumetric feature in
#'   \code{ds} to use as the sample-wise data source (e.g. \code{"bold"},
#'   \code{"beta"}).
#' @param atlas Atlas object for cluster annotation (see
#'   \code{\link{build_cluster_explorer_data}}).
#' @param strategy \code{"collect"} (default) eagerly loads all feature
#'   volumes and stacks them into a \code{NeuroVec} via
#'   \code{\link[neurotabs]{nf_collect_array}}.  \code{"lazy"} resolves
#'   each volume on demand via \code{\link[neurotabs]{nf_sample}}, avoiding
#'   upfront memory allocation.
#' @param sample_tbl Optional \code{data.frame} overriding the design table.
#'   Defaults to \code{\link[neurotabs]{nf_design}(ds)}.
#' @param ... Additional arguments passed to
#'   \code{\link{build_cluster_explorer_data}}.
#'
#' @return A list as returned by \code{\link{build_cluster_explorer_data}}.
#' @seealso \code{\link{nf_cluster_explorer}},
#'   \code{\link{build_cluster_explorer_data}}
#' @export
nf_cluster_data <- function(ds, stat_map, data_feature, atlas,
                             strategy   = c("collect", "lazy"),
                             sample_tbl = NULL,
                             ...) {
  .nf_validate(ds, stat_map, data_feature, dots = list(...))
  strategy   <- match.arg(strategy)
  sample_tbl <- if (is.null(sample_tbl)) neurotabs::nf_design(ds) else sample_tbl

  if (identical(strategy, "collect")) {
    data_source <- .nf_collect_to_neurovec(ds, data_feature, stat_map)
    build_cluster_explorer_data(
      data_source  = data_source,
      atlas        = atlas,
      stat_map     = stat_map,
      sample_table = sample_tbl,
      ...
    )
  } else {
    series_fn <- .nf_make_series_fun(data_feature)
    build_cluster_explorer_data(
      data_source  = ds,
      atlas        = atlas,
      stat_map     = stat_map,
      sample_table = sample_tbl,
      series_fun   = series_fn,
      ...
    )
  }
}

#' Launch Cluster Explorer from an NFTab Dataset
#'
#' Adapter that accepts an \code{\link[neurotabs]{nftab}} object and passes
#' it to \code{\link{cluster_explorer}}, deriving \code{data_source} and
#' \code{sample_table} automatically from \code{ds}.
#'
#' @inheritParams nf_cluster_data
#' @param surfatlas Surface atlas object passed to
#'   \code{\link{cluster_explorer}}.
#' @param ... Additional arguments passed to \code{\link{cluster_explorer}},
#'   including \code{analysis_plugins}, \code{plot_plugins},
#'   \code{default_analysis_plugin}, and \code{default_plot_plugin}.
#'
#' @return A \code{shiny.appobj}.
#' @seealso \code{\link{nf_cluster_data}},
#'   \code{\link{cluster_explorer}}
#' @export
nf_cluster_explorer <- function(ds, stat_map, data_feature, atlas, surfatlas,
                                 strategy   = c("collect", "lazy"),
                                 sample_tbl = NULL,
                                 ...) {
  .nf_validate(ds, stat_map, data_feature, dots = list(...))
  strategy   <- match.arg(strategy)
  sample_tbl <- if (is.null(sample_tbl)) neurotabs::nf_design(ds) else sample_tbl

  if (identical(strategy, "collect")) {
    data_source <- .nf_collect_to_neurovec(ds, data_feature, stat_map)
    cluster_explorer(
      data_source = data_source,
      stat_map    = stat_map,
      atlas       = atlas,
      surfatlas   = surfatlas,
      sample_table = sample_tbl,
      ...
    )
  } else {
    series_fn <- .nf_make_series_fun(data_feature)
    cluster_explorer(
      data_source = ds,
      stat_map    = stat_map,
      atlas       = atlas,
      surfatlas   = surfatlas,
      sample_table = sample_tbl,
      series_fun  = series_fn,
      ...
    )
  }
}
