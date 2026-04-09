#' Create Example Inputs for cluster_report()
#'
#' Builds a small synthetic dataset suitable for demonstrating the
#' `cluster_report()` pipeline. The stat map has one positive and one negative
#' cluster; the 4-D data volume has a condition-by-time signal injected into
#' the positive region.
#'
#' @return A named list with components:
#' \describe{
#'   \item{stat_map}{A `NeuroVol` (10 x 10 x 10) with synthetic z-statistics.}
#'   \item{data_source}{A `NeuroVec` (10 x 10 x 10 x 12) with 12 sample
#'     volumes.}
#'   \item{atlas}{A toy `atlas` object with 2 parcels (`FrontalA`,
#'     `ParietalB`).}
#'   \item{design}{A `data.frame` with 12 rows: `condition` (face / scene) and
#'     `time` (1--6).}
#' }
#'
#' @examples
#' inputs <- example_cluster_inputs()
#' names(inputs)
#' inputs$design
#'
#' @export
example_cluster_inputs <- function() {
  dims <- c(10L, 10L, 10L)
  sp <- neuroim2::NeuroSpace(
    dim = dims, spacing = c(2, 2, 2), origin = c(-10, -20, -15)
  )

  # Stat map: positive cluster in one corner, negative in the other
  stat_arr <- array(stats::rnorm(prod(dims), sd = 0.5), dim = dims)
  stat_arr[2:4, 2:4, 2:4] <-  5.0
  stat_arr[7:9, 7:9, 7:9] <- -4.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = sp)

  # Atlas with two labelled regions
  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:5, 1:5, 1:5]    <- 1L
  atlas_arr[6:10, 6:10, 6:10] <- 2L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp)

  atlas <- list(
    name        = "toy_atlas",
    atlas       = atlas_vol,
    ids         = c(1L, 2L),
    labels      = c("FrontalA", "ParietalB"),
    orig_labels = c("FrontalA", "ParietalB"),
    hemi        = c("left", "right"),
    roi_metadata = tibble::tibble(
      id    = c(1L, 2L),
      label = c("FrontalA", "ParietalB"),
      hemi  = c("left", "right")
    )
  )
  class(atlas) <- c("toy", "atlas")

  # Design: 2 conditions x 6 time points = 12 images
  n_images <- 12L
  design <- data.frame(
    condition = rep(c("face", "scene"), each = 6),
    time      = rep(1:6, times = 2)
  )

  # 4-D data with a condition x time signal in the positive cluster
  set.seed(42)
  sp4 <- neuroim2::NeuroSpace(dim = c(dims, n_images), spacing = c(2, 2, 2))
  data_arr <- array(
    stats::rnorm(prod(dims) * n_images, sd = 0.3),
    dim = c(dims, n_images)
  )
  for (t in seq_len(n_images)) {
    cond_effect <- ifelse(design$condition[t] == "face", 1.5, 0.5)
    time_effect <- design$time[t] * 0.3
    data_arr[2:4, 2:4, 2:4, t] <- cond_effect + time_effect +
      stats::rnorm(27, sd = 0.1)
  }
  data_source <- neuroim2::NeuroVec(data_arr, sp4)

  list(
    stat_map    = stat_map,
    data_source = data_source,
    atlas       = atlas,
    design      = design
  )
}
