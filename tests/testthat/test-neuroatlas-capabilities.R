# neuromosaic uses newer neuroatlas features when the installed neuroatlas
# has them (nearest-label query_point(), exported project_cluster_overlay(),
# network metadata on merged atlases) and its own implementation otherwise.
# Both paths must give the same answers.

na_toy_atlas <- function() {
  sp <- neuroim2::NeuroSpace(c(12L, 12L, 10L), spacing = c(2, 2, 2),
                             origin = c(-11, -11, -9))
  arr <- array(0L, dim = dim(sp))
  arr[2:4, 2:5, 2:4] <- 3L
  arr[8:10, 2:4, 2:4] <- 1L
  # Two regions with a one-voxel gap: points in the gap are equidistant.
  arr[2:4, 8:9, 6:8] <- 7L
  arr[6:8, 8:9, 6:8] <- 5L
  arr[10:11, 10:11, 8:9] <- 2L
  atlas <- list(
    name = "toy",
    atlas = neuroim2::NeuroVol(arr, space = sp),
    ids = c(1L, 2L, 3L, 5L, 7L),
    labels = c("one", "two", "three", "five", "seven"),
    hemi = c("left", "right", "left", "right", "left")
  )
  class(atlas) <- c("toy", "atlas")
  atlas
}

na_toy_coords <- function(atlas) {
  sp <- neuroim2::space(atlas$atlas)
  set.seed(20261003)
  grid <- cbind(stats::runif(300, 0, 13), stats::runif(300, 0, 13),
                stats::runif(300, 0, 11))
  random <- neuroim2::grid_to_coord(sp, grid)
  # Voxel centres in the gap between regions 7 and 5 (exact ties), points
  # inside regions, outside the grid, and non-finite.
  gap <- neuroim2::grid_to_coord(sp, rbind(c(5, 8, 7), c(5, 9, 6)))
  inside <- neuroim2::grid_to_coord(sp, rbind(c(3, 3, 3), c(9, 3, 3)))
  rbind(random, gap, inside, c(80, 80, 80), c(NA, 0, 0))
}

test_that("the nearest-label fallback takes the containing voxel, then the closest, ties to the smallest id", {
  atlas <- na_toy_atlas()
  sp <- neuroim2::space(atlas$atlas)
  gap <- neuroim2::grid_to_coord(sp, rbind(c(5, 8, 7)))
  inside <- neuroim2::grid_to_coord(sp, rbind(c(3, 3, 3)))
  far <- c(80, 80, 80)
  out <- neuromosaic:::.montage_nearest_labels(
    atlas$atlas, rbind(gap, inside, far), radius = 4
  )
  # Voxel (5, 8, 7) is 2 mm from region 7 at x = 4 and from region 5 at x = 6.
  expect_identical(out$id, c(5L, 3L, NA))
  expect_identical(out$distance, c(2, 0, NA))
  # keep filters candidate labels (the cerebellar reference path).
  kept <- neuromosaic:::.montage_nearest_labels(
    atlas$atlas, rbind(gap), radius = 4, keep = 7L
  )
  expect_identical(kept$id, 7L)
})

test_that("query_point(nearest = TRUE) and the fallback label peaks identically", {
  skip_if_not_installed("neuroatlas")
  skip_if_not(neuromosaic:::.montage_query_point_has_nearest(),
              "installed neuroatlas has no query_point(nearest = TRUE)")
  atlas <- na_toy_atlas()
  coords <- na_toy_coords(atlas)
  for (radius in c(0, 2, 4, 6.5)) {
    upstream <- neuromosaic:::.montage_atlas_nearest_labels(atlas, coords, radius)
    local <- neuromosaic:::.montage_nearest_labels(atlas$atlas, coords, radius)
    expect_identical(upstream, local, info = paste("radius", radius))
  }
  # Exercise both outcomes, so the comparison is not vacuous.
  out <- neuromosaic:::.montage_atlas_nearest_labels(atlas, coords, 4)
  expect_true(any(out$distance == 0, na.rm = TRUE))
  expect_true(any(out$distance > 0, na.rm = TRUE))
  expect_true(anyNA(out$id))
})

test_that("nearest labels fall back to neuromosaic's search without the neuroatlas mode", {
  atlas <- na_toy_atlas()
  coords <- na_toy_coords(atlas)
  testthat::local_mocked_bindings(.montage_query_point_has_nearest = function() FALSE)
  expect_identical(
    neuromosaic:::.montage_atlas_nearest_labels(atlas, coords, 4),
    neuromosaic:::.montage_nearest_labels(atlas$atlas, coords, 4)
  )
})

test_that("atlas network metadata is used as is; names are parsed only without it", {
  tbl <- data.frame(peak_mni_x = 0, peak_mni_y = 0, peak_mni_z = 0,
                    network = NA_character_, stringsAsFactors = FALSE)
  networked <- list(ids = 1:2, labels = c("a", "b"), network = c("Vis", NA))
  expect_true(neuromosaic:::.montage_atlas_has_networks(networked))
  testthat::local_mocked_bindings(
    query_point = function(...) stop("not queried again"),
    .package = "neuroatlas"
  )
  expect_identical(
    neuromosaic:::.montage_add_atlas_networks(tbl, networked)$network,
    NA_character_
  )
  expect_false(neuromosaic:::.montage_atlas_has_networks(
    list(ids = 1:2, labels = c("a", "b"))
  ))
  expect_false(neuromosaic:::.montage_atlas_has_networks(
    list(ids = 1:2, network = c(NA, ""))
  ))
})

test_that("the surface projector is neuroatlas's exported one, or none", {
  skip_if_not_installed("neuroatlas")
  projector <- neuromosaic:::.nm_surface_projector()
  if ("project_cluster_overlay" %in% getNamespaceExports("neuroatlas")) {
    expect_identical(projector, getExportedValue("neuroatlas", "project_cluster_overlay"))
  } else {
    expect_null(projector)
  }
  testthat::local_mocked_bindings(.nm_surface_projector = function() NULL)
  expect_null(neuromosaic:::.nm_project_surface_overlay(
    NULL, list(), "avg", "thickness", "linear", "mean", NULL, 0
  ))
})
