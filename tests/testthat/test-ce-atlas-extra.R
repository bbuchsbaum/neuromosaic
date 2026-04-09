# Tests for ce_atlas.R functions

test_that(".has_surface_geometry returns FALSE for non-surfatlas", {
  expect_false(neuromosaic:::.has_surface_geometry(list()))
  expect_false(neuromosaic:::.has_surface_geometry(NULL))
  expect_false(neuromosaic:::.has_surface_geometry("not_an_atlas"))
})

test_that(".has_surface_geometry returns FALSE for surfatlas without geometry", {
  surfatlas <- make_toy_surfatlas()
  expect_false(neuromosaic:::.has_surface_geometry(surfatlas))
})

test_that(".warn_if_atlas_surface_mismatch is silent when NULL inputs", {
  expect_silent(neuromosaic:::.warn_if_atlas_surface_mismatch(NULL, NULL))
})

test_that(".warn_if_atlas_surface_mismatch warns on non-overlapping IDs", {
  atlas <- list(ids = c(1L, 2L, 3L))
  surfatlas <- list(ids = c(10L, 20L, 30L))
  expect_warning(
    neuromosaic:::.warn_if_atlas_surface_mismatch(atlas, surfatlas),
    "No overlapping"
  )
})

test_that(".warn_if_atlas_surface_mismatch warns on partial overlap", {
  atlas <- list(ids = c(1L, 2L, 3L))
  surfatlas <- list(ids = c(2L, 3L, 4L, 5L))
  expect_warning(
    neuromosaic:::.warn_if_atlas_surface_mismatch(atlas, surfatlas),
    "partially overlap"
  )
})

test_that(".warn_if_atlas_surface_mismatch is silent on matching IDs", {
  atlas <- list(ids = c(1L, 2L))
  surfatlas <- list(ids = c(1L, 2L))
  expect_silent(neuromosaic:::.warn_if_atlas_surface_mismatch(atlas, surfatlas))
})

test_that(".warn_if_atlas_surface_mismatch handles empty IDs gracefully", {
  atlas <- list(ids = integer(0))
  surfatlas <- list(ids = c(1L, 2L))
  expect_silent(neuromosaic:::.warn_if_atlas_surface_mismatch(atlas, surfatlas))
})

test_that(".atlas_volume_array converts NeuroVol to integer array", {
  x <- make_toy_cluster_explorer_inputs()
  vol <- neuromosaic:::.get_atlas_volume(x$atlas)
  arr <- neuromosaic:::.atlas_volume_array(vol)
  expect_true(is.array(arr))
  expect_equal(storage.mode(arr), "integer")
  expect_equal(dim(arr), c(5L, 5L, 5L))
})

test_that(".atlas_volume_array errors on unsupported type", {
  expect_error(
    neuromosaic:::.atlas_volume_array("not_a_vol"),
    "Unsupported"
  )
})

test_that(".set_atlas_volume updates $atlas slot", {
  x <- make_toy_cluster_explorer_inputs()
  new_vol <- x$stat_map
  updated <- neuromosaic:::.set_atlas_volume(x$atlas, new_vol)
  expect_identical(updated$atlas, new_vol)
})

test_that(".set_atlas_volume updates $data slot when $atlas is absent", {
  x <- make_toy_cluster_explorer_inputs()
  atlas2 <- x$atlas
  atlas2$data <- atlas2$atlas
  atlas2$atlas <- NULL
  new_vol <- x$stat_map
  updated <- neuromosaic:::.set_atlas_volume(atlas2, new_vol)
  expect_identical(updated$data, new_vol)
})

test_that(".set_atlas_volume creates $atlas when neither slot exists", {
  atlas_bare <- list(name = "bare")
  class(atlas_bare) <- "atlas"
  new_vol <- make_toy_cluster_explorer_inputs()$stat_map
  updated <- neuromosaic:::.set_atlas_volume(atlas_bare, new_vol)
  expect_identical(updated$atlas, new_vol)
})

test_that(".count_nonzero_voxels counts correctly for NeuroVol", {
  x <- make_toy_cluster_explorer_inputs()
  vol <- neuromosaic:::.get_atlas_volume(x$atlas)
  count <- neuromosaic:::.count_nonzero_voxels(vol)
  # toy atlas: parcel 1 = 2x2x2=8 voxels, parcel 2 = 2x2x2=8 voxels
  expect_equal(count, 16L)
})

test_that(".count_nonzero_voxels returns NA for unsupported type", {
  expect_true(is.na(neuromosaic:::.count_nonzero_voxels("not_a_vol")))
})

test_that(".harmonize_cluster_explorer_atlas returns unmodified when dims match", {
  x <- make_toy_cluster_explorer_inputs()
  ret <- neuromosaic:::.harmonize_cluster_explorer_atlas(x$atlas, x$stat_map)
  expect_false(ret$resampled)
  expect_null(ret$message)
  expect_null(ret$warning)
})

test_that(".harmonize_cluster_explorer_atlas returns unmodified for NULL atlas", {
  x <- make_toy_cluster_explorer_inputs()
  ret <- neuromosaic:::.harmonize_cluster_explorer_atlas(NULL, x$stat_map)
  expect_null(ret$atlas)
})

test_that(".harmonize_cluster_explorer_atlas returns unmodified for non-atlas class", {
  x <- make_toy_cluster_explorer_inputs()
  plain <- list(name = "test")  # no "atlas" class
  ret <- neuromosaic:::.harmonize_cluster_explorer_atlas(plain, x$stat_map)
  expect_identical(ret$atlas, plain)
})

test_that(".infer_surfatlas_from_atlas returns NULL for NULL", {
  expect_null(neuromosaic:::.infer_surfatlas_from_atlas(NULL))
})

test_that(".infer_surfatlas_from_atlas returns NULL for non-list", {
  expect_null(neuromosaic:::.infer_surfatlas_from_atlas("not_list"))
})

test_that(".infer_surfatlas_from_atlas returns embedded surfatlas", {
  surfatlas <- make_toy_surfatlas()
  atlas <- list(surfatlas = surfatlas)
  result <- neuromosaic:::.infer_surfatlas_from_atlas(atlas)
  expect_true(inherits(result, "surfatlas"))
})

test_that(".infer_surfatlas_from_atlas returns NULL for plain atlas", {
  x <- make_toy_cluster_explorer_inputs()
  result <- neuromosaic:::.infer_surfatlas_from_atlas(x$atlas)
  expect_null(result)
})
