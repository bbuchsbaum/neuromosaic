# Additional tests for ce_overlay.R pure helpers

test_that(".surface_values_to_numeric returns NULL for NULL input", {
  expect_null(neuromosaic:::.surface_values_to_numeric(NULL))
})

test_that(".surface_values_to_numeric returns NULL for unsupported objects", {
  expect_null(neuromosaic:::.surface_values_to_numeric("not_a_surf"))
})

test_that(".surface_template_defaults returns correct defaults for fsaverage6", {
  d <- neuromosaic:::.surface_template_defaults("fsaverage6")
  expect_equal(d$template_id, "fsaverage")
  expect_equal(d$density, "41k")
  expect_equal(d$resolution, "06")
})

test_that(".surface_template_defaults returns correct defaults for fsaverage5", {
  d <- neuromosaic:::.surface_template_defaults("fsaverage5")
  expect_equal(d$template_id, "fsaverage")
  expect_equal(d$density, "10k")
  expect_equal(d$resolution, "05")
})

test_that(".surface_template_defaults returns correct defaults for fsaverage", {
  d <- neuromosaic:::.surface_template_defaults("fsaverage")
  expect_equal(d$template_id, "fsaverage")
  expect_equal(d$density, "164k")
  expect_null(d$resolution)
})

test_that(".surface_template_defaults handles unknown space", {
  d <- neuromosaic:::.surface_template_defaults("custom_space")
  expect_equal(d$template_id, "custom_space")
  expect_null(d$density)
  expect_null(d$resolution)
})

test_that(".surface_template_defaults handles NULL/empty", {
  d <- neuromosaic:::.surface_template_defaults(NULL)
  expect_equal(d$template_id, "fsaverage")

  d2 <- neuromosaic:::.surface_template_defaults("")
  expect_equal(d2$template_id, "fsaverage")
})

test_that(".build_cluster_overlay_volume handles empty selected IDs", {
  x <- make_toy_cluster_explorer_inputs()
  res <- suppressWarnings(
    build_cluster_explorer_data(
      data_source = x$data_vec, atlas = x$atlas,
      stat_map = x$stat_map, sample_table = x$sample_table,
      threshold = 3, min_cluster_size = 4
    )
  )
  vol <- neuromosaic:::.build_cluster_overlay_volume(
    stat_map = x$stat_map,
    cluster_voxels = res$cluster_voxels,
    selected_cluster_ids = "NONEXISTENT"
  )
  arr <- as.array(vol)
  expect_true(all(arr == 0))
})
