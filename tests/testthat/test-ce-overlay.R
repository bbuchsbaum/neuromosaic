# Tests for ce_overlay.R helpers

test_that(".build_cluster_overlay_volume copies stat values at cluster voxels", {
  x <- make_toy_cluster_explorer_inputs(n_time = 3)
  stat_arr <- as.array(x$stat_map)

  cluster_voxels <- list(
    P1 = matrix(c(1, 1, 1,
                   2, 2, 2), ncol = 3, byrow = TRUE),
    N2 = matrix(c(4, 4, 4,
                   5, 5, 5), ncol = 3, byrow = TRUE)
  )

  vol <- neuromosaic:::.build_cluster_overlay_volume(
    stat_map = x$stat_map,
    cluster_voxels = cluster_voxels
  )

  out_arr <- as.array(vol)
  expect_equal(dim(out_arr), dim(stat_arr))

  # Cluster voxel values should match stat_map
  expect_equal(out_arr[1, 1, 1], stat_arr[1, 1, 1])
  expect_equal(out_arr[2, 2, 2], stat_arr[2, 2, 2])
  expect_equal(out_arr[4, 4, 4], stat_arr[4, 4, 4])
  expect_equal(out_arr[5, 5, 5], stat_arr[5, 5, 5])

  # Non-cluster voxels should be zero
  expect_equal(out_arr[3, 3, 3], 0)
})

test_that(".build_cluster_overlay_volume respects selected_cluster_ids", {
  x <- make_toy_cluster_explorer_inputs(n_time = 3)
  stat_arr <- as.array(x$stat_map)

  cluster_voxels <- list(
    P1 = matrix(c(1, 1, 1, 2, 2, 2), ncol = 3, byrow = TRUE),
    N2 = matrix(c(4, 4, 4, 5, 5, 5), ncol = 3, byrow = TRUE)
  )

  vol <- neuromosaic:::.build_cluster_overlay_volume(
    stat_map = x$stat_map,
    cluster_voxels = cluster_voxels,
    selected_cluster_ids = "P1"
  )

  out_arr <- as.array(vol)
  # P1 voxels should have values
  expect_equal(out_arr[2, 2, 2], stat_arr[2, 2, 2])
  # N2 voxels should be zero (not selected)
  expect_equal(out_arr[4, 4, 4], 0)
  expect_equal(out_arr[5, 5, 5], 0)
})

test_that(".build_cluster_overlay_volume returns zeros for empty selection", {
  x <- make_toy_cluster_explorer_inputs(n_time = 3)

  cluster_voxels <- list(
    P1 = matrix(c(1, 1, 1), ncol = 3)
  )

  vol <- neuromosaic:::.build_cluster_overlay_volume(
    stat_map = x$stat_map,
    cluster_voxels = cluster_voxels,
    selected_cluster_ids = "NONEXISTENT"
  )

  out_arr <- as.array(vol)
  expect_true(all(out_arr == 0))
})

test_that(".surface_values_to_numeric handles NULL and numeric inputs", {
  expect_null(neuromosaic:::.surface_values_to_numeric(NULL))

  # Plain numeric vector wrapped in a list-like object with @data slot
  mock <- structure(list(), class = "mock_surf")
  expect_null(neuromosaic:::.surface_values_to_numeric(mock))
})

test_that(".surface_template_defaults returns valid defaults for known spaces", {
  d6 <- neuromosaic:::.surface_template_defaults("fsaverage6")
  expect_equal(d6$template_id, "fsaverage")
  expect_equal(d6$density, "41k")
  expect_equal(d6$resolution, "06")

  d5 <- neuromosaic:::.surface_template_defaults("fsaverage5")
  expect_equal(d5$template_id, "fsaverage")
  expect_equal(d5$density, "10k")
  expect_equal(d5$resolution, "05")

  d_full <- neuromosaic:::.surface_template_defaults("fsaverage")
  expect_equal(d_full$template_id, "fsaverage")
  expect_equal(d_full$density, "164k")
  expect_null(d_full$resolution)

  d_unknown <- neuromosaic:::.surface_template_defaults("MNI152NLin2009cAsym")
  expect_equal(d_unknown$template_id, "MNI152NLin2009cAsym")
  expect_null(d_unknown$density)

  d_null <- neuromosaic:::.surface_template_defaults(NULL)
  expect_equal(d_null$template_id, "fsaverage")
})

test_that(".overlay_projection_diagnostics returns structured output", {
  x <- make_toy_cluster_explorer_inputs(n_time = 3)

  cluster_vol <- neuromosaic:::.build_cluster_overlay_volume(
    stat_map = x$stat_map,
    cluster_voxels = list(P1 = matrix(c(2, 2, 2), ncol = 3))
  )

  projection <- list(
    overlay = list(lh = c(1.5, 0.3, 0, -0.1), rh = NULL),
    meta = list(
      surface_space = "fsaverage6",
      hemis = list(
        lh = list(target_vertices = 4L, projected_vertices = 4L, finite_vertices = 4L),
        rh = NULL
      )
    )
  )

  diag <- neuromosaic:::.overlay_projection_diagnostics(
    cluster_vol = cluster_vol,
    projection = projection,
    threshold = 0.5,
    sampling = "midpoint",
    fun = "avg"
  )

  expect_type(diag, "list")
  expect_true("cluster_voxels_nonzero" %in% names(diag))
  expect_equal(diag$surface_space, "fsaverage6")
  expect_equal(diag$projection_fun, "avg")
  expect_equal(diag$projection_sampling, "midpoint")
  expect_equal(diag$overlay_threshold, 0.5)
  expect_s3_class(diag$hemi, "data.frame")
  expect_equal(nrow(diag$hemi), 2)

  lh_row <- diag$hemi[diag$hemi$hemi == "lh", ]
  expect_equal(lh_row$target_vertices, 4L)
  expect_equal(lh_row$finite_vertices, 4L)
  expect_equal(lh_row$above_threshold, 1L)  # only 1.5 >= 0.5
})
