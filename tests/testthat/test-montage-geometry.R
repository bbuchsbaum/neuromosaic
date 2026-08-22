test_that("volume geometry records every spatial invariant and a stable digest", {
  volume <- make_toy_cluster_report_inputs()$stat_map
  geometry <- neuromosaic:::.montage_volume_geometry(volume)

  expect_named(
    geometry,
    c("dimensions", "spacing", "origin", "orientation", "affine",
      "fingerprint")
  )
  expect_identical(geometry$dimensions, c(5L, 5L, 5L))
  expect_equal(geometry$spacing, c(2, 2, 2))
  expect_equal(geometry$origin, c(-5, -5, -5))
  expect_identical(geometry$orientation, "RAS")
  expect_length(geometry$affine, 16L)
  expect_match(geometry$fingerprint, "^md5:[0-9a-f]{32}$")
  expect_identical(
    geometry$fingerprint,
    neuromosaic:::.montage_volume_geometry(volume)$fingerprint
  )
})

test_that("geometry comparison uses the documented tolerance and orientation", {
  volume <- make_toy_cluster_report_inputs()$stat_map
  near_space <- neuroim2::NeuroSpace(
    dim = dim(volume),
    spacing = c(2, 2, 2),
    origin = c(-5 + 5e-7, -5, -5)
  )
  far_space <- neuroim2::NeuroSpace(
    dim = dim(volume),
    spacing = c(2, 2, 2),
    origin = c(-5 + 5e-4, -5, -5)
  )

  expect_equal(neuromosaic:::.montage_geometry_tolerance, 1e-6)
  expect_true(neuromosaic:::.same_neuro_space(
    neuroim2::space(volume), near_space
  ))
  expect_false(neuromosaic:::.same_neuro_space(
    neuroim2::space(volume), far_space
  ))

  geometry <- neuromosaic:::.montage_volume_geometry(volume)
  changed <- geometry
  changed$orientation <- "LAS"
  expect_identical(
    neuromosaic:::.montage_geometry_mismatches(geometry, changed),
    "orientation"
  )
})

test_that("report preparation names background geometry failures", {
  inputs <- make_toy_cluster_report_inputs()
  shifted_space <- neuroim2::NeuroSpace(
    dim = dim(inputs$stat_map),
    spacing = c(2, 2, 2),
    origin = c(-4, -5, -5)
  )
  shifted <- neuroim2::NeuroVol(
    as.array(inputs$stat_map), shifted_space
  )
  manifest <- data.frame(
    analysis_id = "faces",
    map_id = "faces_z",
    role = "primary",
    quantity = "test_statistic",
    distribution = "z",
    threshold = 3,
    label = "Z statistic",
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(list(shifted))

  expect_error(
    render_montage_report(
      manifest,
      tempfile(fileext = ".qmd"),
      bg = inputs$stat_map,
      materialize_recipes = FALSE,
      render_peaks = FALSE
    ),
    "analysis_id 'faces'.*map_id 'faces_z'.*origin.*affine"
  )
})

test_that("every affected mask is checked against its target map", {
  inputs <- make_toy_cluster_report_inputs()
  shifted_space <- neuroim2::NeuroSpace(
    dim = dim(inputs$stat_map),
    spacing = c(2, 2, 2),
    origin = c(-5, -4, -5)
  )
  shifted_mask <- neuroim2::NeuroVol(
    array(1, dim = dim(inputs$stat_map)), shifted_space
  )
  manifest <- data.frame(
    analysis_id = "faces",
    map_id = "faces_z",
    role = "primary",
    quantity = "test_statistic",
    distribution = "z",
    threshold = 3,
    label = "Z statistic",
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(list(inputs$stat_map))
  manifest$mask <- I(list(shifted_mask))

  expect_error(
    render_montage_report(
      manifest,
      tempfile(fileext = ".qmd"),
      bg = inputs$stat_map,
      materialize_recipes = FALSE,
      render_peaks = FALSE
    ),
    "analysis_id 'faces'.*map_id 'faces_z'.*analysis mask"
  )
})

test_that("world coordinates are tested in the affine field of view", {
  geometry <- list(
    dimensions = c(5L, 6L, 7L),
    spacing = c(1, 1, 1),
    origin = c(0, 0, 0),
    orientation = "RAS",
    affine = as.numeric(diag(4))
  )
  geometry$fingerprint <- neuromosaic:::.montage_geometry_fingerprint(geometry)

  expect_true(neuromosaic:::.montage_world_coord_in_geometry(
    c(0, 0, 0), geometry
  ))
  expect_true(neuromosaic:::.montage_world_coord_in_geometry(
    c(4, 5, 6), geometry
  ))
  expect_false(neuromosaic:::.montage_world_coord_in_geometry(
    c(100, 0, 0), geometry
  ))
})
