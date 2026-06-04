test_that("prepare_overlay returns aligned NeuroVol pairs", {
  inputs <- make_toy_cluster_report_inputs()

  out <- prepare_overlay(inputs$stat_map, inputs$stat_map)

  expect_true(methods::is(out$background, "NeuroVol"))
  expect_true(methods::is(out$stat, "NeuroVol"))
  expect_true(out$reconciled)
  expect_identical(out$action, "already_aligned")
})

test_that("prepare_overlay loads path-backed volumes", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("prepare-overlay-paths-")
  dir.create(tmpdir, recursive = TRUE)
  bg_path <- file.path(tmpdir, "bg.nii.gz")
  stat_path <- file.path(tmpdir, "stat.nii.gz")
  neuroim2::write_vol(inputs$stat_map, bg_path)
  neuroim2::write_vol(inputs$stat_map, stat_path)

  out <- prepare_overlay(bg_path, stat_path)

  expect_true(methods::is(out$background, "NeuroVol"))
  expect_true(methods::is(out$stat, "NeuroVol"))
  expect_identical(out$action, "already_aligned")
})

test_that("prepare_overlay errors on grid mismatch by default", {
  inputs <- make_toy_cluster_report_inputs()
  stat_space <- neuroim2::NeuroSpace(
    dim = dim(neuroim2::space(inputs$stat_map)),
    spacing = c(2, 2, 2),
    origin = c(100, 100, 100)
  )
  stat <- neuroim2::NeuroVol(as.array(inputs$stat_map), stat_space)

  expect_error(
    prepare_overlay(inputs$stat_map, stat),
    "different grids"
  )
})

test_that("prepare_overlay restamps only when dimensions match", {
  inputs <- make_toy_cluster_report_inputs()
  stat_space <- neuroim2::NeuroSpace(
    dim = dim(neuroim2::space(inputs$stat_map)),
    spacing = c(2, 2, 2),
    origin = c(100, 100, 100)
  )
  stat <- neuroim2::NeuroVol(as.array(inputs$stat_map), stat_space)

  out <- prepare_overlay(inputs$stat_map, stat, on_mismatch = "restamp")

  expect_identical(out$action, "restamped")
  expect_equal(as.array(out$stat), as.array(inputs$stat_map))
  expect_true(.same_neuro_space(neuroim2::space(out$stat),
                                neuroim2::space(inputs$stat_map)))

  bad_space <- neuroim2::NeuroSpace(
    dim = c(6, 6, 6),
    spacing = c(2, 2, 2),
    origin = c(-5, -5, -5)
  )
  bad_stat <- neuroim2::NeuroVol(array(0, dim = c(6, 6, 6)), bad_space)
  expect_error(
    prepare_overlay(inputs$stat_map, bad_stat, on_mismatch = "restamp"),
    "dimensions differ"
  )
})

test_that("prepare_overlay validates inputs", {
  expect_error(prepare_overlay("missing.nii.gz", "missing.nii.gz"), "path not found")
  expect_error(prepare_overlay(list(), list()), "must be a NeuroVol or path")
})
