# Tests for vendored_neuroatlas.R

test_that(".get_atlas_volume extracts from $atlas slot", {
  x <- make_toy_cluster_explorer_inputs()
  vol <- neuromosaic:::.get_atlas_volume(x$atlas)
  expect_true(methods::is(vol, "NeuroVol"))
})

test_that(".get_atlas_volume extracts from $data slot", {
  x <- make_toy_cluster_explorer_inputs()
  atlas2 <- x$atlas
  atlas2$data <- atlas2$atlas
  atlas2$atlas <- NULL
  vol <- neuromosaic:::.get_atlas_volume(atlas2)
  expect_true(methods::is(vol, "NeuroVol"))
})

test_that(".get_atlas_volume errors when no volume found", {
  expect_error(
    neuromosaic:::.get_atlas_volume(list(name = "empty")),
    "Could not determine"
  )
})

test_that(".project_view returns correct structure for all view-hemi combos", {
  verts <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3, byrow = TRUE)
  combos <- list(
    list(view = "lateral", hemi = "left"),
    list(view = "medial", hemi = "left"),
    list(view = "lateral", hemi = "right"),
    list(view = "medial", hemi = "right"),
    list(view = "dorsal", hemi = "left"),
    list(view = "dorsal", hemi = "right"),
    list(view = "ventral", hemi = "left"),
    list(view = "ventral", hemi = "right")
  )
  for (combo in combos) {
    res <- neuromosaic:::.project_view(verts, combo$view, combo$hemi)
    expect_true(is.matrix(res$xy))
    expect_equal(nrow(res$xy), 3L)
    expect_equal(ncol(res$xy), 2L)
    expect_length(res$view_dir, 3L)
  }
})

test_that(".project_view errors on unknown view", {
  verts <- matrix(1:6, ncol = 3)
  expect_error(
    neuromosaic:::.project_view(verts, "posterior", "left"),
    "Unknown view"
  )
})

test_that(".encode_plot_brain_data_id produces correct format", {
  id <- neuromosaic:::.encode_plot_brain_data_id("lh_lateral", 42L, 7L)
  expect_equal(id, "lh_lateral::42::7")
})
