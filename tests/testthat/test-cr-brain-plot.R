test_that("plot_cluster_slices returns a plot object", {
  inputs <- make_toy_cluster_report_inputs()
  peak <- c(-3, -3, -3)  # MNI coordinate in the toy space

  p <- plot_cluster_slices(
    stat_map = inputs$stat_map,
    peak_mni = peak,
    title    = "Test cluster"
  )

  expect_type(p, "list")
  expect_equal(names(p), c("axial", "coronal", "sagittal"))
  expect_true(all(vapply(p, inherits, logical(1), "gg")))
})

test_that("plot_cluster_slices validates inputs", {
  inputs <- make_toy_cluster_report_inputs()

  expect_error(
    plot_cluster_slices(stat_map = "not_a_vol", peak_mni = c(0, 0, 0)),
    "NeuroVol"
  )

  expect_error(
    plot_cluster_slices(stat_map = inputs$stat_map, peak_mni = c(0, 0)),
    "length 3"
  )
})

test_that("plot_all_cluster_slices returns named list", {
  inputs <- make_toy_cluster_report_inputs()
  cluster_data <- build_cluster_explorer_data(
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    stat_map         = inputs$stat_map,
    sample_table     = inputs$design,
    threshold        = 3.0,
    min_cluster_size = 3
  )
  enriched <- enrich_cluster_table(
    cluster_table = cluster_data$cluster_table,
    stat_map      = inputs$stat_map,
    atlas         = inputs$atlas
  )

  slices <- plot_all_cluster_slices(
    stat_map      = inputs$stat_map,
    cluster_table = enriched
  )

  expect_type(slices, "list")
  if (nrow(enriched) > 0) {
    expect_true(length(slices) > 0)
    expect_true(all(names(slices) %in% enriched$cluster_id))
  }
})

test_that("plot_all_cluster_slices is exported", {
  expect_true("plot_all_cluster_slices" %in% getNamespaceExports("neuromosaic"))
})

test_that("plot_all_cluster_slices returns empty list for empty table", {
  inputs <- make_toy_cluster_report_inputs()
  empty_ct <- neuromosaic:::.empty_cluster_table()
  empty_ct$peak_mni_x <- numeric(0)
  empty_ct$peak_mni_y <- numeric(0)
  empty_ct$peak_mni_z <- numeric(0)
  empty_ct$atlas_label <- character(0)
  empty_ct$hemisphere <- character(0)
  empty_ct$n_voxels <- integer(0)

  slices <- plot_all_cluster_slices(
    stat_map      = inputs$stat_map,
    cluster_table = empty_ct
  )

  expect_equal(length(slices), 0)
})
