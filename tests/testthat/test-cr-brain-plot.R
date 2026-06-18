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

test_that(".make_brain_plot hands a NeuroVol overlay to plot_brain for projection", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )
  inputs <- make_toy_cluster_report_inputs()
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .parcel_values_from_clusters = function(...) c(1, -1),
    .fallback_brain_plot = function(...) {
      ggplot2::ggplot() + ggplot2::geom_blank()
    },
    .package = "neuromosaic"
  )
  testthat::local_mocked_bindings(
    plot_brain = function(..., overlay, overlay_fun, overlay_sampling) {
      captured$overlay <- overlay
      captured$fun <- overlay_fun
      captured$sampling <- overlay_sampling
      ggplot2::ggplot() + ggplot2::geom_blank()
    },
    .package = "neuroatlas"
  )

  # The fix: the cluster statistic VOLUME is forwarded so plot_brain projects it
  # with the atlas's own geometry, instead of a pre-projected (and, on fsaverage
  # atlases, all-NA) lh/rh list.
  neuromosaic:::.make_brain_plot(
    surfatlas = make_toy_surfatlas(),
    cluster_parcels = list(), scope_ids = integer(0),
    display_mode = "dominant", use_surface_plot = TRUE,
    overlay_vals = inputs$stat_map,
    overlay_threshold = 3, overlay_alpha = 0.9,
    brain_views = "lateral", brain_hemis = "left",
    palette = "vik", interactive = FALSE,
    overlay_fun = "mode", overlay_sampling = "thickness"
  )

  expect_true(inherits(captured$overlay, "NeuroVol"))
  expect_identical(captured$fun, "mode")
  expect_identical(captured$sampling, "thickness")
})
