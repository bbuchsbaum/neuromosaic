expected_peak_coord <- function(volume, index) {
  grid <- neuroim2::index_to_grid(volume, as.integer(index))
  as.numeric(neuroim2::grid_to_coord(volume, as.matrix(grid))[1L, ])
}

test_that("volume display spec selects a deterministic tail-aware peak", {
  inputs <- make_toy_cluster_report_inputs()
  values <- as.numeric(inputs$stat_map)

  cases <- list(
    positive = list(tail = "positive", index = which.max(values)),
    negative = list(tail = "negative", index = which.min(values)),
    two_sided = list(tail = "two_sided", index = which.max(abs(values)))
  )
  for (case in cases) {
    out <- stat_montage(
      inputs$stat_map,
      inputs$stat_map,
      threshold = 3,
      tail = case$tail,
      draw = FALSE
    )
    expect_s3_class(out$display_spec, "montage_volume_display_spec")
    expect_equal(
      out$initial_world_coord,
      expected_peak_coord(inputs$stat_map, case$index)
    )
  }
})

test_that("volume display spec is the final static and interactive state", {
  inputs <- make_toy_cluster_report_inputs()
  nonnegative <- neuroim2::NeuroVol(
    abs(as.array(inputs$stat_map)), neuroim2::space(inputs$stat_map)
  )
  out <- stat_montage(
    inputs$stat_map,
    nonnegative,
    threshold = NULL,
    signed = FALSE,
    limits = c(0, 7),
    zlevels = c(1, 3, 5),
    ov_cmap = "inferno",
    ov_alpha = 0.4,
    ov_alpha_mode = "binary",
    draw = FALSE
  )
  spec <- out$display_spec

  expect_identical(spec$display_mode, "continuous")
  expect_true(is.na(spec$threshold))
  expect_identical(spec$scale, "sequential")
  expect_null(spec$center)
  expect_equal(spec$limits, c(0, 7))
  expect_identical(spec$palette, "inferno")
  expect_equal(spec$alpha, 0.4)
  expect_identical(spec$alpha_mode, "binary")
  expect_equal(spec$zlevels, c(1, 3, 5))
  expect_equal(spec$zlevel_bookmarks, c(1, 3, 5))
  expect_equal(
    spec$initial_world_coord,
    expected_peak_coord(nonnegative, which.max(as.numeric(nonnegative)))
  )
  expect_identical(out$limits, spec$limits)
  expect_identical(out$zlevels, spec$zlevels)
  expect_identical(out$alpha_mode, spec$alpha_mode)
  expect_false(any(grepl("htmlwidget|javascript|shiny", class(spec),
                         ignore.case = TRUE)))
})

test_that("empty displays use the volume centre as a stable coordinate", {
  inputs <- make_toy_cluster_report_inputs()
  centre_grid <- matrix((dim(inputs$stat_map)[1:3] + 1) / 2, nrow = 1L)
  centre <- as.numeric(
    neuroim2::grid_to_coord(inputs$stat_map, centre_grid)[1L, ]
  )

  expect_warning(
    out <- stat_montage(
      inputs$stat_map,
      inputs$stat_map,
      threshold = 100,
      empty = "warning",
      draw = FALSE
    ),
    "No finite suprathreshold voxels"
  )
  expect_equal(out$n_suprathreshold, 0L)
  expect_equal(out$display_spec$initial_world_coord, centre)
})

test_that("an explicit world coordinate wins over peak and centre defaults", {
  inputs <- make_toy_cluster_report_inputs()
  chosen <- c(11.5, -22, 3.25)
  out <- stat_montage(
    inputs$stat_map,
    inputs$stat_map,
    threshold = 3,
    initial_world_coord = chosen,
    draw = FALSE
  )

  expect_identical(out$initial_world_coord, chosen)
  expect_identical(out$display_spec$initial_world_coord, chosen)
  expect_error(
    stat_montage(
      inputs$stat_map, inputs$stat_map, threshold = 3,
      initial_world_coord = c(1, 2), draw = FALSE
    ),
    "initial_world_coord"
  )
})

test_that("display metadata records custom support and interactive controls", {
  inputs <- make_toy_cluster_report_inputs()
  support <- rep(FALSE, length(inputs$stat_map))
  support[which.max(as.numeric(inputs$stat_map))] <- TRUE
  out <- stat_montage(
    inputs$stat_map,
    inputs$stat_map,
    threshold = NULL,
    signed = TRUE,
    limits = c(-8, 8),
    support_mask = support,
    ov_cmap = "blue-red",
    ov_alpha = 0.65,
    draw = FALSE
  )
  row <- data.frame(
    analysis_id = "faces",
    map_id = "faces_custom",
    quantity = "mylab:stability",
    label = "Stability",
    selector_label = "Stability map",
    effective_scale = "diverging",
    effective_support = "analysis",
    effective_units = "a.u.",
    stringsAsFactors = FALSE
  )
  metadata <- neuromosaic:::.montage_volume_display_metadata(
    out$display_spec, row, rep(TRUE, length(support))
  )

  expect_s3_class(metadata, "montage_volume_display_metadata")
  expect_identical(metadata$quantity, "mylab:stability")
  expect_identical(metadata$selector_label, "Stability map")
  expect_identical(metadata$units, "a.u.")
  expect_identical(metadata$support, "custom")
  expect_identical(metadata$support_mask, support)
  expect_identical(metadata$palette, "blue-red")
  expect_equal(metadata$alpha, 0.65)
  expect_equal(metadata$n_display_voxels, 1L)
})
