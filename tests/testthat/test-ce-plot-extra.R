# Additional tests for ce_plot.R

test_that(".cluster_explorer_demo_inputs returns valid structure", {
  demo <- neuromosaic:::.cluster_explorer_demo_inputs(n_time = 8L)
  expect_true(is.list(demo))
  expect_true(methods::is(demo$stat_map, "NeuroVol"))
  expect_true(methods::is(demo$data_source, "NeuroVec"))
  expect_true(inherits(demo$atlas, "atlas"))
  expect_s3_class(demo$sample_table, "data.frame")
  expect_equal(nrow(demo$sample_table), 8L)
  expect_true(all(c("condition", "run") %in% names(demo$sample_table)))
})

test_that(".cluster_explorer_demo_inputs creates valid atlas with roi_metadata", {
  demo <- neuromosaic:::.cluster_explorer_demo_inputs(n_time = 4L)
  expect_true(!is.null(demo$atlas$roi_metadata))
  expect_true("id" %in% names(demo$atlas$roi_metadata))
  expect_true("label" %in% names(demo$atlas$roi_metadata))
  expect_equal(length(demo$atlas$ids), 6L)
})

test_that(".empty_plot returns a ggplot", {
  p <- neuromosaic:::.empty_plot("Test message")
  expect_s3_class(p, "gg")
})

test_that(".empty_plot with subtitle adds second annotation", {
  p <- neuromosaic:::.empty_plot("Title", subtitle = "Sub")
  expect_s3_class(p, "gg")
})

test_that(".build_design_plot creates a ggplot for continuous x", {
  data <- tibble::tibble(
    cluster_id = rep(c("P1", "P2"), each = 10),
    signal = rnorm(20),
    time = rep(1:10, 2)
  )
  p <- neuromosaic:::.build_design_plot(data, x_var = "time")
  expect_s3_class(p, "gg")
})

test_that(".build_design_plot creates a ggplot for categorical x", {
  data <- tibble::tibble(
    cluster_id = rep("P1", 6),
    signal = rnorm(6),
    condition = rep(c("A", "B"), 3)
  )
  p <- neuromosaic:::.build_design_plot(data, x_var = "condition")
  expect_s3_class(p, "gg")
})

test_that(".build_design_plot handles collapse_vars", {
  data <- tibble::tibble(
    cluster_id = rep("P1", 8),
    signal = rnorm(8),
    time = rep(1:4, 2),
    run = rep(c("r1", "r2"), each = 4)
  )
  p <- neuromosaic:::.build_design_plot(data, x_var = "time",
                                              collapse_vars = "run")
  expect_s3_class(p, "gg")
})

test_that(".build_design_plot supports grouped regression overlays", {
  data <- tibble::tibble(
    cluster_id = rep(c("P1", "N2"), each = 8),
    signal = c(
      0.1, 0.3, 0.5, 0.7, 1.0, 1.2, 1.4, 1.6,
      1.6, 1.4, 1.2, 1.0, 0.7, 0.5, 0.3, 0.1
    ),
    measure = rep(rep(c(-1.5, -0.5, 0.5, 1.5), each = 2), times = 2),
    group = rep(rep(c("control", "patient"), times = 4), times = 2)
  )

  p <- neuromosaic:::.build_design_plot(
    data = data,
    x_var = "measure",
    group_var = "group"
  )

  expect_s3_class(p, "gg")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(p$labels$colour, "group")
  expect_equal(p$layers[[2]]$stat_params$method, "lm")
})

test_that(".build_design_plot interactive returns girafe", {
  data <- tibble::tibble(
    cluster_id = rep("P1", 10),
    signal = seq(0.2, 2, length.out = 10),
    time = 1:10
  )
  g <- neuromosaic:::.build_design_plot(data, x_var = "time",
                                              interactive = TRUE)
  expect_true(inherits(g, "girafe"))
})
