test_that("plot_cluster_timecourse returns ggplot", {
  tc <- tibble::tibble(
    cluster_id = rep("c1", 12),
    .sample_index = 1:12,
    value = rnorm(12),
    condition = rep(c("A", "B"), each = 6),
    time = rep(1:6, 2)
  )

  p <- plot_cluster_timecourse(
    data = tc,
    formula = value ~ condition * time,
    cluster_label = "Test cluster"
  )

  expect_s3_class(p, "gg")
})

test_that("plot_cluster_timecourse handles single-term formula", {
  tc <- tibble::tibble(
    cluster_id = rep("c1", 6),
    .sample_index = 1:6,
    value = rnorm(6),
    time = 1:6
  )

  p <- plot_cluster_timecourse(
    data = tc,
    formula = value ~ time
  )

  expect_s3_class(p, "gg")
})

test_that("plot_cluster_timecourse handles 3-term formula with facets", {
  tc <- tibble::tibble(
    cluster_id = rep("c1", 24),
    .sample_index = 1:24,
    value = rnorm(24),
    group = rep(c("G1", "G2"), each = 12),
    condition = rep(rep(c("A", "B"), each = 6), 2),
    time = rep(1:6, 4)
  )

  p <- plot_cluster_timecourse(
    data = tc,
    formula = value ~ group * condition * time
  )

  expect_s3_class(p, "gg")
})

test_that(".parse_tc_formula identifies vars correctly", {
  parsed <- neuromosaic:::.parse_tc_formula(value ~ condition * time)
  expect_equal(parsed$value_col, "value")
  expect_equal(parsed$x_var, "time")
  expect_equal(parsed$color_var, "condition")
  expect_null(parsed$facet_var)
})

test_that(".parse_tc_formula + is equivalent to *", {
  parsed_star <- neuromosaic:::.parse_tc_formula(value ~ condition * time)
  parsed_plus <- neuromosaic:::.parse_tc_formula(value ~ condition + time)
  expect_equal(parsed_star$x_var, parsed_plus$x_var)
  expect_equal(parsed_star$color_var, parsed_plus$color_var)
})

test_that(".parse_tc_formula with 3 terms", {
  parsed <- neuromosaic:::.parse_tc_formula(value ~ group * condition * time)
  expect_equal(parsed$x_var, "time")
  expect_equal(parsed$color_var, "condition")
  expect_equal(parsed$facet_var, "group")
})

test_that("cluster_display_label produces expected format", {
  row <- tibble::tibble(
    cluster_id = "pos_1",
    hemisphere = "left",
    atlas_label = "FrontalA",
    n_voxels = 89L
  )
  label <- cluster_display_label(row)
  expect_type(label, "character")
  expect_match(label, "pos_1")
  expect_match(label, "left")
  expect_match(label, "FrontalA")
  expect_match(label, "89 vox")
})

test_that("cluster_display_label handles NA hemisphere", {
  row <- tibble::tibble(
    cluster_id = "pos_1",
    hemisphere = NA_character_,
    atlas_label = "FrontalA",
    n_voxels = 50L
  )
  label <- cluster_display_label(row)
  expect_false(grepl("NA", label))
})

test_that("plot_all_clusters returns named list of ggplots", {
  tc <- tibble::tibble(
    cluster_id = rep(c("c1", "c2"), each = 6),
    .sample_index = rep(1:6, 2),
    value = rnorm(12),
    time = rep(1:6, 2)
  )
  ct <- tibble::tibble(
    cluster_id = c("c1", "c2"),
    hemisphere = c("left", "right"),
    atlas_label = c("A", "B"),
    n_voxels = c(10L, 20L)
  )

  plots <- plot_all_clusters(tc, value ~ time, ct)
  expect_type(plots, "list")
  expect_true(all(names(plots) %in% c("c1", "c2")))
  for (p in plots) {
    expect_s3_class(p, "gg")
  }
})

test_that(".aggregate_timecourse computes CI bounds", {
  tc <- tibble::tibble(
    time = rep(1:3, each = 10),
    value = rnorm(30)
  )
  agg <- neuromosaic:::.aggregate_timecourse(tc, "value", "time", ci_level = 0.95)
  expect_true(all(c("mean_value", "se_value", "ci_lower", "ci_upper") %in% names(agg)))
  expect_true(all(agg$ci_lower <= agg$mean_value))
  expect_true(all(agg$ci_upper >= agg$mean_value))
})
