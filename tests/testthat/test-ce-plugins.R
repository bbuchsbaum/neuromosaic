# Tests for ce_plugins.R

# -- infer_design_var_type -----------------------------------------------------

test_that("infer_design_var_type classifies numeric as continuous", {
  expect_equal(infer_design_var_type(c(1.5, 2.3, 3.7)), "continuous")
  expect_equal(infer_design_var_type(1:10), "continuous")
  expect_equal(infer_design_var_type(as.double(1:5)), "continuous")
})

test_that("infer_design_var_type classifies character/factor/logical as categorical", {
  expect_equal(infer_design_var_type(c("a", "b", "c")), "categorical")
  expect_equal(infer_design_var_type(factor(c("x", "y"))), "categorical")
  expect_equal(infer_design_var_type(c(TRUE, FALSE, TRUE)), "categorical")
})

test_that("infer_design_var_type classifies Date/POSIXt as continuous", {
  expect_equal(infer_design_var_type(Sys.Date() + 0:2), "continuous")
  expect_equal(infer_design_var_type(Sys.time() + 0:2), "continuous")
})

test_that("infer_design_var_type defaults to categorical for unknown types", {
  expect_equal(infer_design_var_type(list(1, 2)), "categorical")
})

# -- .as_analysis_plugin -------------------------------------------------------

test_that(".as_analysis_plugin wraps a function into a plugin", {
  fn <- function(ts_data, design, params, context) {
    list(data = ts_data, design = design, diagnostics = NULL, meta = list())
  }
  p <- neuromosaic:::.as_analysis_plugin(fn, fallback_id = "test_fn")
  expect_equal(p$id, "test_fn")
  expect_equal(p$label, "test_fn")
  expect_true(is.function(p$run))
  expect_equal(p$param_defs, list())
})

test_that(".as_analysis_plugin accepts a list with id/label/run", {
  p <- neuromosaic:::.as_analysis_plugin(list(
    id = "my_plugin",
    label = "My Plugin",
    run = function(ts_data, design, params, context) {
      list(data = ts_data, design = design, diagnostics = NULL, meta = list())
    },
    param_defs = list(list(name = "alpha", type = "numeric", default = 0.05))
  ))
  expect_equal(p$id, "my_plugin")
  expect_equal(p$label, "My Plugin")
  expect_length(p$param_defs, 1)
})

test_that(".as_analysis_plugin errors without a run function", {
  expect_error(
    neuromosaic:::.as_analysis_plugin(list(id = "bad")),
    "run"
  )
})

test_that(".as_analysis_plugin returns NULL for NULL input", {
  expect_null(neuromosaic:::.as_analysis_plugin(NULL))
})

test_that(".as_analysis_plugin errors for non-function/non-list input", {
  expect_error(
    neuromosaic:::.as_analysis_plugin("not_a_plugin"),
    "functions or lists"
  )
})

# -- .as_plot_plugin -----------------------------------------------------------

test_that(".as_plot_plugin wraps a function into a plugin", {
  fn <- function(data, params, context, interactive) {
    neuromosaic:::.empty_plot("ok")
  }
  p <- neuromosaic:::.as_plot_plugin(fn, fallback_id = "plot_fn")
  expect_equal(p$id, "plot_fn")
  expect_equal(p$label, "plot_fn")
  expect_true(is.function(p$render))
  expect_equal(p$param_defs, list())
})

test_that(".as_plot_plugin errors without a render function", {
  expect_error(
    neuromosaic:::.as_plot_plugin(list(id = "bad")),
    "render"
  )
})

# -- .normalize_analysis_plugins -----------------------------------------------

test_that(".normalize_analysis_plugins includes 'none' and 'group_mean' by default", {
  plugs <- neuromosaic:::.normalize_analysis_plugins()
  expect_true("none" %in% names(plugs))
  expect_true("group_mean" %in% names(plugs))
  expect_equal(names(plugs)[1], "none")
})

test_that(".normalize_analysis_plugins adds user plugins", {
  user_p <- list(
    custom = list(
      id = "custom",
      label = "Custom",
      run = function(ts_data, design, params, context) {
        list(data = ts_data, design = design, diagnostics = NULL, meta = list())
      }
    )
  )
  plugs <- neuromosaic:::.normalize_analysis_plugins(user_p)
  expect_true("custom" %in% names(plugs))
  expect_true("none" %in% names(plugs))
})

test_that(".normalize_analysis_plugins respects default_plugin", {
  plugs <- neuromosaic:::.normalize_analysis_plugins(
    default_plugin = "group_mean"
  )
  expect_equal(names(plugs)[1], "group_mean")
})

# -- .normalize_plot_plugins ---------------------------------------------------

test_that(".normalize_plot_plugins includes built-in plot plugins", {
  plugs <- neuromosaic:::.normalize_plot_plugins()
  expect_true("auto" %in% names(plugs))
  expect_true("group_overlay" %in% names(plugs))
  expect_equal(names(plugs)[1], "auto")
})

test_that(".normalize_plot_plugins respects default_plugin", {
  plugs <- neuromosaic:::.normalize_plot_plugins(
    default_plugin = "group_overlay"
  )
  expect_equal(names(plugs)[1], "group_overlay")
})

# -- .run_analysis_plugin ------------------------------------------------------

test_that(".run_analysis_plugin returns passthrough for 'none'", {
  none_p <- list(id = "none", label = "None", run = identity, param_defs = list())
  ts <- tibble::tibble(
    .sample_index = 1:3, cluster_id = "P1", signal = c(1, 2, 3)
  )
  design <- tibble::tibble(.sample_index = 1:3, condition = c("A", "B", "A"))
  out <- neuromosaic:::.run_analysis_plugin(none_p, ts, design)
  expect_equal(out$data, ts)
})

test_that(".run_analysis_plugin handles plugin errors gracefully", {
  bad_plugin <- list(
    id = "bad",
    label = "Bad",
    run = function(ts_data, design, params, context) stop("boom"),
    param_defs = list()
  )
  ts <- tibble::tibble(
    .sample_index = 1:3, cluster_id = "P1", signal = c(1, 2, 3)
  )
  design <- tibble::tibble(.sample_index = 1:3)
  out <- neuromosaic:::.run_analysis_plugin(bad_plugin, ts, design)
  expect_equal(out$diagnostics$status, "error")
  expect_true(grepl("boom", out$diagnostics$reason))
  expect_equal(out$data, ts)
})

test_that(".run_analysis_plugin validates output has required columns", {
  missing_cols_plugin <- list(
    id = "mc",
    label = "Missing cols",
    run = function(ts_data, design, params, context) {
      list(data = tibble::tibble(x = 1:3), design = design,
           diagnostics = NULL, meta = list())
    },
    param_defs = list()
  )
  ts <- tibble::tibble(
    .sample_index = 1:3, cluster_id = "P1", signal = c(1, 2, 3)
  )
  design <- tibble::tibble(.sample_index = 1:3)
  out <- neuromosaic:::.run_analysis_plugin(missing_cols_plugin, ts, design)
  expect_equal(out$diagnostics$status, "error")
  expect_equal(out$data, ts)
})

test_that(".run_analysis_plugin accepts data.frame return", {
  df_plugin <- list(
    id = "df",
    label = "DF",
    run = function(ts_data, design, params, context) ts_data,
    param_defs = list()
  )
  ts <- tibble::tibble(
    .sample_index = 1:3, cluster_id = "P1", signal = c(1, 2, 3)
  )
  design <- tibble::tibble(.sample_index = 1:3)
  out <- neuromosaic:::.run_analysis_plugin(df_plugin, ts, design)
  expect_s3_class(out$data, "tbl_df")
})

# -- .run_plot_plugin ----------------------------------------------------------

test_that(".run_plot_plugin renders the grouped overlay plot", {
  plugs <- neuromosaic:::.normalize_plot_plugins()
  dat <- tibble::tibble(
    cluster_id = rep(c("P1", "N2"), each = 6),
    signal = c(0.2, 0.5, 0.8, 1.0, 1.3, 1.6, 1.6, 1.3, 1.0, 0.8, 0.5, 0.2),
    measure = rep(c(-1, 0, 1), times = 4),
    group = rep(c("control", "patient"), times = 6)
  )

  out <- neuromosaic:::.run_plot_plugin(
    plugin = plugs[["group_overlay"]],
    data = dat,
    params = list(group_var = "group"),
    context = list(
      x_var = "measure",
      collapse_vars = character(0),
      categorical_columns = "group"
    ),
    interactive = FALSE
  )

  expect_s3_class(out$plot, "gg")
  expect_null(out$diagnostics)
  expect_equal(out$plot$labels$colour, "group")
})

test_that(".run_plot_plugin handles plugin errors gracefully", {
  bad_plugin <- list(
    id = "bad_plot",
    label = "Bad plot",
    render = function(data, params, context, interactive) stop("plot boom"),
    param_defs = list()
  )

  dat <- tibble::tibble(cluster_id = "P1", signal = 1, x = 1)
  out <- neuromosaic:::.run_plot_plugin(
    plugin = bad_plugin,
    data = dat,
    context = list(x_var = "x"),
    interactive = FALSE
  )

  expect_equal(out$diagnostics$status, "error")
  expect_true(grepl("plot boom", out$diagnostics$reason))
  expect_s3_class(out$plot, "gg")
})

# -- .builtin_group_mean_plugin ------------------------------------------------

test_that("group_mean plugin computes per-group means", {
  gm <- neuromosaic:::.builtin_group_mean_plugin()
  ts <- tibble::tibble(
    .sample_index = 1:6,
    cluster_id = rep("P1", 6),
    signal = c(10, 20, 30, 40, 50, 60),
    condition = rep(c("A", "B"), 3)
  )
  design <- tibble::tibble(.sample_index = 1:6)
  out <- gm$run(ts, design, params = list(group_var = "condition"))
  expect_true("signal" %in% names(out$data))
  expect_true("se" %in% names(out$data))
  expect_equal(nrow(out$data), 2L)
  a_row <- out$data[out$data$condition == "A", ]
  expect_equal(a_row$signal, mean(c(10, 30, 50)))
})

test_that("group_mean plugin shows available columns when group_var is missing", {
  gm <- neuromosaic:::.builtin_group_mean_plugin()
  ts <- tibble::tibble(
    .sample_index = 1:3, cluster_id = "P1",
    signal = 1:3, group = c("x", "y", "x")
  )
  out <- gm$run(ts, tibble::tibble(), params = list(group_var = ""))
  expect_equal(out$diagnostics$status, "info")
  expect_true(grepl("group", out$diagnostics$reason))
})
