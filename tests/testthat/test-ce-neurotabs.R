# Tests for the neurotabs adapter (ce_neurotabs.R).
# All tests are skipped when neurotabs is not installed.

# -- Test helpers -------------------------------------------------------------

# Register a mock backend that serves in-memory arrays keyed by locator string.
# Returns the environment where arrays are stored (tests may inspect it).
.register_mock_ce_backend <- function(backend_name, dims, n_obs) {
  sp3 <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1),
                               origin = c(0, 0, 0))
  arr_env <- new.env(parent = emptyenv())
  for (i in seq_len(n_obs)) {
    arr <- array(0, dim = dims)
    arr[1:2, 1:2, 1:2] <- i
    arr[4:5, 4:5, 4:5] <- 10 + i
    assign(paste0("obs_", i), arr, envir = arr_env)
  }

  neurotabs::nf_register_backend(
    backend_name,
    resolve_fn = function(locator, selector, logical_schema) {
      arr <- get(locator, envir = arr_env, inherits = FALSE)
      array(as.vector(arr), dim = dim(arr))   # plain R array, no S4 dispatch
    },
    native_resolve_fn = function(locator, selector, logical_schema) {
      arr <- get(locator, envir = arr_env, inherits = FALSE)
      neuroim2::NeuroVol(arr, space = sp3)
    }
  )

  invisible(arr_env)
}

# Build a minimal nftab with one volumetric "bold" feature backed by the
# mock backend.  Mirrors the data pattern from make_toy_cluster_explorer_inputs().
make_toy_nftab <- function(n_obs = 4L, dims = c(5L, 5L, 5L),
                            backend_name = "mock_ce_array") {
  skip_if_not_installed("neurotabs")

  .register_mock_ce_backend(backend_name, dims, n_obs)

  vol_support <- neurotabs::nf_support_volume(
    support_id = "grid_toy",
    space      = "unknown",
    grid_id    = "grid_toy"
  )

  feat <- neurotabs::nf_feature(
    logical = neurotabs::nf_logical_schema(
      kind        = "volume",
      axes        = c("x", "y", "z"),
      dtype       = "float32",
      support_ref = "grid_toy",
      shape       = as.integer(dims)
    ),
    encodings = list(
      neurotabs::nf_ref_encoding(
        backend = backend_name,
        locator = neurotabs::nf_col("locator")
      )
    )
  )

  # Each (subject, condition) tuple must be unique for observation_axes
  subj <- paste0("s", rep(seq_len(ceiling(n_obs / 2)), each = 2L,
                          length.out = n_obs))
  cond <- rep(c("A", "B"), length.out = n_obs)
  obs <- data.frame(
    obs_id    = paste0("r", seq_len(n_obs)),
    subject   = subj,
    condition = cond,
    locator   = paste0("obs_", seq_len(n_obs)),
    stringsAsFactors = FALSE
  )

  m <- neurotabs::nf_manifest(
    dataset_id          = "test-ce",
    row_id              = "obs_id",
    observation_axes    = c("subject", "condition"),
    observation_columns = list(
      obs_id    = neurotabs::nf_col_schema("string"),
      subject   = neurotabs::nf_col_schema("string"),
      condition = neurotabs::nf_col_schema("string"),
      locator   = neurotabs::nf_col_schema("string")
    ),
    features = list(bold = feat),
    supports = list(grid_toy = vol_support)
  )

  neurotabs::nftab(manifest = m, observations = obs)
}

make_toy_nftab_from_table <- function(n_obs = 4L) {
  skip_if_not_installed("neurotabs")

  toy <- make_toy_cluster_explorer_inputs(n_time = n_obs)
  tmpdir <- tempfile("ce-neurotabs-adhoc-")
  dir.create(tmpdir, recursive = TRUE)

  dims <- as.integer(dim(toy$stat_map))[1:3]
  sp3 <- neuroim2::space(toy$stat_map)
  measure <- seq(-1.5, 1.5, length.out = n_obs)
  group <- rep(c("control", "patient"), length.out = n_obs)
  obs <- tibble::tibble(
    subject = sprintf("sub-%02d", seq_len(n_obs)),
    group = group,
    measure = measure,
    auc_path = file.path(subject, "maps", "AUC.nii.gz")
  )

  for (i in seq_len(n_obs)) {
    file_dir <- file.path(tmpdir, obs$subject[i], "maps")
    dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)
    arr <- array(stats::rnorm(prod(dims), sd = 0.05), dim = dims)
    arr[1:2, 1:2, 1:2] <- 0.4 + 0.8 * measure[i] +
      ifelse(group[i] == "patient", 0.7, -0.7)
    arr[4:5, 4:5, 4:5] <- 0.2 - 0.6 * measure[i]
    neuroim2::write_vol(
      neuroim2::NeuroVol(arr, space = sp3),
      file.path(tmpdir, obs$auc_path[i])
    )
  }

  ds <- neurotabs::nf_from_table(
    observations = obs,
    feature = "AUC",
    locator_col = "auc_path",
    axes = "subject",
    space = "MNI152NLin2009cAsym",
    dataset_id = "ce-adhoc-demo",
    root = tmpdir
  )

  list(ds = ds, toy = toy, root = tmpdir, observations = obs)
}

# -- Tests --------------------------------------------------------------------

test_that("nf_build_cluster_explorer_data collect strategy returns correct structure", {
  skip_if_not_installed("neurotabs")

  x   <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds  <- make_toy_nftab(n_obs = 4L)

  res <- suppressWarnings(
    nf_cluster_data(
      ds           = ds,
      stat_map     = x$stat_map,
      data_feature = "bold",
      atlas        = x$atlas,
      strategy     = "collect",
      threshold    = 3,
      min_cluster_size = 4
    )
  )

  expect_true(is.list(res))
  expect_true(all(c("cluster_table", "cluster_parcels", "cluster_ts",
                    "sample_table") %in% names(res)))
  expect_equal(nrow(res$sample_table), 4L)
  expect_true(nrow(res$cluster_table) >= 1L)
})

test_that("nf_build_cluster_explorer_data lazy strategy returns correct structure", {
  skip_if_not_installed("neurotabs")

  x  <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds <- make_toy_nftab(n_obs = 4L)

  res <- suppressWarnings(
    nf_cluster_data(
      ds           = ds,
      stat_map     = x$stat_map,
      data_feature = "bold",
      atlas        = x$atlas,
      strategy     = "lazy",
      threshold    = 3,
      min_cluster_size = 4
    )
  )

  expect_true(is.list(res))
  expect_true(all(c("cluster_table", "cluster_parcels", "cluster_ts",
                    "sample_table") %in% names(res)))
  expect_equal(nrow(res$sample_table), 4L)
  expect_true(nrow(res$cluster_table) >= 1L)
})

test_that("collect and lazy strategies agree on cluster count", {
  skip_if_not_installed("neurotabs")

  x  <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds <- make_toy_nftab(n_obs = 4L)

  res_c <- suppressWarnings(
    nf_cluster_data(
      ds = ds, stat_map = x$stat_map, data_feature = "bold",
      atlas = x$atlas, strategy = "collect",
      threshold = 3, min_cluster_size = 4
    )
  )
  res_l <- suppressWarnings(
    nf_cluster_data(
      ds = ds, stat_map = x$stat_map, data_feature = "bold",
      atlas = x$atlas, strategy = "lazy",
      threshold = 3, min_cluster_size = 4
    )
  )

  expect_equal(nrow(res_c$cluster_table), nrow(res_l$cluster_table))
  expect_setequal(res_c$cluster_table$cluster_id,
                  res_l$cluster_table$cluster_id)
})

test_that("nf_design(ds) is used as sample_table by default", {
  skip_if_not_installed("neurotabs")

  x  <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds <- make_toy_nftab(n_obs = 4L)

  res <- suppressWarnings(
    nf_cluster_data(
      ds = ds, stat_map = x$stat_map, data_feature = "bold",
      atlas = x$atlas, threshold = 3, min_cluster_size = 4
    )
  )

  design <- neurotabs::nf_design(ds)
  expect_equal(nrow(res$sample_table), nrow(design))
  # design columns are present (sample_index is added by cluster.explorer)
  expect_true(all(names(design) %in% names(res$sample_table)))
})

test_that("sample_tbl override is propagated correctly", {
  skip_if_not_installed("neurotabs")

  x      <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds     <- make_toy_nftab(n_obs = 4L)
  custom <- data.frame(group = c("ctrl", "trt", "ctrl", "trt"))

  res <- suppressWarnings(
    nf_cluster_data(
      ds = ds, stat_map = x$stat_map, data_feature = "bold",
      atlas = x$atlas, sample_tbl = custom,
      threshold = 3, min_cluster_size = 4
    )
  )

  expect_true("group" %in% names(res$sample_table))
})

test_that("nf_build_cluster_explorer_data errors on non-nftab input", {
  skip_if_not_installed("neurotabs")
  x <- make_toy_cluster_explorer_inputs(n_time = 4)
  expect_error(
    nf_cluster_data(
      ds = list(), stat_map = x$stat_map, data_feature = "bold",
      atlas = x$atlas
    ),
    "nftab"
  )
})

test_that("nf_build_cluster_explorer_data errors on unknown feature", {
  skip_if_not_installed("neurotabs")
  x  <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds <- make_toy_nftab(n_obs = 4L)
  expect_error(
    nf_cluster_data(
      ds = ds, stat_map = x$stat_map, data_feature = "no_such_feature",
      atlas = x$atlas
    ),
    "not found"
  )
})

test_that("nf_build_cluster_explorer_data errors on dimension mismatch", {
  skip_if_not_installed("neurotabs")

  # stat_map is 5x5x5 but nftab arrays will be 5x5x5 — force mismatch via a
  # separate stat_map with different dims
  ds <- make_toy_nftab(n_obs = 4L, dims = c(5L, 5L, 5L))

  # Build a 6x6x6 stat_map
  sp6 <- neuroim2::NeuroSpace(dim = c(6L, 6L, 6L), spacing = c(1, 1, 1))
  arr6 <- array(0, dim = c(6L, 6L, 6L))
  arr6[2:4, 2:4, 2:4] <- 4.5
  stat_map_6 <- neuroim2::NeuroVol(arr6, space = sp6)

  x <- make_toy_cluster_explorer_inputs(n_time = 4)

  expect_error(
    nf_cluster_data(
      ds = ds, stat_map = stat_map_6, data_feature = "bold",
      atlas = x$atlas
    ),
    "do not match stat_map"
  )
})

test_that("nf_build_cluster_explorer_data errors when passing reserved args", {
  skip_if_not_installed("neurotabs")
  x  <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds <- make_toy_nftab(n_obs = 4L)
  expect_error(
    nf_cluster_data(
      ds = ds, stat_map = x$stat_map, data_feature = "bold",
      atlas = x$atlas, sample_table = data.frame()
    ),
    "sample_table"
  )
})

test_that(".nf_make_series_fun closure returns correct matrix dimensions", {
  skip_if_not_installed("neurotabs")

  x  <- make_toy_cluster_explorer_inputs(n_time = 4)
  ds <- make_toy_nftab(n_obs = 4L)

  sfun <- neuromosaic:::.nf_make_series_fun("bold")

  # 3-voxel probe: two from the positive cluster, one from the negative
  vox_coords <- matrix(
    c(1L, 1L, 1L,
      2L, 2L, 2L,
      4L, 4L, 4L),
    ncol = 3, byrow = TRUE
  )

  mat <- sfun(ds, vox_coords)

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(4L, 3L))   # 4 observations, 3 voxels

  # voxels [1,1,1] and [2,2,2] are in the positive cluster — value = obs index i
  # voxel  [4,4,4] is in the negative cluster — value = 10 + i
  for (i in seq_len(4L)) {
    expect_equal(unname(mat[i, 1L]), as.numeric(i), tolerance = 1e-6)
    expect_equal(unname(mat[i, 2L]), as.numeric(i), tolerance = 1e-6)
    expect_equal(unname(mat[i, 3L]), as.numeric(10 + i), tolerance = 1e-6)
  }
})

test_that("nf_cluster_data works with nf_from_table one-file-per-row inputs", {
  skip_if_not_installed("neurotabs")

  adhoc <- make_toy_nftab_from_table(n_obs = 4L)
  on.exit(unlink(adhoc$root, recursive = TRUE), add = TRUE)

  res <- suppressWarnings(
    nf_cluster_data(
      ds = adhoc$ds,
      stat_map = adhoc$toy$stat_map,
      data_feature = "AUC",
      atlas = adhoc$toy$atlas,
      threshold = 3,
      min_cluster_size = 4
    )
  )

  expect_true(all(c("subject", "group", "measure") %in% names(res$sample_table)))
  expect_true(all(c("group", "measure") %in% names(res$cluster_ts)))
  expect_true(nrow(res$cluster_table) >= 1L)
})

test_that("nf_render_manifest adapts file-backed NFTab rows", {
  skip_if_not_installed("neurotabs")

  adhoc <- make_toy_nftab_from_table(n_obs = 3L)
  on.exit(unlink(adhoc$root, recursive = TRUE), add = TRUE)

  manifest <- nf_render_manifest(
    ds = adhoc$ds,
    data_feature = "AUC",
    threshold = 3,
    min_cluster_size = 3L
  )

  expect_s3_class(manifest, "data.frame")
  expect_equal(nrow(manifest), 3L)
  expect_true(all(file.exists(manifest$path)))
  expect_true(all(c("map_id", "label", "path", "stat_kind", "signed",
                    "threshold", "level", "n", "subjects") %in% names(manifest)))
  expect_identical(manifest$level, rep("subject", 3L))
  expect_equal(manifest$n, rep(1, 3L))
  expect_identical(manifest$subjects, adhoc$observations$subject)
  expect_true(all(grepl("^subject-", manifest$map_id)))
})

test_that("nf_render_manifest materializes backend-backed NFTab rows", {
  skip_if_not_installed("neurotabs")

  ds <- make_toy_nftab(n_obs = 2L, backend_name = "mock_ce_array_render")
  out_dir <- tempfile("nf-render-materialized-")

  manifest <- nf_render_manifest(
    ds = ds,
    data_feature = "bold",
    materialize_dir = out_dir,
    threshold = 3,
    min_cluster_size = 3L
  )

  expect_equal(nrow(manifest), 2L)
  expect_true(all(file.exists(manifest$path)))
  expect_true(all(dirname(manifest$path) == normalizePath(out_dir)))
  expect_s3_class(validate_manifest(manifest), "data.frame")
})

test_that("nf_render_manifest validates feature and path column inputs", {
  skip_if_not_installed("neurotabs")

  adhoc <- make_toy_nftab_from_table(n_obs = 2L)
  on.exit(unlink(adhoc$root, recursive = TRUE), add = TRUE)

  expect_error(
    nf_render_manifest(adhoc$ds, data_feature = "missing"),
    "not found"
  )
  expect_error(
    nf_render_manifest(adhoc$ds, data_feature = "AUC", path_col = "missing"),
    "path_col"
  )
})

test_that("nf_cluster_explorer constructs a shiny app from nf_from_table inputs", {
  skip_if_not_installed("neurotabs")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("ggiraph")

  adhoc <- make_toy_nftab_from_table(n_obs = 4L)
  on.exit(unlink(adhoc$root, recursive = TRUE), add = TRUE)

  app <- nf_cluster_explorer(
    ds = adhoc$ds,
    stat_map = adhoc$toy$stat_map,
    data_feature = "AUC",
    atlas = adhoc$toy$atlas,
    surfatlas = make_toy_surfatlas(),
    threshold = 3,
    min_cluster_size = 4
  )

  expect_s3_class(app, "shiny.appobj")
})

test_that("nf_cluster_explorer forwards custom plot plugins through ...", {
  skip_if_not_installed("neurotabs")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("ggiraph")

  adhoc <- make_toy_nftab_from_table(n_obs = 4L)
  on.exit(unlink(adhoc$root, recursive = TRUE), add = TRUE)

  custom_plot <- list(
    id = "custom_scatter",
    label = "Custom Scatter",
    render = function(data, params, context, interactive) {
      plot_data <- data
      plot_data$.x <- plot_data[[context$x_var]]
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .x, y = signal)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", se = FALSE)

      list(
        plot = p,
        diagnostics = list(status = "info", reason = "custom plot used")
      )
    }
  )

  app <- nf_cluster_explorer(
    ds = adhoc$ds,
    stat_map = adhoc$toy$stat_map,
    data_feature = "AUC",
    atlas = adhoc$toy$atlas,
    surfatlas = make_toy_surfatlas(),
    threshold = 3,
    min_cluster_size = 4,
    plot_plugins = list(custom_scatter = custom_plot),
    default_plot_plugin = "custom_scatter"
  )

  bridge <- new.env(parent = emptyenv())
  server_fn <- function(input, output, session) {
    bridge$rv <- app$serverFuncSource()(input, output, session)
  }

  shiny::testServer(server_fn, {
    session$setInputs(
      threshold = 3,
      min_cluster_size = 4L,
      connectivity = "26-connect",
      tail = "two_sided",
      prefetch_mode = TRUE,
      prefetch_max_clusters = 200L,
      prefetch_max_voxels = 100000L,
      map_scope = "all_clusters",
      display_mode = "dominant",
      brain_click_mode = "parcel",
      surface_pick_radius = 2,
      show_cluster_overlay = FALSE,
      overlay_threshold = 1e-06,
      overlay_alpha = 0.45,
      overlay_fun = "avg",
      overlay_space_ui = "auto",
      overlay_sampling = "midpoint",
      x_var = "measure",
      collapse_vars = character(0),
      analysis_plugin_id = "none",
      plot_plugin_id = "custom_scatter",
      apply_btn = 1L
    )

    rv <- bridge$rv
    expect_equal(rv$plot_state$applied_plugin_id, "custom_scatter")
    expect_equal(rv$signal_plot_payload()$diagnostics$reason, "custom plot used")
  })
})
