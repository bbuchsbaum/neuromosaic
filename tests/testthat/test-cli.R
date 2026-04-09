make_cli_fixture <- function(n_obs = 4L) {
  skip_if_not_installed("neurotabs")

  toy <- make_toy_cluster_explorer_inputs(n_time = n_obs)
  tmpdir <- tempfile("neuromosaic-cli-")
  dir.create(tmpdir, recursive = TRUE)

  dims <- as.integer(dim(toy$stat_map))[1:3]
  sp3 <- neuroim2::space(toy$stat_map)
  subject <- sprintf("sub-%02d", seq_len(n_obs))
  measure <- seq(-1.5, 1.5, length.out = n_obs)
  group <- rep(c("control", "patient"), length.out = n_obs)

  design <- data.frame(
    subject = subject,
    group = group,
    measure = measure,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_obs)) {
    file_dir <- file.path(tmpdir, subject[i], "maps")
    dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)

    arr <- array(stats::rnorm(prod(dims), sd = 0.05), dim = dims)
    arr[1:2, 1:2, 1:2] <- 0.4 + 0.8 * measure[i] +
      ifelse(group[i] == "patient", 0.7, -0.7)
    arr[4:5, 4:5, 4:5] <- 0.2 - 0.6 * measure[i]

    neuroim2::write_vol(
      neuroim2::NeuroVol(arr, space = sp3),
      file.path(file_dir, "AUC.nii.gz")
    )
  }

  design_path <- file.path(tmpdir, "design.tsv")
  utils::write.table(
    design,
    file = design_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  stat_map_path <- file.path(tmpdir, "stat_map.nii.gz")
  neuroim2::write_vol(toy$stat_map, stat_map_path)

  atlas_path <- file.path(tmpdir, "atlas.rds")
  saveRDS(toy$atlas, atlas_path)

  surfatlas_path <- file.path(tmpdir, "surfatlas.rds")
  saveRDS(make_toy_surfatlas(), surfatlas_path)

  list(
    root = tmpdir,
    design_path = design_path,
    stat_map_path = stat_map_path,
    atlas_path = atlas_path,
    surfatlas_path = surfatlas_path
  )
}

test_that("formula_plot_plugin maps formula terms onto explorer plot controls", {
  data <- tibble::tibble(
    cluster_id = rep(c("pos_1", "neg_1"), each = 4),
    signal = c(0.2, 0.4, 0.6, 0.8, 1.1, 1.4, 1.7, 2.0),
    measure = rep(c(-1, 0, 1, 2), times = 2),
    group = rep(c("control", "patient"), times = 4),
    condition = rep(c("pre", "post"), each = 2, times = 2)
  )

  mapping <- .resolve_formula_plot_mapping(
    stats::as.formula("AUC ~ condition + measure + group"),
    data
  )
  expect_equal(mapping$x_var, "measure")
  expect_equal(mapping$color_var, "group")
  expect_equal(mapping$facet_var, "condition")

  plugin <- formula_plot_plugin("AUC ~ condition + measure + group")
  out <- .run_plot_plugin(plugin, data = data, interactive = FALSE)

  expect_s3_class(out$plot, "ggplot")
  expect_null(out$diagnostics)
  expect_s3_class(out$plot$facet, "FacetGrid")
})

test_that("formula_plot_plugin reports missing columns cleanly", {
  data <- tibble::tibble(
    cluster_id = "pos_1",
    signal = 1,
    measure = 2
  )

  plugin <- formula_plot_plugin("AUC ~ visit + session")
  out <- .run_plot_plugin(plugin, data = data, interactive = FALSE)

  expect_match(out$diagnostics$reason, "does not reference any columns")
  expect_true(is.list(out$meta))
  expect_true(isTRUE(out$meta$failed))
})

test_that("cli atlas parser accepts friendly atlas aliases", {
  parsed <- .cli_parse_atlas_spec("Schaefer400")
  expect_identical(parsed$kind, "schaefer")
  expect_identical(parsed$parcels, "400")
  expect_identical(parsed$networks, "7")

  parsed17 <- .cli_parse_atlas_spec("Schaefer400x17")
  expect_identical(parsed17$kind, "schaefer")
  expect_identical(parsed17$parcels, "400")
  expect_identical(parsed17$networks, "17")

  expect_match(.cli_help_text("report"), "Schaefer400")
  expect_match(.cli_help_text("report"), "Glasser")
})

test_that("cli_main prepares report specs from ad hoc tables", {
  fixture <- make_cli_fixture()
  out_path <- file.path(fixture$root, "report.html")

  spec <- cli_main(
    c(
      "report",
      "--design", fixture$design_path,
      "--feature", "AUC",
      "--path-template", "{subject}/maps/AUC.nii.gz",
      "--stat-map", fixture$stat_map_path,
      "--atlas", fixture$atlas_path,
      "--formula", "Trend::AUC ~ measure + group",
      "--out", out_path,
      "--no-brain-slices"
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "report")
  expect_equal(spec$args$output_file, out_path)
  expect_true(methods::is(spec$args$data_source, "NeuroVec"))
  expect_false("series_fun" %in% names(spec$args))
  expect_identical(names(spec$args$formulas), "Trend")
})

test_that("cli_main prepares stat-map-only report specs without feature input", {
  fixture <- make_cli_fixture()
  out_path <- file.path(fixture$root, "table-report.pdf")

  spec <- cli_main(
    c(
      "report",
      "--stat-map", fixture$stat_map_path,
      "--atlas", fixture$atlas_path,
      "--out", out_path,
      "--no-brain-slices"
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "report")
  expect_equal(spec$mode, "stat_only")
  expect_null(spec$args$data_source)
  expect_equal(spec$args$output_file, out_path)
})

test_that("cli_main prepares explore specs with formula-driven plugins", {
  fixture <- make_cli_fixture()

  spec <- cli_main(
    c(
      "explore",
      "--design", fixture$design_path,
      "--feature", "AUC",
      "--path-template", "{subject}/maps/AUC.nii.gz",
      "--stat-map", fixture$stat_map_path,
      "--atlas", fixture$atlas_path,
      "--surfatlas", fixture$surfatlas_path,
      "--plot-formula", "AUC ~ measure + group"
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "explore")
  expect_equal(spec$args$default_plot_plugin, "formula")
  expect_true("formula" %in% names(spec$args$plot_plugins))
  expect_true(is.function(spec$args$series_fun))
})

test_that("cli_main manifest create writes a manifest-backed dataset", {
  fixture <- make_cli_fixture()
  out_dir <- file.path(fixture$root, "nftab-out")

  cli_main(
    c(
      "manifest", "create",
      "--design", fixture$design_path,
      "--feature", "AUC",
      "--path-template", "{subject}/maps/AUC.nii.gz",
      "--axes", "subject",
      "--space", "MNI152NLin2009cAsym",
      "--out", out_dir
    ),
    execute = TRUE
  )

  manifest_path <- file.path(out_dir, "nftab.yaml")
  expect_true(file.exists(manifest_path))

  ds <- neurotabs::nf_read(manifest_path)
  expect_identical(neurotabs::nf_feature_names(ds), "AUC")
  expect_true("subject" %in% neurotabs::nf_axes(ds))
})

test_that("cli_main extract writes CSV outputs from ad hoc nftab inputs", {
  fixture <- make_cli_fixture()
  out_dir <- file.path(fixture$root, "extract-out")

  result <- suppressWarnings(
    cli_main(
      c(
        "extract",
        "--design", fixture$design_path,
        "--feature", "AUC",
        "--path-template", "{subject}/maps/AUC.nii.gz",
        "--stat-map", fixture$stat_map_path,
        "--atlas", fixture$atlas_path,
        "--formula", "AUC ~ measure + group",
        "--min-cluster-size", "3",
        "--dir", out_dir,
        "--prefix", "auc-demo",
        "--no-brain-slices"
      ),
      execute = TRUE
    )
  )

  expect_true(all(file.exists(result$paths)))
  expect_true(any(grepl("_clusters\\.csv$", result$paths)))
  expect_true(any(grepl("_parcels\\.csv$", result$paths)))
  expect_true(any(grepl("_timecourses\\.csv$", result$paths)))
})

test_that("cli_main report writes qmd source for stat-map-only reports", {
  fixture <- make_cli_fixture()
  out_path <- file.path(fixture$root, "stat-only-report.qmd")

  result <- suppressWarnings(
    cli_main(
      c(
        "report",
        "--stat-map", fixture$stat_map_path,
        "--atlas", fixture$atlas_path,
        "--out", out_path,
        "--no-brain-slices"
      ),
      execute = TRUE
    )
  )

  expect_true(file.exists(result$report_path))
  expect_true(file.exists(sub("\\.qmd$", "_report-data.rds", result$report_path)))
})

test_that("cli formula parser rejects dangerous calls", {
  expect_error(
    .cli_parse_formulas("value ~ system('whoami')"),
    "disallowed call 'system'"
  )

  expect_error(
    .cli_parse_formulas("value ~ base::system('whoami')"),
    "disallowed call 'base::system'"
  )
})

test_that("cli atlas loaders validate deserialized object classes", {
  fixture <- make_cli_fixture()

  bad_atlas_path <- file.path(fixture$root, "bad-atlas.rds")
  saveRDS(list(not = "an atlas"), bad_atlas_path)
  expect_error(
    .cli_load_atlas(bad_atlas_path),
    "did not contain a valid atlas object"
  )

  bad_surfatlas_path <- file.path(fixture$root, "bad-surfatlas.rds")
  saveRDS(list(not = "a surfatlas"), bad_surfatlas_path)
  expect_error(
    .cli_load_surfatlas(bad_surfatlas_path),
    "did not contain a valid surfatlas object"
  )
})

test_that("cli report formula normalization warns on unsupported left-hand sides", {
  expect_warning(
    out <- .cli_normalize_report_formulas(
      list(stats::as.formula("log(AUC) ~ measure + group")),
      feature_name = "AUC"
    ),
    "Leaving it unchanged; it may fail later"
  )
  expect_identical(
    paste(deparse(out[[1]]), collapse = " "),
    "log(AUC) ~ measure + group"
  )

  expect_no_warning(
    normalized <- .cli_normalize_report_formulas(
      list(stats::as.formula("AUC ~ measure + group")),
      feature_name = "AUC"
    )
  )
  expect_identical(
    paste(deparse(normalized[[1]]), collapse = " "),
    "value ~ measure + group"
  )

  expect_no_warning(
    passthrough_value <- .cli_normalize_report_formulas(
      list(stats::as.formula("value ~ measure + group")),
      feature_name = "AUC"
    )
  )
  expect_identical(
    paste(deparse(passthrough_value[[1]]), collapse = " "),
    "value ~ measure + group"
  )

  expect_no_warning(
    one_sided <- .cli_normalize_report_formulas(
      list(stats::as.formula("~ measure + group")),
      feature_name = "AUC"
    )
  )
  expect_null(rlang::f_lhs(one_sided[[1]]))
  expect_identical(
    paste(deparse(rlang::f_rhs(one_sided[[1]])), collapse = " "),
    "measure + group"
  )
})
