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

write_cli_config <- function(path, command, options = list(), subcommand = NULL) {
  yaml::write_yaml(
    Filter(Negate(is.null), list(
      command = command,
      subcommand = subcommand,
      options = options
    )),
    file = path
  )
  path
}

make_cli_montage_fixture <- function(include_label = TRUE) {
  toy <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("neuromosaic-montage-cli-")
  dir.create(tmpdir, recursive = TRUE)

  stat_map_path <- file.path(tmpdir, "stat-map.nii.gz")
  neuroim2::write_vol(toy$stat_map, stat_map_path)

  manifest <- data.frame(
    map_id = "contrast_a_model_1",
    path = stat_map_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    tail = "two_sided",
    connectivity = "18-connect",
    min_cluster_size = 3,
    contrast = "contrast-a",
    model = "model-1",
    description = "A montage CLI panel.",
    n = 1,
    subjects = "sub-01, sub-02",
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_label)) {
    manifest$label <- "Contrast A - model 1"
  }

  manifest_path <- file.path(tmpdir, "render-manifest.tsv")
  utils::write.table(
    manifest,
    file = manifest_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  labels <- data.frame(
    map_id = "contrast_a_model_1",
    label = "Contrast A - model 1",
    description = "Label-table description.",
    stringsAsFactors = FALSE
  )
  labels_path <- file.path(tmpdir, "labels.tsv")
  utils::write.table(
    labels,
    file = labels_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  surfatlas_path <- file.path(tmpdir, "surfatlas.rds")
  saveRDS(make_toy_surfatlas(), surfatlas_path)

  atlas_path <- file.path(tmpdir, "atlas.rds")
  saveRDS(toy$atlas, atlas_path)

  list(
    root = tmpdir,
    stat_map_path = stat_map_path,
    atlas_path = atlas_path,
    manifest_path = manifest_path,
    labels_path = labels_path,
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

test_that("cli help exposes the montage report dispatch contract", {
  report_help <- .cli_help_text("report")
  montage_help <- .cli_help_text("report", style = "montage")
  explore_help <- .cli_help_text("explore")

  expect_match(report_help, "--style montage", fixed = TRUE)
  expect_match(montage_help, "--render-manifest", fixed = TRUE)
  expect_match(montage_help, "do not require --stat-map", fixed = TRUE)
  expect_match(montage_help, "validate_manifest", fixed = TRUE)
  expect_match(montage_help, "--cache-dir", fixed = TRUE)
  expect_match(montage_help, "--no-cache-surface", fixed = TRUE)
  expect_match(explore_help, "--style montage", fixed = TRUE)
  expect_match(explore_help, "--render-manifest", fixed = TRUE)
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

test_that("cli_main prepares montage report specs without a stat map", {
  fixture <- make_cli_montage_fixture()
  out_path <- file.path(fixture$root, "montage-report.html")

  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--render-manifest", fixture$manifest_path,
      "--layout", "contrast/model",
      "--out", out_path
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "montage_report")
  expect_equal(spec$style, "montage")
  expect_equal(spec$args$output_file, out_path)
  expect_equal(nrow(spec$args$manifest), 1)
  expect_identical(spec$args$layout, c("contrast", "model"))
  expect_false("stat_map" %in% names(spec$args))
  expect_false(spec$args$render_peaks)
  expect_true(spec$args$materialize_recipes)
  expect_true(spec$args$cache_surface)
})

test_that("cli_main prepares montage explorer specs without a stat map", {
  fixture <- make_cli_montage_fixture()
  cache_dir <- file.path(fixture$root, "derived-cache")

  spec <- cli_main(
    c(
      "explore",
      "--style", "montage",
      "--render-manifest", fixture$manifest_path,
      "--layout", "contrast/model",
      "--cache-dir", cache_dir,
      "--no-cache-surface"
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "montage_explore")
  expect_equal(spec$style, "montage")
  expect_equal(spec$manifest_source, "render_manifest")
  expect_equal(nrow(spec$args$manifest), 1L)
  expect_identical(spec$args$layout, c("contrast", "model"))
  expect_equal(spec$args$cache_dir, cache_dir)
  expect_false(spec$args$cache_surface)
  expect_false("stat_map" %in% names(spec$args))
})

test_that("cli_main prepares montage surface report specs", {
  fixture <- make_cli_montage_fixture()
  out_path <- file.path(fixture$root, "montage-surface-report.html")

  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--render-manifest", fixture$manifest_path,
      "--surface",
      "--surfatlas", fixture$surfatlas_path,
      "--atlas", fixture$atlas_path,
      "--out", out_path
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "montage_report")
  expect_true(spec$args$render_surface)
  expect_false(spec$args$render_volume)
  expect_true(spec$args$render_peaks)
  expect_s3_class(spec$args$surfatlas, "surfatlas")
  expect_s3_class(spec$args$atlas, "atlas")
  expect_equal(spec$args$output_file, out_path)

  expect_error(
    cli_main(
      c(
        "report",
        "--style", "montage",
        "--render-manifest", fixture$manifest_path,
        "--surface"
      ),
      execute = FALSE
    ),
    "Missing required option '--surfatlas'"
  )
})

test_that("cli_main builds montage specs from design and path template", {
  fixture <- make_cli_montage_fixture()
  design_path <- file.path(fixture$root, "montage-design.tsv")
  design <- data.frame(
    map_id = "design_map",
    contrast = "faces",
    model = "m1",
    label = "Design-built map",
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    map_file = basename(fixture$stat_map_path),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    design,
    file = design_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--design", design_path,
      "--path-template", "{map_file}",
      "--root", fixture$root,
      "--layout", "contrast/model"
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "montage_report")
  expect_equal(spec$manifest_source, "design_path_template")
  expect_identical(spec$args$manifest$map_id, "design_map")
  expect_identical(spec$args$manifest$label, "Design-built map")
  expect_true(file.exists(spec$args$manifest$path))
})

test_that("cli_main can source montage labels from a sidecar table", {
  fixture <- make_cli_montage_fixture(include_label = FALSE)

  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--render-manifest", fixture$manifest_path,
      "--labels", fixture$labels_path
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "montage_report")
  expect_identical(spec$args$manifest$label, "Contrast A - model 1")
  expect_identical(spec$args$manifest$description, "Label-table description.")
})

test_that("cli_main sources montage narrative from intro/section/interlude files", {
  fixture <- make_cli_montage_fixture()

  intro_path <- file.path(fixture$root, "intro.md")
  writeLines(c("Report **preamble**.", "", "Second paragraph."), intro_path)

  section_path <- file.path(fixture$root, "section-notes.csv")
  utils::write.csv(
    data.frame(
      contrast = "contrast-a",
      model = "",
      text = "Contrast A framing.",
      stringsAsFactors = FALSE
    ),
    section_path,
    row.names = FALSE
  )

  interlude_path <- file.path(fixture$root, "interludes.csv")
  utils::write.csv(
    data.frame(
      map_id = "contrast_a_model_1",
      position = "after",
      text = "Closing note.",
      stringsAsFactors = FALSE
    ),
    interlude_path,
    row.names = FALSE
  )

  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--render-manifest", fixture$manifest_path,
      "--layout", "contrast/model",
      "--intro", intro_path,
      "--section-notes", section_path,
      "--interludes", interlude_path
    ),
    execute = FALSE
  )

  expect_match(spec$args$intro, "Report **preamble**.", fixed = TRUE)
  expect_identical(spec$args$section_notes$text, "Contrast A framing.")
  expect_identical(spec$args$interludes$map_id, "contrast_a_model_1")
  expect_identical(spec$args$interludes$position, "after")
})

test_that("cli_main reads YAML config files and allows CLI overrides", {
  fixture <- make_cli_fixture()
  config_path <- file.path(fixture$root, "report.yml")
  config_out <- file.path(fixture$root, "from-config.qmd")
  override_out <- file.path(fixture$root, "override.qmd")

  write_cli_config(
    config_path,
    command = "report",
    options = list(
      stat_map = fixture$stat_map_path,
      atlas = fixture$atlas_path,
      out = config_out,
      brain_slices = FALSE
    )
  )

  spec <- cli_main(c("report", "--config", config_path), execute = FALSE)
  expect_equal(spec$args$output_file, config_out)
  expect_false(spec$args$brain_slices)

  spec_override <- cli_main(
    c("report", "--config", config_path, "--out", override_out),
    execute = FALSE
  )
  expect_equal(spec_override$args$output_file, override_out)
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

test_that("cli_main dry-run and validate resolve specs without executing", {
  fixture <- make_cli_fixture()
  config_path <- file.path(fixture$root, "report.yml")
  out_path <- file.path(fixture$root, "dry-run-report.html")

  write_cli_config(
    config_path,
    command = "report",
    options = list(
      stat_map = fixture$stat_map_path,
      atlas = fixture$atlas_path,
      out = out_path,
      brain_slices = FALSE
    )
  )

  dry_output <- capture.output(
    dry_spec <- cli_main(c("report", "--config", config_path, "--dry-run"), execute = TRUE)
  )
  expect_equal(dry_spec$type, "report")
  expect_true(any(grepl("^Dry run: report$", dry_output)))
  expect_false(file.exists(out_path))

  validate_output <- capture.output(
    validated_spec <- cli_main(c("report", "--config", config_path, "--validate"), execute = TRUE)
  )
  expect_equal(validated_spec$type, "report")
  expect_true(any(grepl("^Validation OK: report$", validate_output)))
  expect_false(file.exists(out_path))
})

test_that("cli_main montage validate runs manifest QC without rendering", {
  fixture <- make_cli_montage_fixture()
  out_path <- file.path(fixture$root, "montage-validate.html")

  validate_output <- capture.output(
    validated_spec <- cli_main(
      c(
        "report",
        "--style", "montage",
        "--render-manifest", fixture$manifest_path,
        "--out", out_path,
        "--validate"
      ),
      execute = TRUE
    )
  )

  expect_equal(validated_spec$type, "montage_report")
  expect_true(any(grepl("^Validation OK: report$", validate_output)))
  expect_true(any(grepl("Spec type: montage_report", validate_output, fixed = TRUE)))
  expect_false(file.exists(out_path))

  direct_out <- file.path(fixture$root, "direct-executor-validate.html")
  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--render-manifest", fixture$manifest_path,
      "--out", direct_out,
      "--validate"
    ),
    execute = FALSE
  )
  direct_result <- .cli_execute_command(spec)
  expect_equal(direct_result$type, "montage_report")
  expect_false(file.exists(direct_out))
})

test_that("cli_main montage validate fails loudly on label-less manifests", {
  fixture <- make_cli_montage_fixture()
  bad <- utils::read.delim(fixture$manifest_path, stringsAsFactors = FALSE,
                           check.names = FALSE)
  bad$label <- ""
  bad_path <- file.path(fixture$root, "bad-render-manifest.tsv")
  utils::write.table(
    bad,
    file = bad_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  expect_error(
    cli_main(
      c(
        "report",
        "--style", "montage",
        "--render-manifest", bad_path,
        "--validate"
      ),
      execute = TRUE
    ),
    "non-empty 'label'"
  )
})

test_that("cli_main montage validate fails loudly on empty and grid-mismatched manifests", {
  fixture <- make_cli_montage_fixture()

  empty <- utils::read.delim(fixture$manifest_path, stringsAsFactors = FALSE,
                             check.names = FALSE)
  empty$threshold <- 100
  empty_path <- file.path(fixture$root, "empty-render-manifest.tsv")
  utils::write.table(
    empty,
    file = empty_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  expect_error(
    cli_main(
      c(
        "report",
        "--style", "montage",
        "--render-manifest", empty_path,
        "--validate"
      ),
      execute = TRUE
    ),
    "No finite suprathreshold voxels"
  )

  mismatch_space <- neuroim2::NeuroSpace(
    dim = c(6, 6, 6),
    spacing = c(2, 2, 2),
    origin = c(-5, -5, -5)
  )
  mismatch_arr <- array(0, dim = c(6, 6, 6))
  mismatch_arr[2, 2, 2] <- 4
  mismatch_path <- file.path(fixture$root, "mismatch-stat-map.nii.gz")
  neuroim2::write_vol(neuroim2::NeuroVol(mismatch_arr, mismatch_space),
                      mismatch_path)

  mismatch <- empty
  mismatch$path <- mismatch_path
  mismatch$threshold <- 3
  mismatch_manifest_path <- file.path(fixture$root, "mismatch-render-manifest.tsv")
  utils::write.table(
    mismatch,
    file = mismatch_manifest_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  expect_error(
    cli_main(
      c(
        "report",
        "--style", "montage",
        "--render-manifest", mismatch_manifest_path,
        "--background", fixture$stat_map_path,
        "--validate"
      ),
      execute = TRUE
    ),
    "Grid mismatch"
  )
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

test_that("cli_main prepares montage specs from nftab manifests", {
  skip_if_not_installed("neurotabs")

  fixture <- make_cli_fixture()
  out_dir <- file.path(fixture$root, "nftab-montage-out")

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
  out_path <- file.path(fixture$root, "nftab-montage.qmd")
  spec <- cli_main(
    c(
      "report",
      "--style", "montage",
      "--manifest", manifest_path,
      "--feature", "AUC",
      "--root", fixture$root,
      "--threshold", "3",
      "--out", out_path
    ),
    execute = FALSE
  )

  expect_equal(spec$type, "montage_report")
  expect_equal(spec$manifest_source, "nftab_manifest")
  expect_equal(nrow(spec$args$manifest), 4L)
  expect_true(all(file.exists(spec$args$manifest$path)))
  expect_true(all(grepl("^subject-", spec$args$manifest$map_id)))
  expect_identical(spec$args$manifest$n, rep(1, 4L))
  expect_equal(spec$args$output_file, out_path)
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

test_that("cli_main montage report writes qmd source without stat-map", {
  fixture <- make_cli_montage_fixture()
  out_path <- file.path(fixture$root, "montage-report.qmd")

  result <- suppressWarnings(
    cli_main(
      c(
        "report",
        "--style", "montage",
        "--render-manifest", fixture$manifest_path,
        "--background", fixture$stat_map_path,
        "--atlas", fixture$atlas_path,
        "--layout", "contrast/model",
        "--out", out_path,
        "--title", "CLI Montage"
      ),
      execute = TRUE
    )
  )

  expect_true(file.exists(result))
  expect_true(file.exists(sub("\\.qmd$", "_report-data.rds", result)))

  qmd <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_match(qmd, "expects the sidecar file", fixed = TRUE)
  sidecar <- readRDS(sub("\\.qmd$", "_report-data.rds", result))
  expect_identical(sidecar$params$title, "CLI Montage")
  expect_true(file.exists(sidecar$panels$contrast_a_model_1$volume_image))
  expect_gt(sidecar$panels$contrast_a_model_1$volume$n_suprathreshold, 0)
  expect_true("atlas_label" %in% names(sidecar$panels$contrast_a_model_1$peak_table))
  expect_gt(nrow(sidecar$panels$contrast_a_model_1$peak_table), 0)
  expect_equal(sidecar$qc$effective_n[[1]], 1)
  expect_equal(sidecar$qc$source_n[[1]], 2)
  expect_equal(sidecar$qc$dropped_n[[1]], 1)
  expect_true(sidecar$qc$has_dropped_subjects[[1]])
})

test_that("cli formula parser rejects dangerous calls", {
  expect_error(
    .cli_parse_formulas("value ~ system('whoami')"),
    "disallowed call 'system'",
    class = "neuromosaic_error_unsafe_formula"
  )

  expect_error(
    .cli_parse_formulas("value ~ base::system('whoami')"),
    "disallowed call 'base::system'",
    class = "neuromosaic_error_unsafe_formula"
  )
})

test_that("cli atlas loaders validate deserialized object classes", {
  fixture <- make_cli_fixture()

  bad_atlas_path <- file.path(fixture$root, "bad-atlas.rds")
  saveRDS(list(not = "an atlas"), bad_atlas_path)
  expect_error(
    .cli_load_atlas(bad_atlas_path),
    "did not contain a valid atlas object",
    class = "neuromosaic_error_invalid_atlas"
  )

  bad_surfatlas_path <- file.path(fixture$root, "bad-surfatlas.rds")
  saveRDS(list(not = "a surfatlas"), bad_surfatlas_path)
  expect_error(
    .cli_load_surfatlas(bad_surfatlas_path),
    "did not contain a valid surfatlas object",
    class = "neuromosaic_error_invalid_surfatlas"
  )
})

test_that("cli_main reports config errors with a specific class", {
  expect_error(
    cli_main(c("report", "--config", tempfile("missing-", fileext = ".yml")), execute = FALSE),
    class = "neuromosaic_error_cli_config"
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
