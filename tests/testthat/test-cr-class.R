test_that("print.cluster_report_result runs without error", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  expect_output(print(result), "Cluster Report")
  expect_invisible(print(result))
})

test_that("summary.cluster_report_result runs without error", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  expect_output(summary(result), "Cluster Report Summary")
  expect_output(summary(result), "Generated:")
})

test_that("plot.cluster_report_result returns ggplot", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  if (nrow(result$cluster_table) > 0) {
    p <- plot(result, which = 1)
    expect_s3_class(p, "gg")
  }
})

test_that("plot.cluster_report_result errors on bad formula name", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  expect_error(plot(result, formula_name = "nonexistent"), "not found")
})

test_that("$ extraction works (backward compat)", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  expect_true(is.data.frame(result$cluster_table))
  expect_true(is.list(result$plots))
  expect_true(is.list(result$params))
})

test_that("export_csv writes expected files", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  tmp_dir <- tempfile("csv_test")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  paths <- export_csv(result, dir = tmp_dir)
  expect_true(length(paths) >= 1)
  expect_true(all(file.exists(paths)))

  # Cluster table CSV should always exist
  expect_true(any(grepl("clusters\\.csv$", paths)))
  expect_true(any(grepl("provenance\\.yml$", paths)))
  prov_path <- paths[grepl("provenance\\.yml$", paths)][1]
  prov <- yaml::read_yaml(prov_path)
  expect_identical(prov$package, "neuromosaic")
  expect_identical(prov$report_mode, "full")
})

test_that("print works for zero-cluster result", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 100,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  expect_output(print(result), "0 clusters")
})

test_that("cluster_report_result stores provenance metadata", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = list("Test" = value ~ condition * time),
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE,
    provenance       = list(source = "unit-test")
  )

  expect_identical(result$provenance$package, "neuromosaic")
  expect_identical(result$provenance$report_mode, "full")
  expect_identical(result$provenance$source, "unit-test")
  expect_true("Test" %in% names(result$provenance$formulas))
})
