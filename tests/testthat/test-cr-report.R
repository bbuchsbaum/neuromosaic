test_that("cluster_report returns cluster_report_result class", {
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

  expect_s3_class(result, "cluster_report_result")
  expect_true(is.list(result))
})

test_that("cluster_report result has all expected components", {
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

  expect_true("cluster_table" %in% names(result))
  expect_true("cluster_parcels" %in% names(result))
  expect_true("time_courses" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_true("mni_table" %in% names(result))
  expect_true("report_path" %in% names(result))
  expect_true("params" %in% names(result))
  expect_null(result$report_path)
})

test_that("cluster_report includes mean_signal in cluster_table", {
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

  expect_true("mean_signal" %in% names(result$cluster_table))
  expect_true("sd_signal" %in% names(result$cluster_table))
})

test_that("cluster_report generates brain slices when enabled", {
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
    brain_slices     = TRUE
  )

  expect_true("brain_slices" %in% names(result))
  if (nrow(result$cluster_table) > 0) {
    expect_true(is.list(result$brain_slices))
    expect_true(length(result$brain_slices) > 0)
  }
})

test_that("cluster_report validates inputs", {
  inputs <- make_toy_cluster_report_inputs()

  expect_error(
    cluster_report(stat_map = "nonexistent.nii.gz",
                   data_source = inputs$data_vec,
                   atlas = inputs$atlas,
                   design = inputs$design),
    regexp = NULL
  )

  expect_error(
    cluster_report(stat_map = inputs$stat_map,
                   data_source = inputs$data_vec,
                   atlas = inputs$atlas,
                   design = inputs$design,
                   threshold = -1),
    "positive"
  )

  expect_error(
    cluster_report(stat_map = inputs$stat_map,
                   data_source = inputs$data_vec,
                   atlas = inputs$atlas,
                   output_file = NULL,
                   brain_slices = FALSE),
    "design.*required"
  )
})

test_that("cluster_report handles high threshold (no clusters)", {
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

  expect_s3_class(result, "cluster_report_result")
  expect_equal(nrow(result$cluster_table), 0)
})

test_that("cluster_report accepts bare formula", {
  inputs <- make_toy_cluster_report_inputs()
  result <- cluster_report(
    stat_map         = inputs$stat_map,
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    design           = inputs$design,
    threshold        = 3.0,
    formulas         = value ~ time,
    output_file      = NULL,
    min_cluster_size = 3,
    brain_slices     = FALSE
  )

  expect_s3_class(result, "cluster_report_result")
  expect_true(length(result$plots) > 0)
})

test_that("cluster_report supports stat-map-only table reports", {
  inputs <- make_toy_cluster_report_inputs()
  result <- suppressWarnings(
    cluster_report(
      stat_map = inputs$stat_map,
      data_source = NULL,
      atlas = inputs$atlas,
      output_file = NULL,
      threshold = 3.0,
      min_cluster_size = 3,
      brain_slices = FALSE
    )
  )

  expect_s3_class(result, "cluster_report_result")
  expect_gt(nrow(result$cluster_table), 0)
  expect_equal(nrow(result$time_courses), 0)
  expect_length(result$plots, 0)
  expect_equal(result$params$report_mode, "table_only")
})

test_that("cluster_report normalizes empty prefetched signal tables to value schema", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )

  inputs <- make_toy_cluster_report_inputs()
  cluster_tbl <- tibble::tibble(
    cluster_id = "P1",
    n_voxels = 8L,
    max_stat = 4.5,
    peak_mni_x = 0,
    peak_mni_y = 0,
    peak_mni_z = 0,
    network = NA_character_
  )

  testthat::local_mocked_bindings(
    build_cluster_explorer_data = function(...) {
      list(
        cluster_table = cluster_tbl,
        cluster_parcels = tibble::tibble(),
        cluster_ts = tibble::tibble(
          cluster_id = character(0),
          .sample_index = integer(0),
          signal = numeric(0)
        )
      )
    },
    enrich_cluster_table = function(...) cluster_tbl,
    format_mni_table = function(...) tibble::tibble(),
    .package = "neuromosaic"
  )

  result <- cluster_report(
    stat_map = inputs$stat_map,
    data_source = inputs$data_vec,
    atlas = inputs$atlas,
    design = inputs$design,
    threshold = 3.0,
    formulas = list("Test" = value ~ condition * time),
    output_file = NULL,
    min_cluster_size = 3,
    brain_slices = FALSE
  )

  expect_true("value" %in% names(result$time_courses))
  expect_false("signal" %in% names(result$time_courses))
  expect_true(all(c("condition", "time") %in% names(result$time_courses)))
  expect_equal(nrow(result$time_courses), 0)
})
