test_that("enrich_cluster_table adds MNI columns", {
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

  expect_true("peak_mni_x" %in% names(enriched))
  expect_true("peak_mni_y" %in% names(enriched))
  expect_true("peak_mni_z" %in% names(enriched))
  expect_true("atlas_label" %in% names(enriched))
  expect_true("hemisphere" %in% names(enriched))
})

test_that("enrich_cluster_table respects max_clusters", {
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
    atlas         = inputs$atlas,
    max_clusters  = 1
  )

  expect_true(nrow(enriched) <= 1)
})

test_that("enrich_cluster_table handles empty table", {
  empty_ct <- neuromosaic:::.empty_cluster_table()
  inputs <- make_toy_cluster_report_inputs()

  enriched <- enrich_cluster_table(
    cluster_table = empty_ct,
    stat_map      = inputs$stat_map,
    atlas         = inputs$atlas
  )

  expect_equal(nrow(enriched), 0)
  expect_true("peak_mni_x" %in% names(enriched))
})

test_that("format_mni_table returns gt object", {
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

  tbl <- format_mni_table(enriched, style = "gt")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("format_mni_table returns kable", {
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

  tbl <- format_mni_table(enriched, style = "kable")
  expect_true(!is.null(tbl))
})

test_that("format_mni_table returns NULL for empty table", {
  empty_ct <- neuromosaic:::.empty_cluster_table()
  empty_ct$peak_mni_x <- numeric(0)
  empty_ct$peak_mni_y <- numeric(0)
  empty_ct$peak_mni_z <- numeric(0)
  empty_ct$atlas_label <- character(0)
  empty_ct$hemisphere <- character(0)
  empty_ct$network <- character(0)

  tbl <- format_mni_table(empty_ct, style = "gt")
  expect_null(tbl)
})

test_that("format_mni_table includes mean_signal when present", {
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

  # The mni_table is already formatted; check via the cluster_table
  expect_true("mean_signal" %in% names(result$cluster_table))
  expect_true("sd_signal" %in% names(result$cluster_table))

  tbl <- format_mni_table(result$cluster_table, style = "kable")
  out <- paste(capture.output(print(tbl)), collapse = "\n")
  expect_match(out, "Mean Signal")
  expect_match(out, "SD Signal")
})
