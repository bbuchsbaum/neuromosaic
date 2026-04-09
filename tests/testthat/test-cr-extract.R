test_that("extract_cluster_data.NeuroVec returns tibble with value column", {
  inputs <- make_toy_cluster_explorer_inputs(n_time = 8)
  cluster_data <- build_cluster_explorer_data(
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    stat_map         = inputs$stat_map,
    sample_table     = inputs$sample_table,
    threshold        = 3.0,
    min_cluster_size = 3
  )

  design <- data.frame(
    condition = rep(c("A", "B"), each = 4),
    time = rep(1:4, times = 2)
  )

  tc <- extract_cluster_data(
    data_source  = inputs$data_vec,
    cluster_info = cluster_data,
    design       = design
  )

  expect_s3_class(tc, "tbl_df")
  expect_true("value" %in% names(tc))
  expect_true("cluster_id" %in% names(tc))
  expect_true(".sample_index" %in% names(tc))
})

test_that("extract_cluster_data.list works with list of NeuroVol", {
  inputs <- make_toy_cluster_explorer_inputs(n_time = 4)
  cluster_data <- build_cluster_explorer_data(
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    stat_map         = inputs$stat_map,
    sample_table     = inputs$sample_table,
    threshold        = 3.0,
    min_cluster_size = 3
  )

  # Create list of NeuroVol from the 4D NeuroVec
  vol_list <- lapply(1:4, function(i) inputs$stat_map)  # reuse stat_map as proxy

  design <- data.frame(condition = c("A", "A", "B", "B"), time = 1:4)

  tc <- extract_cluster_data(
    data_source  = vol_list,
    cluster_info = cluster_data,
    design       = design
  )

  expect_s3_class(tc, "tbl_df")
  expect_true("value" %in% names(tc))
  expect_true("condition" %in% names(tc))
})

test_that("extract_cluster_data returns empty tibble for empty clusters", {
  inputs <- make_toy_cluster_explorer_inputs(n_time = 4)

  # Construct cluster_info with empty voxels
  empty_info <- list(
    cluster_voxels = list(),
    cluster_table = neuromosaic:::.empty_cluster_table()
  )

  tc <- extract_cluster_data(
    data_source  = inputs$data_vec,
    cluster_info = empty_info,
    design       = inputs$sample_table
  )

  expect_s3_class(tc, "tbl_df")
  expect_equal(nrow(tc), 0)
  expect_true("condition" %in% names(tc))
  expect_true("trial" %in% names(tc))
})

test_that("design columns are merged correctly", {
  inputs <- make_toy_cluster_explorer_inputs(n_time = 4)
  cluster_data <- build_cluster_explorer_data(
    data_source      = inputs$data_vec,
    atlas            = inputs$atlas,
    stat_map         = inputs$stat_map,
    sample_table     = inputs$sample_table,
    threshold        = 3.0,
    min_cluster_size = 3
  )

  design <- data.frame(
    condition = c("X", "Y", "X", "Y"),
    time = 1:4,
    extra_col = c(10, 20, 30, 40)
  )

  tc <- extract_cluster_data(
    data_source  = inputs$data_vec,
    cluster_info = cluster_data,
    design       = design
  )

  if (nrow(tc) > 0) {
    expect_true("extra_col" %in% names(tc))
  }
})
