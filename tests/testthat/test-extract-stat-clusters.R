# Regression tests for cluster extraction on non-finite-background maps (#2).
# Real fMRI statistic maps (fmrireg, FSL, SPM, AFNI) write NaN/Inf out-of-brain
# voxels; clustering must treat those as below-threshold background instead of
# letting them propagate into neuroim2::conn_comp().

make_nan_background_map <- function(blob_value = 5) {
  dims <- c(8, 8, 8)
  sp <- neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2),
                             origin = c(0, 0, 0))
  arr <- array(NaN, dim = dims)        # NaN background
  arr[1, 1, 1] <- Inf                  # an Inf voxel for good measure
  arr[3:5, 3:5, 3:5] <- blob_value     # one supra-threshold blob
  neuroim2::NeuroVol(arr, sp)
}

test_that(".extract_stat_clusters tolerates NaN/Inf background voxels", {
  stat_map <- make_nan_background_map(blob_value = 5)

  comp <- neuromosaic:::.extract_stat_clusters(
    stat_map = stat_map,
    threshold = 3,
    min_cluster_size = 5L,
    connectivity = "18-connect",
    tail = "two_sided"
  )

  expect_s3_class(comp$cluster_table, "data.frame")
  expect_equal(nrow(comp$cluster_table), 1L)
  expect_equal(comp$cluster_table$sign, "positive")
  expect_equal(comp$cluster_table$n_voxels, 27L)
  expect_equal(comp$cluster_table$max_stat, 5)
})

test_that(".extract_stat_clusters handles NaN/Inf background on the negative tail", {
  dims <- c(8, 8, 8)
  sp <- neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2),
                             origin = c(0, 0, 0))
  arr <- array(NaN, dim = dims)
  arr[1, 1, 1] <- Inf
  arr[8, 8, 8] <- -Inf                 # exercise -Inf too
  arr[3:5, 3:5, 3:5] <- -5             # one negative blob
  stat_map <- neuroim2::NeuroVol(arr, sp)

  comp <- neuromosaic:::.extract_stat_clusters(
    stat_map = stat_map,
    threshold = 3,
    min_cluster_size = 5L,
    connectivity = "18-connect",
    tail = "negative"
  )

  expect_equal(nrow(comp$cluster_table), 1L)
  expect_equal(comp$cluster_table$sign, "negative")
  expect_equal(comp$cluster_table$n_voxels, 27L)
  expect_equal(comp$cluster_table$max_stat, -5)
})

test_that("NaN-background clustering matches the zeroed-background result", {
  nan_map <- make_nan_background_map(blob_value = 5)
  arr0 <- as.array(nan_map)
  arr0[!is.finite(arr0)] <- 0
  zero_map <- neuroim2::NeuroVol(arr0, neuroim2::space(nan_map))

  args <- list(threshold = 3, min_cluster_size = 5L,
               connectivity = "18-connect", tail = "two_sided")
  nan_comp <- do.call(neuromosaic:::.extract_stat_clusters,
                      c(list(stat_map = nan_map), args))
  zero_comp <- do.call(neuromosaic:::.extract_stat_clusters,
                       c(list(stat_map = zero_map), args))

  expect_equal(nrow(nan_comp$cluster_table), nrow(zero_comp$cluster_table))
  expect_equal(nan_comp$cluster_table$max_stat, zero_comp$cluster_table$max_stat)
})

test_that("montage_peak_table works on a NaN-background map", {
  inputs <- make_toy_cluster_report_inputs()
  arr <- as.array(inputs$stat_map)
  arr[arr == 0] <- NaN                 # turn the zero background into NaN
  nan_map <- neuroim2::NeuroVol(arr, neuroim2::space(inputs$stat_map))

  peaks <- montage_peak_table(
    nan_map,
    atlas = inputs$atlas,
    threshold = 3,
    min_cluster_size = 3L
  )

  expect_s3_class(peaks, "data.frame")
  expect_gt(nrow(peaks), 0)
})
