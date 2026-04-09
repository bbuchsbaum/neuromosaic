# Additional tests for ce_picking.R

# -- .parse_plot_brain_selection_ids -------------------------------------------

test_that(".parse_plot_brain_selection_ids parses panel::parcel::shape format", {
  res <- neuromosaic:::.parse_plot_brain_selection_ids(
    c("lh_lateral::42::7", "rh_medial::10::3")
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2L)
  expect_equal(res$panel, c("lh_lateral", "rh_medial"))
  expect_equal(res$parcel_id, c(42L, 10L))
  expect_equal(res$shape_id, c(7L, 3L))
})

test_that(".parse_plot_brain_selection_ids parses bare integer IDs", {
  res <- neuromosaic:::.parse_plot_brain_selection_ids(c("5", "12"))
  expect_equal(nrow(res), 2L)
  expect_equal(res$parcel_id, c(5L, 12L))
  expect_true(all(is.na(res$panel)))
})

test_that(".parse_plot_brain_selection_ids handles empty input", {
  res <- neuromosaic:::.parse_plot_brain_selection_ids(character(0))
  expect_equal(nrow(res), 0L)
  expect_true(all(c("raw_id", "panel", "parcel_id", "shape_id") %in% names(res)))
})

test_that(".parse_plot_brain_selection_ids handles NA and empty strings", {
  res <- neuromosaic:::.parse_plot_brain_selection_ids(c(NA, "", "3"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$parcel_id, 3L)
})

# -- .clusters_for_parcels -----------------------------------------------------

test_that(".clusters_for_parcels finds clusters overlapping parcel IDs", {
  cp <- tibble::tibble(
    cluster_id = c("P1", "P1", "P2", "P2"),
    parcel_id = c(1L, 2L, 2L, 3L),
    frac = c(0.6, 0.4, 0.7, 0.3)
  )
  ids <- neuromosaic:::.clusters_for_parcels(cp, parcel_ids = 2L)
  expect_true("P2" %in% ids)
  expect_true("P1" %in% ids)
  # P2 has higher total frac in parcel 2 (0.7 vs 0.4), so it comes first
  expect_equal(ids[1], "P2")
})

test_that(".clusters_for_parcels returns empty for no match", {
  cp <- tibble::tibble(
    cluster_id = "P1", parcel_id = 1L, frac = 1.0
  )
  ids <- neuromosaic:::.clusters_for_parcels(cp, parcel_ids = 99L)
  expect_equal(length(ids), 0L)
})

test_that(".clusters_for_parcels returns empty for empty inputs", {
  cp <- tibble::tibble(cluster_id = character(0), parcel_id = integer(0),
                       frac = numeric(0))
  expect_equal(length(neuromosaic:::.clusters_for_parcels(cp, 1L)), 0L)
  expect_equal(length(neuromosaic:::.clusters_for_parcels(cp, integer(0))), 0L)
})

# -- .parcel_values_from_clusters ----------------------------------------------

test_that(".parcel_values_from_clusters returns named vector", {
  cp <- tibble::tibble(
    cluster_id = c("P1", "P1"),
    parcel_id = c(1L, 2L),
    peak_stat = c(5.0, -3.0),
    max_pos = c(5.0, NA),
    min_neg = c(NA, -3.0)
  )
  vals <- neuromosaic:::.parcel_values_from_clusters(
    cluster_parcels = cp,
    atlas_ids = 1:3,
    mode = "dominant"
  )
  expect_length(vals, 3L)
  expect_equal(vals[["1"]], 5.0)
  expect_equal(vals[["2"]], -3.0)
  expect_true(is.na(vals[["3"]]))
})

test_that(".parcel_values_from_clusters positive_only mode works", {
  cp <- tibble::tibble(
    cluster_id = c("P1", "N1"),
    parcel_id = c(1L, 1L),
    peak_stat = c(5.0, -3.0),
    max_pos = c(5.0, NA),
    min_neg = c(NA, -3.0)
  )
  vals <- neuromosaic:::.parcel_values_from_clusters(
    cluster_parcels = cp,
    atlas_ids = 1L,
    mode = "positive_only"
  )
  expect_equal(vals[["1"]], 5.0)
})

test_that(".parcel_values_from_clusters negative_only mode works", {
  cp <- tibble::tibble(
    cluster_id = c("P1", "N1"),
    parcel_id = c(1L, 1L),
    peak_stat = c(5.0, -3.0),
    max_pos = c(5.0, NA),
    min_neg = c(NA, -3.0)
  )
  vals <- neuromosaic:::.parcel_values_from_clusters(
    cluster_parcels = cp,
    atlas_ids = 1L,
    mode = "negative_only"
  )
  expect_equal(vals[["1"]], -3.0)
})

test_that(".parcel_values_from_clusters filters by selected_cluster_ids", {
  cp <- tibble::tibble(
    cluster_id = c("P1", "N1"),
    parcel_id = c(1L, 1L),
    peak_stat = c(5.0, -8.0),
    max_pos = c(5.0, NA),
    min_neg = c(NA, -8.0)
  )
  vals <- neuromosaic:::.parcel_values_from_clusters(
    cluster_parcels = cp,
    atlas_ids = 1L,
    selected_cluster_ids = "P1",
    mode = "dominant"
  )
  expect_equal(vals[["1"]], 5.0)
})

test_that(".parcel_values_from_clusters returns all NA for empty parcels", {
  cp <- tibble::tibble(
    cluster_id = character(0), parcel_id = integer(0),
    peak_stat = numeric(0), max_pos = numeric(0), min_neg = numeric(0)
  )
  vals <- neuromosaic:::.parcel_values_from_clusters(cp, atlas_ids = 1:2)
  expect_true(all(is.na(vals)))
})

# -- .surface_pick_round_clip_grid ---------------------------------------------

test_that(".surface_pick_round_clip_grid clips to valid range", {
  g <- neuromosaic:::.surface_pick_round_clip_grid(c(-0.3, 10.7, 3.2), c(5, 5, 5))
  expect_equal(g, c(1L, 5L, 3L))
})

test_that(".surface_pick_round_clip_grid handles matrix input", {
  g <- neuromosaic:::.surface_pick_round_clip_grid(
    matrix(c(2.5, 3.5, 4.5), nrow = 1), c(10, 10, 10)
  )
  expect_equal(g, c(2L, 4L, 4L))
})
