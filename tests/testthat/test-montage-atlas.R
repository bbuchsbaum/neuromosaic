test_that("montage_peak_table matches single-map atlas peak labels", {
  inputs <- make_toy_cluster_report_inputs()

  cluster_data <- suppressWarnings(
    neuromosaic:::.build_cluster_report_table_data(
      atlas = inputs$atlas,
      stat_map = inputs$stat_map,
      threshold = 3,
      min_cluster_size = 3L,
      connectivity = "18-connect",
      tail = "two_sided"
    )
  )
  single <- enrich_cluster_table(
    cluster_table = cluster_data$cluster_table,
    stat_map = inputs$stat_map,
    atlas = inputs$atlas
  )

  montage <- suppressWarnings(
    montage_peak_table(
      stat = inputs$stat_map,
      atlas = inputs$atlas,
      threshold = 3,
      min_cluster_size = 3L,
      map_id = "faces_m1"
    )
  )

  expect_s3_class(montage, "data.frame")
  expect_equal(nrow(montage), nrow(single))
  expect_identical(montage$map_id, rep("faces_m1", nrow(montage)))
  expect_equal(montage$cluster_id, single$cluster_id)
  expect_equal(montage$peak_mni_x, single$peak_mni_x)
  expect_equal(montage$peak_mni_y, single$peak_mni_y)
  expect_equal(montage$peak_mni_z, single$peak_mni_z)
  expect_equal(montage$atlas_label, single$atlas_label)
})

test_that("montage_peak_table validates inputs and handles empty clusters", {
  inputs <- make_toy_cluster_report_inputs()

  expect_error(
    montage_peak_table(
      stat = inputs$stat_map,
      atlas = inputs$atlas,
      threshold = 0
    ),
    "positive number"
  )

  empty <- suppressWarnings(
    montage_peak_table(
      stat = inputs$stat_map,
      atlas = inputs$atlas,
      threshold = 100,
      min_cluster_size = 3L
    )
  )
  expect_equal(nrow(empty), 0)
  expect_true("atlas_label" %in% names(empty))
})

test_that("montage QC summary surfaces effective N and dropped subjects", {
  manifest <- data.frame(
    map_id = c("faces_m1", "places_m1"),
    label = c("Faces", "Places"),
    n = c(27, 32),
    subjects = c(
      paste0(sprintf("sub-%02d", 1:32), collapse = ", "),
      paste0(sprintf("sub-%02d", 1:32), collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )

  qc <- neuromosaic:::.montage_qc_summary(manifest)

  expect_s3_class(qc, "data.frame")
  expect_equal(qc$effective_n, c(27, 32))
  expect_equal(qc$source_n, c(32, 32))
  expect_equal(qc$dropped_n, c(5, 0))
  expect_true(qc$has_dropped_subjects[[1]])
  expect_false(qc$has_dropped_subjects[[2]])
  expect_equal(qc$qc_status, c("dropped_subjects", "ok"))
})

test_that("montage QC summary accepts list-column subject metadata", {
  manifest <- data.frame(
    map_id = "faces_m1",
    label = "Faces",
    n = 1,
    stringsAsFactors = FALSE
  )
  manifest$subjects <- I(list(c("sub-01", "sub-02")))
  manifest$dropped_subjects <- I(list("sub-02"))

  qc <- neuromosaic:::.montage_qc_summary(manifest)

  expect_equal(qc$effective_n[[1]], 1)
  expect_equal(qc$source_n[[1]], 2)
  expect_equal(qc$dropped_n[[1]], 1)
  expect_equal(qc$dropped_subjects[[1]], "sub-02")
  expect_true(qc$has_dropped_subjects[[1]])
})
