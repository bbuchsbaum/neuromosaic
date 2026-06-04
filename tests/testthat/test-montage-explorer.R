make_montage_explorer_fixture <- function() {
  tmpdir <- tempfile("montage-explorer-")
  dir.create(tmpdir, recursive = TRUE)
  image_path <- file.path(tmpdir, "panel.png")
  writeLines("png-placeholder", image_path)

  manifest <- data.frame(
    map_id = "faces_m1",
    path = image_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    contrast = "faces",
    model = "m1",
    label = "Faces M1",
    n = 10,
    subjects = paste(sprintf("sub-%02d", 1:12), collapse = ","),
    stringsAsFactors = FALSE
  )
  qc <- neuromosaic:::.montage_qc_summary(manifest)
  panels <- list(
    faces_m1 = list(
      volume_image = image_path,
      peak_table = data.frame(
        cluster_id = "P1",
        sign = "positive",
        n_voxels = 12L,
        peak_mni_x = 1,
        peak_mni_y = 2,
        peak_mni_z = 3,
        max_stat = 5,
        atlas_label = "RegionA",
        stringsAsFactors = FALSE
      ),
      qc = qc[1, , drop = FALSE]
    )
  )

  list(
    manifest = manifest,
    report_data = list(
      manifest = manifest,
      panels = panels,
      qc = qc,
      params = list(title = "Explorer Fixture", layout = c("contrast", "model"))
    ),
    panels = panels,
    qc = qc
  )
}

test_that("montage_explorer_data indexes panels, cluster tables, and QC", {
  fixture <- make_montage_explorer_fixture()
  signals <- data.frame(
    map_id = "faces_m1",
    time = 1:3,
    signal = c(0.2, 0.4, 0.3)
  )

  out <- montage_explorer_data(
    report_data = fixture$report_data,
    signals = signals
  )

  expect_equal(out$panel_index$map_id, "faces_m1")
  expect_equal(out$panel_index$label, "Faces M1")
  expect_true(file.exists(out$panel_index$volume_image))
  expect_equal(out$panel_index$n_clusters, 1L)
  expect_equal(out$panel_index$qc_status, "dropped_subjects")
  expect_equal(nrow(out$cluster_tables$faces_m1), 1L)
  expect_identical(out$signals, signals)
})

test_that("montage_explorer builds a Shiny app from report data", {
  skip_if_not_installed("shiny")

  fixture <- make_montage_explorer_fixture()
  app <- montage_explorer(report_data = fixture$report_data)

  expect_s3_class(app, "shiny.appobj")
})
