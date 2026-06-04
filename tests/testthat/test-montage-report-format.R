test_that("montage_report_formatters formats peak tables without empty columns", {
  fmt <- montage_report_formatters()
  tbl <- data.frame(
    cluster_id = "P1",
    sign = "positive",
    n_voxels = 12L,
    peak_mni_x = 1.234,
    peak_mni_y = -2.345,
    peak_mni_z = 3.456,
    max_stat = 5.678,
    atlas_label = "Region A",
    hemisphere = NA_character_,
    network = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- fmt$format_peak_table(tbl)

  expect_identical(names(out), c(
    "Cluster", "Sign", "N Voxels", "X (MNI)", "Y (MNI)", "Z (MNI)",
    "Peak Stat", "Region"
  ))
  expect_equal(out$`X (MNI)`, 1.2)
  expect_equal(out$`Peak Stat`, 5.68)
})

test_that("montage_report_formatters emits compact metadata and QC", {
  row <- data.frame(
    map_id = "faces_m1",
    stat_kind = "z",
    threshold = 3.456,
    tail = "two_sided",
    connectivity = "18-connect",
    min_cluster_size = 10,
    n = 27,
    stringsAsFactors = FALSE
  )
  fmt <- montage_report_formatters()

  metadata <- paste(capture.output(fmt$emit_metadata(row)), collapse = "\n")

  expect_match(metadata, "Z-statistic", fixed = TRUE)
  expect_match(metadata, "threshold 3.46", fixed = TRUE)
  expect_match(metadata, "two-sided", fixed = TRUE)
  expect_false(grepl("Field", metadata, fixed = TRUE))
  expect_false(grepl("Value", metadata, fixed = TRUE))

  qc <- data.frame(
    map_id = c("faces_m1", "places_m1"),
    label = c("Faces", "Places"),
    effective_n = c(27L, 32L),
    source_n = c(32L, NA_integer_),
    dropped_n = c(5L, 0L),
    qc_status = c("dropped_subjects", "ok"),
    stringsAsFactors = FALSE
  )
  summary <- paste(capture.output(fmt$emit_qc_summary(qc)), collapse = "\n")

  expect_match(summary, "Dropped subjects", fixed = TRUE)
  expect_match(summary, "Dropped", fixed = TRUE)
  expect_match(summary, "Input N", fixed = TRUE)
  expect_false(grepl("dropped_subjects", summary, fixed = TRUE))
})

test_that("montage_report_formatters emits styled HTML overview and QC", {
  manifest <- data.frame(
    map_id = c("faces_m1", "faces_m2"),
    label = c("Faces M1", "Faces M2"),
    contrast = "faces",
    model = c("m1", "m2"),
    stringsAsFactors = FALSE
  )
  fmt <- montage_report_formatters(
    manifest = manifest,
    layout = c("contrast", "model"),
    is_html = TRUE
  )

  styles <- paste(capture.output(fmt$emit_report_styles()), collapse = "\n")
  overview <- paste(capture.output(fmt$emit_report_overview()), collapse = "\n")

  expect_match(styles, "nm-report-overview", fixed = TRUE)
  expect_match(overview, "nm-overview-card", fixed = TRUE)
  expect_match(overview, "contrast / model", fixed = TRUE)
  expect_false(grepl("<th[^>]*>Field</th>", overview))
  expect_false(grepl("<th[^>]*>Value</th>", overview))

  qc <- data.frame(
    map_id = c("faces_m1", "faces_m2"),
    label = c("Faces M1", "Faces M2"),
    effective_n = c(27L, 32L),
    source_n = c(32L, 32L),
    dropped_n = c(5L, 0L),
    qc_status = c("dropped_subjects", "ok"),
    stringsAsFactors = FALSE
  )
  summary <- paste(capture.output(fmt$emit_qc_summary(qc)), collapse = "\n")

  expect_match(summary, "nm-qc-section", fixed = TRUE)
  expect_match(summary, "nm-status-warning", fixed = TRUE)
  expect_match(summary, "nm-status-ok", fixed = TRUE)
  expect_match(summary, "Input N", fixed = TRUE)
})

test_that("montage_report_formatters emits HTML panel heading as breadcrumb", {
  manifest <- data.frame(
    map_id = "faces_m1",
    label = "Faces M1",
    contrast = "faces",
    model = "m1",
    stringsAsFactors = FALSE
  )
  fmt <- montage_report_formatters(
    manifest = manifest,
    layout = c("contrast", "model"),
    is_html = TRUE
  )

  heading <- paste(
    capture.output(fmt$emit_panel_heading(1, "Faces M1")),
    collapse = "\n"
  )

  expect_match(heading, "nm-layout-path", fixed = TRUE)
  expect_match(heading, "faces / m1", fixed = TRUE)
  expect_match(heading, "nm-panel-title", fixed = TRUE)
  expect_false(grepl("^## faces", heading))
  expect_false(grepl("^### m1", heading))
})

test_that("montage_report_formatters suppresses redundant panel QC", {
  fmt <- montage_report_formatters()
  ok <- data.frame(
    map_id = "faces_m1",
    effective_n = 32L,
    source_n = 32L,
    dropped_n = 0L,
    qc_status = "ok",
    stringsAsFactors = FALSE
  )
  dropped <- ok
  dropped$dropped_n <- 2L
  dropped$dropped_subjects <- "sub-01, sub-02"
  dropped$qc_status <- "dropped_subjects"

  expect_identical(capture.output(fmt$emit_panel_qc(ok)), character(0))

  msg <- paste(capture.output(fmt$emit_panel_qc(dropped)), collapse = "\n")
  expect_match(msg, "Caution", fixed = TRUE)
  expect_match(msg, "2 subject(s) dropped", fixed = TRUE)
  expect_match(msg, "sub-01, sub-02", fixed = TRUE)
})

test_that("montage report templates use the shared formatter factory", {
  rmd <- system.file("templates", "montage_report.Rmd", package = "neuromosaic")
  qmd <- system.file("templates", "montage_report.qmd", package = "neuromosaic")
  rmd_text <- paste(readLines(rmd, warn = FALSE), collapse = "\n")
  qmd_text <- paste(readLines(qmd, warn = FALSE), collapse = "\n")

  expect_match(rmd_text, "montage_report_formatters", fixed = TRUE)
  expect_match(qmd_text, "montage_report_formatters", fixed = TRUE)
  expect_match(rmd_text, "emit_report_overview", fixed = TRUE)
  expect_match(qmd_text, "emit_report_overview", fixed = TRUE)
  expect_match(rmd_text, "emit_panel_heading", fixed = TRUE)
  expect_match(qmd_text, "emit_panel_heading", fixed = TRUE)
  expect_match(qmd_text, "#| results: asis", fixed = TRUE)
  expect_false(grepl("format_peak_table <-", rmd_text, fixed = TRUE))
  expect_false(grepl("format_peak_table <-", qmd_text, fixed = TRUE))
  expect_false(grepl("summary_tbl <- data.frame", rmd_text, fixed = TRUE))
  expect_false(grepl("summary_tbl <- data.frame", qmd_text, fixed = TRUE))
  expect_false(grepl("Field = fields\\[keep\\]", qmd_text))
  expect_false(grepl("Value = values\\[keep\\]", qmd_text))
})
