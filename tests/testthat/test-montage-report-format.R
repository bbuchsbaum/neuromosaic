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

test_that("group selectors are accessible, escaped, and progressively enhanced", {
  manifest <- data.frame(
    analysis_id = rep("faces & places", 3L),
    map_id = c("z map", "estimate", "se"),
    role = c("primary", "auxiliary", "auxiliary"),
    quantity = c("test_statistic", "estimate", "standard_error"),
    distribution = c("z", NA, NA),
    selector_label = c("Z <main>", "Estimate", "SE"),
    label = c("Z statistic", "Estimate", "Standard error"),
    stringsAsFactors = FALSE
  )
  group <- list(
    analysis_id = "faces & places",
    analysis_label = "Faces < Places",
    primary_map_id = "z map",
    map_ids = manifest$map_id
  )
  fmt <- montage_report_formatters(
    manifest = manifest,
    is_html = TRUE,
    map_selector = "auto"
  )

  start <- paste(
    capture.output(fmt$emit_group_start(group, 1L, 1:3)),
    collapse = "\n"
  )
  variant <- paste(
    capture.output(fmt$emit_variant_start(manifest[2, , drop = FALSE],
                                          group, 2L)),
    collapse = "\n"
  )
  styles <- paste(capture.output(fmt$emit_report_styles()), collapse = "\n")

  expect_match(start, "role=\"tablist\"", fixed = TRUE)
  expect_match(start, "role=\"tab\"", fixed = TRUE)
  expect_match(start, "aria-controls", fixed = TRUE)
  expect_match(start, "Z &lt;main&gt;", fixed = TRUE)
  expect_match(start, "Faces &lt; Places", fixed = TRUE)
  expect_match(variant, "role=\"tabpanel\"", fixed = TRUE)
  expect_match(variant, "data-nm-primary=\"false\"", fixed = TRUE)
  expect_false(grepl(" hidden", variant, fixed = TRUE))
  expect_match(styles, "DOMContentLoaded", fixed = TRUE)
  expect_match(styles, "ArrowRight", fixed = TRUE)
  expect_match(styles, "if(tabs.length===0&&!select)return;", fixed = TRUE)
  expect_match(styles, "nm-map-change", fixed = TRUE)
  expect_match(styles, "nm-volume-map-request", fixed = TRUE)
  expect_match(styles, ".nm-map-variant[hidden]", fixed = TRUE)
})

test_that("map_selector none emits an expanded group without controls", {
  manifest <- data.frame(
    map_id = c("z", "se"),
    label = c("Z", "SE"),
    selector_label = c("Z", "SE"),
    stringsAsFactors = FALSE
  )
  group <- list(
    analysis_id = "a",
    analysis_label = "Analysis",
    primary_map_id = "z",
    map_ids = manifest$map_id
  )
  fmt <- montage_report_formatters(
    manifest = manifest,
    is_html = TRUE,
    map_selector = "none"
  )
  out <- paste(capture.output(fmt$emit_group_start(group, 1L, 1:2)),
               collapse = "\n")

  expect_false(grepl("nm-map-selector", out, fixed = TRUE))
  expect_false(grepl("role=\"tablist\"", out, fixed = TRUE))
})

test_that("automatic selector uses a select control beyond four maps", {
  manifest <- data.frame(
    map_id = paste0("map", 1:5),
    label = paste("Map", 1:5),
    selector_label = paste("Map", 1:5),
    stringsAsFactors = FALSE
  )
  group <- list(
    analysis_id = "a",
    analysis_label = "Analysis",
    primary_map_id = "map1",
    map_ids = manifest$map_id
  )
  fmt <- montage_report_formatters(manifest = manifest, is_html = TRUE)
  out <- paste(capture.output(fmt$emit_group_start(group, 1L, 1:5)),
               collapse = "\n")

  expect_match(out, "<select", fixed = TRUE)
  expect_match(out, "data-nm-map-select", fixed = TRUE)
  expect_false(grepl("role=\"tablist\"", out, fixed = TRUE))
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

test_that("emit_intro renders report-level preamble in both output modes", {
  none <- montage_report_formatters()
  expect_identical(capture.output(none$emit_intro()), character(0))

  html <- montage_report_formatters(intro = "Read me **first**.", is_html = TRUE)
  html_out <- paste(capture.output(html$emit_intro()), collapse = "\n")
  expect_match(html_out, "::: {.nm-intro}", fixed = TRUE)
  expect_match(html_out, "Read me **first**.", fixed = TRUE)

  md <- montage_report_formatters(intro = c("Line one.", "Line two."))
  md_out <- paste(capture.output(md$emit_intro()), collapse = "\n")
  expect_match(md_out, "Line one.", fixed = TRUE)
  expect_match(md_out, "Line two.", fixed = TRUE)
  expect_false(grepl("nm-intro", md_out, fixed = TRUE))
})

test_that("section notes render under headings (markdown) and breadcrumb (HTML)", {
  manifest <- data.frame(
    map_id = c("faces_m1", "faces_m2"),
    label = c("Faces M1", "Faces M2"),
    contrast = "faces",
    model = c("m1", "m2"),
    stringsAsFactors = FALSE
  )
  section_notes <- data.frame(
    contrast = c("faces", "faces"),
    model = c(NA, "m1"),
    text = c("Faces framing.", "Model 1 detail."),
    stringsAsFactors = FALSE
  )

  md <- montage_report_formatters(
    manifest = manifest,
    layout = c("contrast", "model"),
    section_notes = section_notes
  )
  first <- paste(capture.output(md$emit_panel_heading(1, "Faces M1")), collapse = "\n")
  expect_match(first, "## faces", fixed = TRUE)
  expect_match(first, "Faces framing.", fixed = TRUE)
  expect_match(first, "Model 1 detail.", fixed = TRUE)

  # The contrast note fires once; entering model m2 has no note of its own.
  second <- paste(capture.output(md$emit_panel_heading(2, "Faces M2")), collapse = "\n")
  expect_false(grepl("Faces framing.", second, fixed = TRUE))
  expect_false(grepl("Model 1 detail.", second, fixed = TRUE))

  html <- montage_report_formatters(
    manifest = manifest,
    layout = c("contrast", "model"),
    section_notes = section_notes,
    is_html = TRUE
  )
  html_first <- paste(
    capture.output(html$emit_panel_heading(1, "Faces M1")),
    collapse = "\n"
  )
  expect_match(html_first, "::: {.nm-section-note}", fixed = TRUE)
  expect_match(html_first, "Faces framing.", fixed = TRUE)
  expect_match(html_first, "Model 1 detail.", fixed = TRUE)
  html_second <- paste(
    capture.output(html$emit_panel_heading(2, "Faces M2")),
    collapse = "\n"
  )
  expect_false(grepl("Faces framing.", html_second, fixed = TRUE))
})

test_that("emit_interludes filters by map_id and position", {
  interludes <- data.frame(
    map_id = c("faces_m1", "faces_m1", "faces_m2"),
    position = c("before", "after", "before"),
    text = c("Intro to M1.", "Recap of M1.", "Now M2."),
    stringsAsFactors = FALSE
  )
  fmt <- montage_report_formatters(interludes = interludes, is_html = TRUE)

  before <- paste(capture.output(fmt$emit_interludes("faces_m1", "before")), collapse = "\n")
  expect_match(before, "::: {.nm-interlude}", fixed = TRUE)
  expect_match(before, "Intro to M1.", fixed = TRUE)
  expect_false(grepl("Recap of M1.", before, fixed = TRUE))

  after <- paste(capture.output(fmt$emit_interludes("faces_m1", "after")), collapse = "\n")
  expect_match(after, "Recap of M1.", fixed = TRUE)

  # No interlude for this anchor/position emits nothing.
  expect_identical(
    capture.output(fmt$emit_interludes("faces_m2", "after")),
    character(0)
  )

  md <- montage_report_formatters(interludes = interludes)
  md_before <- paste(capture.output(md$emit_interludes("faces_m2", "before")), collapse = "\n")
  expect_match(md_before, "Now M2.", fixed = TRUE)
  expect_false(grepl("nm-interlude", md_before, fixed = TRUE))
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
  expect_match(rmd_text, "emit_intro", fixed = TRUE)
  expect_match(qmd_text, "emit_intro", fixed = TRUE)
  expect_match(rmd_text, "emit_interludes", fixed = TRUE)
  expect_match(qmd_text, "emit_interludes", fixed = TRUE)
  expect_match(rmd_text, "emit_group_start", fixed = TRUE)
  expect_match(qmd_text, "emit_variant_start", fixed = TRUE)
  expect_match(qmd_text, "#| results: asis", fixed = TRUE)
  expect_false(grepl("format_peak_table <-", rmd_text, fixed = TRUE))
  expect_false(grepl("format_peak_table <-", qmd_text, fixed = TRUE))
  expect_false(grepl("summary_tbl <- data.frame", rmd_text, fixed = TRUE))
  expect_false(grepl("summary_tbl <- data.frame", qmd_text, fixed = TRUE))
  expect_false(grepl("Field = fields\\[keep\\]", qmd_text))
  expect_false(grepl("Value = values\\[keep\\]", qmd_text))
})
