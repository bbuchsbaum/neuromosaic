# Report page (round 1): results overview, peak-table presentation, QC
# collapse, provenance, semantics, and the page assets that carry the dark
# theme, narrow-screen contents, print, and view-router placement.

peak_fixture <- function() {
  tbl <- data.frame(
    cluster_id = c("P2", "P1", "N13", "P3"),
    sign = c("positive", "positive", "negative", "positive"),
    n_voxels = c(500L, 900L, 40L, 30L),
    peak_mni_x = c(28, -50, 32, 0),
    peak_mni_y = c(10, -10, -60, 4),
    peak_mni_z = c(56, -16, -30, 2),
    max_stat = c(20.7, 26.01, -17.3, 6.2),
    atlas_label = c("FEF_2", "Temp_3", NA, NA),
    hemisphere = c("right", "left", NA, NA),
    network = c("DorsAttn", "Default", NA, NA),
    stringsAsFactors = FALSE
  )
  attr(tbl, "n_total") <- 22L
  attr(tbl, "n_by_sign") <- c(positive = 15L, negative = 7L)
  attr(tbl, "n_voxels_total") <- 98751
  tbl
}

test_that("peak-row selection keeps every sign and never alters values", {
  clusters <- data.frame(
    cluster_id = c("P1", "P2", "P3", "N4"),
    sign = c("positive", "positive", "positive", "negative"),
    n_voxels = c(100L, 50L, 20L, 37L),
    max_stat = c(17.3, 11.6, 10.1, -6.5)
  )
  keep <- neuromosaic:::.montage_select_peak_rows(clusters, 2L)
  expect_length(keep, 2L)
  expect_setequal(clusters$sign[keep], c("positive", "negative"))
  # The strongest cluster stays; the weakest positive made room.
  expect_identical(clusters$cluster_id[keep], c("P1", "N4"))
  # Enough room: plain |peak| order, nothing replaced.
  expect_identical(neuromosaic:::.montage_select_peak_rows(clusters, 10L),
                   c(1L, 2L, 3L, 4L))
  expect_length(neuromosaic:::.montage_select_peak_rows(clusters[0, ], 5L), 0L)
})

test_that("montage_peak_table records totals independent of truncation", {
  inputs <- make_toy_cluster_report_inputs()
  one <- suppressWarnings(montage_peak_table(
    stat = inputs$stat_map, atlas = inputs$atlas, threshold = 3,
    min_cluster_size = 3L, max_clusters = 1L
  ))
  all <- suppressWarnings(montage_peak_table(
    stat = inputs$stat_map, atlas = inputs$atlas, threshold = 3,
    min_cluster_size = 3L, max_clusters = 20L
  ))
  expect_equal(nrow(one), 1L)
  expect_identical(attr(one, "n_total"), 2L)
  expect_identical(attr(all, "n_total"), 2L)
  expect_equal(unname(attr(one, "n_by_sign")), c(1, 1))
  expect_equal(attr(one, "n_voxels_total"), sum(all$n_voxels))
  # The reported row is the same cluster with the same numbers.
  expect_equal(one$max_stat, all$max_stat[1])
  expect_equal(one$n_voxels, all$n_voxels[1])
})

test_that("Schaefer network names are recovered and shown with the region", {
  expect_identical(
    neuromosaic:::.montage_schaefer_network(c("RH_DorsAttn_FEF_2", "LH_Vis_1",
                                              "Thalamus", NA)),
    c("DorsAttn", "Vis", NA, NA)
  )
  region <- neuromosaic:::.montage_report_region_label(
    c("Temp_3", "Vis_11", "Caudate", NA),
    c("Default", "Vis", NA, NA)
  )
  expect_identical(region[1:3], c("Default · Temp 3", "Visual · 11",
                                  "Caudate"))
  expect_true(is.na(region[4]))
})

test_that("peak tables mark off-atlas peaks, infer hemisphere, and order by ID", {
  fmt <- montage_report_formatters()
  out <- fmt$format_peak_table(peak_fixture(), value_label = "z")
  expect_identical(out$Cluster, c("P1", "P2", "P3", "N13"))
  expect_true("Peak z" %in% names(out))
  expect_false("Network" %in% names(out))
  expect_identical(out$Region, c("Default · Temp 3",
                                 "Dorsal attention · FEF 2",
                                 "outside atlas", "outside atlas"))
  # x = 0 is midline; x = 32 is right. Atlas hemispheres are untouched.
  expect_identical(out$Hemisphere, c("left", "right", "midline", "right"))
  # Numbers are unchanged, only reordered.
  expect_equal(out$`Peak z`, c(26.01, 20.7, 6.2, -17.3))
  expect_equal(out$`N Voxels`, c(900L, 500L, 30L, 40L))

  html <- paste(capture.output(
    montage_report_formatters(is_html = TRUE)$emit_table(
      peak_fixture(), data.frame(quantity = "test_statistic", distribution = "z")
    )
  ), collapse = "")
  expect_match(html, "Showing 4 of 22 clusters", fixed = TRUE)
  expect_match(html, "3 of 15 positive, 1 of 7 negative", fixed = TRUE)
  expect_match(html, "nm-cluster-table", fixed = TRUE)
  expect_match(html, "nm-off-atlas\" data-label=\"Region\">outside atlas<", fixed = TRUE)
  expect_match(html, "nm-derived", fixed = TRUE)
  expect_match(html, "class=\"num nm-col-peak-z nm-neg\"", fixed = TRUE)
  # The sign column is dropped in HTML: the P/N ID and coloured peak carry it.
  expect_false(grepl("nm-sign nm-sign-pos", html, fixed = TRUE))
  expect_match(html, "P1<span class=\"nm-sr-only\"> (positive)</span>", fixed = TRUE)
  md <- capture.output(montage_report_formatters()$emit_table(peak_fixture()))
  expect_true(any(grepl("Showing 4 of 22 clusters", md, fixed = TRUE)))
})

test_that("uniform QC collapses to one line; differing QC keeps the table", {
  qc <- data.frame(
    map_id = c("a", "b", "c"), label = c("A", "B", "C"),
    effective_n = 486L, source_n = NA_integer_, dropped_n = 0L,
    qc_status = "ok", stringsAsFactors = FALSE
  )
  html <- montage_report_formatters(is_html = TRUE)
  line <- paste(capture.output(html$emit_qc_summary(qc)), collapse = "")
  expect_match(line, "N = 486 for all 3 maps · QC: ok (as supplied)", fixed = TRUE)
  expect_false(grepl("<table", line, fixed = TRUE))
  md <- capture.output(montage_report_formatters()$emit_qc_summary(qc))
  expect_true(any(grepl("QC: ok (as supplied)", md, fixed = TRUE)))

  # Stated in the header: the summary emitter then stays silent.
  manifest <- data.frame(map_id = qc$map_id, label = qc$label)
  fmt <- montage_report_formatters(manifest = manifest, is_html = TRUE)
  header <- paste(capture.output(fmt$emit_report_header("T", qc = qc)), collapse = "")
  expect_match(header, "QC: ok (as supplied)", fixed = TRUE)
  expect_length(capture.output(fmt$emit_qc_summary(qc)), 0L)

  qc$effective_n[2] <- 480L
  table <- paste(capture.output(html$emit_qc_summary(qc)), collapse = "")
  expect_match(table, "nm-qc-table", fixed = TRUE)
  expect_null(neuromosaic:::.montage_report_qc_line(qc))
})

test_that("the header is a real h1 with a skip link and shared settings", {
  manifest <- data.frame(
    map_id = c("a", "b"), label = c("A", "B"), stat_kind = "z",
    threshold = 5, tail = "two_sided", connectivity = "18-connect",
    min_cluster_size = 20L, n = 486L, stringsAsFactors = FALSE
  )
  fmt <- montage_report_formatters(manifest = manifest, is_html = TRUE)
  out <- paste(capture.output(fmt$emit_report_header("HCP <task>", "2026-10-01")),
               collapse = "")
  expect_match(out, "<h1 class=\"nm-report-title\">HCP &lt;task&gt;</h1>", fixed = TRUE)
  expect_false(grepl("role=\"heading\"", out, fixed = TRUE))
  expect_match(out, "<a class=\"nm-skip\" href=\"#nm-main\">", fixed = TRUE)
  expect_match(out, "threshold 5 (two-sided)", fixed = TRUE)
  expect_match(out, "18-connect, min 20 vox", fixed = TRUE)
})

test_that("results overview shows one card per map and never an empty heading", {
  manifest <- data.frame(
    map_id = c("m1", "m2"), analysis_id = c("m1", "m2"),
    label = c("2-back > 0-back", "Faces > average"),
    task = c("Working memory", "Working memory"), stringsAsFactors = FALSE
  )
  groups <- list(list(analysis_id = "m1", map_ids = "m1", primary_map_id = "m1"),
                 list(analysis_id = "m2", map_ids = "m2", primary_map_id = "m2"))
  thumb <- tempfile(fileext = ".png")
  grDevices::png(thumb, width = 40, height = 25)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  neg_only <- peak_fixture()[3, ]
  attr(neg_only, "n_total") <- 1L
  attr(neg_only, "n_by_sign") <- c(positive = 0L, negative = 1L)
  attr(neg_only, "n_voxels_total") <- 40
  panels <- list(m1 = list(table = peak_fixture(), summary_image = thumb),
                 m2 = list(table = neg_only))
  fmt <- montage_report_formatters(manifest = manifest, layout = "task",
                                   is_html = TRUE, groups = groups,
                                   panels = panels)
  out <- paste(capture.output(fmt$emit_results_overview()), collapse = "\n")
  expect_match(out, "^# Results at a glance")
  expect_identical(lengths(regmatches(out, gregexpr("nm-atlas-card", out))), 2L)
  expect_match(out, "98,751</b> voxels", fixed = TRUE)
  expect_match(out, "<b>22</b> clusters", fixed = TRUE)
  expect_match(out, "+26.0</span><span class=\"nm-atlas-region\" title=\"Default · Temp 3 (L)\">Default · <span class=\"nm-nowrap\">Temp 3 (L)",
               fixed = TRUE)
  expect_match(out, "−17.3", fixed = TRUE)
  expect_match(out, "no positive clusters", fixed = TRUE)
  expect_match(out, "nm-atlas-thumb", fixed = TRUE)
  expect_match(out, "nm-atlas-nothumb", fixed = TRUE)
  expect_match(out, paste0("href=\"#", neuromosaic:::.montage_report_map_anchor("m1", "m1")),
               fixed = TRUE)
  expect_match(out, "Working memory", fixed = TRUE)

  # One map, or maps without cluster tables: no section at all.
  single <- montage_report_formatters(manifest = manifest[1, ], is_html = TRUE,
                                      groups = groups[1], panels = panels)
  expect_length(capture.output(single$emit_results_overview()), 0L)
  bare <- montage_report_formatters(manifest = manifest, is_html = TRUE,
                                    groups = groups, panels = list())
  expect_length(capture.output(bare$emit_results_overview()), 0L)

  md <- montage_report_formatters(manifest = manifest, is_html = FALSE,
                                  groups = groups, panels = panels)
  md_out <- capture.output(md$emit_results_overview())
  expect_identical(md_out[1], "# Results at a glance")
  expect_true(any(grepl("Default · Temp 3", md_out, fixed = TRUE)))
})

test_that("map articles carry an anchor, a labelled heading, and a view slot", {
  manifest <- data.frame(map_id = "m1", label = "Story > math", task = "Language",
                         stringsAsFactors = FALSE)
  fmt <- montage_report_formatters(manifest = manifest, layout = "task",
                                   is_html = TRUE)
  start <- paste(capture.output(fmt$emit_map_start(1, "Story > math", "m1")),
                 collapse = "\n")
  anchor <- neuromosaic:::.montage_report_map_anchor("m1", "m1")
  expect_match(start, paste0("<article class=\"nm-map\" id=\"", anchor, "\""), fixed = TRUE)
  expect_match(start, paste0("aria-labelledby=\"", anchor, "-title\""), fixed = TRUE)
  expect_match(start, paste0("<h3 class=\"nm-panel-title\" id=\"", anchor, "-title\">"),
               fixed = TRUE)
  slot <- paste(capture.output(fmt$emit_view_slot("m1", "Story > math")), collapse = "")
  expect_match(slot, "data-nm-view-slot=\"m1\" data-nm-view-label=\"Story &gt; math\"",
               fixed = TRUE)
  expect_match(paste(capture.output(fmt$emit_map_end()), collapse = ""), "</article>",
               fixed = TRUE)
  md <- montage_report_formatters(manifest = manifest, layout = "task")
  md_out <- capture.output(md$emit_map_start(1, "Story > math", "m1"))
  expect_true(any(grepl("^### Story > math", md_out)))
  expect_length(capture.output(md$emit_view_slot("m1")), 0L)
})

test_that("figures carry captions only when they show something", {
  png <- tempfile(fileext = ".png")
  grDevices::png(png, width = 20, height = 10)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  fmt <- montage_report_formatters(is_html = TRUE)
  out <- paste(capture.output(fmt$emit_panel_image(png, "alt", caption = "Left on left")),
               collapse = "")
  expect_match(out, "<figcaption><span class=\"nm-fig-cap\">Left on left</span>", fixed = TRUE)
  empty <- paste(capture.output(fmt$emit_panel_image(png, "alt", empty = TRUE,
                                                     caption = "x")), collapse = "")
  expect_false(grepl("figcaption", empty, fixed = TRUE))
})

test_that("provenance is a closed disclosure with readable keys and short hashes", {
  hash <- paste(rep("0c5176bf47d69c29", 4), collapse = "")
  prov <- list(
    generated_at = "2026-10-02 00:07:45 UTC", package = "neuromosaic",
    package_version = "0.2.0", interactive_runtime_sha256 = hash,
    interactive_compressed_bytes = 11694013,
    interactive_asset_hashes = paste0("background-ab=", hash, "; overlay-cd=", hash),
    interactive_surface_engine = "neurosurf SurfaceScene / surfview",
    empty = NULL
  )
  html <- paste(capture.output(
    montage_report_formatters(is_html = TRUE)$emit_provenance(prov)
  ), collapse = "")
  expect_match(html, "^<details class=\"nm-details nm-provenance\"><summary>")
  expect_false(grepl("<details[^>]*open", html))
  expect_match(html, "neuromosaic 0.2.0", fixed = TRUE)
  expect_match(html, "<dt>Runtime SHA-256</dt>", fixed = TRUE)
  expect_match(html, "<h3 class=\"nm-prov-head\">Interactive surface</h3>", fixed = TRUE)
  expect_match(html, paste0("<code class=\"nm-hash\" title=\"", hash, "\">", hash),
               fixed = TRUE)
  expect_match(html, "11.2 MiB", fixed = TRUE)
  expect_match(html, "<ul class=\"nm-prov-items\"><li><span class=\"nm-prov-name\">background-ab</span>",
               fixed = TRUE)
  expect_false(grepl("interactive_runtime_sha256", html, fixed = TRUE))

  md <- capture.output(montage_report_formatters()$emit_provenance(prov))
  expect_true(any(grepl("Interactive volume: Runtime SHA-256", md, fixed = TRUE)))
  expect_identical(capture.output(montage_report_formatters()$emit_provenance(NULL)),
                   "*No provenance metadata available.*")
})

test_that("templates declare language, page title, landmark, and no empty summary", {
  for (name in c("montage_report.Rmd", "montage_report.qmd")) {
    path <- system.file("templates", name, package = "neuromosaic")
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    yaml <- strsplit(text, "\n---\n", fixed = TRUE)[[1]][1]
    expect_match(yaml, "lang: en", fixed = TRUE)
    expect_match(yaml, "pagetitle:", fixed = TRUE)
    expect_false(grepl("\ntitle:", yaml, fixed = TRUE))
    expect_match(text, "role=\\\"main\\\"", fixed = TRUE)
    expect_match(text, "fmt$emit_results_overview()", fixed = TRUE)
    expect_match(text, "fmt$emit_provenance(rd$provenance)", fixed = TRUE)
    expect_match(text, "fmt$emit_view_slot(", fixed = TRUE)
    expect_false(grepl("cat(\"# Summary", text, fixed = TRUE))
  }
})

test_that("report assets carry the dark theme, page guards, print and nav", {
  css <- paste(readLines(system.file("report", "montage-report.css",
                                     package = "neuromosaic"), warn = FALSE),
               collapse = "\n")
  js <- paste(readLines(system.file("report", "montage-report.js",
                                    package = "neuromosaic"), warn = FALSE),
              collapse = "\n")
  # Dark tokens are screen-only (round 3): paper always prints light.
  expect_match(css, "@media screen and (prefers-color-scheme: dark) {\n  :root:not([data-theme=\"light\"])",
               fixed = TRUE)
  expect_match(css, ":root[data-theme=\"dark\"]", fixed = TRUE)
  # The scroll guard for non-report tables applies at every width.
  first_media <- regexpr("@media", css, fixed = TRUE)
  guard <- regexpr("table:not(.nm-table) { display: block", css, fixed = TRUE)
  expect_true(guard > 0 && guard < first_media ||
                guard < regexpr("@media (max-width", css, fixed = TRUE))
  expect_match(css, "@media print", fixed = TRUE)
  expect_match(css, ".nm-map { break-before: page", fixed = TRUE)
  expect_match(css, ".nm-minibar", fixed = TRUE)
  expect_false(grepl("@import|fonts.googleapis|url\\(http", css))
  expect_match(js, "beforeprint", fixed = TRUE)
  expect_match(js, "data-nm-view-slot", fixed = TRUE)
  expect_match(js, "nm-minibar", fixed = TRUE)
  expect_match(js, "nm-report-title", fixed = TRUE)
})
