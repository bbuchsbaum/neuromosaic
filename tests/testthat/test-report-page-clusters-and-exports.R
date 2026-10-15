# Round 2 (report page): within-sign cluster numbering, nearest-region and
# cerebellar peak labels, honest counts, CSV and figure exports, dark/narrow
# figure sources, one statement of shared settings, and the page script's
# integration hooks.

toy_space <- function(dims = c(10, 10, 10)) {
  neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2), origin = c(-9, -9, -9))
}

toy_atlas <- function() {
  sp <- toy_space()
  arr <- array(0L, dim = dim(sp))
  arr[1:3, 1:3, 1:3] <- 1L        # a "Schaefer" parcel in the corner
  vol <- neuroim2::NeuroVol(arr, space = sp)
  atlas <- list(
    name = "toy", atlas = vol, ids = 1L, labels = "PFCl_4",
    orig_labels = "RH_Cont_PFCl_4", hemi = "right",
    roi_metadata = tibble::tibble(id = 1L, label = "PFCl_4", hemi = "right")
  )
  class(atlas) <- c("toy", "atlas")
  atlas
}

test_that("clusters are numbered within sign, values and order unchanged", {
  clusters <- data.frame(
    cluster_id = c("P1", "P2", "N3", "N4", "N5"),
    sign = c("positive", "positive", "negative", "negative", "negative"),
    max_stat = c(9, 7, -8, -6, -5), stringsAsFactors = FALSE
  )
  out <- neuromosaic:::.montage_sign_cluster_ids(clusters)
  expect_identical(out$cluster_id, c("P1", "P2", "N1", "N2", "N3"))
  expect_identical(out$max_stat, clusters$max_stat)
  # Already-per-sign IDs are left as they are.
  expect_identical(neuromosaic:::.montage_sign_cluster_ids(out)$cluster_id, out$cluster_id)
})

test_that("unlabelled peaks take the nearest region within the radius, marked", {
  atlas <- toy_atlas()
  sp <- neuroim2::space(atlas$atlas)
  # Voxel centres: grid 1..3 -> world -9, -7, -5. A peak at world (-1, -7, -7)
  # sits 4 mm from the parcel's edge voxel (-5, -7, -7); one at (5, 5, 5) is
  # far from it.
  tbl <- data.frame(
    cluster_id = c("P1", "P2", "N1"), sign = c("positive", "positive", "negative"),
    n_voxels = c(30L, 20L, 25L), peak_mni_x = c(-7, -1, 5), peak_mni_y = c(-7, -7, 5), peak_mni_z = c(-7, -7, 5),
    max_stat = c(9, 8, -7), atlas_label = c("PFCl_4", NA, NA),
    hemisphere = c("right", NA, NA), network = c("Cont", NA, NA),
    stringsAsFactors = FALSE
  )
  withr::local_options(neuromosaic.cerebellum_reference = FALSE)
  out <- neuromosaic:::.montage_fill_peak_labels(tbl, atlas, radius = 4)
  expect_identical(out$label_source, c("atlas", "nearest", NA))
  expect_equal(out$label_distance_mm, c(0, 4, NA))
  expect_identical(out$atlas_label, c("PFCl_4", "PFCl_4", NA))
  expect_identical(out$network, c("Cont", "Cont", NA))
  expect_identical(out$hemisphere, c("right", "right", NA))
  # Nothing within a 3 mm radius: never invent a label.
  tight <- neuromosaic:::.montage_fill_peak_labels(tbl, atlas, radius = 3)
  expect_true(is.na(tight$atlas_label[[2]]))

  region <- neuromosaic:::.montage_report_region_text(out)
  expect_identical(as.character(region), c("Control · PFCl 4",
                                           "≈ Control · PFCl 4 (4 mm)", NA))
  fmt <- montage_report_formatters()$format_peak_table(out, value_label = "z")
  expect_identical(fmt$Region, c("Control · PFCl 4",
                                 "≈ Control · PFCl 4 (4 mm)", "outside atlas"))
  expect_identical(attr(fmt, "nm_cell_class")$Region,
                   c("", "nm-near-atlas", "nm-off-atlas"))
})

test_that("cerebellar peaks are labelled from the reference segmentation", {
  atlas <- toy_atlas()
  sp <- neuroim2::space(atlas$atlas)
  cere <- array(0L, dim = dim(sp))
  cere[8:10, 1:3, 1:3] <- 47L     # right cerebellum (FreeSurfer 47)
  cere[1:2, 8:10, 1:3] <- 8L      # left cerebellum (FreeSurfer 8)
  ref <- neuroim2::NeuroVol(cere, space = sp)
  withr::local_options(neuromosaic.cerebellum_reference = ref)
  tbl <- data.frame(
    cluster_id = c("P1", "N1", "N2"), sign = c("positive", "negative", "negative"),
    peak_mni_x = c(7, -9, 1), peak_mni_y = c(-7, 7, 1), peak_mni_z = c(-7, -7, 9),
    max_stat = c(9, -8, -7), atlas_label = NA_character_,
    hemisphere = NA_character_, network = NA_character_, stringsAsFactors = FALSE
  )
  out <- neuromosaic:::.montage_fill_peak_labels(tbl, atlas, radius = 4)
  expect_identical(out$atlas_label, c("Cerebellum", "Cerebellum", NA))
  expect_identical(out$hemisphere, c("right", "left", NA))
  expect_identical(out$label_source, c("cerebellum", "cerebellum", NA))
  # An atlas that already has cerebellar regions is never overridden.
  atlas$labels <- "Cerebellum_Cortex"
  expect_true(neuromosaic:::.montage_atlas_has_cerebellum(atlas))
})

test_that("montage_peak_table renumbers by sign and keeps the full list", {
  inputs <- make_toy_cluster_report_inputs()
  withr::local_options(neuromosaic.cerebellum_reference = FALSE)
  tbl <- suppressWarnings(montage_peak_table(
    inputs$stat_map, inputs$atlas, threshold = 3, min_cluster_size = 3L,
    max_clusters = 1L
  ))
  all <- attr(tbl, "all_clusters")
  expect_identical(sort(all$cluster_id), c("N1", "P1"))
  expect_equal(nrow(tbl), 1L)
  expect_true(all(c("label_source", "label_distance_mm") %in% names(all)))
  expect_error(suppressWarnings(montage_peak_table(
    inputs$stat_map, inputs$atlas, threshold = 3, label_radius = -1
  )), "non-negative")
})

test_that("cluster captions separate shown from total, per sign", {
  tbl <- data.frame(cluster_id = c("P1", "P2", "N1"),
                    sign = c("positive", "positive", "negative"),
                    max_stat = c(9, 8, -7), stringsAsFactors = FALSE)
  attr(tbl, "n_total") <- 22L
  attr(tbl, "n_by_sign") <- c(positive = 12L, negative = 10L)
  expect_identical(
    neuromosaic:::.montage_report_cluster_caption(tbl),
    "Showing 3 of 22 clusters · 2 of 12 positive, 1 of 10 negative · strongest by |peak|, each sign kept; numbered by |peak| within sign."
  )
  attr(tbl, "n_total") <- 3L
  attr(tbl, "n_by_sign") <- c(positive = 2L, negative = 1L)
  expect_identical(neuromosaic:::.montage_report_cluster_caption(tbl),
                   "All 3 clusters · 2 positive, 1 negative.")
})

test_that("cluster tables link rows to peaks and offer the full list as CSV", {
  tbl <- data.frame(
    cluster_id = c("P1", "N1"), sign = c("positive", "negative"),
    n_voxels = c(10L, 5L), peak_mni_x = c(-50, 32), peak_mni_y = c(-10, -60),
    peak_mni_z = c(-16, -30), max_stat = c(26.01, -17.3),
    atlas_label = c("Temp_3", "Cerebellum"), hemisphere = c("left", "right"),
    network = c("Default", NA), label_source = c("atlas", "cerebellum"),
    label_distance_mm = c(0, 0), stringsAsFactors = FALSE
  )
  all <- rbind(tbl, data.frame(
    cluster_id = "P2", sign = "positive", n_voxels = 3L, peak_mni_x = 58,
    peak_mni_y = 18, peak_mni_z = 30, max_stat = 6, atlas_label = "PFCl_4",
    hemisphere = "right", network = "Cont", label_source = "nearest",
    label_distance_mm = 2, stringsAsFactors = FALSE
  ))
  attr(tbl, "n_total") <- 3L
  attr(tbl, "n_by_sign") <- c(positive = 2L, negative = 1L)
  attr(tbl, "all_clusters") <- all
  row <- data.frame(label = "Story > math", quantity = "test_statistic",
                    distribution = "z", stringsAsFactors = FALSE)
  html <- paste(capture.output(
    montage_report_formatters(is_html = TRUE)$emit_table(tbl, row)
  ), collapse = "")
  expect_match(html, "<tr data-nm-peak=\"-50,-10,-16\">", fixed = TRUE)
  expect_false(grepl("data-label=\"Sign\"", html, fixed = TRUE))
  expect_match(html, "download=\"story-gt-math_clusters.csv\"", fixed = TRUE)
  expect_match(html, "Download all 3 clusters (CSV)", fixed = TRUE)
  expect_match(html, "≈ nearest atlas region", fixed = TRUE)
  csv <- neuromosaic:::.montage_report_cluster_csv(all, row)
  lines <- strsplit(csv, "\r\n", fixed = TRUE)[[1]]
  expect_identical(lines[[1]], paste0("map,cluster,sign,n_voxels,x_mm,y_mm,z_mm,",
                                      "peak_z,region,hemisphere,label_source,label_distance_mm"))
  # Positive clusters first, each sign in number order; values untouched.
  expect_identical(sub(",.*", "", sub("^[^,]*,", "", lines[-1])), c("P1", "P2", "N1"))
  expect_match(lines[[3]], "58,18,30,6,Control · PFCl 4,right,nearest,2", fixed = TRUE)
  expect_match(lines[[2]], "^\"Story > math\"|^Story > math")
})

test_that("figures get dark and narrow sources, a download and a full caption", {
  mk <- function(w, h) {
    p <- tempfile(fileext = ".png")
    grDevices::png(p, width = w, height = h)
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new(); grDevices::dev.off()
    p
  }
  light <- mk(40, 20); dark <- mk(40, 20); narrow <- mk(20, 30)
  dir <- tempfile("figs")
  fmt <- montage_report_formatters(is_html = TRUE,
                                   figure_assets = list(dir = dir, href = "f"))
  out <- paste(capture.output(fmt$emit_panel_image(
    light, "alt", caption = "Axial slices", dark = dark, narrow = narrow,
    full_caption = "Full caption.", download_name = "x_volume.png"
  )), collapse = "")
  expect_match(out, "<picture><source media=\"(max-width: 600px)\"", fixed = TRUE)
  expect_match(out, "media=\"screen and (prefers-color-scheme: dark)\" data-nm-media=", fixed = TRUE)
  expect_match(out, "data-nm-theme=\"dark\"", fixed = TRUE)
  expect_match(out, "nm-figure nm-figure-dark-ready", fixed = TRUE)
  expect_match(out, "download=\"x_volume.png\">Download PNG</a>", fixed = TRUE)
  expect_match(out, "data-nm-copy-caption>Copy caption</button>", fixed = TRUE)
  expect_match(out, "data-nm-caption-text>Full caption.</span>", fixed = TRUE)
  # Inline figures are not duplicated into the download link.
  inline <- paste(capture.output(montage_report_formatters(is_html = TRUE)$emit_panel_image(
    light, "alt", caption = "c")), collapse = "")
  expect_match(inline, "data-nm-download download=", fixed = TRUE)
  expect_equal(lengths(regmatches(inline, gregexpr("data:image/png", inline))), 1L)
  # No variants: a plain image, still with a plate in dark mode.
  plain <- paste(capture.output(fmt$emit_panel_image(light, "alt", caption = "c")), collapse = "")
  expect_false(grepl("<picture>", plain, fixed = TRUE))
  expect_false(grepl("nm-figure-dark-ready", plain, fixed = TRUE))
})

test_that("figure captions state contrast, threshold, extent, N, cap and slices", {
  row <- data.frame(
    label = "2-back > 0-back", analysis_label = "Working memory (N-back)",
    stat_kind = "z", effective_threshold = 5, effective_tail = "two_sided",
    effective_connectivity = "18-connect", effective_min_cluster_size = 20L,
    n = 486L, quantity = "test_statistic", distribution = "z",
    stringsAsFactors = FALSE
  )
  panel <- list(volume = list(cap = 17.85, signed = TRUE,
                              slice_labels = c("z = −30", "z = −22", "z = +62")),
                surface = list(cap = 17.85, signed = TRUE))
  cap <- neuromosaic:::.montage_report_figure_caption(row, panel, "volume")
  expect_match(cap, "^2-back > 0-back \\(Working memory \\(N-back\\)\\)\\.")
  expect_match(cap, "N = 486", fixed = TRUE)
  expect_match(cap, "|z| ≥ 5 (two-sided)", fixed = TRUE)
  expect_match(cap, "clusters of at least 20 voxels (18-connectivity)", fixed = TRUE)
  expect_match(cap, "colour scale −17.9 to 17.9", fixed = TRUE)
  expect_match(cap, "z = −30, −22, +62 mm", fixed = TRUE)
  surf <- neuromosaic:::.montage_report_figure_caption(row, panel, "surface")
  expect_match(surf, "surface mapping details not recorded",
               fixed = TRUE)
})

test_that("settings shared by every map are stated once, deviations per map", {
  manifest <- data.frame(
    map_id = c("a", "b"), label = c("A", "B"), stat_kind = "z",
    effective_threshold = c(5, 4), effective_tail = "two_sided",
    effective_connectivity = "18-connect", effective_min_cluster_size = 20L,
    n = 486L, stringsAsFactors = FALSE
  )
  shared <- neuromosaic:::.montage_report_shared_spec(manifest, n_stated = FALSE)
  expect_identical(shared, c("Z-statistic", "18-connect, min 20 vox", "N = 486"))
  fmt <- montage_report_formatters(manifest = manifest, is_html = TRUE)
  head <- paste(capture.output(fmt$emit_report_header("T", "d")), collapse = "")
  expect_match(head, "18-connect, min 20 vox", fixed = TRUE)
  line <- paste(capture.output(fmt$emit_metadata(manifest[2, ])), collapse = "")
  expect_match(line, "threshold 4 (two-sided)", fixed = TRUE)
  expect_false(grepl("min 20 vox", line, fixed = TRUE))
  expect_false(grepl("N = 486", line, fixed = TRUE))
  # Markdown/PDF keeps the full line.
  md <- capture.output(montage_report_formatters(manifest = manifest)$emit_metadata(manifest[2, ]))
  expect_true(any(grepl("min 20 vox", md, fixed = TRUE)))
})

test_that("the glance grid states its bar scale and thumbnail semantics", {
  mk <- function() {
    p <- tempfile(fileext = ".png")
    grDevices::png(p, width = 40, height = 20)
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new(); grDevices::dev.off()
    p
  }
  tbl <- function(pos, neg) {
    t <- data.frame(cluster_id = c("P1", "N1"), sign = c("positive", "negative"),
                    n_voxels = c(10L, 5L), peak_mni_x = c(-50, 32),
                    peak_mni_y = c(-10, -60), peak_mni_z = c(-16, -30),
                    max_stat = c(pos, neg), atlas_label = c("Temp_3", NA),
                    hemisphere = c("left", NA), network = c("Default", NA),
                    stringsAsFactors = FALSE)
    attr(t, "n_total") <- 2L
    attr(t, "n_by_sign") <- c(positive = 1L, negative = 1L)
    t
  }
  manifest <- data.frame(map_id = c("a", "b"), analysis_id = c("a", "b"),
                         label = c("A > B", "C > D"), quantity = "test_statistic",
                         distribution = "z", task = "Working memory (N-back)",
                         stringsAsFactors = FALSE)
  panels <- list(a = list(table = tbl(26.01, -17.3), summary_image = mk(),
                          summary_image_dark = mk()),
                 b = list(table = tbl(12, -29.57), summary_image = mk()))
  fmt <- montage_report_formatters(manifest = manifest, layout = "task",
                                   is_html = TRUE, panels = panels)
  out <- paste(capture.output(fmt$emit_results_overview()), collapse = "")
  expect_match(out, "bars show |peak z| on one 0–29.6 scale.", fixed = TRUE)
  expect_match(out, "strongest |z| along each line of sight", fixed = TRUE)
  expect_match(out, "alt=\"Glass brain of A &gt; B: maximum-|z| projection\"", fixed = TRUE)
  expect_match(out, "nm-thumb-dark-ready", fixed = TRUE)
  expect_match(out, "Working memory <span class=\"nm-nowrap\">(N-back)</span>", fixed = TRUE)
})

test_that("the page script wires the view swap, peak rows and contents panel", {
  js <- paste(readLines(system.file("report", "montage-report.js",
                                    package = "neuromosaic")), collapse = "\n")
  expect_match(js, "addEventListener('nm-view-change'", fixed = TRUE)
  expect_match(js, "addEventListener('nm-volume-hidden'", fixed = TRUE)
  expect_match(js, "setWorldCoord", fixed = TRUE)
  expect_match(js, "addEventListener('focusout', leave)", fixed = TRUE)
  expect_match(js, "data-nm-copy-caption", fixed = TRUE)
  expect_match(js, "attributeFilter: ['data-theme']", fixed = TRUE)
  # The swap no longer depends on the router being moved by this script.
  expect_false(grepl("router.parentNode === slot) return", js, fixed = TRUE))
})

test_that("cerebellar reference is opt-in: unset never fetches", {
  withr::local_options(neuromosaic.cerebellum_reference = NULL)
  fetched <- FALSE
  local_mocked_bindings(
    get_template = function(...) {
      fetched <<- TRUE
      stop("network fetch attempted")
    },
    .package = "neuroatlas"
  )
  rm(list = ls(neuromosaic:::.montage_cerebellum_cache),
     envir = neuromosaic:::.montage_cerebellum_cache)
  expect_null(neuromosaic:::.montage_cerebellum_reference())
  expect_false(fetched)

  withr::local_options(neuromosaic.cerebellum_reference = TRUE)
  expect_null(neuromosaic:::.montage_cerebellum_reference())
  expect_true(fetched)
  rm(list = ls(neuromosaic:::.montage_cerebellum_cache),
     envir = neuromosaic:::.montage_cerebellum_cache)
})
