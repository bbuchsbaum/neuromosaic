# Round 4b corrections: the Methods surface sentence describes the projected
# input exactly, the viewer's cluster records carry the table's tie digits,
# and the page/viewer scripts carry the print, readout, keyboard, wheel and
# file:// fixes (behaviour itself is checked in a browser by the gate).

r4b_lib <- function(...) {
  lib <- system.file("htmlwidgets", "lib", package = "neuromosaic")
  paste(readLines(file.path(lib, ...), warn = FALSE), collapse = "\n")
}

r4b_methods <- function(k = 20L) {
  manifest <- data.frame(
    map_id = c("a", "b"), label = c("A > B", "C > D"), analysis_id = c("x", "y"),
    stat_kind = "z", quantity = "test_statistic", distribution = "z",
    level = "group", n = 486L, threshold = 5,
    effective_threshold = 5, effective_tail = "two_sided",
    effective_min_cluster_size = k, effective_connectivity = "18-connect",
    effective_q = NA_real_, stringsAsFactors = FALSE
  )
  vol <- list(cap = 22.25, signed = TRUE, slice_labels = c("z = -30", "z = 62"))
  rd <- list(
    manifest = manifest,
    panels = list(a = list(volume = vol, surface_image = "s.png"), b = list(volume = vol)),
    labelling = list(atlases = "Schaefer-200 (7 networks)", radius = 4, citations = list()),
    params = list(policy = montage_policy(cap_quantile = 0.99), layout = character()),
    underlay = list(template = "MNI152NLin6Asym", spacing = c(2, 2, 2)),
    surface_method = list(space = "fsaverage6", surface = "inflated", vertices = 40962L,
                          fun = "avg", sampling = "thickness", interpolation = "linear",
                          aggregate = "mean", depth = seq(0.1, 0.9, length.out = 5L),
                          smooth_fwhm = 0, medial_wall = TRUE),
    provenance = list(package_version = "9.9.9", software = "neuromosaic 9.9.9",
                      r_version = "4.5.1")
  )
  neuromosaic:::.montage_report_methods(rd)$text
}

test_that("Methods say the surface samples the unthresholded map, not the clusters", {
  text <- r4b_methods()
  expect_match(
    text,
    "Surface figures sample the unthresholded z map (voxels of clusters smaller than 20 voxels removed) on the fsaverage6 cortical surface",
    fixed = TRUE
  )
  expect_match(text, "average them per vertex, and show vertices whose depth-averaged value reaches |z| ≥ 5",
               fixed = TRUE)
  expect_false(grepl("values of the displayed volume clusters", text, fixed = TRUE))
  expect_false(grepl("still reaches", text, fixed = TRUE))
  # Without an extent rule nothing is said to be removed.
  text1 <- r4b_methods(k = 1L)
  expect_match(text1, "sample the unthresholded z map on the", fixed = TRUE)
})

test_that("viewer cluster records carry the table's digits for tied peaks", {
  peaks <- data.frame(
    cluster_id = c("N5", "N6", "P1"), sign = c("negative", "negative", "positive"),
    peak_x = c(10, 20, 30), peak_y = 1:3, peak_z = 1:3,
    max_stat = c(-11.6329, -11.6315, 9.25),
    hemisphere = "left", region = "A", stringsAsFactors = FALSE
  )
  records <- neuromosaic:::.montage_volume_scene_clusters(peaks)
  expect_identical(records[[1]]$peak_digits, 3L)
  expect_identical(records[[2]]$peak_digits, 3L)
  # R's rounding, as the table prints it.
  expect_equal(records[[1]]$peak_value, -11.633)
  expect_equal(records[[2]]$peak_value, round(-11.6315, 3))
  expect_null(records[[3]]$peak_digits)
  adapter <- r4b_lib("neuromosaic-volume", "adapter.js")
  expect_match(adapter, "formatPeak(c.peak, record)", fixed = TRUE)
  expect_match(adapter, "record.peak_value", fixed = TRUE)
})

test_that("viewer scripts carry the readout, palette, keyboard, wheel and resume fixes", {
  adapter <- r4b_lib("neuromosaic-volume", "adapter.js")
  # Readout state from the live threshold, not only from NaN.
  expect_match(adapter, "NeuroMosaicVolumeDisplay.readoutState(", fixed = TRUE)
  expect_false(grepl("function hiddenValue", adapter, fixed = TRUE))
  # The report palette is rebuilt for the layer's range.
  expect_match(adapter, "colors, display.limits, display.threshold, threshold, range", fixed = TRUE)
  # The runtime's document-level arrow handler is guarded.
  expect_match(adapter, "document.removeEventListener(\"keydown\", inner)", fixed = TRUE)
  expect_match(adapter, "style[data-nm-slider]", fixed = TRUE)
  # The wheel scrolls the page unless the pane is focused or a modifier held.
  expect_match(adapter, "addEventListener(\"wheel\", state.onSliceWheel", fixed = TRUE)
  expect_match(adapter, "elsewhere the wheel scrolls the page", fixed = TRUE)
  # Repeated context loss is not blamed on too many views; resume focuses a pane.
  expect_match(adapter, "park(host, height, \"lost\")", fixed = TRUE)
  expect_match(adapter, "focusFirstPane(host)", fixed = TRUE)
  bridge <- r4b_lib("neuromosaic-surface", "bridge.js")
  expect_match(bridge, "parkSurface(host, \"lost\")", fixed = TRUE)
  expect_match(bridge, "vertexEdges(surface);", fixed = TRUE)
})

test_that("the page prints through the report and hides file:// image-only links", {
  css <- paste(readLines(system.file("report", "montage-report.css", package = "neuromosaic"),
                         warn = FALSE), collapse = "\n")
  js <- paste(readLines(system.file("report", "montage-report.js", package = "neuromosaic"),
                        warn = FALSE), collapse = "\n")
  expect_match(css, ".nm-fig-action[hidden] { display: none !important; }", fixed = TRUE)
  expect_match(js, "if (plainLink) plainLink.hidden = true;", fixed = TRUE)
  expect_match(js, "api.preparePrint = function", fixed = TRUE)
  expect_match(js, "e.preventDefault();\n    api.printReport();", fixed = TRUE)
  expect_match(js, "NeuroMosaicReport.preparePrint() first, or use the R/PDF route", fixed = TRUE)
  expect_match(css, ".nm-print-action", fixed = TRUE)
})
