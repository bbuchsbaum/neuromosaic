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
  # neuroimjs >= 0.5 keeps its document-level arrow keys out of form controls
  # itself; the adapter no longer rewires that listener.
  expect_false(grepl("handleKeydownBound", adapter, fixed = TRUE))
  expect_match(adapter, "style[data-nm-slider]", fixed = TRUE)
  # The wheel scrolls the page unless the pane is focused or a modifier held.
  expect_match(adapter, "addEventListener(\"wheel\", state.onSliceWheel", fixed = TRUE)
  expect_match(adapter, "elsewhere the wheel scrolls the page", fixed = TRUE)
  # Repeated context loss is not blamed on too many views; resume focuses a pane.
  expect_match(adapter, "park(host, height, \"lost\")", fixed = TRUE)
  expect_match(adapter, "focusFirstPane(host)", fixed = TRUE)
  bridge <- r4b_lib("neuromosaic-surface", "bridge.js")
  expect_match(bridge, "parkSurface(host, \"lost\")", fixed = TRUE)
  # surfview draws the one-sided threshold edge itself.
  expect_false(grepl("vertexEdges", bridge, fixed = TRUE))
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
  # Headless pipelines are pointed at preparePrint() (documented in
  # ?render_montage_report).
  expect_match(js, "should await NeuroMosaicReport.preparePrint() first", fixed = TRUE)
  expect_match(css, ".nm-print-action", fixed = TRUE)
})

test_that("precomputed panels with only an image path render (no partial matching)", {
  # `panel$volume` partially matches `volume_image` (a path), so metadata
  # lookups must take the exact `volume` entry, and only when it is a list.
  row <- data.frame(map_id = "m1", label = "Faces", analysis_label = "Faces",
                    threshold = 3.1, tail = "two_sided", stringsAsFactors = FALSE)
  img <- tempfile(fileext = ".png")
  grDevices::png(img, width = 40, height = 20)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  grDevices::dev.off()
  panel <- list(volume_image = img)
  expect_no_error(
    out <- utils::capture.output(
      neuromosaic:::.montage_report_emit_map_figures(row, panel, is_html = TRUE)
    )
  )
  expect_true(any(grepl("<img", out, fixed = TRUE)))
  expect_no_error(cap <- neuromosaic:::.montage_report_figure_caption(row, panel, "volume"))
  expect_type(cap, "character")
  # The Methods colour-cap and slice rules use the same exact lookup.
  expect_null(neuromosaic:::.montage_report_volume_meta(panel))
  expect_identical(
    neuromosaic:::.montage_report_volume_meta(list(volume = list(cap = 3)))$cap, 3
  )
})

test_that("an HTML report rendered in a C locale keeps its non-ASCII text", {
  skip_if_not(rmarkdown::pandoc_available())
  # cat() in a non-UTF-8 locale prints UTF-8 text as "<U+00B7>"; the report
  # renders with a UTF-8 character type whatever the session's locale.
  dir <- withr::local_tempdir()
  img <- file.path(dir, "z.png")
  grDevices::png(img, width = 40, height = 20)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  grDevices::dev.off()
  manifest <- data.frame(
    analysis_id = "faces", analysis_label = "Faces versus places",
    map_id = "faces_z", role = "primary", quantity = "test_statistic",
    distribution = "z", threshold = 3.1, label = "Z statistic",
    n = 32, stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(function(row) 1))
  old <- Sys.getlocale("LC_CTYPE")
  withr::defer(Sys.setlocale("LC_CTYPE", old))
  Sys.setlocale("LC_CTYPE", "C")
  out <- render_montage_report(
    manifest, output_file = file.path(dir, "c-locale.html"),
    title = "C locale · fixture",
    panels = list(faces_z = list(volume_image = img)),
    materialize_recipes = FALSE, check_files = FALSE, quiet = TRUE
  )
  expect_identical(Sys.getlocale("LC_CTYPE"), "C", label = "the locale is restored")
  html <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_false(grepl("<U+", html, fixed = TRUE))
  expect_false(grepl("&lt;U+", html, fixed = TRUE))
  expect_true(grepl("·", html, fixed = TRUE))
})
