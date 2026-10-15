# Round 4 (report page): one hemisphere rule, peaks that never look tied,
# Methods that name the underlay, the surface method, the glass brain and the
# code that drew the report, QC stated as supplied metadata, one h1, a file
# stem per map, and page-script rules for position, prefetch, print and
# file:// links (behaviour itself is checked in a browser by the gate).

test_that("hemisphere follows the peak's x coordinate, midline when |x| < 2 mm", {
  hemi <- neuromosaic:::.montage_hemisphere_from_x
  expect_identical(
    hemi(c(-4, -2, -1.5, 0, 1.9, 2, NA), fallback = c(rep(NA, 6), "left")),
    c("left", "left", "midline", "midline", "midline", "right", "left")
  )
  # Labelled and unlabelled peaks get the same rule: an atlas parcel's own
  # hemisphere does not override x = 0.
  tbl <- data.frame(peak_mni_x = c(0, -2, 0, 6), hemisphere = c("left", "left", NA, "right"),
                    stringsAsFactors = FALSE)
  expect_identical(neuromosaic:::.montage_peak_hemisphere(tbl)$hemisphere,
                   c("midline", "left", "midline", "right"))
  # Table and CSV derive out-of-atlas rows with the same rule.
  cells <- neuromosaic:::.montage_report_region_cells(data.frame(
    peak_mni_x = c(-2, 0, 1), atlas_label = NA_character_, hemisphere = NA_character_,
    stringsAsFactors = FALSE
  ))
  expect_identical(cells$hemisphere, c("left", "midline", "midline"))
})

test_that("peaks equal at 2 dp are printed with the digits that separate them", {
  digits <- neuromosaic:::.montage_report_tie_digits
  expect_identical(digits(c(-11.6329, -11.6312, 5.1, 7.3)), c(3L, 3L, 2L, 2L))
  # Exactly equal values stay at 2 dp (more digits would not separate them).
  expect_identical(digits(c(4.5, 4.5, 9)), c(2L, 2L, 2L))
  expect_identical(digits(c(7.80041, 7.80049)), c(4L, 4L))
  tbl <- data.frame(
    cluster_id = c("N5", "N6", "P1"), sign = c("negative", "negative", "positive"),
    n_voxels = c(281L, 1654L, 30L), peak_mni_x = c(-4, 10, 20), peak_mni_y = 0,
    peak_mni_z = 0, max_stat = c(-11.6329, -11.6312, 9.25),
    atlas_label = "Caudate", hemisphere = NA_character_, network = NA_character_,
    stringsAsFactors = FALSE
  )
  formatted <- neuromosaic:::.montage_report_format_peak_table(tbl, value_label = "z")
  html <- neuromosaic:::.montage_report_html_table(formatted)
  expect_match(html, "−11.633", fixed = TRUE)
  expect_match(html, "−11.631", fixed = TRUE)
  expect_match(html, ">9.25<", fixed = TRUE)
  # The Sign column drop in emit_table keeps the per-cell digits.
  out <- paste(utils::capture.output(
    neuromosaic:::.montage_report_emit_table(tbl, TRUE, data.frame(stat_kind = "z"))
  ), collapse = "")
  expect_match(out, "−11.633", fixed = TRUE)
})

test_that("uniform QC is stated as supplied metadata, not a check the report ran", {
  qc <- data.frame(map_id = c("a", "b"), n = 486L, qc_status = "ok", stringsAsFactors = FALSE)
  line <- neuromosaic:::.montage_report_qc_line(qc)
  if (!is.null(line)) {
    expect_match(line, "QC: ok (as supplied)", fixed = TRUE)
    expect_false(grepl("QC passed", line, fixed = TRUE))
  }
  # In the header the N and the QC statement wrap as separate facts (no
  # 320 px overflow on one long nowrap run).
  manifest <- data.frame(map_id = c("a", "b"), label = c("A", "B"), stringsAsFactors = FALSE)
  html <- paste(neuromosaic:::.montage_report_header_html(
    "T", "2026-01-01", manifest, character(), NULL,
    qc_line = "N = 486 for all 2 maps \u00b7 QC: ok (as supplied)"
  ), collapse = "")
  expect_match(html, "<span class=\"nm-fact-qc\">N = 486 for all 2 maps</span>", fixed = TRUE)
  expect_match(html, "<span class=\"nm-fact-qc\">QC: ok (as supplied)</span>", fixed = TRUE)
  src <- paste(deparse(neuromosaic:::.montage_report_qc_line), collapse = "\n")
  expect_false(grepl("QC passed", src, fixed = TRUE))
})

r4_methods_rd <- function() {
  manifest <- data.frame(
    map_id = c("a", "b"), label = c("A > B", "C > D"), analysis_id = c("x", "y"),
    stat_kind = "z", quantity = "test_statistic", distribution = "z",
    level = "group", n = 486L, threshold = 5,
    effective_threshold = 5, effective_tail = "two_sided",
    effective_min_cluster_size = 20L, effective_connectivity = "18-connect",
    effective_q = NA_real_, stringsAsFactors = FALSE
  )
  panels <- list(
    a = list(volume = list(cap = 22.25, signed = TRUE, slice_selection = "automatic", slice_labels = c("z = -30", "z = 62")),
             surface_image = "s.png", summary_image = "t.png"),
    b = list(volume = list(cap = 22.25, signed = TRUE, slice_selection = "automatic", slice_labels = c("z = -30", "z = 62")),
             summary_image = "u.png")
  )
  list(
    manifest = manifest, panels = panels,
    labelling = list(atlases = "Schaefer-200 (7 networks)", radius = 4, citations = list()),
    params = list(policy = montage_policy(cap_quantile = 0.99), layout = character()),
    underlay = list(template = "MNI152NLin6Asym", spacing = c(2, 2, 2)),
    surface_method = list(space = "fsaverage6", surface = "inflated", vertices = 40962L,
                          fun = "avg", sampling = "thickness", interpolation = "linear",
                          aggregate = "mean", depth = seq(0.1, 0.9, length.out = 5L),
                          smooth_fwhm = 0, medial_wall = TRUE),
    provenance = list(package_version = "9.9.9", software = "neuromosaic 9.9.9",
                      package_source = "git 0123456789ab (with uncommitted changes)",
                      package_url = "https://github.com/bbuchsbaum/neuromosaic",
                      r_version = "4.5.1")
  )
}

test_that("Methods name the underlay, surface method, glass brain and the code's source", {
  rd <- r4_methods_rd()
  m <- neuromosaic:::.montage_report_methods(rd)
  text <- m$text
  expect_match(text, "neuromosaic 9.9.9 (https://github.com/bbuchsbaum/neuromosaic)", fixed = TRUE)
  expect_match(text, "on the MNI152NLin6Asym brain template (2 mm voxels)", fixed = TRUE)
  # Slice wording matches the rule: whole-voxel steps, centred in the band.
  expect_match(text, "spaced in equal whole-voxel steps and centred within the 5th–95th percentile range",
               fixed = TRUE)
  expect_false(grepl("spaced evenly over the central 90%", text, fixed = TRUE))
  # Surface: mesh, depth sampling, re-thresholding after averaging, no extent claim.
  expect_match(text, "the fsaverage6 cortical surface (40,962 vertices per hemisphere)", fixed = TRUE)
  expect_match(text, "at 5 depths from 10% to 90% of cortical thickness (trilinear interpolation)", fixed = TRUE)
  expect_match(text, "aggregation: mean; projection function: avg", fixed = TRUE)
  expect_match(text, "with the medial wall masked", fixed = TRUE)
  expect_match(text, "removed before projection; the remaining volume values are sampled before thresholding", fixed = TRUE)
  expect_match(text, "maximum-|z| projections (glass brain)", fixed = TRUE)
  expect_match(text, "the hemisphere follows the peak's x coordinate (midline when |x| < 2 mm)", fixed = TRUE)
  expect_match(text, "peaks equal at the printed precision are ordered by their unrounded values", fixed = TRUE)
  expect_match(text, "(neuromosaic git 0123456789ab (with uncommitted changes))", fixed = TRUE)
  refs <- vapply(m$references, neuromosaic:::.montage_report_reference_text, "")
  expect_match(refs[[1]], "^neuromosaic 9.9.9\\. .*R package")
  expect_match(refs[[1]], "https://github.com/bbuchsbaum/neuromosaic", fixed = TRUE)
  html <- paste(utils::capture.output(neuromosaic:::.montage_report_emit_methods(rd, TRUE)), collapse = "")
  expect_match(html, "<a href=\"https://github.com/bbuchsbaum/neuromosaic\">", fixed = TRUE)
  # Without method details, do not invent a projection or aggregation rule.
  rd$surface_method <- NULL
  rd$underlay <- NULL
  text2 <- neuromosaic:::.montage_report_methods(rd)$text
  expect_match(text2, "surface mapping details not recorded", fixed = TRUE)
  expect_false(grepl("brain template", text2, fixed = TRUE))
})

test_that("underlay, surface method and package source are summarised from real inputs", {
  sp <- neuroim2::NeuroSpace(dim = c(6, 6, 6), spacing = c(2, 2, 2))
  vol <- neuroim2::NeuroVol(array(1, c(6, 6, 6)), space = sp)
  path <- file.path(tempdir(), "tpl-MNI152NLin6Asym_res-02_desc-brain_T1w.nii.gz")
  neuroim2::write_vol(vol, path)
  u <- neuromosaic:::.montage_underlay_summary(path)
  expect_identical(u$template, "MNI152NLin6Asym")
  expect_equal(u$spacing, c(2, 2, 2))
  u2 <- neuromosaic:::.montage_underlay_summary(neuroim2::read_vol(path))
  expect_identical(u2$template, "MNI152NLin6Asym")
  expect_null(neuromosaic:::.montage_underlay_summary(NULL))
  src <- neuromosaic:::.montage_package_source()
  expect_true(is.character(src) && length(src) == 1L)
  if (!is.na(src) && startsWith(src, "git ")) {
    expect_match(src, "^git [0-9a-f]{12}( \\(with uncommitted changes\\))?$")
  }
  url <- neuromosaic:::.montage_package_url()
  expect_match(url, "github.com/bbuchsbaum/neuromosaic", fixed = TRUE)
  fake <- structure(list(surface_space = "fsaverage6", surf_type = "inflated",
                         cortex_mask = list(lh = TRUE)), class = "surfatlas")
  sm <- neuromosaic:::.montage_surface_method(fake, list())
  expect_identical(sm$space, "fsaverage6")
  expect_identical(sm$sampling, "thickness")
  expect_identical(sm$interpolation, "linear")
  expect_equal(sm$depth, seq(0.1, 0.9, length.out = 5L))
  expect_true(sm$medial_wall)
  expect_identical(neuromosaic:::.montage_surface_method(fake, list(sampling = "midpoint"))$sampling,
                   "midpoint")
})

test_that("one h1: sections render as h2, map titles as h3, maps carry a file stem", {
  manifest <- data.frame(map_id = "m1", label = "Story > math", task = "Language",
                         stringsAsFactors = FALSE)
  fmt <- montage_report_formatters(manifest = manifest, layout = "task", is_html = TRUE)
  start <- paste(capture.output(fmt$emit_map_start(1, "Story > math", "m1")), collapse = "\n")
  expect_match(start, "<h3 class=\"nm-panel-title\"", fixed = TRUE)
  expect_false(grepl("<h2 class=\"nm-panel-title\"", start, fixed = TRUE))
  expect_match(start, "data-nm-file-stem=\"story-gt-math\"", fixed = TRUE)
  of <- neuromosaic:::.montage_rmarkdown_output_format("html")
  expect_true("--shift-heading-level-by=1" %in% of$pandoc$args)
  rmd <- readLines(system.file("templates", "montage_report.Rmd", package = "neuromosaic"))
  expect_true(any(grepl("--shift-heading-level-by=1", rmd, fixed = TRUE)))
  qmd <- readLines(system.file("templates", "montage_report.qmd", package = "neuromosaic"))
  expect_true(any(grepl("shift-heading-level-by: 1", qmd, fixed = TRUE)))
})

test_that("page script: history position, neighbour prefetch, print notes, file:// links", {
  js <- paste(readLines(system.file("report", "montage-report.js", package = "neuromosaic")),
              collapse = "\n")
  # Reload and Back keep the browser's position: no manual restoration, and
  # cold-hash holds only on a fresh navigation.
  expect_false(grepl("scrollRestoration = 'manual'", js, fixed = TRUE))
  expect_match(js, "navigationType() !== 'navigate'", fixed = TRUE)
  expect_match(js, "window.addEventListener('popstate'", fixed = TRUE)
  expect_match(js, "nmScrollY", fixed = TRUE)
  # Peak jumps leave a reader who moved on alone.
  expect_match(js, "inputSince(asked)", fixed = TRUE)
  expect_match(js, "'Slices ready at '", fixed = TRUE)
  # Prefetch: neighbouring maps only, nothing on slow or metered links, no
  # speculative print copies.
  expect_match(js, "api.prefetchNeighbours", fixed = TRUE)
  expect_match(js, "c.type === 'cellular'", fixed = TRUE)
  expect_match(js, "c.effectiveType !== '4g'", fixed = TRUE)
  expect_false(grepl("prefetchLight", js, fixed = TRUE))
  # Print: figures not yet loaded say so on paper.
  expect_match(js, "nm-print-pending", fixed = TRUE)
  # file://: the titled download is relabelled, never a fake download.
  expect_match(js, "a.textContent = 'Open image'", fixed = TRUE)
  expect_match(js, "a.removeAttribute('download')", fixed = TRUE)
  # Contents: sections are the pandoc section headings, h1 or h2.
  expect_match(js, "h.tagName === 'H1' || h.tagName === 'H2'", fixed = TRUE)
})

test_that("page CSS: print keeps blocks together, equal figure widths, phone glance 2-up", {
  css <- paste(readLines(system.file("report", "montage-report.css", package = "neuromosaic")),
               collapse = "\n")
  print_css <- substring(css, regexpr("@media print {", css, fixed = TRUE))
  expect_match(print_css, ".nm-atlas-legend { break-before: avoid;", fixed = TRUE)
  expect_match(print_css, ".nm-figure img { display: block; width: 76%;", fixed = TRUE)
  expect_match(print_css, ".nm-figure-surface img { max-height: none; }", fixed = TRUE)
  expect_match(print_css, "background: none !important", fixed = TRUE)
  expect_match(print_css, ".nm-figure.nm-print-pending::before", fixed = TRUE)
  expect_match(print_css, ".nm-atlas-section { white-space: normal;", fixed = TRUE)
  expect_match(print_css, ".section.level2:has(> .nm-map) > h2", fixed = TRUE)
  expect_false(grepl("level1", css, fixed = TRUE))
  phone <- substring(css, regexpr("@media (max-width: 480px) {", css, fixed = TRUE))
  expect_match(phone, "grid-template-columns: repeat(2, minmax(0, 1fr))", fixed = TRUE)
  expect_match(phone, ".nm-atlas-peak .nm-atlas-region { display: none; }", fixed = TRUE)
  # One line per glance peak on screen (the full region is the tooltip).
  expect_match(css, ".nm-atlas-region {\n  min-width: 0;\n  overflow: hidden;", fixed = TRUE)
})
