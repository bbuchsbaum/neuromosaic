# Round 3 (report page): cluster IDs by peak strength within sign, explicit
# cerebellar reference and stated label sources, the generated Methods
# paragraph, CSV parity with the table, deferred figures that still print,
# titled PNG downloads, and touch-size controls.

r3_space <- function(dims = c(10, 10, 10)) {
  neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2), origin = c(-9, -9, -9))
}

r3_atlas <- function() {
  sp <- r3_space()
  arr <- array(0L, dim = dim(sp))
  arr[1:3, 1:3, 1:3] <- 1L
  atlas <- list(
    name = "Schaefer-200-7networks::ASEG",
    atlas = neuroim2::NeuroVol(arr, space = sp), ids = 1L, labels = "PFCl_4",
    orig_labels = "RH_Cont_PFCl_4", hemi = "right",
    metadata = list(citations = data.frame(
      role = "atlas", title = "Local-Global Parcellation", author = "Alexander Schaefer and others",
      year = 2018, journal = "Cerebral Cortex", doi = "10.1093/cercor/bhx179",
      stringsAsFactors = FALSE
    ))
  )
  class(atlas) <- c("toy", "atlas")
  atlas
}

r3_cerebellum <- function() {
  sp <- r3_space()
  cere <- array(0L, dim = dim(sp))
  cere[8:10, 1:3, 1:3] <- 47L
  neuroim2::NeuroVol(cere, space = sp)
}

test_that("clusters are numbered within sign by |peak|; ties go to the larger cluster", {
  clusters <- data.frame(
    cluster_id = as.character(1:7),
    sign = c("positive", "positive", "positive", "negative", "negative", "negative", "positive"),
    max_stat = c(20.70, 20.25, 21.58, -5, -9, -7, 20.25),
    n_voxels = c(500, 300, 20, 40, 30, 30, 900),
    stringsAsFactors = FALSE
  )
  out <- neuromosaic:::.montage_sign_cluster_ids(clusters)
  # 21.58 is the strongest positive; the two 20.25 peaks tie and the larger
  # cluster (900 voxels) comes first.
  expect_identical(out$cluster_id, c("P2", "P4", "P1", "N3", "N1", "N2", "P3"))
  # Values, sizes and row order are untouched.
  expect_identical(out$max_stat, clusters$max_stat)
  expect_identical(out$n_voxels, clusters$n_voxels)
  # Numbering is idempotent.
  expect_identical(neuromosaic:::.montage_sign_cluster_ids(out)$cluster_id, out$cluster_id)
})

test_that("montage_peak_table: P1 is the strongest positive and N1 the strongest negative", {
  sp <- r3_space(c(12, 12, 12))
  arr <- array(0, dim = dim(sp))
  arr[2:6, 2:6, 2:6] <- 6          # big, weaker positive cluster (125 voxels)
  arr[4, 4, 4] <- 7
  arr[9:10, 9:10, 9:10] <- 9       # small, stronger positive cluster
  arr[2:3, 9:11, 9:11] <- -6       # negative cluster
  stat <- neuroim2::NeuroVol(arr, space = sp)
  atl <- neuroim2::NeuroVol(array(0L, dim = dim(sp)), space = sp)
  atlas <- list(name = "toy", atlas = atl, ids = 1L, labels = "x", hemi = "left")
  class(atlas) <- c("toy", "atlas")
  tbl <- suppressWarnings(montage_peak_table(
    stat, atlas, threshold = 3, min_cluster_size = 2L, cerebellum_reference = FALSE
  ))
  pos <- tbl[tbl$sign == "positive", ]
  expect_identical(pos$cluster_id[which.max(pos$max_stat)], "P1")
  expect_equal(max(pos$max_stat), 9)
  expect_identical(tbl$cluster_id[tbl$sign == "negative"], "N1")
  all <- attr(tbl, "all_clusters")
  expect_setequal(all$cluster_id, c("P1", "P2", "N1"))
  # The viewer opens at P1: the scene's first positive cluster is the strongest.
  coord <- neuromosaic:::.montage_volume_scene_p1_coord(
    tbl, list(affine = as.numeric(neuroim2::trans(sp)), dimensions = dim(sp))
  )
  p1 <- all[all$cluster_id == "P1", ]
  expect_equal(coord, c(p1$peak_mni_x, p1$peak_mni_y, p1$peak_mni_z), tolerance = 1e-6)
  # The documented rule matches the behaviour.
  # Source tree under devtools::test(); the installed Rd database under R CMD check.
  rd_path <- test_path("../../man/montage_peak_table.Rd")
  rd <- if (file.exists(rd_path)) {
    readLines(rd_path, warn = FALSE)
  } else {
    utils::capture.output(print(tools::Rd_db("neuromosaic")[["montage_peak_table.Rd"]]))
  }
  expect_false(any(grepl("strongest-first order of the", rd, fixed = TRUE)))
  expect_true(any(grepl("by peak strength", rd, fixed = TRUE)))
})

test_that("an explicit cerebellar reference wins over the option and is recorded", {
  atlas <- r3_atlas()
  tbl <- data.frame(
    cluster_id = c("P1", "P2"), sign = "positive", n_voxels = c(30L, 20L),
    peak_mni_x = c(9, 5), peak_mni_y = c(-7, 9), peak_mni_z = c(-7, 9),
    max_stat = c(9, 8), atlas_label = NA_character_, hemisphere = NA_character_,
    network = NA_character_, stringsAsFactors = FALSE
  )
  withr::local_options(neuromosaic.cerebellum_reference = FALSE)
  ref <- r3_cerebellum()
  out <- neuromosaic:::.montage_fill_peak_labels(tbl, atlas, radius = 0, reference = ref)
  expect_identical(out$atlas_label, c("Cerebellum", NA))
  expect_identical(out$label_source, c("cerebellum", NA))
  expect_identical(attr(out, "cerebellum_source"), "user-supplied cerebellar segmentation")
  # FALSE as the argument means none, even with the option set.
  withr::local_options(neuromosaic.cerebellum_reference = ref)
  none <- neuromosaic:::.montage_fill_peak_labels(tbl, atlas, radius = 0, reference = FALSE)
  expect_true(all(is.na(none$atlas_label)))
  expect_null(attr(none, "cerebellum_source"))
  # A missing path warns instead of silently fetching anything.
  expect_warning(
    expect_null(neuromosaic:::.montage_cerebellum_reference(tempfile(fileext = ".nii.gz"))),
    "not found"
  )
  expect_error(neuromosaic:::.montage_cerebellum_reference(42), "cerebellum_reference")
  expect_true("cerebellum_reference" %in% names(formals(render_montage_report)))
  expect_true("cerebellum_reference" %in% names(formals(montage_peak_table)))
})

test_that("label sources are summarised, stated in the header, provenance and table foot", {
  atlas <- r3_atlas()
  ref <- r3_cerebellum()
  attr(ref, "nm_source") <- neuromosaic:::.montage_hcp_cerebellum_source
  all <- data.frame(
    cluster_id = c("P1", "P2", "P3"), sign = "positive", n_voxels = c(30L, 20L, 10L),
    peak_mni_x = c(9, -7, 5), peak_mni_y = c(-7, -7, 9), peak_mni_z = c(-7, -7, 9),
    max_stat = c(9, 8, 7), atlas_label = c("Cerebellum", "PFCl_4", NA),
    hemisphere = c("right", "right", NA), network = c(NA, "Cont", NA),
    label_source = c("cerebellum", "atlas", NA), label_distance_mm = c(0, 0, NA),
    stringsAsFactors = FALSE
  )
  peak <- all
  attr(peak, "all_clusters") <- all
  attr(peak, "cerebellum_source") <- attr(ref, "nm_source")
  lab <- neuromosaic:::.montage_labelling_summary(atlas, list(m = list(peak_table = peak)),
                                                 reference = ref)
  expect_identical(lab$atlases, c("Schaefer-200 (7 networks)", "FreeSurfer ASEG"))
  expect_identical(lab$cerebellum$n, 1L)
  expect_true(lab$cerebellum$hcp)
  expect_identical(lab$citations[[1]]$short, "Schaefer et al., 2018")
  line <- neuromosaic:::.montage_report_label_line(lab)
  expect_match(line, "Peak labels: Schaefer-200 (7 networks), FreeSurfer ASEG", fixed = TRUE)
  expect_match(line, "cerebellum: HCP grayordinate segmentation (TemplateFlow)", fixed = TRUE)
  expect_match(line, "≈ nearest region ≤ 4 mm", fixed = TRUE)
  header <- neuromosaic:::.montage_report_header_html(
    "T", NULL, data.frame(), character(), NULL, label_line = line
  )
  expect_match(header, "nm-report-labels", fixed = TRUE)
  expect_match(header, "href=\"#nm-methods\"", fixed = TRUE)
  # The link never wraps onto a line of its own.
  expect_match(header, "<span class=\"nm-nowrap\">≈ nearest region ≤ 4 mm. <a href=\"#nm-methods\">Methods</a></span>",
               fixed = TRUE)
  prov <- neuromosaic:::.montage_report_label_provenance(
    list(package = "neuromosaic"), lab,
    data.frame(label = "A", path = "/x/tfMRI_A_zstat1.nii.gz", stringsAsFactors = FALSE)
  )
  expect_identical(prov$peak_label_atlas, "Schaefer-200 (7 networks) + FreeSurfer ASEG")
  expect_match(prov$peak_label_cerebellum, "HCP grayordinate subcortical segmentation.*\\(1 peak\\)")
  expect_identical(prov$input_files, "A = tfMRI_A_zstat1.nii.gz")
  foot <- neuromosaic:::.montage_report_table_foot(peak)
  expect_match(foot, "Cerebellum labels: HCP grayordinate subcortical segmentation", fixed = TRUE)
  # Without a reference, nothing is claimed about the cerebellum.
  bare <- neuromosaic:::.montage_labelling_summary(atlas, list(m = list(peak_table = all)))
  expect_null(bare$cerebellum)
  expect_false(grepl("cerebellum", neuromosaic:::.montage_report_label_line(bare)))
  expect_identical(neuromosaic:::.montage_report_label_provenance(list(), bare, NULL)$peak_label_cerebellum,
                   "none")
})

test_that("the CSV carries the table's region and hemisphere text for out-of-atlas rows", {
  all <- data.frame(
    cluster_id = c("P1", "P2", "N1"), sign = c("positive", "positive", "negative"),
    n_voxels = c(30L, 20L, 25L), peak_mni_x = c(-4, 0, 6), peak_mni_y = c(-38, -24, 16),
    peak_mni_z = c(4, 12, -2), max_stat = c(9, 8, -7),
    atlas_label = c(NA, NA, "Caudate"), hemisphere = c(NA, NA, "right"),
    network = NA_character_, label_source = c(NA, NA, "atlas"),
    label_distance_mm = c(NA, NA, 0), stringsAsFactors = FALSE
  )
  csv <- neuromosaic:::.montage_report_cluster_csv(all)
  rows <- utils::read.csv(text = csv, stringsAsFactors = FALSE)
  expect_identical(rows$region, c("outside atlas", "outside atlas", "Caudate"))
  expect_identical(rows$hemisphere, c("left", "midline", "right"))
  expect_identical(rows$label_source, c("none", "none", "atlas"))
  table <- neuromosaic:::.montage_report_format_peak_table(all)
  expect_identical(table$Region, rows$region)
  expect_identical(table$Hemisphere, rows$hemisphere)
})

test_that("bundle figures are deferred with a reserved box; inline figures load directly", {
  png <- tempfile(fileext = ".png")
  grDevices::png(png, width = 400, height = 200)
  graphics::plot.new()
  grDevices::dev.off()
  dark <- sub("\\.png$", "-dark.png", png)
  file.copy(png, dark)
  narrow <- sub("\\.png$", "-narrow.png", png)
  grDevices::png(narrow, width = 200, height = 300)
  graphics::plot.new()
  grDevices::dev.off()
  assets <- list(dir = tempfile("assets"), href = "index_files/figures")
  html <- paste(utils::capture.output(neuromosaic:::.montage_report_emit_panel_image(
    png, "alt text", is_html = TRUE, figure_assets = assets, caption = "Axial",
    dark = dark, narrow = narrow, full_caption = "Full caption.", download_name = "map_volume.png",
    title = "Story > math", kicker = "Language"
  )), collapse = "")
  expect_match(html, "<img data-nm-src=\"index_files/figures/", fixed = TRUE)
  expect_false(grepl("<img src=\"index_files", sub("<noscript>.*$", "", html), fixed = TRUE))
  expect_match(html, "data-nm-srcset=\"index_files/figures/", fixed = TRUE)
  expect_false(grepl(" srcset=\"", html, fixed = TRUE))
  # The reserved box follows the source in use: the phone variant has its own
  # ratio (a single inline aspect-ratio squashed it), applied by the CSS under
  # the same media query as its <source>.
  expect_match(html, "style=\"--nm-ar: 400 / 200; --nm-ar-narrow: 200 / 300\"", fixed = TRUE)
  expect_false(grepl("style=\"aspect-ratio", html, fixed = TRUE))
  expect_match(html, "<source media=\"\\(max-width: 600px\\)\"[^>]*data-nm-srcset=\"[^\"]+\" width=\"200\" height=\"300\"")
  css <- paste(readLines(system.file("report", "montage-report.css", package = "neuromosaic")),
               collapse = "\n")
  expect_match(css, "@media (max-width: 600px) {\n  .nm-figure img[style*=\"--nm-ar-narrow\"] { aspect-ratio: var(--nm-ar-narrow); }",
               fixed = TRUE)
  expect_match(html, "width=\"400\" height=\"200\"", fixed = TRUE)
  expect_match(html, "<noscript><img src=\"index_files/figures/", fixed = TRUE)
  expect_match(html, "data-nm-title=\"Story &gt; math\"", fixed = TRUE)
  expect_match(html, "data-nm-kicker=\"Language\"", fixed = TRUE)
  expect_match(html, "data-nm-titled href=\"index_files/figures/[^\"]+\" download=\"map_volume.png\">Download PNG")
  expect_match(html, "download=\"map_volume_untitled.png\">image only</a>", fixed = TRUE)
  inline <- paste(utils::capture.output(neuromosaic:::.montage_report_emit_panel_image(
    png, "alt", is_html = TRUE, caption = "Axial", full_caption = "Full."
  )), collapse = "")
  expect_match(inline, "<img src=\"data:image/png;base64,", fixed = TRUE)
  expect_false(grepl("data-nm-src", inline, fixed = TRUE))
  expect_match(inline, "data-nm-titled", fixed = TRUE)
})

test_that("the Methods paragraph states inputs, threshold, cap, slices, labels and software", {
  manifest <- data.frame(
    map_id = c("a", "b"), label = c("A > B", "C > D"), analysis_id = c("x", "y"),
    stat_kind = "z", quantity = "test_statistic", distribution = "z",
    level = "group", n = 486L, threshold = 5,
    effective_threshold = 5, effective_tail = "two_sided",
    effective_min_cluster_size = 20L, effective_connectivity = "18-connect",
    effective_q = NA_real_, stringsAsFactors = FALSE
  )
  panels <- list(
    a = list(volume = list(cap = 22.2539, signed = TRUE,
                           slice_labels = c("z = -30", "z = -22", "z = 62")),
             surface_image = "s.png"),
    b = list(volume = list(cap = 22.2539, signed = TRUE,
                           slice_labels = c("z = -30", "z = -22", "z = 62")))
  )
  ref <- r3_cerebellum()
  attr(ref, "nm_source") <- neuromosaic:::.montage_hcp_cerebellum_source
  lab <- list(atlases = c("Schaefer-200 (7 networks)", "ASEG"), radius = 4,
              citations = list(list(short = "Schaefer et al., 2018", title = "T",
                                    journal = "Cerebral Cortex", doi = "10.1093/cercor/bhx179")),
              cerebellum = list(source = attr(ref, "nm_source"), hcp = TRUE, n = 3L,
                                short = "HCP grayordinate segmentation (TemplateFlow)",
                                citations = list(list(short = "Glasser et al., 2013", doi = "10.1016/j.neuroimage.2013.04.127"),
                                                 list(short = "Ciric et al., 2022", doi = "10.1038/s41592-022-01681-2"))))
  rd <- list(
    manifest = manifest, panels = panels, labelling = lab,
    params = list(policy = montage_policy(cap_quantile = 0.99), layout = character()),
    provenance = list(package_version = "9.9.9", software = "neuromosaic 9.9.9; neuroim2 1.0",
                      r_version = "4.5.1")
  )
  m <- neuromosaic:::.montage_report_methods(rd)
  text <- m$text
  expect_match(text, "^Two group-level z-statistic maps \\(N = 486 each\\)")
  expect_match(text, "neuromosaic 9.9.9", fixed = TRUE)
  expect_match(text, "a fixed voxelwise threshold of |z| ≥ 5 (two-sided)", fixed = TRUE)
  expect_match(text, "clusters smaller than 20 voxels (18-connectivity) were removed", fixed = TRUE)
  expect_match(text, "saturate at ±22.3, the 99th percentile of suprathreshold |z| pooled", fixed = TRUE)
  expect_match(text, "up to 3 axial slices", fixed = TRUE)
  expect_match(text, "averaged across cortical depth", fixed = TRUE)
  expect_match(text, "P1 is the strongest positive cluster", fixed = TRUE)
  expect_match(text, "Schaefer-200 (7 networks) and ASEG atlases (Schaefer et al., 2018)", fixed = TRUE)
  expect_match(text, "within 4 mm", fixed = TRUE)
  expect_match(text, "HCP grayordinate subcortical segmentation (Glasser et al., 2013) obtained from TemplateFlow (Ciric et al., 2022)", fixed = TRUE)
  expect_match(text, "Software: neuromosaic 9.9.9, neuroim2 1.0; R 4.5.1.", fixed = TRUE)
  expect_length(m$references, 4L) # + neuromosaic itself (round 4)
  html <- paste(utils::capture.output(neuromosaic:::.montage_report_emit_methods(rd, TRUE)), collapse = "")
  expect_match(html, "id=\"nm-methods\"", fixed = TRUE)
  expect_match(html, "data-nm-copy-target=\"nm-methods-copy\">Copy methods</button>", fixed = TRUE)
  expect_match(html, "<a href=\"https://doi.org/10.1093/cercor/bhx179\">", fixed = TRUE)
  md <- paste(utils::capture.output(neuromosaic:::.montage_report_emit_methods(rd, FALSE)), collapse = "\n")
  expect_match(md, "- Glasser et al., 2013.", fixed = TRUE)
  # An FDR rule is named as such; a p-derived threshold says uncorrected.
  rd$manifest$threshold <- NULL
  rd$manifest$effective_q <- 0.05
  expect_match(neuromosaic:::.montage_report_methods(rd)$text,
               "Benjamini–Hochberg FDR q = 0.05 per map", fixed = TRUE)
  rd$manifest$effective_q <- NA_real_
  expect_match(neuromosaic:::.montage_report_methods(rd)$text,
               "voxelwise p < 0.005 (uncorrected)", fixed = TRUE)
  # Both templates place Methods before Provenance.
  for (tpl in c("montage_report.Rmd", "montage_report.qmd")) {
    lines <- readLines(system.file("templates", tpl, package = "neuromosaic"))
    expect_lt(which(lines == "# Methods"), which(lines == "# Provenance"))
    expect_true(any(grepl("labelling = rd$labelling", lines, fixed = TRUE)))
  }
})

test_that("page CSS: no content-visibility, screen-only dark tokens, 24 px targets", {
  css <- paste(readLines(system.file("report", "montage-report.css", package = "neuromosaic")),
               collapse = "\n")
  expect_false(grepl("content-visibility", css, fixed = TRUE))
  expect_match(css, "@media screen and (prefers-color-scheme: dark)", fixed = TRUE)
  expect_match(css, "@media screen {\n  :root[data-theme=\"dark\"]", fixed = TRUE)
  for (sel in c(".nm-peak-go {", ".nm-fig-action {")) {
    rule <- sub("\\}.*$", "", substring(css, regexpr(sel, css, fixed = TRUE)))
    expect_match(rule, "min-height: 24px", fixed = TRUE)
  }
  expect_match(css, "grid-template-rows: subgrid", fixed = TRUE)
})

test_that("page script: deferred loading, print, landing jumps, announcements", {
  js <- paste(readLines(system.file("report", "montage-report.js", package = "neuromosaic")),
              collapse = "\n")
  expect_match(js, "new IntersectionObserver", fixed = TRUE)
  expect_match(js, "window.addEventListener('beforeprint'", fixed = TRUE)
  expect_match(js, "api.loadAll()", fixed = TRUE)
  expect_match(js, "src.setAttribute('media', 'not all')", fixed = TRUE)
  expect_match(js, "api.holdTarget", fixed = TRUE)
  expect_match(js, "aria-live', 'polite'", fixed = TRUE)
  expect_match(js, "' copied to the clipboard'", fixed = TRUE)
  expect_match(js, "canvas.toBlob", fixed = TRUE)
  expect_match(js, "if (c.saveData) return true;", fixed = TRUE)
  expect_false(grepl("top.href = '#'", js, fixed = TRUE))
})

test_that("shared slice sets are captioned from the renderer's slice scope", {
  scope <- list(scope = "section", section = "Working memory (N-back)",
                map_ids = c("a", "b", "c"))
  share <- neuromosaic:::.montage_report_slice_share(scope)
  expect_identical(share$short, "same slices for all 3 maps in Working memory (N-back)")
  expect_null(neuromosaic:::.montage_report_slice_share(list(scope = "map", map_ids = "a")))
  expect_null(neuromosaic:::.montage_report_slice_share(NULL))
  row <- data.frame(map_id = "a", label = "A > B", stat_kind = "z", stringsAsFactors = FALSE)
  panel <- list(volume = list(slice_labels = c("z = -30", "z = 58")), slice_scope = scope)
  cap <- neuromosaic:::.montage_report_figure_caption(row, panel, "volume")
  expect_match(cap, "The same slices are shown for all 3 maps in Working memory (N-back).", fixed = TRUE)
  rd <- list(manifest = data.frame(map_id = c("a", "b"), label = c("A", "B"), stringsAsFactors = FALSE),
             panels = list(a = panel, b = panel), params = list(policy = montage_policy()))
  expect_match(neuromosaic:::.montage_report_methods(rd)$text, "one slice set shared by the maps of a section",
               fixed = TRUE)
})

test_that("the glance legend uses the renderer's own thumbnail semantics", {
  manifest <- data.frame(map_id = c("a", "b"), label = c("A > B", "C > D"),
                         analysis_id = c("a", "b"), stringsAsFactors = FALSE)
  tbl <- function(p, n) {
    t <- data.frame(cluster_id = c("P1", "N1"), sign = c("positive", "negative"),
                    n_voxels = c(10L, 10L), peak_mni_x = c(1, -1), peak_mni_y = 0, peak_mni_z = 0,
                    max_stat = c(p, n), atlas_label = "x", hemisphere = "left", network = NA_character_,
                    stringsAsFactors = FALSE)
    attr(t, "n_total") <- 2L
    t
  }
  png <- tempfile(fileext = ".png")
  grDevices::png(png, width = 840, height = 330)
  graphics::plot.new()
  grDevices::dev.off()
  panels <- list(a = list(table = tbl(9, -7), summary_image = png, summary_semantics = "Glass brain: SEMANTICS."),
                 b = list(table = tbl(8, -6), summary_image = png))
  fmt <- montage_report_formatters(manifest = manifest, is_html = TRUE, panels = panels)
  out <- paste(capture.output(fmt$emit_results_overview()), collapse = "")
  expect_match(out, "<p class=\"nm-atlas-legend\">Glass brain: SEMANTICS.</p>", fixed = TRUE)
  expect_false(grepl("opacity rises", out, fixed = TRUE))
})
