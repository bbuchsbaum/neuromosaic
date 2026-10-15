test_that("slice alt text and captions describe resolved quantities and palettes", {
  path <- withr::local_tempfile(fileext = ".png")
  grDevices::png(path, width = 100, height = 100)
  grid::grid.newpage()
  grDevices::dev.off()
  for (case in list(
    list(label = "Participants above cutoff", units = "%", limits = c(0, 100), palette = "Reds", signed = FALSE),
    list(label = "Valid participants", units = "participants", limits = c(0, 17), palette = "Blues", signed = FALSE),
    list(label = "Z statistic", units = "z", limits = c(-5, 5), palette = "nm-signed", signed = TRUE))) {
    row <- data.frame(map_id = "m", label = "Map", quantity = "estimate")
    meta <- list(legend_title = case$label, units = case$units, limits = case$limits,
                 palette = case$palette, signed = case$signed, slice_labels = c("z = −10", "z = +2"))
    panel <- list(volume_image = path, volume = meta)
    html <- paste(capture.output(.montage_report_emit_map_figures(row, panel, is_html = TRUE)), collapse = "\n")
    expect_match(html, case$label, fixed = TRUE)
    expect_match(html, "z = −10, +2 mm", fixed = TRUE)
    expect_match(html, paste(vapply(case$limits, .nm_format_tick, ""), collapse = " to "), fixed = TRUE)
    if (case$signed) expect_match(html, "positive values in warm colours", fixed = TRUE) else {
      expect_match(html, paste0(case$palette, " palette"), fixed = TRUE)
      expect_false(grepl("warm|cool|negative", html))
    }
  }
})

test_that("rendered and cached parcel panels record their actual source and views", {
  inputs <- make_toy_cluster_report_inputs()
  atlas <- make_toy_surfatlas()
  manifest <- data.frame(map_id = "parcel", label = "Parcel effects", quantity = "estimate",
                         threshold = .1, min_cluster_size = 1)
  manifest$stat_map <- list(inputs$stat_map)
  manifest$parcel_values <- I(list(c(`1` = 2, `2` = -3)))
  # Keep the real report/metadata/scene path; the expensive drawing is irrelevant
  # to the description and is replaced with a blank plot.
  local_mocked_bindings(plot_brain = function(...) ggplot2::ggplot(), .package = "neuroatlas")
  dir <- withr::local_tempdir()
  output <- file.path(dir, "report.qmd")
  args <- list(manifest = manifest, output_file = output, bg = inputs$stat_map,
    surfatlas = atlas, volume_args = list(zlevels = c(2L, 3L, 5L)), render_peaks = FALSE,
    surface_args = list(render_device = "png", views = c("lateral", "medial", "ventral")),
    image_width = 300, image_height = 200, image_res = 72)
  for (pass in 1:2) {
    do.call(render_montage_report, args)
    rd <- readRDS(sub(".qmd", "_report-data.rds", output, fixed = TRUE))
    panel <- rd$panels$parcel
    expect_identical(panel$surface$method$source, "parcel_values")
    expect_identical(panel$volume$slice_selection, "explicit")
    expect_equal(panel$volume$slice_indices, c(2, 3, 5))
    text <- paste(.montage_report_methods(rd)$text,
                  .montage_report_figure_caption(rd$manifest, panel, "surface"), panel$surface_alt)
    expect_match(text, "parcel values mapped directly", fixed = TRUE)
    expect_match(text, "lateral, medial and ventral views", fixed = TRUE)
    expect_match(text, "user-supplied slice indices", fixed = TRUE)
    expect_false(grepl("averaged|trilinear|5th|95th|peaks are lower", text))
    if (pass == 2) expect_true(panel$surface$diagnostics$cache_hit)
  }
  expect_identical(manifest$parcel_values[[1]], c(`1` = 2, `2` = -3))
})

test_that("slice selection provenance distinguishes per-map overrides and automatic choices", {
  inputs <- make_toy_cluster_report_inputs()
  manifest <- data.frame(map_id = c("a", "b"), analysis_id = "x", label = c("A", "B"),
    quantity = "estimate", role = c("primary", "auxiliary"), threshold = 3, min_cluster_size = 1)
  manifest$stat_map <- rep(list(inputs$stat_map), 2)
  manifest$zlevels <- list(c(2L, 3L, 5L), NULL)
  resolved <- resolve_montage_policy(manifest, stat_maps = manifest$stat_map)
  z <- .montage_group_zlevels(resolved, manifest$stat_map, rep(list(NULL), 2))
  expect_identical(z$a$selection, "explicit")
  expect_identical(z$b$selection, "automatic")
  panels <- list(a = list(volume = list(slice_labels = "z = 2", slice_selection = "explicit")),
                 b = list(volume = list(slice_labels = "z = 3", slice_selection = "automatic")))
  text <- .montage_report_methods(list(manifest = resolved, panels = panels))$text
  expect_match(text, "user-supplied indices for some maps", fixed = TRUE)
  expect_false(grepl("5th|95th", text))
})

test_that("surface descriptions retain projection settings without assuming averaging", {
  row <- data.frame(map_id = "a", label = "A", quantity = "estimate", min_cluster_size = 1)
  panel <- list(surface = list(views = "ventral", hemis = "left",
    method = list(source = "volume_projection", sampling = "thickness", interpolation = "nearest",
                  aggregate = "max", fun = "max", depth = c(.2, .8))))
  text <- .montage_report_surface_text(row, panel, detailed = TRUE)
  expect_match(text, "ventral views of the left hemisphere", fixed = TRUE)
  expect_match(text, "2 depths from 20% to 80%", fixed = TRUE)
  expect_match(text, "nearest interpolation", fixed = TRUE)
  expect_match(text, "aggregation: max", fixed = TRUE)
  expect_false(grepl("averaged|lower than", text))
  expect_match(.montage_report_surface_text(row, list()), "mapping details not recorded", fixed = TRUE)
})
