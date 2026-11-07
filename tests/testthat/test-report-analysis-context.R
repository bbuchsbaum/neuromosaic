test_that("singleton headings identify analyses even when all variant labels match", {
  maps <- data.frame(analysis_id = letters[1:6], map_id = paste0(letters[1:6], "_all"),
    analysis_label = paste("Analysis", LETTERS[1:6]), label = "All eligible runs")
  for (html in c(TRUE, FALSE)) {
    fmt <- montage_report_formatters(maps, is_html = html)
    for (i in seq_len(nrow(maps))) {
      text <- paste(capture.output(fmt$emit_map_start(i, maps$label[[i]], maps$analysis_id[[i]])), collapse = "\n")
      expect_match(text, paste0(maps$analysis_label[[i]], " \u2014 All eligible runs"), fixed = TRUE)
    }
  }
  maps$analysis_label[[1]] <- "Faces <script> & places"
  fmt <- montage_report_formatters(maps, is_html = TRUE)
  text <- paste(capture.output(fmt$emit_map_start(1, maps$label[[1]], "a")), collapse = "\n")
  expect_match(text, "Faces &lt;script&gt; &amp; places", fixed = TRUE)
  expect_no_match(text, "<script>", fixed = TRUE)
})

test_that("static image exports and alt text retain analysis identity with layout", {
  path <- tempfile(fileext = ".png")
  withr::defer(unlink(path))
  grDevices::png(path, width = 40, height = 40)
  graphics::par(mar = rep(0, 4)); graphics::plot.new(); grDevices::dev.off()
  row <- data.frame(analysis_label = "Faces contrast", label = "All eligible runs",
    task = "Perception", effective_display_mode = "continuous")
  text <- paste(capture.output(neuromosaic:::.montage_report_emit_map_figures(
    row, list(volume_image = path, surface_image = path), is_html = TRUE, layout = "task"
  )), collapse = "\n")
  expect_match(text, 'data-nm-kicker="Perception / Faces contrast"', fixed = TRUE)
  expect_match(text, "slice montage for Faces contrast", fixed = TRUE)
  expect_match(text, "Analysis: Faces contrast.", fixed = TRUE)
})
