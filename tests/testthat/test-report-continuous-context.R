make_continuous_context_manifest <- function() {
  maps <- data.frame(
    analysis_id = rep(c("faces", "places"), each = 2),
    analysis_label = rep(c("Faces contrast", "Places contrast"), each = 2),
    map_id = c("faces_all", "faces_subset", "places_all", "places_subset"),
    path = paste0(seq_len(4), ".nii.gz"),
    role = rep(c("primary", "auxiliary"), 2), quantity = "estimate",
    distribution = NA_character_, label = rep(c("All eligible runs", "Subset"), 2)
  )
  resolve_montage_policy(maps, montage_policy(min_cluster_size = 1))
}

test_that("continuous-only summary tables provide descriptive navigation", {
  maps <- make_continuous_context_manifest()
  groups <- lapply(c("faces", "places"), function(id) {
    rows <- maps[maps$analysis_id == id, ]
    list(analysis_id = id, analysis_label = rows$analysis_label[[1]],
         map_ids = rows$map_id, primary_map_id = rows$map_id[[1]])
  })
  fmt <- montage_report_formatters(maps, groups = groups, is_html = TRUE)
  text <- paste(capture.output(fmt$emit_summary_matrix()), collapse = "\n")
  expect_match(text, "Unthresholded descriptive maps", fixed = TRUE)
  expect_false(grepl("[Cc]luster|[Ss]urviving|[Pp]eaks|statistic", text))
  expect_match(text, "Faces contrast", fixed = TRUE)
  expect_match(text, "data-nm-goto-map=\"faces_subset\"", fixed = TRUE)
  expect_match(text, "View map", fixed = TRUE)

  maps$effective_display_mode[c(1, 3)] <- "thresholded"
  panels <- list(faces_all = list(table = data.frame(max_stat = 4)),
                 places_all = list(table = data.frame(max_stat = 5)))
  fmt <- montage_report_formatters(maps, groups = groups, panels = panels, is_html = TRUE)
  text <- paste(capture.output(fmt$emit_summary_matrix()), collapse = "\n")
  expect_match(text, "Surviving clusters", fixed = TRUE)
})

test_that("continuous captions and metadata do not claim a threshold or extent", {
  row <- make_continuous_context_manifest()[1, ]
  # Raw settings are not an applied threshold when resolved display is continuous.
  row$threshold <- 3.1
  panel <- list(surface = list(method = list(source = "volume_projection")))
  fmt <- montage_report_formatters(is_html = TRUE)
  for (kind in c("volume", "surface")) {
    caption <- fmt$figure_caption(row, panel, kind)
    expect_false(grepl("clusters|thresholded at|before thresholding", caption))
  }
  text <- paste(capture.output(fmt$emit_metadata(row)), collapse = "\n")
  expect_false(grepl("threshold 3.1|min 1 vox|uncorrected", text))

  row$effective_display_mode <- "thresholded"
  row$effective_threshold <- 3.1
  expect_match(fmt$figure_caption(row), "clusters of at least 1 voxels", fixed = TRUE)
})
