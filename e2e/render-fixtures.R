repo_root <- normalizePath(getwd(), mustWork = TRUE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required to render the Playwright fixtures.")
}
if (!requireNamespace("rmarkdown", quietly = TRUE) ||
    !rmarkdown::pandoc_available()) {
  stop("Package 'rmarkdown' and Pandoc are required for the Playwright fixtures.")
}

pkgload::load_all(repo_root, quiet = TRUE)

artifact_dir <- file.path(repo_root, "e2e", ".artifacts")
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

map_ids <- c(
  "faces_z", "faces_estimate", "faces_se",
  "wm_z", "wm_estimate", "wm_se", "wm_variance", "wm_probability"
)
labels <- c(
  "Z statistic", "Estimate", "Standard error",
  "Z statistic", "Estimate", "Standard error", "Variance", "Probability"
)
colours <- c(
  "#9f1239", "#1d4ed8", "#047857",
  "#9f1239", "#1d4ed8", "#047857", "#7e22ce", "#b45309"
)

make_panel_image <- function(map_id, label, colour) {
  path <- file.path(artifact_dir, paste0(map_id, ".png"))
  grDevices::png(path, width = 960, height = 420, res = 120)
  graphics::par(mar = rep(0, 4), bg = "white")
  graphics::plot.new()
  graphics::rect(0.03, 0.08, 0.97, 0.92, col = "#f8fafc", border = "#cbd5e1",
                 lwd = 3)
  graphics::rect(0.08, 0.18, 0.92, 0.73, col = colour, border = NA)
  graphics::text(0.5, 0.82, label, cex = 2.0, font = 2, col = "#172033")
  graphics::text(0.5, 0.45, paste("Rendered panel", map_id), cex = 1.5,
                 col = "white")
  grDevices::dev.off()
  normalizePath(path, mustWork = TRUE)
}

image_paths <- stats::setNames(
  Map(make_panel_image, map_ids, labels, colours),
  map_ids
)

manifest <- data.frame(
  analysis_id = c(rep("faces", 3L), rep("working-memory", 5L)),
  analysis_label = c(rep("Faces versus places", 3L),
                     rep("Working-memory load", 5L)),
  map_id = map_ids,
  role = c("primary", "auxiliary", "auxiliary",
           "primary", rep("auxiliary", 4L)),
  quantity = c(
    "test_statistic", "estimate", "standard_error",
    "test_statistic", "estimate", "standard_error", "variance", "probability"
  ),
  distribution = c("z", NA, NA, "z", rep(NA, 4L)),
  threshold = c(3.1, NA, NA, 3.1, rep(NA, 4L)),
  selector_label = c("Z", "Estimate", "SE", "Z", "Estimate", "SE",
                     "Variance", "Probability"),
  label = labels,
  description = paste("DESCRIPTION", map_ids),
  n = c(32, 30, 32, rep(28, 5L)),
  source_n = c(32, 32, 32, rep(28, 5L)),
  dropped_subjects = c("", "sub-07, sub-19", "", rep("", 5L)),
  display_order = c(1:3, 1:5),
  stringsAsFactors = FALSE
)
manifest$recipe <- I(lapply(seq_len(nrow(manifest)), function(i) {
  force(i)
  function(row) i
}))

panels <- stats::setNames(lapply(seq_along(map_ids), function(i) {
  map_id <- map_ids[[i]]
  list(
    volume_image = image_paths[[map_id]],
    table = data.frame(
      Metric = c("Map marker", "Display order"),
      Value = c(paste("TABLE", map_id), manifest$display_order[[i]]),
      stringsAsFactors = FALSE
    )
  )
}), map_ids)

render_fixture <- function(filename, fixture_manifest, fixture_panels,
                           map_selector) {
  neuromosaic::render_montage_report(
    fixture_manifest,
    output_file = file.path(artifact_dir, filename),
    template = file.path(repo_root, "inst", "templates", "montage_report.Rmd"),
    title = "Neuromosaic map selector browser fixture",
    intro = "A deterministic report used for live browser verification.",
    panels = fixture_panels,
    map_selector = map_selector,
    materialize_recipes = FALSE,
    check_files = FALSE,
    quiet = TRUE
  )
}

render_fixture("selector-auto.html", manifest, panels, "auto")

expanded_rows <- manifest$analysis_id == "faces"
render_fixture(
  "selector-none.html",
  manifest[expanded_rows, , drop = FALSE],
  panels[manifest$map_id[expanded_rows]],
  "none"
)

volume_space <- neuroim2::NeuroSpace(
  dim = c(12, 13, 14),
  spacing = c(2, 2, 2),
  origin = c(-12, -13, -14)
)
grid <- expand.grid(
  x = seq_len(12), y = seq_len(13), z = seq_len(14)
)
background_values <- array(
  20 + grid$x + 0.5 * grid$y + 0.25 * grid$z,
  dim = c(12, 13, 14)
)
z_values <- array(0, dim = c(12, 13, 14))
z_values[3:6, 4:8, 5:9] <- 4.5
z_values[8:10, 8:11, 8:12] <- -5.25
z_values[5, 6, 7] <- 7.75
estimate_values <- z_values / 4
estimate_values[1:3, 1:3, 1:3] <- -0.5
se_values <- abs(z_values) / 10 + 0.15
reliability_values <- pmin(1, 0.35 + abs(estimate_values) / 2)
empty_values <- array(0, dim = c(12, 13, 14))

background <- neuroim2::NeuroVol(background_values, volume_space)
z_volume <- neuroim2::NeuroVol(z_values, volume_space)
estimate_volume <- neuroim2::NeuroVol(estimate_values, volume_space)
se_volume <- neuroim2::NeuroVol(se_values, volume_space)
reliability_volume <- neuroim2::NeuroVol(reliability_values, volume_space)
empty_volume <- neuroim2::NeuroVol(empty_values, volume_space)

interactive_manifest <- data.frame(
  analysis_id = c(rep("faces-interactive", 4L), "empty-interactive"),
  analysis_label = c(rep("Faces interactive analysis", 4L),
                     "Empty interactive analysis"),
  map_id = c(
    "interactive_z", "interactive_estimate", "interactive_se",
    "interactive_reliability", "interactive_empty"
  ),
  role = c("primary", rep("auxiliary", 3L), "primary"),
  quantity = c(
    "test_statistic", "estimate", "standard_error", "mylab:reliability",
    "test_statistic"
  ),
  distribution = c("z", NA, NA, NA, "z"),
  threshold = c(3.1, NA, NA, NA, 3.1),
  selector_label = c("Z", "Estimate", "SE", "Reliability", "Empty Z"),
  label = c(
    "Z statistic", "Beta estimate", "Standard error", "Reliability",
    "Empty Z statistic"
  ),
  description = c(
    "Primary thresholded result.",
    "Continuous signed estimate.",
    "Continuous non-negative standard error.",
    "Namespaced project-specific reliability metric.",
    "No voxel survives the report threshold."
  ),
  display_order = c(seq_len(4L), 1L),
  stringsAsFactors = FALSE
)
interactive_manifest$stat_map <- I(list(
  z_volume, estimate_volume, se_volume, reliability_volume, empty_volume
))

render_interactive_fixture <- function(filename, assets, map_selector = "auto") {
  neuromosaic::render_montage_report(
    interactive_manifest,
    output_file = file.path(artifact_dir, filename),
    template = file.path(repo_root, "inst", "templates", "montage_report.Rmd"),
    title = "Neuromosaic interactive volume browser fixture",
    intro = paste(
      "A deterministic report proving lazy orthogonal exploration, map",
      "switching, and reader-controlled display settings."
    ),
    bg = background,
    map_selector = map_selector,
    profiles = list(`mylab:reliability` = neuromosaic::montage_map_profile(
      "mylab:reliability",
      label = "Reliability",
      display_mode = "continuous",
      scale = "sequential",
      domain = c(0, 1),
      limits = c(0, 1),
      palette_family = "sequential",
      alpha_mode = "binary",
      units = "agreement"
    )),
    interactive = neuromosaic::montage_interactive(
      assets = assets,
      controls = c("threshold", "palette", "opacity"),
      max_embed_mb = 10,
      cache_maps = 2
    ),
    materialize_recipes = FALSE,
    render_peaks = FALSE,
    image_width = 560,
    image_height = 420,
    image_res = 96,
    quiet = TRUE
  )
}

render_interactive_fixture("interactive-volume.html", "embed")
render_interactive_fixture("interactive-volume-bundle.html", "bundle")
render_interactive_fixture("interactive-volume-none.html", "embed", "none")
