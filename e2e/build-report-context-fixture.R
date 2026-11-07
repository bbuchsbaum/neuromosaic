# Synthetic, offline fixtures for analysis identity and descriptive report text.
local({
  root <- normalizePath(getwd())
  pkgload::load_all(root, quiet = TRUE)
  dest <- file.path(root, "e2e", ".artifacts")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  sp <- neuroim2::NeuroSpace(c(8, 8, 8), spacing = c(2, 2, 2), origin = c(-8, -8, -8))
  values <- array(0, c(8, 8, 8))
  values[2:4, 2:4, 2:4] <- 2
  values[5:7, 5:7, 5:7] <- -1
  effect <- neuroim2::NeuroVol(values, sp)
  background <- neuroim2::NeuroVol(array(1, dim(values)), sp)
  faces <- matrix(c(0, 1, 2, 0, 1, 3, 0, 2, 3, 1, 2, 3), ncol = 3, byrow = TRUE)
  surface <- function(side) {
    x <- if (side == "lh") c(-2, -1, -1, -1) else c(2, 1, 1, 1)
    vertices <- cbind(x, c(0, 1, -1, 0), c(0, 0, 0, 1))
    geometry <- neurosurf::SurfaceGeometry(vertices, faces, hemi = side)
    methods::new("LabeledNeuroSurface", geometry = geometry,
      indices = as.integer(1:4), data = c(1, 1, 2, 2),
      labels = c("Region A", "Region B"), cols = c("#336699", "#CC6633"))
  }
  atlas <- structure(list(ids = c(1L, 2L), labels = c("Region A", "Region B"),
    hemi = c("left", "right"), lh_atlas = surface("lh"), rh_atlas = surface("rh"),
    surf_type = "white", surface_space = "synthetic"), class = c("surfatlas", "atlas"))
  image <- file.path(dest, "context-surface.png")
  grDevices::png(image, width = 480, height = 240)
  graphics::plot.new()
  graphics::text(.5, .5, "Synthetic surface fixture")
  grDevices::dev.off()
  render <- function(grouped) {
    ids <- if (grouped) rep(c("a", "b"), each = 2) else letters[1:6]
    labels <- if (grouped) rep(c("All eligible runs", "Subset"), 2) else rep("All eligible runs", 6)
    maps <- data.frame(analysis_id = ids, analysis_label = paste("Analysis", toupper(ids)),
      map_id = paste0(ids, "_", seq_along(ids)),
      role = if (grouped) rep(c("primary", "auxiliary"), 2) else "primary",
      quantity = "estimate", distribution = NA_character_, label = labels,
      selector_label = labels, signed = TRUE)
    maps$stat_map <- I(rep(list(effect), nrow(maps)))
    projected <- stats::setNames(rep(list(list(left = c(2, 2, -1, -1),
      right = c(2, 2, -1, -1))), nrow(maps)), maps$map_id)
    neuromosaic::render_montage_report(maps,
      file.path(dest, if (grouped) "context-grouped.html" else "context-singletons.html"),
      title = "Report context fixture", bg = background, surfatlas = atlas,
      panels = stats::setNames(rep(list(list(surface_image = image)), nrow(maps)), maps$map_id),
      policy = neuromosaic::montage_policy(min_cluster_size = 1),
      render_surface = FALSE, render_peaks = FALSE, map_selector = "tabs",
      interactive = neuromosaic::montage_interactive(assets = "bundle"),
      surface = neuromosaic::montage_surface(values = projected, assets = "bundle", height = "360px"),
      assets = "bundle", materialize_recipes = FALSE,
      image_width = 420, image_height = 240, image_res = 72)
  }
  render(FALSE)
  render(TRUE)
})
