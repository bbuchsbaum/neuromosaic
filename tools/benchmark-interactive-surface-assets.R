# Reproduce representative interactive-surface asset sizes and preparation time.
#
# Usage:
#   RGL_USE_NULL=TRUE Rscript tools/benchmark-interactive-surface-assets.R

repo_root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run this benchmark from the neuromosaic repository root.")
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required.")
}
pkgload::load_all(repo_root, quiet = TRUE)

vertex_count <- 40962L
face_count <- 81920L
map_count <- 4L
index <- seq.int(0, vertex_count - 1L)
phi <- acos(1 - 2 * (index + 0.5) / vertex_count)
theta <- pi * (1 + sqrt(5)) * index
left_vertices <- cbind(
  -50 + 45 * sin(phi) * cos(theta),
  45 * sin(phi) * sin(theta),
  45 * cos(phi)
)
right_vertices <- left_vertices
right_vertices[, 1] <- -left_vertices[, 1]
face_start <- seq.int(0, face_count - 1L) %% (vertex_count - 2L)
faces <- cbind(face_start, face_start + 1L, face_start + 2L)

left <- neurosurf::SurfaceGeometry(left_vertices, faces, hemi = "lh")
right <- neurosurf::SurfaceGeometry(right_vertices, faces, hemi = "rh")
values <- list(
  z = 6 * sin(index / 401),
  estimate = 1.5 * cos(index / 613),
  se = 0.1 + abs(sin(index / 997)),
  reliability = pmin(1, 0.3 + abs(cos(index / 733)) / 2)
)
layers <- Map(function(name, value) {
  neurosurf::surface_layer(
    name,
    values = list(left = value, right = rev(value)),
    colormap = if (name == "z") "RdBu" else "inferno",
    limits = range(value),
    threshold = if (name == "z") c(-3.1, 3.1) else NULL,
    opacity = 0.85
  )
}, names(values), values)
scene <- neurosurf::surface_scene(
  left = left,
  right = right,
  layers = layers,
  selected_layer = "z",
  fallback = "Static surface benchmark fallback.",
  alt_text = "Synthetic fsaverage6-sized bilateral benchmark surface.",
  mode = "report"
)

raw_dir <- tempfile("neuromosaic-surface-benchmark-raw-")
gzip_dir <- tempfile("neuromosaic-surface-benchmark-gzip-")
dir.create(raw_dir)
dir.create(gzip_dir)
on.exit(unlink(c(raw_dir, gzip_dir), recursive = TRUE), add = TRUE)

serialization <- system.time({
  manifest <- neurosurf::surface_scene_manifest(
    scene, asset_mode = "directory", asset_dir = raw_dir
  )
})[["elapsed"]]
assets <- manifest$assets
compression <- system.time({
  gzip_paths <- vapply(assets, function(asset) {
    source <- file.path(raw_dir, asset$uri)
    target <- file.path(gzip_dir, paste0(asset$uri, ".gz"))
    neuromosaic:::.montage_gzip_file(source, target)
    target
  }, character(1))
})[["elapsed"]]

roles <- vapply(assets, `[[`, character(1), "role")
raw_bytes <- vapply(assets, `[[`, numeric(1), "byteLength")
gzip_bytes <- as.numeric(file.info(gzip_paths)$size)
base64_bytes <- 4 * ceiling(gzip_bytes / 3)
geometry <- roles %in% c("vertices", "faces", "curvature")
values_role <- roles %in% c("values", "indices")

fmt_mib <- function(x) sprintf("%.2f", sum(x) / 1024^2)
cat(
  "scenario=synthetic-fsaverage6-sized\n",
  "hemispheres=2\n",
  "vertices_per_hemisphere=", vertex_count, "\n",
  "faces_per_hemisphere=", face_count, "\n",
  "maps=", map_count, "\n",
  "unique_assets=", length(assets), "\n",
  "geometry_raw_mib=", fmt_mib(raw_bytes[geometry]), "\n",
  "geometry_gzip_mib=", fmt_mib(gzip_bytes[geometry]), "\n",
  "values_raw_mib=", fmt_mib(raw_bytes[values_role]), "\n",
  "values_gzip_mib=", fmt_mib(gzip_bytes[values_role]), "\n",
  "total_raw_mib=", fmt_mib(raw_bytes), "\n",
  "total_gzip_mib=", fmt_mib(gzip_bytes), "\n",
  "total_base64_mib=", fmt_mib(base64_bytes), "\n",
  "serialize_seconds=", sprintf("%.3f", serialization), "\n",
  "gzip_seconds=", sprintf("%.3f", compression), "\n",
  sep = ""
)
