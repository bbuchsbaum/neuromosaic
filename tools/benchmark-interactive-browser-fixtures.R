# Build representative bundle reports for browser performance measurement.
#
# Run from the repository root:
#   RGL_USE_NULL=TRUE Rscript tools/benchmark-interactive-browser-fixtures.R
#
# Optional environment variables:
#   NM_BENCH_GRIDS=2mm,1mm
#   NM_BENCH_DENSITIES=sparse,dense

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required.")
}
pkgload::load_all(getwd(), quiet = TRUE)

split_env <- function(name, defaults) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(defaults)
  trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
}

grid_specs <- list(
  `2mm` = list(dimensions = c(91L, 109L, 91L), spacing = 2),
  `1mm` = list(dimensions = c(182L, 218L, 182L), spacing = 1)
)
grids <- split_env("NM_BENCH_GRIDS", names(grid_specs))
densities <- split_env("NM_BENCH_DENSITIES", c("sparse", "dense"))
if (!all(grids %in% names(grid_specs))) stop("Unknown NM_BENCH_GRIDS value.")
if (!all(densities %in% c("sparse", "dense"))) {
  stop("Unknown NM_BENCH_DENSITIES value.")
}

artifact_dir <- file.path(getwd(), "e2e", ".artifacts", "benchmark")
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

make_values <- function(count, density, phase, background = FALSE) {
  index <- seq_len(count)
  if (identical(density, "sparse")) {
    values <- numeric(count)
    selected <- ((index + phase * 17L) %% 23L) == 0L
    if (background) selected <- ((index + phase * 11L) %% 7L) == 0L
    values[selected] <- if (background) {
      round(60 + 20 * sin(index[selected] / (701 + phase)), 3)
    } else {
      round(5 * sin(index[selected] / (311 + phase)), 4)
    }
    return(values)
  }
  if (background) {
    return(round(
      70 + 25 * sin(index / (1901 + phase)) +
        10 * cos(index / (3271 + phase)),
      3
    ))
  }
  round(
    0.8 * sin(index / (701 + phase * 13)) +
      0.2 * cos(index / (1103 + phase * 17)),
    4
  )
}

render_benchmark <- function(grid, density) {
  spec <- grid_specs[[grid]]
  dimensions <- spec$dimensions
  count <- prod(dimensions)
  space <- neuroim2::NeuroSpace(
    dim = dimensions,
    spacing = rep(spec$spacing, 3L),
    origin = -dimensions * spec$spacing / 2
  )
  background <- neuroim2::NeuroVol(
    array(make_values(count, density, 0L, background = TRUE),
          dim = dimensions),
    space
  )
  map_ids <- paste0("map_", seq_len(8L))
  maps <- lapply(seq_len(8L), function(index) {
    values <- make_values(count, density, index, background = FALSE)
    if (index == 1L) values <- values * 7
    neuroim2::NeuroVol(array(values, dim = dimensions), space)
  })
  manifest <- data.frame(
    analysis_id = rep(paste(grid, density, sep = "-"), 8L),
    analysis_label = rep(paste(grid, density, "browser benchmark"), 8L),
    map_id = map_ids,
    role = c("primary", rep("auxiliary", 7L)),
    quantity = c("test_statistic", rep("estimate", 7L)),
    distribution = c("z", rep(NA_character_, 7L)),
    threshold = c(3.1, rep(NA_real_, 7L)),
    selector_label = paste("Map", seq_len(8L)),
    label = paste("Benchmark map", seq_len(8L)),
    display_order = seq_len(8L),
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(maps)
  output <- file.path(
    artifact_dir,
    paste0("interactive-benchmark-", grid, "-", density, ".html")
  )
  elapsed <- system.time(neuromosaic::render_montage_report(
    manifest,
    output_file = output,
    title = paste("Interactive benchmark", grid, density),
    intro = "Deterministic browser performance fixture.",
    bg = background,
    interactive = neuromosaic::montage_interactive(
      assets = "bundle", cache_maps = 2L
    ),
    materialize_recipes = FALSE,
    render_peaks = FALSE,
    image_width = 320,
    image_height = 220,
    image_res = 72,
    quiet = TRUE
  ))[["elapsed"]]
  # HTML rendering removes its parameter sidecar; recover scene metadata from
  # the deterministic companion JSON instead.
  scene_path <- file.path(
    sub("\\.html$", "_files", output), "interactive", "scene.json"
  )
  scene <- jsonlite::read_json(scene_path, simplifyVector = FALSE)
  assets <- scene$assets
  background_asset <- assets[vapply(
    assets, function(asset) identical(asset$kind, "background"), logical(1)
  )][[1L]]
  overlay_assets <- assets[vapply(
    assets, function(asset) identical(asset$kind, "overlay"), logical(1)
  )]
  do.call(rbind, lapply(c(1L, 4L, 8L), function(n_overlays) {
    scenario_assets <- c(list(background_asset), overlay_assets[seq_len(n_overlays)])
    raw_bytes <- sum(vapply(
      scenario_assets, `[[`, numeric(1), "uncompressed_bytes"
    ))
    gzip_bytes <- sum(vapply(
      scenario_assets, `[[`, numeric(1), "compressed_bytes"
    ))
    base64_bytes <- sum(vapply(
      scenario_assets,
      function(asset) 4 * ceiling(asset$compressed_bytes / 3),
      numeric(1)
    ))
    data.frame(
      grid = grid,
      density = density,
      dimensions = paste(dimensions, collapse = "x"),
      overlays = n_overlays,
      raw_mib = raw_bytes / 1024^2,
      gzip_mib = gzip_bytes / 1024^2,
      base64_mib = base64_bytes / 1024^2,
      render_seconds = elapsed,
      stringsAsFactors = FALSE
    )
  }))
}

rows <- list()
position <- 0L
for (grid in grids) {
  for (density in densities) {
    position <- position + 1L
    rows[[position]] <- render_benchmark(grid, density)
    invisible(gc())
  }
}
results <- do.call(rbind, rows)
utils::write.csv(
  results,
  file.path(artifact_dir, "fixture-build.csv"),
  row.names = FALSE
)
print(results, row.names = FALSE)
