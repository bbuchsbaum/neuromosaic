# Round 2 interactive volume viewer: lazy runtime, client-side extent,
# positive-first opening, cluster labels, single-map chooser.

round2_space <- function() {
  neuroim2::NeuroSpace(dim = c(10, 10, 10), spacing = c(2, 2, 2),
                       origin = c(-9, -9, -9))
}

# Clusters that straddle the report extent (k = 5, 18-connectivity): one
# large and one tiny cluster per sign, plus a sub-threshold halo around the
# large positive one so a lower browser threshold has values to reveal.
round2_stat <- function() {
  arr <- array(0, dim = c(10, 10, 10))
  arr[2:4, 2:4, 2:4] <- 6          # 27 voxels, positive, kept
  arr[1, 2:4, 2:4] <- 2            # halo below threshold
  arr[8, 8, 8] <- 7                # strongest positive voxel, alone: excluded
  arr[8, 2, 2] <- 4; arr[9, 3, 2] <- 4  # edge pair, size 2: excluded
  arr[6:8, 6:7, 2] <- -5           # 6 voxels, negative, kept
  arr[2, 8, 8] <- -9               # strongest |peak|, alone: excluded
  arr
}

round2_inputs <- function(n_maps = 1L) {
  sp <- round2_space()
  stat <- neuroim2::NeuroVol(round2_stat(), sp)
  bg <- neuroim2::NeuroVol(array(seq_len(1000) / 10, dim = c(10, 10, 10)), sp)
  manifest <- data.frame(
    analysis_id = rep("task", n_maps),
    map_id = c("task_z", "task_beta")[seq_len(n_maps)],
    role = c("primary", "auxiliary")[seq_len(n_maps)],
    quantity = c("test_statistic", "estimate")[seq_len(n_maps)],
    distribution = c("z", NA)[seq_len(n_maps)],
    threshold = c(3, NA)[seq_len(n_maps)],
    min_cluster_size = c(5L, NA)[seq_len(n_maps)],
    connectivity = "18-connect",
    label = c("Task z", "Task beta")[seq_len(n_maps)],
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(rep(list(stat), n_maps))
  list(manifest = manifest, background = bg, stat = stat)
}

round2_render <- function(assets = "bundle", n_maps = 1L) {
  inputs <- round2_inputs(n_maps)
  output_dir <- tempfile("volume-viewer-")
  dir.create(output_dir)
  output <- file.path(output_dir, "report.qmd")
  result <- suppressWarnings(suppressMessages(render_montage_report(
    inputs$manifest,
    output_file = output,
    bg = inputs$background,
    interactive = montage_interactive(assets = assets, max_embed_mb = 5),
    materialize_recipes = FALSE,
    render_peaks = FALSE,
    image_width = 300,
    image_height = 220,
    image_res = 72
  )))
  list(
    rd = readRDS(sub("\\.qmd$", "_report-data.rds", result)),
    dir = output_dir,
    inputs = inputs
  )
}

# Runs display.js's extent rule in node on a value array.
js_extent_excluded <- function(values, dims, threshold, k, connectivity) {
  node <- Sys.which("node")
  display <- system.file(
    "htmlwidgets", "lib", "neuromosaic-volume", "display.js",
    package = "neuromosaic"
  )
  input <- tempfile(fileext = ".json")
  on.exit(unlink(input), add = TRUE)
  values[!is.finite(values)] <- NA
  jsonlite::write_json(list(
    values = values, dims = dims, threshold = threshold, k = k,
    connectivity = connectivity
  ), input, digits = NA, na = "null", auto_unbox = TRUE)
  script <- paste0(
    "const d = require(", jsonlite::toJSON(display, auto_unbox = TRUE), ");",
    "const x = JSON.parse(require('fs').readFileSync(",
    jsonlite::toJSON(input, auto_unbox = TRUE), ", 'utf8'));",
    "const v = Float64Array.from(x.values, (a) => a === null ? NaN : a);",
    "const m = d.extentExcluded(v, x.dims, x.threshold, x.k, x.connectivity);",
    "const out = []; if (m) m.forEach((e, i) => { if (e) out.push(i + 1); });",
    "process.stdout.write(JSON.stringify(out));"
  )
  out <- system2(node, c("-e", shQuote(script)), stdout = TRUE)
  as.integer(jsonlite::fromJSON(paste(out, collapse = "")))
}

test_that("the browser extent rule reproduces the report extent exactly", {
  skip_if(!nzchar(Sys.which("node")), "node is not available")
  set.seed(42)
  dims <- c(12L, 11L, 10L)
  raw <- array(stats::rnorm(prod(dims)), dim = dims)
  # Light smoothing gives clusters of many sizes.
  smooth <- raw
  for (d in 1:3) {
    smooth <- smooth + aperm(apply(smooth, setdiff(1:3, d), function(v) {
      c(v[-1], 0) + c(0, v[-length(v)])
    }), order(c(d, setdiff(1:3, d))))
  }
  values <- as.numeric(smooth)
  values[c(5, 77, 300)] <- NA
  for (connectivity in c("6-connect", "18-connect", "26-connect")) {
    for (k in c(3L, 8L)) {
      supra <- neuromosaic:::.suprathreshold_mask(values, 2, "two_sided")
      expected <- which(neuromosaic:::.montage_extent_excluded(
        values, dims, supra, min_cluster_size = k, connectivity = connectivity
      ))
      expect_gt(length(expected), 0L)
      expect_identical(
        js_extent_excluded(values, dims, c(-2, 2), k, connectivity),
        expected,
        info = paste(connectivity, k)
      )
    }
  }
})

test_that("interactive assets keep sub-extent values and the JS rule restores the report mask", {
  skip_if(!nzchar(Sys.which("node")), "node is not available")
  rendered <- round2_render("bundle")
  rd <- rendered$rd
  meta <- rd$panels$task_z$volume
  scene <- rd$interactive$scene
  map <- scene$analyses[[1L]]$maps[[1L]]
  asset <- scene$assets[[match(map$asset_id, vapply(
    scene$assets, `[[`, character(1), "asset_id"
  ))]]
  path <- file.path(rendered$dir, asset$location$ref)
  values <- as.numeric(as.array(neuroim2::read_vol(path)))
  stat <- as.numeric(round2_stat())

  # Support-masked only: the sub-extent voxels keep their values.
  expect_true(meta$n_extent_excluded > 0L)
  expect_equal(values, stat)

  # Asset + browser rule at the report threshold == the report's own mask.
  threshold <- c(-map$display$threshold, map$display$threshold)
  js <- js_extent_excluded(values, dim(round2_stat()), threshold,
                           map$display$min_cluster_size,
                           map$display$connectivity)
  report <- which(meta$support_mask & !meta$display_support_mask)
  expect_identical(js, report)
  expect_identical(js, which(neuromosaic:::.montage_extent_excluded(
    stat, c(10, 10, 10),
    neuromosaic:::.suprathreshold_mask(stat, 3, "two_sided"),
    min_cluster_size = 5L, connectivity = "18-connect"
  )))
  expect_identical(map$display$min_cluster_size, 5L)
  expect_identical(map$display$connectivity, "18-connect")
})

test_that("two-sided viewers open at the strongest positive surviving peak", {
  rendered <- round2_render("bundle")
  sp <- round2_space()
  # +7 and -9 are sub-extent; the strongest positive surviving voxel is +6.
  expected <- as.numeric(neuroim2::grid_to_coord(sp, matrix(c(2, 2, 2), 1L)))
  expect_equal(rendered$rd$interactive$scene$analyses[[1L]]$initial_world_coord,
               expected)

  negative_only <- neuroim2::NeuroVol(-abs(round2_stat()), sp)
  spec <- stat_montage(negative_only, negative_only, threshold = 3,
                       tail = "two_sided", draw = FALSE)
  # No positive voxel: fall back to the strongest |peak| (-9).
  expect_equal(spec$initial_world_coord,
               as.numeric(neuroim2::grid_to_coord(sp, matrix(c(2, 8, 8), 1L))))
})

test_that("bundle pages load the neuroimjs runtime lazily; embed pages inline it", {
  bundled <- round2_render("bundle")
  x <- bundled$rd$interactive
  html <- neuromosaic:::.montage_interactive_document_html(x, is_html = TRUE)
  runtime_bytes <- file.info(neuromosaic:::.montage_volume_runtime_path(
    "neuroimjs-0.4.0.umd.js"
  ))$size
  expect_lt(nchar(html, type = "bytes"), runtime_bytes / 4)
  expect_false(grepl("<script data-nm-volume-runtime>", html, fixed = TRUE))
  expect_match(html, "data-nm-volume-runtime-src", fixed = TRUE)
  spec <- regmatches(html, regexpr(
    "(?<=data-nm-volume-runtime-src>)[^<]+", html, perl = TRUE
  ))
  ref <- jsonlite::fromJSON(spec)$src
  expect_identical(ref, "report_files/interactive/runtime/neuroimjs-0.4.0.umd.js")
  expect_true(file.exists(file.path(bundled$dir, ref)))
  expect_match(html, "data-nm-volume-adapter", fixed = TRUE)

  embedded <- round2_render("embed")
  inline <- neuromosaic:::.montage_interactive_document_html(
    embedded$rd$interactive, is_html = TRUE
  )
  expect_null(embedded$rd$interactive$runtime_ref)
  expect_match(inline, "<script data-nm-volume-runtime>", fixed = TRUE)
  expect_gt(nchar(inline, type = "bytes"), runtime_bytes)
})

test_that("scene maps name every report cluster by its peak voxel", {
  inputs <- make_toy_cluster_report_inputs()
  # The toy atlas has one partition per cluster, which neuroim2 notes.
  peaks <- suppressWarnings(montage_peak_table(
    inputs$stat_map, inputs$atlas, threshold = 3, min_cluster_size = 1
  ))
  clusters <- neuromosaic:::.montage_volume_scene_clusters(peaks)
  ids <- vapply(clusters, `[[`, character(1), "id")
  expect_setequal(ids, c("P1", "N1"))
  p1 <- clusters[[match("P1", ids)]]
  # Peak (1, 1, 1) in R's one-based grid is voxel (0, 0, 0) in the browser.
  expect_identical(p1$voxel, c(0L, 0L, 0L))
  expect_identical(p1$region, "RegionA")
  expect_identical(p1$hemisphere, "left")
  expect_null(neuromosaic:::.montage_volume_scene_clusters(NULL))
  # Viewers open at P1, the first row of the peak table.
  geometry <- neuromosaic:::.montage_volume_geometry(inputs$stat_map)
  expect_equal(
    neuromosaic:::.montage_volume_scene_p1_coord(peaks, geometry),
    as.numeric(neuroim2::grid_to_coord(inputs$stat_map, matrix(c(1, 1, 1), 1L)))
  )
  negative_only <- peaks[peaks$sign == "negative", , drop = FALSE]
  attr(negative_only, "all_clusters") <- NULL
  expect_null(neuromosaic:::.montage_volume_scene_p1_coord(negative_only, geometry))
  expect_error(
    neuromosaic:::.montage_scene_validate_clusters(
      list(list(id = "P1", sign = "positive", voxel = c(-1, 0, 0))), "x"
    ),
    "zero-based"
  )
})

test_that("a single-map viewer names its map instead of offering a chooser", {
  single <- round2_render("embed", n_maps = 1L)
  host <- neuromosaic:::.montage_interactive_host_html(
    single$rd$interactive, "task", is_html = TRUE
  )
  expect_match(host, "nm-volume-map-field--single", fixed = TRUE)
  expect_match(host, "data-nm-volume-map hidden", fixed = TRUE)
  expect_match(host, "nm-volume-map-name\">Task z<", fixed = TRUE)

  multi <- round2_render("embed", n_maps = 2L)
  host <- neuromosaic:::.montage_interactive_host_html(
    multi$rd$interactive, "task", is_html = TRUE
  )
  expect_false(grepl("nm-volume-map-field--single", host, fixed = TRUE))
  expect_match(host, "<label class=\"nm-volume-map-field\" for=\"", fixed = TRUE)
})
