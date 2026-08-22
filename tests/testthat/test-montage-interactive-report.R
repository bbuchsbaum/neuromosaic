make_interactive_montage_inputs <- function(label = "Z statistic") {
  inputs <- make_toy_cluster_report_inputs()
  estimate <- neuroim2::NeuroVol(
    as.array(inputs$stat_map) / 5,
    neuroim2::space(inputs$stat_map)
  )
  standard_error <- neuroim2::NeuroVol(
    abs(as.array(inputs$stat_map)) / 20 + 0.1,
    neuroim2::space(inputs$stat_map)
  )
  manifest <- data.frame(
    analysis_id = rep("faces", 3L),
    map_id = c("faces_z", "faces_estimate", "faces_se"),
    role = c("primary", "auxiliary", "auxiliary"),
    quantity = c("test_statistic", "estimate", "standard_error"),
    distribution = c("z", NA, NA),
    threshold = c(3, NA, NA),
    label = c(label, "Estimate", "Standard error"),
    selector_label = c("Z", "Beta", "SE"),
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(list(inputs$stat_map, estimate, standard_error))
  list(background = inputs$stat_map, manifest = manifest)
}

render_interactive_qmd <- function(config, label = "Z statistic") {
  inputs <- make_interactive_montage_inputs(label)
  output_dir <- tempfile("interactive-report-")
  dir.create(output_dir)
  output <- file.path(output_dir, "report.qmd")
  result <- suppressMessages(render_montage_report(
    inputs$manifest,
    output_file = output,
    bg = inputs$background,
    interactive = config,
    materialize_recipes = FALSE,
    render_peaks = FALSE,
    image_width = 300,
    image_height = 220,
    image_res = 72
  ))
  list(
    output = result,
    sidecar = sub("\\.qmd$", "_report-data.rds", result),
    inputs = inputs
  )
}

test_that("bundle reports carry a primary-first VolumeScene beside static panels", {
  rendered <- render_interactive_qmd(montage_interactive(
    assets = "bundle",
    controls = c("threshold", "palette", "opacity"),
    cache_maps = 3
  ))
  rd <- readRDS(rendered$sidecar)
  scene <- rd$interactive$scene
  analysis <- scene$analyses[[1L]]

  expect_s3_class(rd$interactive, "montage_interactive_report")
  expect_s3_class(scene, "montage_volume_scene")
  expect_true(rd$params$interactive_requested)
  expect_identical(scene$view$type, "orthogonal")
  expect_identical(
    scene$view$controls,
    c("threshold", "palette", "opacity")
  )
  expect_identical(scene$view$cache_maps, 3L)
  expect_identical(analysis$primary_map_id, "faces_z")
  expect_identical(
    vapply(analysis$maps, `[[`, character(1), "map_id"),
    c("faces_z", "faces_estimate", "faces_se")
  )
  expect_identical(analysis$maps[[1L]]$display$mode, "thresholded")
  expect_equal(analysis$maps[[1L]]$display$threshold, 3)
  expect_identical(analysis$maps[[1L]]$display$palette, "blue-red")
  expect_identical(analysis$maps[[3L]]$display$mode, "continuous")
  expect_null(analysis$maps[[3L]]$display$threshold)
  expect_identical(analysis$maps[[3L]]$display$palette, "inferno")
  expect_true(all(vapply(rd$panels, function(panel) {
    !startsWith(panel$volume_image, "/") &&
      file.exists(file.path(dirname(rendered$output), panel$volume_image))
  }, logical(1))))
  expect_false("stat_map" %in% names(rd$manifest))

  refs <- vapply(scene$assets, function(asset) asset$location$ref, character(1))
  expect_true(all(vapply(scene$assets, function(asset) {
    identical(asset$location$kind, "relative")
  }, logical(1))))
  expect_true(all(file.exists(file.path(dirname(rendered$output), refs))))
  expect_true(all(grepl(
    "report_files/interactive/volumes/", refs, fixed = TRUE
  )))
  expect_false(grepl(tempdir(), rd$interactive$scene_json, fixed = TRUE))
  interactive_root <- file.path(dirname(rendered$output),
                                "report_files", "interactive")
  expect_true(file.exists(file.path(interactive_root, "scene.json")))
  expect_setequal(
    basename(rd$interactive$bundle_files),
    c(
      "scene.json", "neuroimjs-0.3.0.umd.js", "adapter.js",
      "adapter.css", "display.js", "geometry.js", "runtime.json",
      "LICENSE-neuroimjs"
    )
  )
  expect_true(all(!startsWith(rd$interactive$bundle_files, "/")))
  expect_null(names(rd$interactive$bundle_files))
  expect_true(all(file.exists(file.path(
    dirname(rendered$output), rd$interactive$bundle_files
  ))))
  expect_identical(
    paste(readLines(file.path(interactive_root, "scene.json")),
          collapse = "\n"),
    rd$interactive$scene_json
  )
  runtime <- jsonlite::read_json(file.path(
    interactive_root, "runtime", "runtime.json"
  ))
  expect_identical(runtime$sha256, rd$interactive$runtime_sha256)
  expect_identical(rd$interactive$summary$packaging, "bundle")
  expect_identical(rd$interactive$summary$asset_count,
                   length(scene$assets))
  expect_gt(rd$interactive$summary$compressed_bytes, 0)
  expect_gt(rd$interactive$summary$uncompressed_bytes, 0)
  expect_identical(rd$interactive$summary$embedded_bytes, 0)
  expect_match(rd$provenance$interactive_engine, "neuroimjs 0.3.0",
               fixed = TRUE)
  expect_identical(rd$provenance$interactive_asset_count,
                   length(scene$assets))
  expect_identical(rd$provenance$interactive_scene_version, 1L)
  expect_identical(rd$provenance$interactive_runtime_sha256,
                   rd$interactive$runtime_sha256)
  expect_match(rd$provenance$interactive_asset_hashes,
               scene$assets[[1L]]$hash, fixed = TRUE)
  expect_match(rd$provenance$interactive_transformations,
               "storage_float64_lossless", fixed = TRUE)
  parsed <- jsonlite::fromJSON(
    rd$interactive$scene_json, simplifyVector = FALSE
  )
  expect_null(names(parsed$analyses))
  expect_null(names(parsed$analyses[[1L]]$maps))
  expect_null(names(parsed$assets))

  sentinel <- file.path(interactive_root, "keep-existing.txt")
  writeLines("preserve me", sentinel)
  suppressMessages(render_montage_report(
    rendered$inputs$manifest,
    output_file = rendered$output,
    bg = rendered$inputs$background,
    interactive = montage_interactive(assets = "bundle"),
    materialize_recipes = FALSE,
    render_peaks = FALSE,
    image_width = 300,
    image_height = 220,
    image_res = 72
  ))
  expect_identical(readLines(sentinel), "preserve me")
})

test_that("embedded reports are self-contained within their explicit byte budget", {
  rendered <- render_interactive_qmd(
    montage_interactive(assets = "embed", max_embed_mb = 5),
    label = "Z <stat> & result"
  )
  rd <- readRDS(rendered$sidecar)
  scene <- rd$interactive$scene

  expect_length(rd$interactive$payloads, length(scene$assets))
  expected_embedded_bytes <- sum(vapply(
    rd$interactive$payloads,
    function(payload) nchar(payload$base64, type = "bytes"),
    numeric(1)
  ))
  expect_identical(rd$interactive$summary$embedded_bytes,
                   expected_embedded_bytes)
  expect_gt(rd$interactive$summary$embedded_bytes,
            rd$interactive$summary$compressed_bytes)
  expect_true(all(vapply(scene$assets, function(asset) {
    identical(asset$location$kind, "embedded")
  }, logical(1))))
  expect_false(dir.exists(file.path(
    dirname(rendered$output), "report_files", "interactive", "volumes"
  )))
  expect_match(rd$interactive$scene_json, "\\\\u003cstat\\\\u003e")
  parsed <- jsonlite::fromJSON(rd$interactive$scene_json, simplifyVector = FALSE)
  expect_identical(parsed$analyses[[1L]]$maps[[1L]]$label,
                   "Z <stat> & result")

  html <- neuromosaic:::.montage_interactive_document_html(
    rd$interactive, is_html = TRUE
  )
  expect_match(html, "data-nm-volume-runtime", fixed = TRUE)
  expect_match(html, "application/octet-stream", fixed = TRUE)
  expect_false(grepl(
    "</script", tolower(rd$interactive$scene_json), fixed = TRUE
  ))
  expect_identical(
    neuromosaic:::.montage_interactive_document_html(
      rd$interactive, is_html = FALSE
    ),
    ""
  )
})

test_that("embedded reports fail before exceeding their declared byte budget", {
  inputs <- make_interactive_montage_inputs()
  output <- file.path(tempfile("interactive-budget-"), "report.qmd")

  expect_error(
    suppressMessages(render_montage_report(
      inputs$manifest,
      output_file = output,
      bg = inputs$background,
      interactive = montage_interactive(
        assets = "embed", max_embed_mb = 1e-6
      ),
      materialize_recipes = FALSE,
      render_peaks = FALSE,
      image_width = 300,
      image_height = 220,
      image_res = 72
    )),
    "raw.*gzip.*base64.*exceeding max_embed_mb"
  )
  expect_false(file.exists(output))
})

test_that("bundle and embed delivery preserve identical scientific scene state", {
  bundled <- render_interactive_qmd(montage_interactive(assets = "bundle"))
  embedded <- render_interactive_qmd(montage_interactive(
    assets = "embed", max_embed_mb = 5
  ))
  bundle_scene <- readRDS(bundled$sidecar)$interactive$scene
  embed_scene <- readRDS(embedded$sidecar)$interactive$scene

  expect_identical(bundle_scene$engine, embed_scene$engine)
  expect_identical(bundle_scene$view, embed_scene$view)
  expect_identical(bundle_scene$background, embed_scene$background)
  expect_identical(bundle_scene$analyses, embed_scene$analyses)
  expect_identical(
    lapply(bundle_scene$assets, function(asset) {
      asset$location <- NULL
      asset
    }),
    lapply(embed_scene$assets, function(asset) {
      asset$location <- NULL
      asset
    })
  )
})

test_that("interactive rendering requires an exact static volume fallback", {
  inputs <- make_interactive_montage_inputs()
  output <- tempfile("interactive-contract-", fileext = ".qmd")

  expect_error(
    render_montage_report(
      inputs$manifest,
      output_file = output,
      bg = inputs$background,
      interactive = list(),
      materialize_recipes = FALSE
    ),
    "created by montage_interactive"
  )
  expect_error(
    render_montage_report(
      inputs$manifest,
      output_file = output,
      bg = inputs$background,
      interactive = montage_interactive(),
      render_volume = FALSE,
      materialize_recipes = FALSE
    ),
    "static fallback"
  )
  expect_error(
    render_montage_report(
      inputs$manifest,
      output_file = output,
      bg = inputs$background,
      interactive = montage_interactive(),
      volume_args = list(on_mismatch = "restamp"),
      materialize_recipes = FALSE
    ),
    "do not support.*restamp"
  )
})

test_that("interactive host markup remains lazy and analysis scoped", {
  rendered <- render_interactive_qmd(montage_interactive(assets = "embed"))
  rd <- readRDS(rendered$sidecar)
  host <- neuromosaic:::.montage_interactive_host_html(
    rd$interactive, "faces", is_html = TRUE
  )

  expect_match(host, "Explore volume interactively", fixed = TRUE)
  expect_match(host, "Interactive map: <strong data-nm-volume-target>Z</strong>",
               fixed = TRUE)
  expect_match(host, "data-nm-volume-analysis=\"faces\"", fixed = TRUE)
  expect_match(host, "data-nm-volume-map=\"faces_z\"", fixed = TRUE)
  expect_match(host, "data-nm-volume-modified=\"false\"", fixed = TRUE)
  expect_match(host, "data-nm-volume-shell hidden", fixed = TRUE)
  expect_match(host, "static montage is the report default", fixed = TRUE)
  expect_match(host, "Reset to report position", fixed = TRUE)
  expect_match(host, "recoverable voxel data", fixed = TRUE)
  expect_match(host, "JavaScript is disabled", fixed = TRUE)
  expect_error(
    neuromosaic:::.montage_interactive_host_html(
      rd$interactive, "unknown", is_html = TRUE
    ),
    "no analysis_id"
  )

  hooks <- montage_interactive_report_hooks(rd$interactive, is_html = TRUE)
  expect_named(hooks, c("emit_document", "emit_host"))
  expect_true(any(grepl(
    "data-nm-volume-runtime", capture.output(hooks$emit_document()),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "data-nm-volume-analysis=\"faces\"",
    capture.output(hooks$emit_host("faces")), fixed = TRUE
  )))
  expect_silent(montage_interactive_report_hooks(
    rd$interactive, is_html = FALSE
  )$emit_document())
  expect_error(montage_interactive_report_hooks(list(), is_html = TRUE),
               "prepared montage interactive data")
})
