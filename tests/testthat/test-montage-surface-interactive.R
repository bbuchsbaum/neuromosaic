make_interactive_surface_geometry <- function(hemi = c("lh", "rh"),
                                              offset = 0) {
  hemi <- match.arg(hemi)
  vertices <- matrix(
    c(
      0 + offset, 0, 0,
      1 + offset, 0, 0,
      0 + offset, 1, 0,
      1 + offset, 1, 0
    ),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(c(0, 1, 2, 1, 3, 2), ncol = 3, byrow = TRUE)
  neurosurf::SurfaceGeometry(vertices, faces, hemi = hemi)
}

make_interactive_surface_atlas <- function() {
  left <- make_interactive_surface_geometry("lh")
  right <- make_interactive_surface_geometry("rh", offset = 3)
  labeled <- function(geometry) {
    methods::new(
      "LabeledNeuroSurface",
      geometry = geometry,
      indices = as.integer(seq_len(4L)),
      data = c(1, 1, 2, 2),
      labels = c("Region A", "Region B"),
      cols = c("#336699", "#CC6633")
    )
  }
  atlas <- list(
    ids = c(1L, 2L),
    labels = c("Region A", "Region B"),
    hemi = c("left", "right"),
    lh_atlas = labeled(left),
    rh_atlas = labeled(right),
    surf_type = "white",
    surface_space = "toy"
  )
  class(atlas) <- c("toy_surface", "surfatlas", "atlas")
  atlas
}

make_interactive_surface_manifest <- function() {
  manifest <- data.frame(
    map_id = c("faces_z", "faces_beta"),
    analysis_id = "faces",
    analysis_label = "Faces contrast",
    role = c("primary", "auxiliary"),
    quantity = c("test_statistic", "estimate"),
    distribution = c("z", NA_character_),
    units = c("z", "beta"),
    signed = TRUE,
    threshold = c(3, NA_real_),
    tail = "two_sided",
    selector_label = c("Z statistic", "Beta"),
    display_order = c(1, 2),
    label = c("Faces Z statistic", "Faces beta estimate"),
    stringsAsFactors = FALSE
  )
  manifest$parcel_values <- I(list(
    c(`1` = 4.5, `2` = -5),
    c(`1` = 0.8, `2` = -0.3)
  ))
  manifest
}

make_interactive_surface_panels <- function(map_ids, image) {
  stats::setNames(lapply(map_ids, function(map_id) {
    list(surface_image = image)
  }), map_ids)
}

read_embedded_surface_values <- function(surface, asset_id) {
  payload <- surface$payloads[[asset_id]]
  bytes <- jsonlite::base64_dec(payload$base64)
  if (identical(payload$compression, "gzip")) {
    input <- gzcon(rawConnection(bytes, open = "rb"))
    on.exit(close(input), add = TRUE)
    bytes <- readBin(input, what = "raw", n = 10^7)
  }
  readBin(bytes, what = "numeric", n = length(bytes) / 4L,
          size = 4L, endian = "little")
}

test_that("montage_surface validates its portable display contract", {
  config <- montage_surface()

  expect_s3_class(config, "montage_surface")
  expect_identical(config$projection, "auto")
  expect_equal(config$depth, seq(0.1, 0.9, length.out = 5L))
  expect_identical(config$preset, "paper-light")
  expect_identical(config$height, "600px")
  expect_identical(config$assets, "bundle")
  expect_identical(config$compression, "gzip")
  expect_equal(config$max_embed_mb, 25)
  expect_identical(
    config$controls,
    c("threshold", "range", "palette", "opacity")
  )

  expect_error(montage_surface(opacity = 2), "between zero and one")
  expect_error(montage_surface(height = ""), "CSS size string")
  expect_error(
    montage_surface(values = list(a = 1, a = 2)),
    "uniquely named"
  )
  expect_error(
    montage_surface(interpolation = "linear", aggregate = "mode"),
    "invalid with linear interpolation",
    fixed = TRUE
  )
  expect_error(
    montage_surface(controls = c("palette", "palette")),
    "unique subset"
  )
  expect_error(montage_surface(controls = "camera"), "unique subset")
  expect_error(montage_surface(assets = "remote"), "arg")
  expect_error(montage_surface(compression = "zstd"), "arg")
  expect_error(montage_surface(max_embed_mb = 0), "positive")
})

test_that("render_montage_report builds one synchronized surface scene per analysis", {
  output_dir <- tempfile("interactive-surface-report-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()
  output <- file.path(output_dir, "report.qmd")

  render_montage_report(
    manifest,
    output_file = output,
    surfatlas = make_interactive_surface_atlas(),
    panels = make_interactive_surface_panels(manifest$map_id, image),
    render_surface = FALSE,
    materialize_recipes = FALSE,
    check_files = FALSE,
    surface = montage_surface(assets = "embed", compression = "none")
  )

  rd <- readRDS(file.path(output_dir, "report_report-data.rds"))
  expect_s3_class(rd$surface, "montage_surface_report")
  expect_length(rd$surface$scenes, 1L)
  group <- rd$surface$scenes[["faces"]]
  expect_s3_class(group, "montage_surface_group")
  expect_null(group$scene)
  expect_identical(group$manifest$schemaVersion, "surfview.scene.v1")
  expect_length(group$manifest$layers, 2L)
  expect_identical(
    group$manifest$selectedLayer,
    unname(group$map_to_layer[["faces_z"]])
  )
  expect_identical(
    unname(group$layer_to_map[group$manifest$selectedLayer]),
    "faces_z"
  )

  z_layer <- group$manifest$layers[[unname(group$map_to_layer[["faces_z"]])]]
  beta_layer <- group$manifest$layers[[unname(group$map_to_layer[["faces_beta"]])]]
  expect_identical(z_layer$metadata$map_id, "faces_z")
  expect_identical(beta_layer$metadata$map_id, "faces_beta")
  expect_equal(
    read_embedded_surface_values(rd$surface, z_layer$values$left$values),
    c(4.5, 4.5, -5, -5)
  )
  expect_equal(
    read_embedded_surface_values(rd$surface, z_layer$values$right$values),
    c(4.5, 4.5, -5, -5)
  )
  expect_equal(z_layer$threshold, c(-3, 3))
  expect_null(beta_layer$threshold)
  expect_identical(z_layer$legend$title, "Test statistic")
  expect_identical(beta_layer$legend$title, "Estimate")
  expect_equal(rd$provenance$interactive_surface_scene_count, 1L)
  expect_equal(rd$provenance$interactive_surface_layer_count, 2L)
  expect_identical(rd$surface$summary$packaging, "embed")
  expect_identical(rd$surface$summary$compression, "none")
  expect_gt(rd$surface$summary$asset_count, 0L)
  expect_gt(rd$surface$summary$geometry_asset_count, 0L)
  expect_gt(rd$surface$summary$value_asset_count, 0L)
  expect_equal(
    rd$surface$summary$compressed_bytes,
    rd$surface$summary$uncompressed_bytes
  )
  expect_gt(rd$surface$summary$embedded_bytes, 0)
  expect_match(rd$surface$runtime$adapter_version, "^1\\.0\\.0$")
  expect_match(rd$surface$runtime$adapter_sha256, "^[0-9a-f]{64}$")
  expect_match(rd$provenance$interactive_surface_asset_hashes, "sha256")
  expect_false(any(grepl(output_dir, unlist(rd$surface$assets), fixed = TRUE)))
})

test_that("interactive surface values require exact map and topology coverage", {
  output_dir <- tempfile("interactive-surface-validation-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()
  atlas <- make_interactive_surface_atlas()
  panels <- make_interactive_surface_panels(manifest$map_id, image)
  values <- list(
    faces_z = list(left = 1:4, right = 4:1),
    faces_beta = list(left = seq(0.1, 0.4, 0.1), right = seq(0.4, 0.1, -0.1))
  )

  expect_error(
    render_montage_report(
      manifest,
      output_file = file.path(output_dir, "missing.qmd"),
      surfatlas = atlas,
      panels = panels,
      render_surface = FALSE,
      materialize_recipes = FALSE,
      check_files = FALSE,
      surface = montage_surface(values = values["faces_z"])
    ),
    "must match manifest map_id values exactly"
  )

  bad_left <- make_interactive_surface_geometry("lh")
  bad_faces <- matrix(c(0, 1, 3, 0, 3, 2), ncol = 3, byrow = TRUE)
  bad_left <- neurosurf::SurfaceGeometry(
    neurosurf::coords(bad_left), bad_faces, hemi = "lh"
  )
  expect_error(
    render_montage_report(
      manifest,
      output_file = file.path(output_dir, "topology.qmd"),
      surfatlas = atlas,
      panels = panels,
      render_surface = FALSE,
      materialize_recipes = FALSE,
      check_files = FALSE,
      surface = montage_surface(
        geometry = list(left = bad_left, right = atlas$rh_atlas@geometry),
        values = values
      )
    ),
    "Topology mismatch"
  )
})

test_that("interactive surface mode retains a static fallback for every map", {
  output_dir <- tempfile("interactive-surface-fallback-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()
  panels <- make_interactive_surface_panels(manifest$map_id[[1L]], image)

  expect_error(
    render_montage_report(
      manifest,
      output_file = file.path(output_dir, "report.qmd"),
      surfatlas = make_interactive_surface_atlas(),
      panels = panels,
      render_surface = FALSE,
      materialize_recipes = FALSE,
      check_files = FALSE,
      surface = montage_surface()
    ),
    "static surface fallback.*faces_beta"
  )
})

test_that("surface report hooks emit a synchronized widget host", {
  output_dir <- tempfile("interactive-surface-hooks-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()
  output <- file.path(output_dir, "report.qmd")
  render_montage_report(
    manifest,
    output_file = output,
    surfatlas = make_interactive_surface_atlas(),
    panels = make_interactive_surface_panels(manifest$map_id, image),
    render_surface = FALSE,
    materialize_recipes = FALSE,
    check_files = FALSE,
    surface = montage_surface()
  )
  rd <- readRDS(file.path(output_dir, "report_report-data.rds"))
  hooks <- montage_surface_report_hooks(rd$surface, is_html = TRUE)

  document <- paste(capture.output(hooks$emit_document()), collapse = "\n")
  host <- paste(capture.output(hooks$emit_host("faces")), collapse = "\n")
  expect_match(document, "data-nm-surface-bridge", fixed = TRUE)
  expect_match(document, "data-nm-surface-display", fixed = TRUE)
  expect_match(document, "NeuroMosaicSurfaceDisplay", fixed = TRUE)
  expect_match(document, "handle.selectLayer", fixed = TRUE)
  expect_match(document, "controlTarget", fixed = TRUE)
  expect_match(document, "nm-map-request", fixed = TRUE)
  expect_match(document, "pagehide", fixed = TRUE)
  expect_match(document, "updateScalarMapping", fixed = TRUE)
  expect_match(document, "setLayerOpacity", fixed = TRUE)
  expect_match(document, "data-nm-view-router", fixed = TRUE)
  expect_match(document, "Analysis view", fixed = TRUE)
  expect_match(host, "data-nm-surface-host", fixed = TRUE)
  expect_match(host, "Explore surface interactively", fixed = TRUE)
  expect_match(host, "class=\"surfwidget nm-surface-widget\"", fixed = TRUE)
  expect_match(host, "faces_z", fixed = TRUE)
  expect_match(host, "faces_beta", fixed = TRUE)
  expect_match(host, "data-nm-surface-bilateral", fixed = TRUE)
  expect_match(host, "Surface colormap", fixed = TRUE)
  expect_match(host, "Surface threshold low", fixed = TRUE)
  expect_match(host, "Surface range high", fixed = TRUE)
  expect_match(host, "Surface opacity", fixed = TRUE)
  expect_match(host, "Reset map display", fixed = TRUE)

  static_hooks <- montage_surface_report_hooks(NULL, is_html = TRUE)
  expect_output(static_hooks$emit_document(), NA)
  expect_output(static_hooks$emit_host("faces"), NA)
  expect_error(
    montage_surface_report_hooks(list(), is_html = TRUE),
    "prepared montage surface data"
  )
  expect_error(hooks$emit_host("missing"), "no analysis_id 'missing'")
})

test_that("surface threshold tails retain explicit display semantics", {
  expect_equal(
    neuromosaic:::.montage_surface_threshold_pair(
      "thresholded", 3, "two_sided", c(-8, 8)
    ),
    c(-3, 3)
  )
  expect_equal(
    neuromosaic:::.montage_surface_threshold_pair(
      "thresholded", 3, "positive", c(0, 8)
    ),
    c(0, 3)
  )
  expect_equal(
    neuromosaic:::.montage_surface_threshold_pair(
      "thresholded", 3, "negative", c(-8, 0)
    ),
    c(-3, 0)
  )
  expect_null(neuromosaic:::.montage_surface_threshold_pair(
    "continuous", NA_real_, "two_sided", c(-8, 8)
  ))
})

test_that("volume-to-surface projection is rejected when explicitly disabled", {
  output_dir <- tempfile("interactive-surface-no-projection-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()[1, , drop = FALSE]
  manifest$parcel_values <- NULL
  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2))
  manifest$stat_map <- I(list(neuroim2::NeuroVol(
    array(c(0, 0, 0, 4, 0, 0, 0, -5), dim = c(2, 2, 2)),
    space
  )))

  expect_error(
    render_montage_report(
      manifest,
      output_file = file.path(output_dir, "report.qmd"),
      surfatlas = make_interactive_surface_atlas(),
      panels = make_interactive_surface_panels(manifest$map_id, image),
      render_surface = FALSE,
      materialize_recipes = FALSE,
      check_files = FALSE,
      surface = montage_surface(projection = "none")
    ),
    "projection = 'none'.*forbids volume-to-surface projection"
  )
})

test_that("HTML reports knit grouped surface widgets and their dependencies", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for render tests")
  output_dir <- tempfile("interactive-surface-html-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()
  output <- file.path(output_dir, "report.html")

  render_montage_report(
    manifest,
    output_file = output,
    surfatlas = make_interactive_surface_atlas(),
    panels = make_interactive_surface_panels(manifest$map_id, image),
    render_surface = FALSE,
    materialize_recipes = FALSE,
    check_files = FALSE,
    surface = montage_surface(height = "420px"),
    quiet = TRUE
  )

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "data-nm-surface-host", fixed = TRUE)
  expect_match(html, "class=\"surfwidget nm-surface-widget\"", fixed = TRUE)
  expect_match(html, "surfview.mountSurfView", fixed = TRUE)
  expect_match(html, "data-nm-surface-bridge", fixed = TRUE)
  expect_match(html, "nm-map-request", fixed = TRUE)
  expect_match(html, "faces_z", fixed = TRUE)
  expect_match(html, "faces_beta", fixed = TRUE)
  expect_match(html, "height:420px", fixed = TRUE)
})

test_that("surface bundles reuse content-addressed geometry across analyses", {
  output_dir <- tempfile("interactive-surface-bundle-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  first <- make_interactive_surface_manifest()
  second <- first
  second$analysis_id <- "scenes"
  second$analysis_label <- "Scenes contrast"
  second$map_id <- c("scenes_z", "scenes_beta")
  manifest <- rbind(first, second)
  manifest$parcel_values <- I(c(first$parcel_values, second$parcel_values))
  output <- file.path(output_dir, "report.qmd")

  render_montage_report(
    manifest,
    output_file = output,
    surfatlas = make_interactive_surface_atlas(),
    panels = make_interactive_surface_panels(manifest$map_id, image),
    render_surface = FALSE,
    materialize_recipes = FALSE,
    check_files = FALSE,
    surface = montage_surface(assets = "bundle", compression = "gzip")
  )

  rd <- readRDS(file.path(output_dir, "report_report-data.rds"))
  asset_dir <- file.path(
    output_dir, "report_files", "interactive", "surfaces"
  )
  files <- list.files(asset_dir, pattern = "\\.bin\\.gz$", full.names = TRUE)
  expect_length(files, rd$surface$summary$asset_count)
  expect_true(all(file.exists(files)))
  expect_true(all(vapply(
    rd$surface$assets,
    function(asset) identical(asset$location$kind, "relative"),
    logical(1)
  )))
  expect_true(all(vapply(rd$surface$assets, function(asset) {
    !grepl("^/|^[A-Za-z]:", asset$location$ref)
  }, logical(1))))
  expect_true(all(vapply(rd$surface$assets, function(asset) {
    path <- file.path(output_dir, asset$location$ref)
    identical(
      neuromosaic:::.montage_surface_asset_sha256(path, "gzip"),
      asset$hash
    )
  }, logical(1))))
  geometry_refs <- unlist(lapply(rd$surface$scenes, function(group) {
    unlist(lapply(group$manifest$geometries, function(geometry) {
      unlist(geometry[c("vertices", "faces", "curvature")], use.names = FALSE)
    }), use.names = FALSE)
  }), use.names = FALSE)
  expect_gt(length(geometry_refs), length(unique(geometry_refs)))
  expect_true(any(vapply(rd$surface$assets, function(asset) {
    "compression_gzip" %in% asset$transformations
  }, logical(1))))

  # Re-rendering the same bundle validates and reuses existing addressed files.
  expect_no_error(render_montage_report(
    manifest,
    output_file = output,
    surfatlas = make_interactive_surface_atlas(),
    panels = make_interactive_surface_panels(manifest$map_id, image),
    render_surface = FALSE,
    materialize_recipes = FALSE,
    check_files = FALSE,
    surface = montage_surface(assets = "bundle", compression = "gzip")
  ))
})

test_that("embedded compressed surfaces enforce an explicit byte budget", {
  output_dir <- tempfile("interactive-surface-budget-")
  dir.create(output_dir)
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  manifest <- make_interactive_surface_manifest()

  expect_error(
    render_montage_report(
      manifest,
      output_file = file.path(output_dir, "report.qmd"),
      surfatlas = make_interactive_surface_atlas(),
      panels = make_interactive_surface_panels(manifest$map_id, image),
      render_surface = FALSE,
      materialize_recipes = FALSE,
      check_files = FALSE,
      surface = montage_surface(
        assets = "embed", compression = "gzip", max_embed_mb = 1e-8
      )
    ),
    "raw.*gzip.*base64.*exceeding max_embed_mb"
  )
})
