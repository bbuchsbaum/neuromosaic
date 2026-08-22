.scene_geometry <- function(fingerprint = "geom-1") {
  geometry <- list(
    dimensions = c(5L, 6L, 7L),
    spacing = c(2, 2, 2),
    origin = c(0, 0, 0),
    orientation = "RAS",
    affine = as.numeric(diag(4)),
    fingerprint = fingerprint
  )
  geometry$fingerprint <- neuromosaic:::.montage_geometry_fingerprint(geometry)
  geometry
}

.scene_asset <- function(asset_id,
                         kind,
                         ref = paste0("interactive/", asset_id, ".nii.gz")) {
  list(
    asset_id = asset_id,
    kind = kind,
    location = list(kind = "relative", ref = ref),
    encoding = "nifti",
    datatype = if (identical(kind, "mask")) "uint8" else "float64",
    compression = "gzip",
    hash_algorithm = "md5",
    hash_scope = "uncompressed",
    hash = neuromosaic:::.montage_md5_text(asset_id),
    compressed_bytes = 128L,
    uncompressed_bytes = 512L,
    geometry = .scene_geometry(),
    transformations = character()
  )
}

.scene_display <- function(mode = "thresholded",
                           scale = "diverging",
                           units = "z") {
  list(
    mode = mode,
    scale = scale,
    center = if (identical(scale, "diverging")) 0 else NULL,
    limits = if (identical(scale, "diverging")) c(-6, 6) else c(0, 1),
    threshold = if (identical(mode, "thresholded")) 3.1 else NULL,
    tail = "two_sided",
    palette = if (identical(scale, "diverging")) "blue-red" else "inferno",
    alpha = 0.7,
    alpha_mode = "soft",
    support = "analysis",
    units = units
  )
}

.scene_map <- function(map_id = "zstat",
                       quantity = "test_statistic",
                       asset_id = map_id,
                       display = .scene_display(),
                       empty = FALSE) {
  list(
    map_id = map_id,
    quantity = quantity,
    label = paste("Map", map_id),
    units = display$units,
    asset_id = asset_id,
    display = display,
    status = list(
      state = if (isTRUE(empty)) "empty" else "ready",
      n_display_voxels = if (isTRUE(empty)) 0L else 12L
    ),
    selector_label = map_id
  )
}

.scene_analysis <- function(maps,
                            analysis_id = "analysis-1",
                            primary_map_id = maps[[1]]$map_id) {
  list(
    analysis_id = analysis_id,
    primary_map_id = primary_map_id,
    initial_world_coord = c(2, 3, 3),
    zlevel_bookmarks = c(2, 4, 6),
    maps = maps
  )
}

.valid_volume_scene <- function(maps = list(.scene_map()),
                                map_assets = lapply(maps, function(x) {
                                  .scene_asset(x$asset_id, "overlay")
                                }),
                                interactive = montage_interactive()) {
  neuromosaic:::.new_montage_volume_scene(
    interactive = interactive,
    background = list(asset_id = "background"),
    analyses = list(.scene_analysis(maps)),
    assets = c(list(.scene_asset("background", "background")), map_assets),
    engine = list(
      name = "neuroimjs",
      version = "0.3.0",
      adapter_version = "1.0.0"
    ),
    provenance = list(neuromosaic_version = "test")
  )
}

test_that("montage_interactive has safe orthogonal bundle defaults", {
  x <- montage_interactive()

  expect_s3_class(x, "montage_interactive")
  expect_identical(x$view, "orthogonal")
  expect_identical(x$assets, "bundle")
  expect_identical(x$compression, "gzip")
  expect_identical(x$initial_position, "primary_peak")
  expect_null(x$world_coord)
  expect_identical(x$controls, c("threshold", "palette", "opacity"))
  expect_identical(x$cache_maps, 2L)
  expect_true(x$static_fallback)
  expect_identical(x$engine, "neuroimjs")

  printed <- capture.output(print(x))
  expect_true(any(grepl("orthogonal", printed, fixed = TRUE)))
  expect_true(any(grepl("static fallback: yes", printed, fixed = TRUE)))
})

test_that("montage_interactive validates packaging and viewer choices", {
  explicit <- montage_interactive(
    assets = "embed",
    compression = "none",
    world_coord = c(1, 2, 3),
    controls = character(),
    max_embed_mb = 10.5,
    cache_maps = 4
  )
  expect_identical(explicit$initial_position, "explicit")
  expect_equal(explicit$world_coord, c(1, 2, 3))
  expect_identical(explicit$controls, character())
  expect_identical(explicit$cache_maps, 4L)

  expect_error(montage_interactive(view = "mosaic"), "arg")
  expect_error(montage_interactive(assets = "remote"), "arg")
  expect_error(montage_interactive(compression = "zstd"), "arg")
  expect_error(montage_interactive(world_coord = c(1, 2)), "three finite")
  expect_error(montage_interactive(world_coord = c(1, 2, Inf)), "three finite")
  expect_error(montage_interactive(controls = "window"), "Unsupported")
  expect_error(montage_interactive(max_embed_mb = 0), "positive finite")
  expect_error(montage_interactive(cache_maps = 1.5), "positive integer")
})

test_that("VolumeScene schema exposes the versioned generic contract", {
  schema <- montage_volume_scene_schema()

  expect_s3_class(schema, "data.frame")
  expect_named(schema, c("object", "field", "required", "type", "role"))
  expect_equal(nrow(schema), 67L)
  expect_true(all(c(
    "analysis_id", "map_id", "quantity", "initial_world_coord",
    "zlevel_bookmarks", "palette", "compressed_bytes", "fingerprint"
  ) %in% schema$field))
  expect_false(any(c("cope", "varcope", "zstat", "stat_map", "recipe") %in%
                     schema$field))
})

test_that("VolumeScene validates a one-map orthogonal scene", {
  scene <- .valid_volume_scene()

  expect_s3_class(scene, "montage_volume_scene")
  expect_identical(scene$schema, "org.neuromosaic.volume-scene")
  expect_identical(scene$schema_version, 1L)
  expect_identical(scene$analyses[[1]]$primary_map_id, "zstat")

  printed <- capture.output(print(scene))
  expect_true(any(grepl("analyses: 1", printed, fixed = TRUE)))
  expect_true(any(grepl("maps: 1", printed, fixed = TRUE)))
})

test_that("VolumeScene accepts grouped custom and empty maps", {
  custom_display <- .scene_display(
    mode = "continuous", scale = "sequential", units = "agreement"
  )
  maps <- list(
    .scene_map(),
    .scene_map(
      map_id = "reliability",
      quantity = "mylab:reliability",
      display = custom_display
    ),
    .scene_map(
      map_id = "empty-se",
      quantity = "standard_error",
      display = .scene_display(
        mode = "continuous", scale = "sequential", units = "SE"
      ),
      empty = TRUE
    )
  )

  scene <- .valid_volume_scene(maps)

  expect_identical(
    scene$analyses[[1]]$maps[[2]]$quantity,
    "mylab:reliability"
  )
  expect_identical(scene$analyses[[1]]$maps[[3]]$status$state, "empty")
  expect_identical(scene$analyses[[1]]$maps[[3]]$status$n_display_voxels, 0L)
})

test_that("VolumeScene rejects source-bearing or executable values", {
  scene <- .valid_volume_scene()

  bad_path <- unclass(scene)
  bad_path$assets[[1]]$location$ref <- "/private/data/template.nii.gz"
  expect_error(validate_montage_volume_scene(bad_path), "path-safe")

  bad_parent <- unclass(scene)
  bad_parent$assets[[1]]$location$ref <- "../template.nii.gz"
  expect_error(validate_montage_volume_scene(bad_parent), "path-safe")

  bad_source <- unclass(scene)
  bad_source$provenance$source_path <- "/private/data/zstat.nii.gz"
  expect_error(validate_montage_volume_scene(bad_source), "source-bearing")

  bad_function <- unclass(scene)
  bad_function$provenance$loader <- function() NULL
  expect_error(validate_montage_volume_scene(bad_function), "cannot enter")
})

test_that("VolumeScene fails closed on invalid references and surface assets", {
  scene <- .valid_volume_scene()

  missing_asset <- unclass(scene)
  missing_asset$analyses[[1]]$maps[[1]]$asset_id <- "missing"
  expect_error(validate_montage_volume_scene(missing_asset), "unknown asset")

  missing_primary <- unclass(scene)
  missing_primary$analyses[[1]]$primary_map_id <- "missing"
  expect_error(validate_montage_volume_scene(missing_primary), "not present")

  surface <- unclass(scene)
  surface$assets[[2]]$kind <- "surface"
  expect_error(validate_montage_volume_scene(surface), "must be one of")

  duplicate <- unclass(scene)
  duplicate$assets[[2]]$asset_id <- "background"
  expect_error(validate_montage_volume_scene(duplicate), "must be unique")
})

test_that("VolumeScene rejects misleading display defaults", {
  scene <- .valid_volume_scene()

  bad_threshold <- unclass(scene)
  bad_threshold$analyses[[1]]$maps[[1]]$display$threshold <- 0
  expect_error(
    validate_montage_volume_scene(bad_threshold),
    "require a positive threshold"
  )

  bad_center <- unclass(scene)
  bad_center$analyses[[1]]$maps[[1]]$display$center <- 1
  expect_error(validate_montage_volume_scene(bad_center), "center = 0")

  bad_empty <- unclass(scene)
  bad_empty$analyses[[1]]$maps[[1]]$status <- list(
    state = "empty", n_display_voxels = 2L
  )
  expect_error(validate_montage_volume_scene(bad_empty), "must have")
})

test_that("VolumeScene rejects geometry drift and invalid group coordinates", {
  scene <- .valid_volume_scene()

  bad_geometry <- unclass(scene)
  bad_geometry$assets[[2]]$geometry$origin[[1]] <- 0.25
  bad_geometry$assets[[2]]$geometry$fingerprint <-
    neuromosaic:::.montage_geometry_fingerprint(
      bad_geometry$assets[[2]]$geometry
    )
  expect_error(
    validate_montage_volume_scene(bad_geometry),
    "does not match background geometry"
  )

  bad_fingerprint <- unclass(scene)
  bad_fingerprint$assets[[2]]$geometry$fingerprint <- paste0(
    "md5:", strrep("0", 32L)
  )
  expect_error(
    validate_montage_volume_scene(bad_fingerprint),
    "fingerprint.*does not match"
  )

  outside <- unclass(scene)
  outside$analyses[[1]]$initial_world_coord <- c(100, 100, 100)
  expect_error(validate_montage_volume_scene(outside), "outside")

  bad_bookmark <- unclass(scene)
  bad_bookmark$analyses[[1]]$zlevel_bookmarks <- 100
  expect_error(validate_montage_volume_scene(bad_bookmark), "z slices")
})
