test_that("materialize_montage_recipes writes and reuses derived maps", {
  inputs <- make_toy_cluster_report_inputs()
  cache_dir <- tempfile("montage-recipe-cache-")
  calls <- 0L

  manifest <- data.frame(
    map_id = "derived_z",
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    label = "Derived Z",
    stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(function(row) {
    calls <<- calls + 1L
    inputs$stat_map
  }))

  out <- materialize_montage_recipes(
    manifest,
    cache_dir = cache_dir,
    check_files = TRUE
  )

  expect_equal(calls, 1L)
  expect_true(file.exists(out$path[[1]]))
  expect_true(out$recipe_materialized[[1]])
  expect_false(is.na(out$map_hash[[1]]))

  calls <- 0L
  reused <- materialize_montage_recipes(
    manifest,
    cache_dir = cache_dir,
    check_files = TRUE
  )

  expect_equal(calls, 0L)
  expect_identical(reused$path, out$path)
  expect_identical(reused$map_hash, out$map_hash)
})

test_that("recipe-backed manifests can be overlay-QC validated directly", {
  inputs <- make_toy_cluster_report_inputs()
  manifest <- data.frame(
    map_id = "recipe_qc",
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    label = "Recipe QC",
    stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(list(
    key = "qc",
    fun = function(row) inputs$stat_map
  )))

  out <- validate_manifest(
    manifest,
    background = inputs$stat_map,
    load_maps = TRUE,
    check_files = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_identical(out$map_id, "recipe_qc")
})

test_that("render_montage_report materializes recipes into qmd sidecar data", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-recipe-render-")
  dir.create(tmpdir, recursive = TRUE)
  cache_dir <- file.path(tmpdir, "cache")

  manifest <- data.frame(
    map_id = "recipe_render",
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    label = "Recipe Render",
    stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(function(row) inputs$stat_map))

  out <- render_montage_report(
    manifest,
    output_file = file.path(tmpdir, "recipe-report.qmd"),
    bg = inputs$stat_map,
    cache_dir = cache_dir,
    image_width = 700,
    image_height = 500,
    image_res = 72
  )

  rd <- readRDS(sub("\\.qmd$", "_report-data.rds", out))
  expect_true(file.exists(rd$manifest$path[[1]]))
  expect_true(startsWith(rd$manifest$path[[1]], normalizePath(cache_dir)))
  expect_false(is.na(rd$manifest$map_hash[[1]]))
  expect_true(file.exists(rd$panels$recipe_render$volume_image))
})
