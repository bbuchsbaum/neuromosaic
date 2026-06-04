write_placeholder_map <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("placeholder", path)
  path
}

make_build_manifest_table <- function(path) {
  data.frame(
    map_id = "map_a",
    path = path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    tail = "two_sided",
    connectivity = "18-connect",
    min_cluster_size = 3,
    label = "Map A",
    stringsAsFactors = FALSE
  )
}

test_that("build_manifest reads and validates TSV render manifests", {
  tmpdir <- tempfile("build-manifest-table-")
  dir.create(tmpdir, recursive = TRUE)
  map_path <- write_placeholder_map(file.path(tmpdir, "map-a.nii.gz"))
  manifest <- make_build_manifest_table(map_path)
  manifest_path <- file.path(tmpdir, "manifest.tsv")
  utils::write.table(
    manifest,
    file = manifest_path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  out <- build_manifest(manifest_path)

  expect_s3_class(out, "data.frame")
  expect_identical(out$map_id, "map_a")
  expect_identical(out$label, "Map A")
})

test_that("build_manifest parses globbed filename entities and applies overrides", {
  tmpdir <- tempfile("build-manifest-glob-")
  paths <- c(
    write_placeholder_map(file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")),
    write_placeholder_map(file.path(tmpdir, "contrast-places_model-m2_stat-z.nii.gz"))
  )

  out <- build_manifest(
    pattern = "*.nii.gz",
    root = tmpdir,
    overrides = list(
      stat_kind = "z",
      units = "z",
      signed = TRUE,
      threshold = 3,
      tail = "two_sided",
      connectivity = "18-connect",
      min_cluster_size = 5
    ),
    labeller = function(entities) {
      list(
        title = paste(entities$contrast, entities$model),
        legend_semantics = paste("warm =", entities$contrast)
      )
    }
  )

  expect_equal(sort(out$path), sort(normalizePath(paths)))
  expect_true(all(c("contrast", "model", "stat") %in% names(out)))
  expect_identical(sort(out$contrast), c("faces", "places"))
  expect_identical(sort(out$label), c("faces m1", "places m2"))
  expect_true(all(out$min_cluster_size == 5))
})

test_that("build_manifest applies override tables by map_id", {
  tmpdir <- tempfile("build-manifest-overrides-")
  map_path <- write_placeholder_map(file.path(tmpdir, "map-a.nii.gz"))
  manifest <- make_build_manifest_table(map_path)
  manifest$label <- "Original"

  overrides <- data.frame(
    map_id = "map_a",
    label = "Overridden",
    threshold = 4,
    stringsAsFactors = FALSE
  )
  out <- build_manifest(manifest, overrides = overrides)

  expect_identical(out$label, "Overridden")
  expect_equal(out$threshold, 4)
})

test_that("build_manifest fails loudly when labels remain uncovered", {
  tmpdir <- tempfile("build-manifest-missing-label-")
  map_path <- write_placeholder_map(file.path(tmpdir, "map-a.nii.gz"))
  manifest <- make_build_manifest_table(map_path)
  manifest$label <- NULL

  expect_error(
    build_manifest(manifest),
    "missing required column.*label"
  )
})

test_that("build_manifest validates source and override errors", {
  tmpdir <- tempfile("build-manifest-errors-")
  dir.create(tmpdir, recursive = TRUE)

  expect_error(
    build_manifest(pattern = "*.nii.gz", root = tmpdir),
    "No files matched"
  )

  map_path <- write_placeholder_map(file.path(tmpdir, "map-a.nii.gz"))
  manifest <- make_build_manifest_table(map_path)
  expect_error(
    build_manifest(manifest, overrides = list(label = c("a", "b", "c"))),
    "length 1 or nrow"
  )
  expect_error(
    build_manifest(manifest, overrides = data.frame(label = "x")),
    "map_id.*path"
  )
})
