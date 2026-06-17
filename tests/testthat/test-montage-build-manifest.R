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

test_that("build_manifest keeps bare tokens distinct in map_id (#3)", {
  tmpdir <- tempfile("build-manifest-bare-")
  write_placeholder_map(
    file.path(tmpdir, "group_task-recog_cue_model-metaRandom_z_g.nii.gz"))
  write_placeholder_map(
    file.path(tmpdir, "group_task-recog_probe_old_model-metaRandom_z_g.nii.gz"))

  out <- build_manifest(pattern = "*.nii.gz", root = tmpdir, validate = FALSE)

  # The contrast lives in a bare token; map_id must stay unique across maps.
  expect_equal(length(unique(out$map_id)), 2L)
  expect_setequal(
    out$map_id,
    c("group_task-recog_cue_model-metaRandom_z_g",
      "group_task-recog_probe_old_model-metaRandom_z_g")
  )
})

test_that("build_manifest derives stat_kind/signed from a bare stat token (#3)", {
  tmpdir <- tempfile("build-manifest-stat-token-")
  write_placeholder_map(
    file.path(tmpdir, "group_cue_model-metaRandom_z_g.nii.gz"))

  out <- build_manifest(pattern = "*.nii.gz", root = tmpdir, validate = FALSE)

  expect_true(all(c("stat_kind", "signed") %in% names(out)))
  expect_identical(out$stat_kind, "z")
  expect_true(out$signed)
})

test_that("build_manifest derives stat_kind from a stat- entity (#3)", {
  tmpdir <- tempfile("build-manifest-stat-entity-")
  write_placeholder_map(
    file.path(tmpdir, "contrast-cue_model-metaRandom_stat-t.nii.gz"))

  out <- build_manifest(pattern = "*.nii.gz", root = tmpdir, validate = FALSE)

  expect_identical(out$stat_kind, "t")
  expect_true(out$signed)
  expect_identical(out$map_id, "contrast-cue_model-metaRandom_stat-t")
})

test_that("build_manifest warns on colliding map_id from filenames (#3)", {
  tmpdir <- tempfile("build-manifest-collide-")
  write_placeholder_map(file.path(tmpdir, "a", "zstat.nii.gz"))
  write_placeholder_map(file.path(tmpdir, "b", "zstat.nii.gz"))

  expect_warning(
    build_manifest(pattern = file.path("*", "zstat.nii.gz"),
                   root = tmpdir, validate = FALSE),
    "Duplicate map_id"
  )
})

test_that("build_manifest does not clobber an explicit signed entity (#3)", {
  tmpdir <- tempfile("build-manifest-signed-")
  write_placeholder_map(
    file.path(tmpdir, "contrast-a_stat-beta_signed-false.nii.gz"))

  out <- build_manifest(pattern = "*.nii.gz", root = tmpdir, validate = FALSE)

  expect_identical(out$stat_kind, "beta")
  # The explicit signed-false token must survive derivation (coerced later by
  # validate_manifest); deriving stat_kind must not force signed = TRUE.
  expect_identical(as.character(out$signed), "false")
})

test_that("build_manifest validates a pattern manifest with derived stat_kind/signed (#3)", {
  tmpdir <- tempfile("build-manifest-derived-validate-")
  write_placeholder_map(file.path(tmpdir, "contrast-cue_z.nii.gz"))

  # Only `label` is supplied; stat_kind/signed are derived and must satisfy
  # validate_manifest() in the intended pattern workflow (validate = TRUE).
  out <- build_manifest(
    pattern = "*.nii.gz",
    root = tmpdir,
    overrides = list(label = "Cue"),
    validate = TRUE
  )

  expect_identical(out$map_id, "contrast-cue_z")
  expect_identical(out$stat_kind, "z")
  expect_true(out$signed)
  expect_identical(out$label, "Cue")
})

test_that("build_manifest treats a bare t/z token as the statistic kind (#3, documented contract)", {
  tmpdir <- tempfile("build-manifest-bare-t-")
  write_placeholder_map(file.path(tmpdir, "sub-01_task-rest_t_map.nii.gz"))

  out <- build_manifest(pattern = "*.nii.gz", root = tmpdir, validate = FALSE)

  # Bare statistic tokens are inferred by design (issue #3 wants `..._z_g`-style
  # names to work). Non-BIDS names that should NOT infer must pass an explicit
  # `source`/`overrides`. This test pins the contract so it stays intentional.
  expect_identical(out$stat_kind, "t")
  expect_true(out$signed)
})

test_that("build_manifest warns when an override map_id matches nothing (#3)", {
  tmpdir <- tempfile("build-manifest-stale-override-")
  write_placeholder_map(file.path(tmpdir, "contrast-cue_z.nii.gz"))

  overrides <- data.frame(
    map_id = "old-entity-style-id",
    label = "Cue",
    stringsAsFactors = FALSE
  )
  expect_warning(
    build_manifest(pattern = "*.nii.gz", root = tmpdir,
                   overrides = overrides, validate = FALSE),
    "matched no manifest rows"
  )
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
