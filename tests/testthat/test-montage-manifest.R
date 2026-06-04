make_valid_montage_manifest <- function(path = "map.nii.gz") {
  data.frame(
    map_id = "grp_vivid_onesample_s2",
    path = path,
    stat_kind = "T",
    df = "31",
    units = "t",
    signed = "true",
    p = "0.005",
    tail = "two_sided",
    connectivity = "18-connect",
    min_cluster_size = "10",
    level = "group",
    label = "Vividness modulation - visualization",
    description = "A test panel.",
    n = "27",
    stringsAsFactors = FALSE
  )
}

test_that("montage_manifest_schema carries cluster-report parity fields", {
  schema <- montage_manifest_schema()

  expect_s3_class(schema, "data.frame")
  expect_true(all(c("field", "required", "type", "role") %in% names(schema)))
  expect_true(all(c(
    "map_id", "path", "recipe", "stat_kind", "df", "units", "signed",
    "p", "threshold", "tail", "connectivity", "min_cluster_size",
    "space", "template", "mask", "label", "n", "subjects"
  ) %in% schema$field))
  expect_true(schema$required[schema$field == "map_id"])
  expect_true(schema$required[schema$field == "label"])
})

test_that("validate_manifest accepts and normalizes a valid TSV-style manifest", {
  manifest <- make_valid_montage_manifest()

  out <- validate_manifest(manifest, check_files = FALSE)

  expect_s3_class(out, "data.frame")
  expect_identical(out$stat_kind, "t")
  expect_identical(out$signed, TRUE)
  expect_equal(out$df, 31)
  expect_equal(out$min_cluster_size, 10)
  expect_equal(out$n, 27)
})

test_that("validate_manifest enforces required identity and source fields", {
  manifest <- make_valid_montage_manifest()

  expect_error(
    validate_manifest(manifest[names(manifest) != "map_id"], check_files = FALSE),
    "missing required column.*map_id"
  )

  blank_label <- manifest
  blank_label$label <- ""
  expect_error(
    validate_manifest(blank_label, check_files = FALSE),
    "non-empty 'label'"
  )

  duplicate <- rbind(manifest, manifest)
  expect_error(
    validate_manifest(duplicate, check_files = FALSE),
    "must be unique"
  )

  no_source <- manifest[names(manifest) != "path"]
  expect_error(
    validate_manifest(no_source, check_files = FALSE),
    "must define 'path' or 'recipe'"
  )
})

test_that("validate_manifest enforces policy parity fields", {
  manifest <- make_valid_montage_manifest()

  bad_p <- manifest
  bad_p$p <- 1.5
  expect_error(
    validate_manifest(bad_p, check_files = FALSE),
    "between 0 and 1"
  )

  bad_tail <- manifest
  bad_tail$tail <- "upper"
  expect_error(
    validate_manifest(bad_tail, check_files = FALSE),
    "tail.*two_sided"
  )

  bad_conn <- manifest
  bad_conn$connectivity <- "12-connect"
  expect_error(
    validate_manifest(bad_conn, check_files = FALSE),
    "connectivity"
  )

  missing_df <- manifest
  missing_df$df <- NA
  missing_df$threshold <- NA
  expect_error(
    validate_manifest(missing_df, check_files = FALSE),
    "require 'df'"
  )
})

test_that("validate_manifest checks path existence when requested", {
  manifest <- make_valid_montage_manifest("missing-map.nii.gz")

  expect_error(
    validate_manifest(manifest, check_files = TRUE),
    "path.*do not exist"
  )
})

test_that("validate_manifest overlay QC catches empty maps and grid mismatch", {
  inputs <- make_toy_cluster_report_inputs()
  map_path <- tempfile("montage-map-", fileext = ".nii.gz")
  neuroim2::write_vol(inputs$stat_map, map_path)

  manifest <- make_valid_montage_manifest(map_path)
  manifest$threshold <- 3

  expect_s3_class(
    validate_manifest(
      manifest,
      background = inputs$stat_map,
      load_maps = TRUE
    ),
    "data.frame"
  )

  empty <- manifest
  empty$threshold <- 100
  expect_error(
    validate_manifest(empty, background = inputs$stat_map, load_maps = TRUE),
    "No finite suprathreshold voxels"
  )

  mismatch_space <- neuroim2::NeuroSpace(
    dim = c(6, 6, 6),
    spacing = c(2, 2, 2),
    origin = c(-5, -5, -5)
  )
  mismatch_arr <- array(0, dim = c(6, 6, 6))
  mismatch_arr[2, 2, 2] <- 4
  mismatch_path <- tempfile("montage-mismatch-", fileext = ".nii.gz")
  neuroim2::write_vol(neuroim2::NeuroVol(mismatch_arr, mismatch_space),
                      mismatch_path)

  mismatch <- manifest
  mismatch$path <- mismatch_path
  expect_error(
    validate_manifest(
      mismatch,
      background = inputs$stat_map,
      load_maps = TRUE
    ),
    "Grid mismatch"
  )
})
