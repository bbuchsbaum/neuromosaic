read_asset_raw <- function(path, compression) {
  con <- if (identical(compression, "gzip")) {
    gzfile(path, open = "rb")
  } else {
    file(path, open = "rb")
  }
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = file.info(path)$size * 20 + 4096)
}

raw_contains <- function(haystack, needle) {
  target <- as.integer(charToRaw(needle))
  values <- as.integer(haystack)
  if (!length(target) || length(target) > length(values)) return(FALSE)
  any(vapply(seq_len(length(values) - length(target) + 1L), function(i) {
    identical(values[i + seq_along(target) - 1L], target)
  }, logical(1)))
}

test_that("gzip assets preserve supported float64 values losslessly", {
  inputs <- make_toy_cluster_report_inputs()
  values <- as.array(inputs$stat_map)
  values[1] <- pi
  values[2] <- -sqrt(2)
  values[3] <- NA_real_
  volume <- neuroim2::NeuroVol(values, neuroim2::space(inputs$stat_map))
  support <- rep(TRUE, length(volume))
  support[c(4, 5)] <- FALSE
  out <- neuromosaic:::.montage_materialize_volume_asset(
    volume,
    kind = "overlay",
    output_dir = tempfile("volume-assets-"),
    compression = "gzip",
    support_mask = support,
    ref_prefix = "report_files/volumes"
  )
  roundtrip <- neuroim2::read_vol(out$path)
  expected <- as.numeric(volume)
  expected[!support] <- NA_real_

  expect_s3_class(out, "montage_materialized_volume_asset")
  expect_match(out$path, "\\.nii\\.gz$")
  expect_identical(out$asset$datatype, "float64")
  expect_identical(out$asset$hash_scope, "uncompressed")
  expect_identical(as.numeric(roundtrip), expected)
  expect_identical(
    neuroim2:::.source_nifti_header(roundtrip)$data_storage,
    "DOUBLE"
  )
  expect_identical(
    neuromosaic:::.montage_asset_content_hash(out$path, "gzip"),
    out$asset$hash
  )
  expect_true(all(c(
    "canonical_nifti_header", "outside_support_to_nan",
    "storage_float64_lossless"
  ) %in% out$asset$transformations))
  expect_equal(as.numeric(roundtrip)[1:2], c(pi, -sqrt(2)))
  expect_true(all(is.na(as.numeric(roundtrip)[c(3, 4, 5)])))
})

test_that("canonical headers omit source paths and NIfTI extensions", {
  inputs <- make_toy_cluster_report_inputs()
  source_dir <- tempfile("secret-analysis-location-")
  dir.create(source_dir)
  source <- file.path(source_dir, "participant-private-zstat.nii.gz")
  neuroim2::write_vol(inputs$stat_map, source)
  out <- neuromosaic:::.montage_materialize_volume_asset(
    source,
    kind = "overlay",
    output_dir = tempfile("sanitized-assets-"),
    compression = "gzip"
  )
  bytes <- read_asset_raw(out$path, "gzip")

  expect_false(raw_contains(bytes, "participant-private-zstat"))
  expect_false(raw_contains(bytes, source_dir))
  # NIfTI-1 extension flag immediately follows the 348-byte header.
  expect_true(all(as.integer(bytes[349:352]) == 0L))
  expect_false(any(grepl(
    "crop|quant|downsample|float32",
    out$asset$transformations,
    ignore.case = TRUE
  )))
})

test_that("uncompressed diagnostic assets and logical masks round-trip", {
  inputs <- make_toy_cluster_report_inputs()
  mask_values <- rep(c(TRUE, FALSE), length.out = length(inputs$stat_map))
  mask_values[[3L]] <- NA
  mask <- neuroim2::NeuroVol(
    array(mask_values, dim = dim(inputs$stat_map)),
    neuroim2::space(inputs$stat_map)
  )
  output_dir <- tempfile("mask-assets-")
  out <- neuromosaic:::.montage_materialize_volume_asset(
    mask,
    kind = "mask",
    output_dir = output_dir,
    compression = "none"
  )
  duplicate <- neuromosaic:::.montage_materialize_volume_asset(
    mask,
    kind = "mask",
    output_dir = output_dir,
    compression = "none"
  )
  roundtrip <- neuroim2::read_vol(out$path)

  expect_match(out$path, "\\.nii$")
  expect_identical(out$asset$datatype, "uint8")
  expect_identical(out$asset$compression, "none")
  expect_identical(out$path, duplicate$path)
  expect_equal(out$asset$compressed_bytes, out$asset$uncompressed_bytes)
  expect_identical(as.numeric(roundtrip), as.numeric(
    neuromosaic:::.normalize_montage_support_mask(
      mask_values, length(mask_values)
    )
  ))
  expect_identical(
    neuroim2:::.source_nifti_header(roundtrip)$data_storage,
    "UBYTE"
  )
})

test_that("asset bundles materialize one background and deduplicate maps", {
  inputs <- make_toy_cluster_report_inputs()
  manifest <- data.frame(
    analysis_id = "faces",
    map_id = c("faces_z", "faces_copy"),
    role = c("primary", "auxiliary"),
    quantity = c("test_statistic", "mylab:copy"),
    label = c("Z", "Copy"),
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(list(inputs$stat_map, inputs$stat_map))
  bundle <- neuromosaic:::.montage_materialize_volume_assets(
    manifest,
    background = inputs$stat_map,
    output_dir = tempfile("asset-bundle-"),
    compression = "gzip",
    support_masks = list(NULL, NULL),
    ref_prefix = "report_files/volumes"
  )

  expect_s3_class(bundle, "montage_volume_asset_bundle")
  expect_length(bundle$assets, 2L) # one background plus one shared overlay
  expect_length(bundle$files, 2L)
  expect_identical(
    names(bundle$files),
    vapply(bundle$assets, `[[`, character(1), "asset_id")
  )
  expect_identical(
    bundle$maps$faces_z$asset$asset_id,
    bundle$maps$faces_copy$asset$asset_id
  )
  expect_identical(bundle$maps$faces_z$path, bundle$maps$faces_copy$path)
  expect_true(all(file.exists(bundle$files)))
})

test_that("recipe and path sources share the canonical asset path", {
  inputs <- make_toy_cluster_report_inputs()
  source_dir <- tempfile("asset-sources-")
  dir.create(source_dir)
  source_path <- file.path(source_dir, "source.nii.gz")
  neuroim2::write_vol(inputs$stat_map, source_path, data_type = "DOUBLE")
  manifest <- data.frame(
    analysis_id = "faces",
    map_id = c("from_path", "from_recipe"),
    role = c("primary", "auxiliary"),
    quantity = c("test_statistic", "mylab:recipe"),
    label = c("Path", "Recipe"),
    path = c(source_path, NA_character_),
    stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(
    NULL,
    function(row) inputs$stat_map
  ))
  bundle <- neuromosaic:::.montage_materialize_volume_assets(
    manifest,
    background = inputs$stat_map,
    output_dir = tempfile("canonical-sources-"),
    compression = "gzip",
    support_masks = list(NULL, NULL)
  )

  expect_identical(
    bundle$maps$from_path$asset$hash,
    bundle$maps$from_recipe$asset$hash
  )
  expect_identical(bundle$maps$from_path$path, bundle$maps$from_recipe$path)
})

test_that("validation failure writes no scene and preserves unrelated files", {
  inputs <- make_toy_cluster_report_inputs()
  bad_space <- neuroim2::NeuroSpace(
    dim = c(4, 5, 5), spacing = c(2, 2, 2), origin = c(-5, -5, -5)
  )
  bad <- neuroim2::NeuroVol(array(1, dim = c(4, 5, 5)), bad_space)
  manifest <- data.frame(
    analysis_id = "faces",
    map_id = c("faces_z", "bad_map"),
    role = c("primary", "auxiliary"),
    quantity = c("test_statistic", "standard_error"),
    label = c("Z", "Bad"),
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(list(inputs$stat_map, bad))
  output_dir <- tempfile("partial-assets-")
  dir.create(output_dir)
  unrelated <- file.path(output_dir, "keep-me.txt")
  writeLines("untouched", unrelated)

  expect_error(
    neuromosaic:::.montage_materialize_volume_assets(
      manifest,
      background = inputs$stat_map,
      output_dir = output_dir,
      compression = "gzip",
      support_masks = list(NULL, NULL)
    ),
    "analysis_id 'faces'.*map_id 'bad_map'"
  )
  expect_identical(readLines(unrelated), "untouched")
  expect_identical(list.files(output_dir), "keep-me.txt")
})
