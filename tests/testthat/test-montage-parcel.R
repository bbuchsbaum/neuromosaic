test_that("parcel_render_manifest builds grouped volume and surface sources", {
  inputs <- make_toy_cluster_report_inputs()
  results <- tibble::tibble(
    id = c(2L, 1L),
    z_stat = c(-5.5, 4.5),
    beta = c(-0.4, 0.8),
    standard_error = c(0.3, 0.2),
    reliability = c(0.6, 0.7)
  )

  manifest <- parcel_render_manifest(
    results,
    inputs$atlas,
    analysis_id = "faces"
  )

  expect_identical(
    manifest$parcel_metric,
    c("z_stat", "beta", "standard_error", "reliability")
  )
  expect_identical(
    manifest$quantity,
    c(
      "test_statistic", "estimate", "standard_error",
      "parcel:reliability"
    )
  )
  expect_identical(manifest$distribution, c("z", NA, NA, NA))
  expect_identical(manifest$role, c("primary", rep("auxiliary", 3L)))
  expect_identical(
    manifest$map_id,
    c("faces_z_stat", "faces_beta", "faces_standard_error",
      "faces_reliability")
  )

  expect_equal(manifest$parcel_values[[1]], c(`1` = 4.5, `2` = -5.5))
  expect_length(manifest$stat_map, 4L)
  expect_true(all(vapply(
    manifest$stat_map,
    methods::is,
    logical(1),
    class2 = "NeuroVol"
  )))
  expect_equal(
    neuroim2::space(manifest$stat_map[[1]]),
    neuroim2::space(inputs$atlas$atlas)
  )

  atlas_values <- as.numeric(inputs$atlas$atlas)
  z_values <- as.numeric(manifest$stat_map[[1]])
  expect_true(all(is.na(z_values[atlas_values == 0L])))
  expect_true(all(z_values[atlas_values == 1L] == 4.5))
  expect_true(all(z_values[atlas_values == 2L] == -5.5))
})

test_that("parcel_render_manifest accepts named semantic overrides", {
  surface_atlas <- make_toy_surfatlas()
  results <- tibble::tibble(
    roi_index = c(2L, 1L),
    se = c(0.3, 0.2),
    stability = c(-0.1, 0.8)
  )

  manifest <- parcel_render_manifest(
    results,
    surface_atlas,
    metrics = c("se", "stability"),
    by = c(id = "roi_index"),
    primary = "stability",
    quantity = c(se = "standard_error", stability = "mylab:stability"),
    labels = c(se = "Uncertainty", stability = "Bootstrap stability"),
    units = c(se = "SE", stability = "correlation"),
    analysis_id = "bootstrap"
  )

  expect_false("stat_map" %in% names(manifest))
  expect_identical(manifest$role, c("auxiliary", "primary"))
  expect_identical(
    manifest$quantity,
    c("standard_error", "mylab:stability")
  )
  expect_identical(manifest$label, c("Uncertainty", "Bootstrap stability"))
  expect_equal(manifest$parcel_values[[2]], c(`1` = 0.8, `2` = -0.1))
})

test_that("parcel metrics do not standardize FSL-specific names", {
  inputs <- make_toy_cluster_report_inputs()
  results <- tibble::tibble(
    id = 1:2,
    cope = c(0.8, -0.4),
    varcope = c(0.04, 0.09)
  )

  manifest <- parcel_render_manifest(
    results,
    inputs$atlas,
    metrics = c("cope", "varcope"),
    include_volume = FALSE
  )

  expect_identical(manifest$quantity, c("parcel:cope", "parcel:varcope"))
  expect_identical(manifest$role, c("primary", "auxiliary"))
})

test_that("parcel_render_manifest keeps partial coverage explicit", {
  inputs <- make_toy_cluster_report_inputs()
  results <- tibble::tibble(id = 1L, z_stat = 4.5)

  expect_error(
    parcel_render_manifest(results, inputs$atlas, metrics = "z_stat"),
    "missing 1 atlas parcel",
    class = "neuroatlas_error_missing_parcel_key"
  )

  manifest <- parcel_render_manifest(
    results,
    inputs$atlas,
    metrics = "z_stat",
    allow_partial = TRUE
  )
  expect_equal(manifest$parcel_values[[1]], c(`1` = 4.5, `2` = NA_real_))

  atlas_values <- as.numeric(inputs$atlas$atlas)
  map_values <- as.numeric(manifest$stat_map[[1]])
  expect_true(all(is.na(map_values[atlas_values == 2L])))
})

test_that("parcel_render_manifest validates metric and atlas contracts", {
  inputs <- make_toy_cluster_report_inputs()
  results <- tibble::tibble(id = 1:2, t_stat = c(3, -4))

  expect_error(
    parcel_render_manifest(results, inputs$atlas, metrics = "missing"),
    "not found"
  )
  expect_error(
    parcel_render_manifest(results, inputs$atlas, metrics = "t_stat"),
    "require 'df'"
  )

  surface_atlas <- inputs$atlas
  class(surface_atlas) <- c("toy_surface", "surfatlas", "atlas")
  expect_error(
    parcel_render_manifest(
      results,
      surface_atlas,
      metrics = "t_stat",
      df = 20,
      include_volume = TRUE
    ),
    "requires a volumetric atlas"
  )
})

test_that("parcel manifests use volume and surface renderer channels", {
  inputs <- make_toy_cluster_report_inputs()
  results <- tibble::tibble(
    id = c(2L, 1L),
    z_stat = c(-5.5, 4.5),
    beta = c(-0.4, 0.8)
  )
  manifest <- parcel_render_manifest(
    results,
    inputs$atlas,
    metrics = c("z_stat", "beta"),
    analysis_id = "faces"
  )

  expect_s4_class(
    neuromosaic:::.montage_manifest_stat_source(manifest, 1L),
    "NeuroVol"
  )
  surface <- neuromosaic:::.montage_manifest_surface_source_args(manifest, 1L)
  expect_equal(surface$vals, manifest$parcel_values[[1]])
  expect_null(surface$stat)
})

test_that("parcel manifests render through the grouped report path", {
  inputs <- make_toy_cluster_report_inputs()
  manifest <- parcel_render_manifest(
    tibble::tibble(
      id = c(2L, 1L),
      z_stat = c(-5.5, 4.5),
      beta = c(-0.4, 0.8)
    ),
    inputs$atlas,
    metrics = c("z_stat", "beta"),
    analysis_id = "faces"
  )
  output_dir <- tempfile("parcel-montage-report-")
  dir.create(output_dir)
  output <- file.path(output_dir, "parcel-report.qmd")

  suppressWarnings(render_montage_report(
    manifest,
    output_file = output,
    bg = inputs$stat_map,
    atlas = inputs$atlas,
    interactive = montage_interactive(
      assets = "embed",
      compression = "gzip",
      max_embed_mb = 1
    ),
    image_width = 500,
    image_height = 400,
    image_res = 72,
    quiet = TRUE
  ))

  report_data <- readRDS(file.path(
    output_dir,
    "parcel-report_report-data.rds"
  ))
  expect_identical(report_data$groups[[1]]$primary_map_id, "faces_z_stat")
  expect_equal(report_data$panels$faces_z_stat$peaks$n_clusters, 2L)
  expect_match(
    report_data$panels$faces_beta$peaks$note,
    "not generated for continuous"
  )
  expect_true(all(vapply(
    report_data$panels,
    function(panel) file.exists(file.path(output_dir, panel$volume_image)),
    logical(1)
  )))
  expect_identical(
    vapply(
      report_data$interactive$scene$analyses[[1]]$maps,
      `[[`,
      character(1),
      "map_id"
    ),
    c("faces_z_stat", "faces_beta")
  )
  expect_false(any(c("stat_map", "parcel_values") %in%
                   names(report_data$manifest)))
})
