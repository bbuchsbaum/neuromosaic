test_that("nested validation respects loading permission for mixed sources", {
  inputs <- make_toy_cluster_report_inputs()
  directory <- withr::local_tempdir()
  path <- file.path(directory, "stat.nii.gz")
  neuroim2::write_vol(inputs$stat_map, path)
  manifest <- data.frame(
    map_id = c("parcel", "volume", "memory"),
    path = c(NA, path, NA), stat_kind = "z", threshold = 3,
    label = c("Parcels", "Path volume", "Memory volume")
  )
  manifest$parcel_values <- list(c(4.5, -5.5), NULL, NULL)
  manifest$stat_map <- list(NULL, NULL, inputs$stat_map)
  manifest <- build_manifest(manifest, validate = FALSE)
  manifest <- validate_manifest(manifest, load_maps = TRUE, check_files = TRUE)

  expect_error(resolve_montage_policy(manifest), "requires load_maps=TRUE")
  resolved <- resolve_montage_policy(manifest, load_maps = TRUE)
  expect_equal(resolved$effective_threshold, rep(3, 3))
  expect_null(resolved$stat_map[[2]])

  # Caller-supplied FDR/profile values must not lose explicit QC permission.
  resolved <- resolve_montage_policy(
    manifest, load_maps = TRUE,
    stat_maps = list(c(4.5, -5.5), inputs$stat_map, inputs$stat_map)
  )
  expect_equal(resolved$effective_threshold, rep(3, 3))

  labellers <- list(
    NULL,
    function(row) list(title = paste("Labelled", row$map_id)),
    data.frame(map_id = manifest$map_id, label = paste("Table", manifest$map_id))
  )
  image <- file.path(directory, "supplied.png")
  grDevices::png(image, width = 200, height = 200)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Supplied panel")
  grDevices::dev.off()
  panels <- setNames(lapply(manifest$map_id, function(id) {
    list(volume_image = image)
  }), manifest$map_id)

  for (i in seq_along(labellers)) {
    labelled <- apply_montage_labeller(
      manifest, labeller = labellers[[i]], load_maps = TRUE
    )
    expect_identical(labelled$map_id, manifest$map_id)
    output <- file.path(directory, paste0("report-", i, ".qmd"))
    expect_no_error(render_montage_report(
      manifest, output_file = output, labeller = labellers[[i]],
      panels = panels, render_volume = FALSE, render_surface = FALSE,
      load_maps = TRUE
    ))
    rd <- readRDS(sub("\\.qmd$", "_report-data.rds", output))
    expect_identical(rd$manifest$label, labelled$label)
    expect_length(rd$panels, 3)
    for (panel in rd$panels) {
      expect_true(file.exists(file.path(directory, panel$volume_image)))
    }
  }

  # Loading remains real QC, even when the report only uses supplied images.
  empty <- inputs$stat_map
  empty[] <- 0
  neuroim2::write_vol(empty, path)
  expect_error(
    resolve_montage_policy(manifest, load_maps = TRUE),
    "No finite suprathreshold voxels for map_id 'volume'"
  )
  expect_error(
    render_montage_report(
      manifest, output_file = file.path(directory, "empty.qmd"),
      panels = panels, render_volume = FALSE, render_surface = FALSE,
      load_maps = TRUE, empty = "error"
    ),
    "No finite suprathreshold voxels for map_id 'volume'"
  )
  expect_warning(
    resolve_montage_policy(manifest, load_maps = TRUE, empty = "warning"),
    "No finite suprathreshold voxels"
  )
  unlink(path)
  expect_error(resolve_montage_policy(manifest, load_maps = TRUE))
})
