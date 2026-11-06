test_that("unknown quantities cannot silently discard explicit threshold settings", {
  for (field in c("threshold", "p", "q")) {
    maps <- data.frame(
      map_id = "faces", path = "z.nii.gz", label = "Faces",
      quantity = "z", distribution = "z", signed = TRUE
    )
    maps[[field]] <- if (field == "threshold") 3.1 else 0.05
    manifest <- build_manifest(maps, check_files = FALSE)

    expect_error(
      resolve_montage_profiles(manifest, map_values = list(c(-10, -2, 0, 2, 10))),
      "Quantity 'z'.*faces.*test_statistic",
      info = field
    )
  }
})

test_that("unknown unthresholded quantities retain automatic continuous display", {
  maps <- data.frame(
    map_id = "reliability", path = "reliability.nii.gz", label = "Reliability",
    quantity = "mylab:reliability", threshold = NA_real_, p = NA_real_, q = NA_real_
  )
  out <- resolve_montage_profiles(maps, map_values = list(c(0.1, 0.2, 0.9)))
  expect_identical(out$effective_display_mode, "continuous")
})

test_that("report rendering rejects an ambiguous threshold before writing HTML", {
  inputs <- make_toy_cluster_explorer_inputs()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, path)
  maps <- build_manifest(data.frame(
    map_id = "faces", path = path, label = "Faces",
    quantity = "z", distribution = "z", signed = TRUE,
    threshold = 3.1, tail = "two_sided"
  ))
  output <- file.path(dir, "report.html")
  expect_error(
    render_montage_report(maps, output, bg = inputs$atlas$atlas, assets = "embed"),
    "Quantity 'z'.*faces.*test_statistic"
  )
  expect_false(file.exists(output))
})

test_that("explicit custom display semantics and built-in statistics remain usable", {
  maps <- data.frame(
    map_id = "faces", path = "z.nii.gz", label = "Faces",
    quantity = "z", distribution = "z", signed = TRUE, threshold = 3.1
  )
  maps$display_mode <- "thresholded"
  out <- resolve_montage_profiles(maps)
  expect_identical(out$effective_display_mode, "thresholded")

  maps$display_mode <- NULL
  profile <- montage_map_profile("z", display_mode = "thresholded")
  out <- resolve_montage_profiles(maps, profiles = profile)
  expect_identical(out$effective_display_mode, "thresholded")

  partial <- montage_map_profile("z", label = "Custom statistic")
  expect_error(resolve_montage_profiles(maps, profiles = partial),
               "no default display mode")

  maps$display_mode <- "continuous"
  out <- resolve_montage_profiles(maps)
  expect_identical(out$effective_display_mode, "continuous")
  maps$display_mode <- NULL

  maps$quantity <- "test_statistic"
  out <- resolve_montage_profiles(maps)
  expect_identical(out$effective_display_mode, "thresholded")

  maps$quantity <- "estimate"
  maps$distribution <- NA_character_
  out <- resolve_montage_profiles(maps)
  expect_identical(out$effective_display_mode, "continuous")
})
