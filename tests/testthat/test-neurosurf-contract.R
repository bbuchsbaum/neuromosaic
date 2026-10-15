test_that("the installed surface dependency provides the report contract", {
  exports <- getNamespaceExports("neurosurf")
  expect_true(all(c("render_surface_rgba", "surface_layer", "surface_scene",
                    "surface_scene_manifest", "surfwidget") %in% exports))
  expect_true(all(c("tail", "threshold", "cortex_mask") %in%
                    names(formals(neurosurf::render_surface_rgba))))
  expect_true(all(c("threshold", "legend", "metadata", "provenance") %in%
                    names(formals(neurosurf::surface_layer))))
  runtime <- neuromosaic:::.montage_surface_runtime_info()
  expect_true(nzchar(runtime$sha256))
  source <- paste(readLines(system.file(runtime$source, runtime$script,
                           package = "neurosurf"), warn = FALSE), collapse = "\n")
  expect_true(grepl("mountSurfView", source, fixed = TRUE))
  # Behavioral coverage of the one-sided threshold edge lives in the existing
  # JS runtime and native browser tests; a version number alone is insufficient.
})
