# TemplateFlow's tpl-fsaverage selects surfaces by density (den-10k, den-41k)
# and only offers resolution 1. The defaults used to ask for resolution "05"/"06",
# which neuroatlas rejects, so fsaverage5 surfaces silently came back NULL
# (issue #44).

clear_overlay_geom_cache <- function() {
  cache <- neuromosaic:::.overlay_geom_cache
  rm(list = ls(cache), envir = cache)
}

test_that("fsaverage spaces are selected by density, without a resolution", {
  defaults <- neuromosaic:::.surface_template_defaults
  expect_null(defaults("fsaverage5")$resolution)
  expect_equal(defaults("fsaverage5")$density, "10k")
  expect_null(defaults("fsaverage6")$resolution)
  expect_equal(defaults("fsaverage6")$density, "41k")
  expect_null(defaults(NULL)$resolution)
  expect_null(defaults("")$resolution)
  expect_equal(defaults("")$density, "41k")
})

test_that("the fsaverage5 loader asks TemplateFlow for density 10k only", {
  clear_overlay_geom_cache()
  withr::defer(clear_overlay_geom_cache())
  seen <- NULL
  local_mocked_bindings(
    load_surface_template = function(template_id, surface_type, hemi,
                                     density = NULL, resolution = NULL, ...) {
      seen <<- list(template_id = template_id, surface_type = surface_type,
                    hemi = hemi, density = density, resolution = resolution)
      "a surface"
    },
    .package = "neuroatlas"
  )

  out <- neuromosaic:::.load_overlay_surface_geometry("fsaverage5", "white", "lh")

  expect_identical(out, "a surface")
  expect_equal(seen$template_id, "fsaverage")
  expect_equal(seen$density, "10k")
  expect_null(seen$resolution)
  expect_equal(seen$hemi, "L")
})

test_that("a surface that no route can load warns with the reasons", {
  clear_overlay_geom_cache()
  withr::defer(clear_overlay_geom_cache())
  local_mocked_bindings(
    load_surface_template = function(...) stop("no such density"),
    get_surface_template = function(...) stop("download failed"),
    .package = "neuroatlas"
  )

  expect_warning(
    out <- neuromosaic:::.load_overlay_surface_geometry("fsaverage5", "pial", "rh"),
    "fsaverage5 pial surface \\(rh\\).*load_surface_template: no such density.*get_surface_template: download failed"
  )
  expect_null(out)
})

test_that("the TemplateFlow fsaverage5 white surface loads with 10,242 vertices", {
  skip_on_cran()
  skip_if_offline()
  clear_overlay_geom_cache()
  withr::defer(clear_overlay_geom_cache())

  geom <- neuromosaic:::.load_overlay_surface_geometry("fsaverage5", "white", "lh")

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(neurosurf::coords(geom)), 10242L)
})
