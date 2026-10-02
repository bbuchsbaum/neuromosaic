# Round 3 interactive viewers: medial-wall mask on the 3D surface, the
# lateral default view, reader-facing copy, and the browser lifecycle code
# that keeps viewers alive (WebGL budget, file:// handling, export).

r3_toy_geometry <- function(hemi, offset = 0) {
  vertices <- matrix(
    c(0 + offset, 0, 0, 1 + offset, 0, 0, 0 + offset, 1, 0, 1 + offset, 1, 0),
    ncol = 3, byrow = TRUE
  )
  faces <- matrix(c(0, 1, 2, 1, 3, 2), ncol = 3, byrow = TRUE)
  neurosurf::SurfaceGeometry(vertices, faces, hemi = hemi)
}

test_that("the cortex mask helper reads the atlas's lh/rh medial-wall domain", {
  atlas <- list(cortex_mask = list(lh = c(TRUE, FALSE, TRUE, TRUE),
                                   rh = c(TRUE, TRUE, TRUE, FALSE)))
  expect_identical(.montage_surface_cortex_mask(atlas, "lh", "left", 4L),
                   c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(.montage_surface_cortex_mask(atlas, "rh", "right", 4L),
                   c(TRUE, TRUE, TRUE, FALSE))
  # Absent, wrong-length or incomplete masks are ignored (no masking).
  expect_null(.montage_surface_cortex_mask(list(), "lh", "left", 4L))
  expect_null(.montage_surface_cortex_mask(atlas, "lh", "left", 5L))
  bad <- list(cortex_mask = list(lh = c(TRUE, NA, TRUE, TRUE)))
  expect_null(.montage_surface_cortex_mask(bad, "lh", "left", 4L))
  named <- list(cortex_mask = list(left = c(FALSE, TRUE, TRUE, TRUE)))
  expect_identical(.montage_surface_cortex_mask(named, "lh", "left", 4L),
                   c(FALSE, TRUE, TRUE, TRUE))
})

test_that("projected surface values leave the medial wall empty, as the static figure does", {
  geometry <- list(left = r3_toy_geometry("lh"), right = r3_toy_geometry("rh", 3))
  atlas <- list(cortex_mask = list(lh = c(TRUE, FALSE, TRUE, TRUE),
                                   rh = c(FALSE, TRUE, TRUE, TRUE)))
  stat <- neuroim2::NeuroVol(array(7, dim = c(4, 4, 4)),
                             neuroim2::NeuroSpace(c(4, 4, 4)))
  local_mocked_bindings(
    .resolve_overlay_surface_pair = function(surfatlas, hemi, ...) {
      g <- if (identical(hemi, "lh")) geometry$left else geometry$right
      list(white = g, pial = g)
    },
    .montage_repair_surface_geometry = function(geometry) geometry
  )
  # Every vertex samples a suprathreshold value, as medial-wall vertices do
  # when the ribbon passes through caudate or thalamus.
  local_mocked_bindings(
    vol_to_surf = function(surf_wm, ...) {
      n <- nrow(neurosurf::coords(surf_wm))
      neurosurf::NeuroSurface(surf_wm, indices = seq_len(n), data = rep(7, n))
    },
    .package = "neurosurf"
  )
  out <- .montage_surface_project_volume(
    stat = stat, support_mask = NULL, display_mode = "thresholded",
    tail = "two_sided", surfatlas = atlas, geometry = geometry,
    config = list(), map_id = "toy"
  )
  expect_identical(is.na(out$values$left), c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(is.na(out$values$right), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(out$values$left[!is.na(out$values$left)], c(7, 7, 7))
  # Without a declared mask every vertex keeps its value.
  bare <- .montage_surface_project_volume(
    stat = stat, support_mask = NULL, display_mode = "thresholded",
    tail = "two_sided", surfatlas = list(), geometry = geometry,
    config = list(), map_id = "toy"
  )
  expect_false(anyNA(unlist(bare$values)))
})

test_that("the 3D surface opens on the static strip's first view, lateral left", {
  expect_identical(montage_surface()$view, "left")
  expect_identical(montage_surface(view = "oblique")$view, "oblique")
  expect_identical(montage_surface(view = "right-medial")$view, "right-medial")
  expect_error(montage_surface(view = "sideways"))
})

test_that("viewer copy never points at a hidden montage or gives developer advice", {
  surface_src <- paste(deparse(.montage_surface_host_tag), collapse = "\n")
  expect_match(surface_src, "The Static view is the figure of record", fixed = TRUE)
  expect_no_match(surface_src, "static montage above is the figure of record", fixed = TRUE)
  lib <- system.file("htmlwidgets", "lib", package = "neuromosaic")
  adapter <- paste(readLines(file.path(lib, "neuromosaic-volume", "adapter.js"),
                             warn = FALSE), collapse = "\n")
  bridge <- paste(readLines(file.path(lib, "neuromosaic-surface", "bridge.js"),
                            warn = FALSE), collapse = "\n")
  for (js in list(adapter, bridge)) {
    expect_no_match(js, "montage_interactive(assets", fixed = TRUE)
    expect_no_match(js, "static panel remains authoritative", fixed = TRUE)
    expect_match(js, "Interactive views need the report served over HTTP", fixed = TRUE)
  }
})

test_that("the viewers share one WebGL budget and release contexts explicitly", {
  lib <- system.file("htmlwidgets", "lib", package = "neuromosaic")
  read <- function(...) paste(readLines(file.path(lib, ...), warn = FALSE),
                              collapse = "\n")
  adapter <- read("neuromosaic-volume", "adapter.js")
  bridge <- read("neuromosaic-surface", "bridge.js")
  for (js in list(adapter, bridge)) {
    expect_match(js, "NeuroMosaicGL", fixed = TRUE)
    expect_match(js, "WEBGL_lose_context", fixed = TRUE)
    expect_match(js, "webglcontextlost", fixed = TRUE)
  }
  # Slices cost three contexts, a 3D surface one.
  expect_match(adapter, "cost: 3", fixed = TRUE)
  expect_match(bridge, "cost: 1", fixed = TRUE)
  # Any viewer failure hands the map back to the router's Static view.
  expect_match(adapter, "nm-viewer-failed", fixed = TRUE)
  expect_match(bridge, "nm-viewer-failed", fixed = TRUE)
  # Hover picks only drawn surfaces; exports use the light report ground.
  expect_match(bridge, "pickVisible", fixed = TRUE)
  expect_match(bridge, "transparent: false", fixed = TRUE)
  expect_match(adapter, "Save view as PNG", fixed = TRUE)
})

test_that("the volume runtime manifest still describes the shipped files", {
  dir <- system.file("htmlwidgets", "lib", "neuromosaic-volume", package = "neuromosaic")
  manifest <- jsonlite::read_json(file.path(dir, "runtime.json"))
  for (file in manifest$files) {
    path <- file.path(dir, file$path)
    expect_identical(as.numeric(file.size(path)), as.numeric(file$bytes),
                     info = file$path)
    expect_identical(digest::digest(path, algo = "sha256", file = TRUE),
                     file$sha256, info = file$path)
  }
})
