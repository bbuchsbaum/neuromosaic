# Round 4 interactive viewers: a WebGL lifecycle that cannot loop, quiet
# screen-reader output with distinct names per map, one hemisphere rule,
# report file stems for exports, and the one-sided 3D threshold edge.

r4_read_lib <- function(...) {
  lib <- system.file("htmlwidgets", "lib", package = "neuromosaic")
  paste(readLines(file.path(lib, ...), warn = FALSE), collapse = "\n")
}

r4_interactive <- function() {
  list(
    scene_id = "nm-scene",
    scene = list(analyses = list(list(
      analysis_id = "faces", primary_map_id = "m1",
      maps = list(list(map_id = "m1", label = "Faces > average"))
    ))),
    summary = list(packaging = "bundle", compressed_bytes = 2e6, asset_count = 1)
  )
}

test_that("each map's viewer controls carry the map's name, and the readout is not a live region", {
  host <- .montage_interactive_host_html(r4_interactive(), "faces")
  expect_match(host, 'aria-label="Slice view controls: Faces &gt; average"', fixed = TRUE)
  expect_no_match(host, "Interactive map controls", fixed = TRUE)
  readout <- regmatches(host, regexpr("<p class=\"nm-volume-readout\"[^>]*>", host))
  expect_length(readout, 1L)
  expect_no_match(readout, "aria-live", fixed = TRUE)
})

test_that("the surface display panel is an h4 named for its map", {
  html <- as.character(.montage_surface_control_tags(
    c("palette", "threshold", "opacity"), "nm-x-display", label = "Faces > average"
  ))
  expect_match(html, "<h4>\\s*Display")
  expect_no_match(html, "<h3", fixed = TRUE)
  expect_match(html, "nm-sv-visually-hidden\"> — Faces &gt; average", fixed = TRUE)
  expect_match(html, 'aria-label="Surface display settings: Faces &gt; average"', fixed = TRUE)
  # Without a label the panel keeps its generic name.
  plain <- as.character(.montage_surface_control_tags("palette", "nm-y"))
  expect_match(plain, 'aria-label="Surface display settings"', fixed = TRUE)
})

test_that("the viewer cluster list keeps the table's midline hemisphere", {
  peaks <- data.frame(
    cluster_id = c("P1", "N1", "P2"), sign = c("positive", "negative", "positive"),
    peak_x = c(10, 20, 30), peak_y = 1:3, peak_z = 1:3,
    hemisphere = c("left", "midline", NA), region = c("A", "B", NA),
    stringsAsFactors = FALSE
  )
  records <- .montage_volume_scene_clusters(peaks)
  expect_identical(vapply(records, `[[`, "", "hemisphere"), c("left", "midline", ""))
})

test_that("viewer exports use the report's file stem and draw cap triangles", {
  adapter <- r4_read_lib("neuromosaic-volume", "adapter.js")
  bridge <- r4_read_lib("neuromosaic-surface", "bridge.js")
  expect_match(adapter, "[data-nm-file-stem]", fixed = TRUE)
  expect_match(bridge, "[data-nm-file-stem]", fixed = TRUE)
  expect_match(adapter, "lastExportCaps", fixed = TRUE)
  expect_match(bridge, "__nmLastExportCaps", fixed = TRUE)
  # The surface host names its primary map so other maps keep their ids.
  src <- paste(deparse(.montage_surface_host_tag), collapse = "\n")
  expect_match(src, "data-nm-surface-primary-map", fixed = TRUE)
})

test_that("the WebGL lifecycle parks off-screen viewers instead of rebuilding them", {
  adapter <- r4_read_lib("neuromosaic-volume", "adapter.js")
  bridge <- r4_read_lib("neuromosaic-surface", "bridge.js")
  block <- function(js) {
    sub(".*(// <nm-gl-budget>.*// </nm-gl-budget>).*", "\\1", js)
  }
  expect_identical(block(adapter), block(bridge))
  for (js in list(adapter, bridge)) {
    expect_match(js, "IntersectionObserver", fixed = TRUE)
    expect_match(js, "GL.reserve(", fixed = TRUE)
    expect_match(js, "GL.park(", fixed = TRUE)
  }
  # A browser-killed context is rebuilt at most twice in 15 s, then waits.
  expect_match(adapter, "__nmLosses.length > 2", fixed = TRUE)
  expect_match(bridge, "__nmSurfaceLosses.length > 2", fixed = TRUE)
  # The viewer does not announce peak jumps itself (the page does, once).
  expect_no_match(adapter, "live.textContent = \"Slices at \"", fixed = TRUE)
})

test_that("surfview itself draws the one-sided threshold edge the bridge relies on", {
  # surfviewjs 1dfed43 (neurosurf >= 0.1.0.9004) ships the one-sided edge and
  # per-vertex visibility signs; the bridge no longer rewrites the shader.
  bridge <- r4_read_lib("neuromosaic-surface", "bridge.js")
  for (patch in c("SYMMETRIC_EDGE", "ONE_SIDED_EDGE", "onBeforeCompile", "vertexEdges")) {
    expect_false(grepl(patch, bridge, fixed = TRUE), info = patch)
  }
  runtime <- tryCatch(.montage_surface_runtime_info(), error = function(e) NULL)
  skip_if(is.null(runtime), "neurosurf surfview runtime unavailable")
  path <- file.path(system.file(runtime$source, package = "neurosurf"), runtime$script)
  shader <- paste(readLines(path, warn = FALSE), collapse = "\n")
  inside <- regmatches(shader, regexpr("float surfviewInside = [^;]+;", shader))
  expect_length(inside, 1L)
  inside <- gsub("\\\\n|\\s+", " ", inside)
  # No colour on the hidden side, at least half on the shown side.
  expect_match(inside, "vSurfviewEdge >= 0.0", fixed = TRUE)
  expect_match(inside, "max( 0.5, smoothstep( 0.0, surfviewWidth, vSurfviewEdge ) )", fixed = TRUE)
  expect_no_match(inside, "-0.5 * surfviewWidth", fixed = TRUE)
})
