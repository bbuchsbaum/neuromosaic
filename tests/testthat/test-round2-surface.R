# Round 2: the surfview runtime is a lazily loaded attachment, and each layer
# records its data range for the browser key's cap triangles and threshold
# clamping.

r2_surface_fixture <- function(output_dir) {
  geometry <- function(hemi, offset = 0) {
    vertices <- matrix(
      c(0 + offset, 0, 0, 1 + offset, 0, 0, 0 + offset, 1, 0, 1 + offset, 1, 0),
      ncol = 3, byrow = TRUE
    )
    faces <- matrix(c(0, 1, 2, 1, 3, 2), ncol = 3, byrow = TRUE)
    neurosurf::SurfaceGeometry(vertices, faces, hemi = hemi)
  }
  labeled <- function(geom) {
    methods::new(
      "LabeledNeuroSurface",
      geometry = geom, indices = as.integer(seq_len(4L)),
      data = c(1, 1, 2, 2), labels = c("Region A", "Region B"),
      cols = c("#336699", "#CC6633")
    )
  }
  atlas <- list(
    ids = c(1L, 2L), labels = c("Region A", "Region B"),
    hemi = c("left", "right"),
    lh_atlas = labeled(geometry("lh")), rh_atlas = labeled(geometry("rh", 3)),
    surf_type = "white", surface_space = "toy"
  )
  class(atlas) <- c("toy_surface", "surfatlas", "atlas")
  manifest <- data.frame(
    map_id = "faces_z", analysis_id = "faces", analysis_label = "Faces",
    role = "primary", quantity = "test_statistic", distribution = "z",
    units = "z", signed = TRUE, threshold = 3, tail = "two_sided",
    selector_label = "Z statistic", display_order = 1,
    label = "Faces Z statistic", stringsAsFactors = FALSE
  )
  manifest$parcel_values <- I(list(c(`1` = 4.5, `2` = -5)))
  image <- file.path(output_dir, "static-surface.png")
  writeBin(charToRaw("static surface fallback"), image)
  list(
    atlas = atlas, manifest = manifest,
    panels = list(faces_z = list(surface_image = image))
  )
}

test_that("surface layers record their finite data range", {
  output_dir <- tempfile("r2-surface-range-")
  dir.create(output_dir)
  fx <- r2_surface_fixture(output_dir)
  suppressMessages(render_montage_report(
    fx$manifest,
    output_file = file.path(output_dir, "report.qmd"),
    surfatlas = fx$atlas, panels = fx$panels,
    render_surface = FALSE, materialize_recipes = FALSE, check_files = FALSE,
    surface = montage_surface(assets = "embed", compression = "none")
  ))
  rd <- readRDS(file.path(output_dir, "report_report-data.rds"))
  group <- rd$surface$scenes[["faces"]]
  layer <- group$manifest$layers[[unname(group$map_to_layer[["faces_z"]])]]
  expect_equal(layer$metadata$data_range, c(-5, 4.5))
  expect_null(.montage_surface_value_range(list(left = c(NA, NaN, Inf))))
  expect_equal(
    .montage_surface_value_range(list(left = c(-1, NA), right = c(2, Inf))),
    c(-1, 2)
  )
})

test_that("the surfview runtime is an attachment the bridge loads on demand", {
  output_dir <- tempfile("r2-surface-deps-")
  dir.create(output_dir)
  fx <- r2_surface_fixture(output_dir)
  suppressMessages(render_montage_report(
    fx$manifest,
    output_file = file.path(output_dir, "report.qmd"),
    surfatlas = fx$atlas, panels = fx$panels,
    render_surface = FALSE, materialize_recipes = FALSE, check_files = FALSE,
    surface = montage_surface()
  ))
  rd <- readRDS(file.path(output_dir, "report_report-data.rds"))
  dependency <- .montage_surface_runtime_dependency(rd$surface$runtime)
  expect_null(dependency$script)
  expect_identical(unname(dependency$attachment), rd$surface$runtime$script)
  expect_identical(names(dependency$attachment), "runtime")
  # The bridge looks the runtime up by this id; htmltools derives it from the
  # dependency name and the attachment name.
  rendered <- htmltools::renderDependencies(list(dependency), "file")
  expect_match(rendered, "id=\"surfview-runtime-attachment\"", fixed = TRUE)
  expect_no_match(rendered, "<script", fixed = TRUE)
  bridge <- .montage_surface_bridge_assets()$js
  expect_match(bridge, "surfview-runtime-attachment", fixed = TRUE)
  expect_match(bridge, "nm-view-change", fixed = TRUE)
  # A runtime that no longer matches the recorded hash is refused.
  tampered <- rd$surface$runtime
  tampered$sha256 <- strrep("0", 64)
  expect_error(
    .montage_surface_runtime_dependency(tampered),
    "does not match the report's recorded runtime hash"
  )
})

test_that("bundled and self-contained HTML both carry the runtime by link only", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for render tests")
  for (assets in c("bundle", "embed")) {
    output_dir <- tempfile(paste0("r2-surface-html-", assets, "-"))
    dir.create(output_dir)
    fx <- r2_surface_fixture(output_dir)
    output <- file.path(output_dir, "report.html")
    suppressMessages(render_montage_report(
      fx$manifest,
      output_file = output,
      surfatlas = fx$atlas, panels = fx$panels,
      render_surface = FALSE, materialize_recipes = FALSE, check_files = FALSE,
      surface = montage_surface(assets = assets), quiet = TRUE
    ))
    html <- paste(readLines(output, warn = FALSE), collapse = "\n")
    expect_no_match(html, "<script[^>]+surfview[^>]*src|<script[^>]+src=[^>]+surfview")
    link <- regmatches(html, regexpr(
      "<link[^>]+id=\"surfview-runtime-attachment\"[^>]*>", html
    ))
    expect_length(link, 1L)
    href <- sub(".*href=\"([^\"]+)\".*", "\\1", link)
    if (identical(assets, "bundle")) {
      path <- file.path(output_dir, utils::URLdecode(href))
      expect_true(file.exists(path))
      expect_identical(
        digest::digest(file = path, algo = "sha256", serialize = FALSE),
        .montage_surface_runtime_info()$sha256
      )
    } else {
      expect_match(href, "^data:")
    }
  }
})
