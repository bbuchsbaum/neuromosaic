test_that("surface strips preserve portable device and fonts in every variant", {
  parts <- list(
    panels = lapply(list(c("left", "lateral"), c("left", "medial"),
                        c("right", "medial"), c("right", "lateral")), function(x) {
      list(raster = matrix("#A0A0A0", 40, 80), hemi = x[[1]], view = x[[2]],
           anterior = .nm_surface_anterior_side(x[[1]], x[[2]]))
    }),
    key = list(palette = "inferno", limits = c(0, 100), threshold = 10,
               signed = FALSE, tail = "positive", continuous = FALSE,
               over_low = FALSE, over_high = FALSE,
               title = "Participants exceeding cutoff (%)"),
    render_device = "png", plain_fonts = TRUE
  )
  env <- .nm_font_env
  old <- env$force_plain
  on.exit(env$force_plain <- old, add = TRUE)
  env$force_plain <- FALSE
  text_grobs <- function(g) {
    c(if (inherits(g, "text")) list(g),
      unlist(lapply(g$children, text_grobs), recursive = FALSE))
  }
  directory <- withr::local_tempdir()
  local_mocked_bindings(.nm_surface_strip_parts = function(...) parts)
  local_mocked_bindings(plot_brain = function(...) ggplot2::ggplot(),
                        .package = "neuroatlas")
  inputs <- make_toy_cluster_report_inputs()
  env$force_plain <- TRUE
  primary <- surf_montage(
    stat = inputs$stat_map, surfatlas = make_toy_surfatlas(), threshold = 3,
    output_file = file.path(directory, "primary.png"), render_device = "png"
  )
  expect_true(env$force_plain)
  expect_true(primary$strip$plain_fonts)
  expect_identical(primary$strip$render_device, "png")
  expect_true(primary$render$plain_fonts)
  variants <- .montage_write_surface_variants(primary$strip, primary$image)
  expect_length(variants, 3)
  expect_true(env$force_plain)
  env$force_plain <- FALSE
  for (theme in c("light", "dark")) for (variant in c("wide", "narrow")) {
    path <- file.path(directory, paste0(theme, "-", variant, ".png"))
    spec <- .nm_write_surface_strip(parts, path, theme, variant)
    expect_identical(spec$device, "png")
    expect_false(env$force_plain)
    text <- text_grobs(spec$grob)
    expect_true(all(vapply(text, function(g) identical(g$gp$fontfamily, "sans"), logical(1))))
    labels <- vapply(text, function(g) paste(g$label, collapse = " "), character(1))
    expect_true(all(c("Left lateral", "Left medial", "Right lateral", "Right medial") %in% labels))
    expect_true(any(grepl("Participants exceeding cutoff", gsub("\n", " ", labels), fixed = TRUE)))
    expect_true(any(grepl("(%)", labels, fixed = TRUE)))
    expect_gt(file.info(path)$size, 1000)
  }
  expect_error(.nm_write_surface_strip(parts, file.path(directory, "missing", "x.png")))
  expect_false(env$force_plain)
})

test_that("device font selection respects and restores an active plain setting", {
  skip_if_not_installed("ragg")
  env <- .nm_font_env
  old <- env$force_plain
  on.exit(env$force_plain <- old, add = TRUE)
  env$force_plain <- TRUE
  path <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(.nm_with_device_fonts(.nm_plain_fonts()))
  expect_true(env$force_plain)
})
