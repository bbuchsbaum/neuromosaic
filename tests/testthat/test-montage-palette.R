test_that("single-hue names resolve to shared light-to-dark ordered tables", {
  endpoints <- list(
    Greens = c("#F7FCF5", "#00441B"),
    Blues = c("#F7FBFF", "#08306B"),
    Reds = c("#FFF5F0", "#67000D")
  )
  for (name in names(endpoints)) {
    colors <- .montage_palette_colors(name)
    expect_length(colors, 256L)
    expect_identical(colors[c(1L, 256L)], endpoints[[name]])
    expect_identical(.montage_palette_colors(paste0(" ", tolower(name), " ")),
                     colors)
    expect_identical(.montage_surface_palette(name, "sequential"), colors)
    expect_identical(.montage_surface_browser_palette(name, "sequential"), colors)
    rgb <- .nm_value_rgb(c(0, 10), name, c(0, 10))
    expect_equal(rgb, unname(t(grDevices::col2rgb(endpoints[[name]])) / 255))
  }
  expect_identical(.montage_surface_palette("diverging", "diverging"), "nm-signed")
  expect_identical(.montage_volume_palette("diverging", "diverging"), "nm-signed")
  expect_null(.montage_palette_colors("vik"))
  expect_warning(.montage_surface_browser_palette("not-a-palette", "sequential"),
                 "no browser preset")
})

test_that("explicit tables survive static volume, metadata and scene serialization", {
  inputs <- make_toy_cluster_report_inputs()
  row <- data.frame(analysis_id = "colors", map_id = "colors", quantity = "estimate",
                    label = "Colors", effective_scale = "sequential",
                    effective_support = "analysis", effective_units = NA_character_)
  palettes <- list("Greens", "Blues", "Reds", c("#00112240", "white", "#CCDDEE"))
  for (palette in palettes) {
    out <- stat_montage(inputs$stat_map, inputs$stat_map, threshold = NULL,
                        signed = FALSE, limits = c(0, 6), ov_cmap = palette,
                        draw = FALSE)
    expected <- .montage_palette_colors(palette)
    expect_identical(out$display_spec$palette_value, expected)
    expect_identical(out$plot$parts$render$palette, expected)
    metadata <- .montage_volume_display_metadata(out$display_spec, row,
                                                 rep(TRUE, length(inputs$stat_map)))
    expect_identical(metadata$palette_colors, expected)
    scene <- .montage_volume_scene_map(row, metadata, "asset")
    expect_identical(scene$display$palette_colors, expected)
    expect_no_error(.montage_scene_validate_display(scene$display, "display"))
    invalid <- scene$display
    invalid$palette_colors <- c("red", "blue")
    expect_error(.montage_scene_validate_display(invalid, "display"), "#RRGGBB")
    json <- jsonlite::fromJSON(jsonlite::toJSON(scene, auto_unbox = TRUE, null = "null"))
    expect_identical(json$display$palette_colors, expected)
  }
  # A short vector is a lookup table, not a promise of 256 entries.
  colors <- c("#001122", "#334455", "#CCDDEE")
  expect_equal(.nm_value_rgb(c(-1, 0, 3, 6, 9), colors, c(0, 6)),
               unname(t(grDevices::col2rgb(colors[c(1, 1, 2, 3, 3)])) / 255))
})

test_that("static surfaces receive the same explicit palette as browser surfaces", {
  inputs <- make_toy_cluster_report_inputs()
  captured <- NULL
  draw <- function(...) {
    captured <<- list(...)
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }
  for (palette in list("Greens", "Blues", "Reds", c("#00112240", "#CCDDEE"))) {
    result <- surf_montage(stat = inputs$stat_map, surfatlas = make_toy_surfatlas(),
      output_file = tempfile(fileext = ".png"), threshold = 3, cap = 6,
      signed = FALSE, tail = "positive", overlay_palette = palette,
      plot_fun = draw, width = 100, height = 100, res = 72)
    colors <- .montage_palette_colors(palette)
    expect_identical(captured$overlay_palette, colors)
    expect_identical(result$render$overlay_palette, colors)
    expect_identical(.montage_surface_browser_palette(palette, "sequential"), colors)
  }
})

test_that("invalid explicit tables fail before rendering or scene export", {
  for (colors in list(c("red", NA_character_), c("red", "bogus"), c("", "blue"))) {
    expect_error(.montage_palette_colors(colors), "RGB/RGBA")
  }
  for (colors in list(character(), "#FFFFFF", 1:3)) {
    expect_error(.montage_validate_palette_colors(colors), "at least two")
  }
  expect_identical(.montage_palette_colors(c("red", "#00112280")),
                   c("#FF0000", "#00112280"))
})

test_that("surface scenes retain ordered tables even with a heat preset", {
  row <- data.frame(analysis_id = "colors", map_id = "colors", quantity = "estimate",
    label = "Colors", effective_scale = "sequential", effective_lower = 0,
    effective_upper = 6, effective_threshold = 3, effective_display_mode = "thresholded",
    effective_tail = "positive", effective_units = NA_character_)
  for (palette in list("Greens", "Blues", "Reds", c("#00112240", "#CCDDEE"))) {
    row$effective_palette_family <- if (length(palette) == 1L) palette else "sequential"
    layer <- .montage_surface_scene_layer(row, list(surface = list(palette = palette)),
      values = list(left = c(0, 3, 6), right = c(1, 4, 5)),
      provenance = list(), heat = TRUE)
    expect_identical(layer$colormap, .montage_palette_colors(palette))
  }
  schema <- montage_volume_scene_schema()
  field <- schema[schema$object == "display" & schema$field == "palette_colors", ]
  expect_equal(nrow(field), 1L)
  expect_false(field$required)
})
