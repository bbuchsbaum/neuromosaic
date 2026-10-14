# Ordered colour tables shared by static and interactive renderers. These
# ColorBrewer sequential ramps run from low (light) to high (dark), unlike
# the same-named HCL palettes. Resolve them before handing off to a renderer.
.montage_palette_colors <- function(palette) {
  if (!is.character(palette)) return(NULL)
  if (length(palette) == 1L && !is.na(palette)) {
    stops <- switch(tolower(trimws(palette)),
      greens = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476",
                 "#41AB5D", "#238B45", "#006D2C", "#00441B"),
      blues = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
                "#4292C6", "#2171B5", "#08519C", "#08306B"),
      reds = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A",
               "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
      NULL
    )
    if (is.null(stops)) return(NULL)
    return(grDevices::colorRampPalette(stops, space = "rgb")(256L))
  }
  if (length(palette) < 2L) return(NULL)
  .montage_validate_palette_colors(palette)
}

# Hex RGB/RGBA is portable to both browser runtimes. R colour names are
# normalized once, without resampling, reordering, or dropping alpha.
.montage_validate_palette_colors <- function(colors, label = "palette_colors") {
  if (!is.character(colors) || length(colors) < 2L || anyNA(colors) ||
      any(!nzchar(colors))) {
    stop("'", label, "' must contain at least two valid RGB/RGBA colours.",
         call. = FALSE)
  }
  rgba <- tryCatch(grDevices::col2rgb(colors, alpha = TRUE), error = function(e) {
    stop("'", label, "' must contain valid RGB/RGBA colours: ",
         conditionMessage(e), call. = FALSE)
  })
  out <- grDevices::rgb(rgba[1L, ], rgba[2L, ], rgba[3L, ],
                       alpha = rgba[4L, ], maxColorValue = 255)
  opaque <- rgba[4L, ] == 255L
  out[opaque] <- substr(out[opaque], 1L, 7L)
  hex <- grepl("^#[[:xdigit:]]{6}([[:xdigit:]]{2})?$", colors)
  out[hex] <- colors[hex]
  unname(out)
}

.montage_resolve_palette <- function(palette) {
  .montage_palette_colors(palette) %||% palette
}
