# Round-2 report figures: edge-voxel fidelity of the static montage, smooth
# silhouettes, theme/width variants and their type, honest glass-brain
# thumbnails, the static surface strip and its A/P marks, and report-wide
# colour caps.

r2_space <- function(dims = c(12, 14, 16)) {
  neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2),
                       origin = c(-11, -13, -20))
}

r2_stat <- function() {
  arr <- array(0, dim = c(12, 14, 16))
  arr[3:5, 3:5, 6:8] <- 6
  arr[8:10, 3:5, 9:11] <- -6.5
  neuroim2::NeuroVol(arr, r2_space())
}

r2_background <- function() {
  arr <- array(0, dim = c(12, 14, 16))
  arr[2:11, 2:13, 2:15] <- 100
  neuroim2::NeuroVol(arr, r2_space())
}

r2_window <- function() {
  list(floor = 4, lo = 20, hi = 100, min = 0)
}

# Pixel (row, col) of the upsampled image nearest the centre of voxel (i, j).
r2_centre <- function(i, j, f) c((i - 1) * f + f / 2, (j - 1) * f + f / 2)

test_that("every kept edge voxel is drawn and no hidden voxel centre is coloured", {
  f <- 4L
  bg <- matrix(100, 12, 12)
  st <- matrix(0, 12, 12)
  # Near-threshold kept voxels with hidden (0) neighbours: bilinear
  # interpolation of values before thresholding used to drop these.
  st[3, 3] <- 5.2                     # isolated, just above threshold
  st[6, 6:8] <- c(5.5, 9, 5.1)        # a row with weak ends
  st[9:11, 9:11] <- 7                 # a ring of kept voxels ...
  st[10, 10] <- 0                     # ... around a hidden centre
  st[3, 9] <- -5.3                    # a weak negative speck
  layers <- neuromosaic:::.nm_composite_slice(
    bg, st, r2_window(), "nm-signed", c(-10, 10), threshold = 5,
    tail = "two_sided", continuous = FALSE, f = f, layers = TRUE
  )
  kept <- which(abs(st) >= 5, arr.ind = TRUE)
  for (k in seq_len(nrow(kept))) {
    rc <- r2_centre(kept[k, 1], kept[k, 2], f)
    expect_equal(layers$overlay_alpha[rc[1], rc[2]], 1,
                 info = paste("kept voxel", kept[k, 1], kept[k, 2]))
    # Colour is clamped to the drawn range: never below threshold.
    expect_gte(abs(layers$overlay_value[rc[1], rc[2]]), 5)
  }
  hidden <- which(abs(st) < 5, arr.ind = TRUE)
  centres <- t(apply(hidden, 1L, function(v) r2_centre(v[1], v[2], f)))
  expect_true(all(layers$overlay_alpha[centres] == 0))
  # The negative speck keeps its sign.
  rc <- r2_centre(3, 9, f)
  expect_lt(layers$overlay_value[rc[1], rc[2]], 0)
})

test_that("the silhouette rim is lifted to the interior grey, not a dark line", {
  bg <- matrix(0, 20, 20)
  bg[4:17, 4:17] <- 100
  # A dim partial-volume rim, as at a skull-strip edge.
  bg[4, 4:17] <- 8
  bg[17, 4:17] <- 8
  bg[4:17, 4] <- 8
  bg[4:17, 17] <- 8
  col <- neuromosaic:::.nm_composite_slice(
    bg, matrix(0, 20, 20), list(floor = 4, lo = 20, hi = 100, min = 0),
    "nm-signed", c(-10, 10), threshold = 5, tail = "two_sided",
    continuous = FALSE, f = 4L
  )
  lum <- function(x) colMeans(grDevices::col2rgb(x))
  interior <- lum(col[40, 40])
  rim <- lum(col[(4 - 1) * 4 + 2, 40]) # centre of the rim voxel row
  expect_gt(rim, 0.75 * interior)
  # Transparent outside the brain; opaque inside.
  expect_equal(unname(grDevices::col2rgb(col[2, 2], alpha = TRUE)[4, 1]), 0)
  expect_equal(unname(grDevices::col2rgb(col[40, 40], alpha = TRUE)[4, 1]), 255)
})

test_that("figure variants share tiles and set one family at legible sizes", {
  out <- stat_montage(r2_background(), r2_stat(), threshold = 5,
                      zlevels = 5:12, draw = FALSE)
  spec <- out$plot
  dark <- neuromosaic:::.nm_volume_figure_variant(spec, "dark", "wide")
  narrow <- neuromosaic:::.nm_volume_figure_variant(spec, "light", "narrow")
  expect_identical(spec$ncol, 6L)
  expect_identical(narrow$ncol, 4L)
  expect_identical(narrow$width, 800)
  expect_identical(dark$canvas, neuromosaic:::.nm_canvas_dark)
  expect_identical(dark$labels, spec$labels)

  text_gp <- function(grob) {
    out <- list()
    walk <- function(g) {
      if (inherits(g, "text")) out[[length(out) + 1L]] <<- g$gp
      if (inherits(g, "gTree")) for (k in g$children) walk(k)
    }
    walk(grob)
    out
  }
  fonts <- neuromosaic:::.nm_figure_fonts()
  for (s in list(spec, dark, narrow)) {
    gps <- text_gp(s$grob)
    expect_gt(length(gps), 8L)
    families <- unique(vapply(gps, function(g) g$fontfamily, character(1)))
    expect_true(all(families %in% c(fonts$regular, fonts$medium)))
    sizes <- vapply(gps, function(g) g$fontsize, numeric(1))
    # 144-dpi figures shown at half size: points equal CSS pixels.
    expect_gte(min(sizes), 10)
    if (identical(fonts$medium_face, 1L)) {
      # A registered medium weight: nothing is set in bold.
      faces <- vapply(gps, function(g) as.integer(g$font %||% 1L), integer(1))
      expect_true(all(faces == 1L))
    }
  }
  expect_gt(min(vapply(text_gp(narrow$grob), `[[`, numeric(1), "fontsize")),
            min(vapply(text_gp(spec$grob), `[[`, numeric(1), "fontsize")))

  skip_if_not_installed("png")
  path <- tempfile(fileext = ".png")
  neuromosaic:::.nm_write_figure(dark, path)
  img <- png::readPNG(path)
  expect_equal(round(img[2, 2, 1:3] * 255),
               as.numeric(grDevices::col2rgb(neuromosaic:::.nm_canvas_dark)))
})

test_that("glass-brain thumbnails show strength as lightness, cropped, no letters", {
  bg <- r2_background()
  frame <- neuromosaic:::.nm_axis_frame(neuroim2::space(bg))
  weak <- array(0, dim = dim(bg))
  weak[4:6, 4:6, 6:8] <- 5.5
  strong <- weak
  strong[strong > 0] <- 19
  window <- neuromosaic:::.nm_underlay_window(as.numeric(bg))
  panel <- function(arr) {
    proj <- neuromosaic:::.nm_signed_projections(arr, 3L, frame)
    depth <- neuromosaic:::.nm_mean_projection(as.array(bg), 3L, frame,
                                              window$floor)
    neuromosaic:::.nm_glass_panel(
      proj, depth, threshold = 5, limits = c(-20, 20), palette = "nm-signed",
      tail = "two_sided", continuous = FALSE,
      theme = neuromosaic:::.nm_figure_theme("light")
    )
  }
  pw <- panel(weak)
  ps <- panel(strong)
  glass <- panel(array(0, dim = dim(bg)))
  # Opaque colour whose departure from the (light) card grows with |z|:
  # round 4 made the strong effect darker than the weak one on light ground
  # (see test-report-figures-salience.R for both themes).
  lightness <- function(p) {
    changed <- p != glass
    a <- grDevices::col2rgb(p[changed]) / 255
    mean(colSums(a * c(0.2126, 0.7152, 0.0722)))
  }
  expect_lt(lightness(ps), 0.6 * lightness(pw))

  skip_if_not_installed("png")
  path <- tempfile(fileext = ".png")
  res <- neuromosaic:::.nm_glass_brain(
    bg, as.numeric(strong), threshold = 5, limits = c(-20, 20),
    palette = "nm-signed", tail = "two_sided", continuous = FALSE,
    path = path, width_px = 420
  )
  expect_identical(res$views, c("sagittal", "coronal", "axial"))
  img <- png::readPNG(path)
  expect_equal(dim(img)[[2L]], 420L)
  expect_lt(dim(img)[[1L]], 220L)
  expect_lt(file.size(path), 60 * 1024)
})

test_that("the sagittal glass view puts anterior on the left", {
  bg <- r2_background()
  arr <- array(0, dim = dim(bg))
  arr[5:7, 11:13, 7:9] <- 15 # anterior (high y)
  skip_if_not_installed("png")
  path <- tempfile(fileext = ".png")
  res <- neuromosaic:::.nm_glass_brain(
    bg, as.numeric(arr), threshold = 5, limits = c(-20, 20),
    palette = "nm-signed", tail = "two_sided", continuous = FALSE,
    path = path, width_px = 420
  )
  img <- png::readPNG(path)
  red <- img[, , 1] > 0.6 & img[, , 3] < 0.45
  # First third of the strip is the sagittal panel.
  third <- seq_len(floor(ncol(red) / 3))
  cols <- which(colSums(red[, third, drop = FALSE]) > 0)
  brain <- which(colSums(img[, third, 1] < 0.97) > 0)
  expect_gt(length(cols), 0L)
  expect_lt(mean(cols), mean(range(brain)))
})

test_that("anterior side follows the renderer's camera for every view", {
  skip_if_not_installed("rgl")
  skip_if_not_installed("neurosurf")
  side <- neuromosaic:::.nm_surface_anterior_side
  expect_identical(side("left", "lateral"), "left")
  expect_identical(side("left", "medial"), "right")
  expect_identical(side("right", "medial"), "left")
  expect_identical(side("right", "lateral"), "right")

  ico <- rgl::subdivision3d(rgl::icosahedron3d(), 2)
  vert <- t(ico$vb[1:3, ] / rep(ico$vb[4, ], each = 3))
  vert[, 2] <- vert[, 2] * 3 # elongated along y (anterior-posterior)
  faces <- t(ico$it) - 1L # SurfaceGeometry takes 0-based faces
  for (hemi in c("left", "right")) {
    vx <- vert
    vx[, 1] <- vx[, 1] + if (hemi == "left") -1.5 else 1.5
    geom <- neurosurf::SurfaceGeometry(vx, faces, hemi = if (hemi == "left") "lh" else "rh")
    for (view in c("lateral", "medial")) {
      img <- neurosurf::render_surface_rgba(
        geom, vertex_values = vx[, 2], camera = view, width = 300L,
        height = 160L, palette = c("#0000FF", "#FF0000"),
        limits = range(vx[, 2]), lighting = FALSE, outer_contour = FALSE,
        background = "#FFFFFF", antialias = 1L
      )
      rgba <- img$rgba
      r <- as.integer(rgba[, , 1])
      b <- as.integer(rgba[, , 3])
      cols <- matrix(rep(seq_len(dim(rgba)[2]), each = dim(rgba)[1]),
                     dim(rgba)[1])
      red_x <- mean(cols[r > 200 & b < 80])
      blue_x <- mean(cols[b > 200 & r < 80])
      observed <- if (red_x > blue_x) "right" else "left"
      expect_identical(side(hemi, view), observed,
                       info = paste(hemi, view))
    }
  }
})

r2_fake_brain_plot <- function() {
  key <- neuromosaic:::.nm_chroma_key
  ras <- matrix(key, 60, 100)
  ras[15:45, 10:90] <- "#B0B0B0"
  ras[14, 10:90] <- "#6EB76E" # anti-aliased contour over the key
  spec <- list(c("Left Lateral", "lh", "lateral"),
               c("Right Lateral", "rh", "lateral"),
               c("Left Medial", "lh", "medial"),
               c("Right Medial", "rh", "medial"))
  plots <- lapply(spec, function(x) {
    ggplot2::ggplot() +
      ggplot2::annotation_raster(ras, 0, 1, 0, 1) +
      ggplot2::ggtitle(x[[1]])
  })
  p <- patchwork::wrap_plots(plots, ncol = 2)
  cams <- stats::setNames(lapply(spec, function(x) {
    list(view = x[[3]], hemi = x[[2]])
  }), vapply(spec, `[[`, character(1), 1L))
  attr(p, "plot_brain_anatomy") <- list(camera = cams)
  p
}

test_that("the surface strip is cut out, ordered, and marks A at the anterior end", {
  skip_if_not_installed("patchwork")
  key <- list(palette = "nm-signed", limits = c(-10, 10), threshold = 5,
              signed = TRUE, tail = "two_sided", continuous = FALSE,
              over_low = FALSE, over_high = FALSE, title = "z")
  parts <- neuromosaic:::.nm_surface_strip_parts(r2_fake_brain_plot(), key)
  expect_identical(
    vapply(parts$panels, function(x) paste(x$hemi, x$view), character(1)),
    c("left lateral", "left medial", "right medial", "right lateral")
  )
  ras <- parts$panels[[1]]$raster
  # Cropped to the cut-out brain plus its fringe; the key is fully clear.
  expect_identical(dim(ras), c(32L, 81L))
  alpha <- grDevices::col2rgb(ras, alpha = TRUE)[4, ]
  expect_true(all(alpha[matrix(seq_along(ras), nrow(ras))[2:32, ]] == 255))
  fringe <- grDevices::col2rgb(ras[1, 5], alpha = TRUE)
  expect_true(fringe[4] > 0 && fringe[4] < 255)
  expect_lt(abs(fringe[1] - fringe[2]), 12) # green removed from the fringe

  for (theme in c("light", "dark")) for (variant in c("wide", "narrow")) {
    spec <- neuromosaic:::.nm_surface_strip_layout(parts, theme = theme,
                                                   variant = variant)
    for (m in spec$marks) {
      centre <- m$panel_x0 + m$panel_w / 2
      if (identical(m$anterior, "right")) {
        expect_gt(m$a_x, centre)
      } else {
        expect_lt(m$a_x, centre)
      }
      expect_identical(m$anterior,
                       neuromosaic:::.nm_surface_anterior_side(m$hemi, m$view))
    }
  }
  wide <- neuromosaic:::.nm_surface_strip_layout(parts)
  expect_equal(wide$width, 1760)
  # Lateral views are drawn whole, so the strip stays short.
  expect_lt(wide$height, 700)
})

test_that("the surface and volume keys share one horizontal geometry", {
  # Round 3: the key group is centred on the figure (see
  # test-report-figures-keys-and-thumbnails.R); both figures still take one shared geometry.
  geo <- neuromosaic:::.nm_key_geometry()
  expect_equal(geo$w, 0.42)
  expect_equal(geo$x0, (1 - geo$w) / 2)
  expect_identical(
    neuromosaic:::.nm_key_geometry(FALSE, "Z-statistic", 11, 1760),
    neuromosaic:::.nm_key_geometry(FALSE, "Z-statistic", 11, 1760)
  )
})

test_that("maps of one quantity share a report-wide colour cap by default", {
  manifest <- data.frame(
    map_id = c("strong", "weak", "tmap"),
    path = c("strong.nii.gz", "weak.nii.gz", "t.nii.gz"),
    stat_kind = c("z", "z", "t"),
    df = c(NA, NA, 20),
    units = c("z", "z", "t"),
    signed = TRUE,
    label = c("Strong", "Weak", "T map"),
    stringsAsFactors = FALSE
  )
  keys <- function(policy) {
    resolve_montage_policy(manifest, policy)$cap_key
  }
  shared <- keys(montage_policy())
  expect_identical(shared[[1]], shared[[2]])
  expect_false(identical(shared[[1]], shared[[3]]))
  per_map <- keys(montage_policy(cap_scope = "map"))
  expect_identical(per_map, manifest$map_id)
  expect_error(montage_policy(cap_scope = "page"))
})

test_that("shared caps pool the suprathreshold values of every member", {
  manifest <- data.frame(
    map_id = c("strong", "weak"), cap_key = c("k", "k"),
    effective_display_mode = "thresholded", effective_threshold = 3,
    effective_tail = "two_sided", stringsAsFactors = FALSE
  )
  strong <- c(rep(0, 50), seq(3, 30, length.out = 200))
  weak <- c(rep(0, 50), seq(3, 8, length.out = 200))
  caps <- neuromosaic:::.montage_shared_caps(
    manifest, list(strong, weak), policy = montage_policy(cap_quantile = 1)
  )
  expect_length(caps, 1L)
  expect_equal(caps[["k"]], 30)
})
