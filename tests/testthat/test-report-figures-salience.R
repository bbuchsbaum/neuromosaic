# Round 4 figure fixes: glass-brain salience (contrast with the card rises
# with |z| on light and dark cards, so weak maps look weak) and the
# kept/hidden voxel-centre guarantee at the shipped figure resolution.

r4_space <- function(dims) {
  neuroim2::NeuroSpace(dims, spacing = c(2, 2, 2),
                       origin = -dims * 2 / 2)
}

r4_lab <- function(rgb) {
  grDevices::convertColor(rgb, from = "sRGB", to = "Lab", scale.in = 1)
}

r4_hex_rgb <- function(hex) t(grDevices::col2rgb(hex)) / 255

# CIE76 colour difference between rows of `rgb` (0-1) and one ground colour.
r4_delta_e <- function(rgb, ground) {
  lab <- r4_lab(rgb)
  g <- r4_lab(r4_hex_rgb(ground))
  sqrt(rowSums(sweep(lab, 2, g[1, ])^2))
}

# WCAG relative-luminance contrast ratio against one ground colour.
r4_contrast <- function(rgb, ground) {
  lum <- function(x) {
    lin <- ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
    as.numeric(lin %*% c(0.2126, 0.7152, 0.0722))
  }
  a <- lum(rgb)
  b <- lum(r4_hex_rgb(ground))
  (pmax(a, b) + 0.05) / (pmin(a, b) + 0.05)
}

# Card grounds read from the report tokens: the thumbnail plate and the card.
r4_grounds <- list(
  light = c(plate = "#FBFBF8", card = "#FFFFFF"),
  dark = c(plate = "#0F1214", card = "#161B1E")
)

test_that("glance thumbnails draw on the report's card grounds", {
  css <- readLines(system.file("report", "montage-report.css",
                               package = "neuromosaic"))
  css <- paste(css, collapse = "\n")
  expect_match(css, "--nm-page: #FBFBF8", fixed = TRUE)
  expect_match(css, "--nm-page: #0F1214", fixed = TRUE)
  expect_identical(neuromosaic:::.nm_figure_theme("light")$canvas,
                   unname(r4_grounds$light[["plate"]]))
  expect_identical(neuromosaic:::.nm_figure_theme("dark")$canvas,
                   unname(r4_grounds$dark[["plate"]]))
})

test_that("glass-brain contrast with the card rises with |z| in both themes", {
  zs <- c(5, 5.5, 6, 7, 8, 10, 12, 14, 16, 18, 20, 22)
  nr <- 10
  nc <- length(zs) * 5
  strip <- matrix(0, nr, nc)
  for (k in seq_along(zs)) strip[, (k - 1) * 5 + 1:5] <- zs[[k]]
  depth <- list(img = matrix(5, nr, nc))
  for (theme in c("light", "dark")) {
    th <- neuromosaic:::.nm_figure_theme(theme)
    glass <- neuromosaic:::.nm_glass_tones(theme)
    silhouette <- grDevices::rgb(glass$lo[[1]], glass$lo[[2]], glass$lo[[3]])
    for (sgn in c(1, -1)) {
      proj <- list(pos = list(img = if (sgn > 0) strip else strip * 0),
                   neg = list(img = if (sgn > 0) strip * 0 else -strip))
      col <- neuromosaic:::.nm_glass_panel(
        proj, depth, threshold = 5, limits = c(-22, 22),
        palette = "nm-signed", tail = "two_sided", continuous = FALSE,
        theme = th
      )
      f <- nrow(col) / nr
      row <- round(nrow(col) / 2)
      rgb <- t(vapply(seq_along(zs), function(k) {
        c0 <- round(((k - 1) * 5 + 3 - 0.5) * f)
        grDevices::col2rgb(col[row, c0]) / 255
      }, numeric(3)))
      for (ground in r4_grounds[[theme]]) {
        de <- r4_delta_e(rgb, ground)
        cr <- r4_contrast(rgb, ground)
        info <- paste(theme, sgn, ground, paste(round(de, 1), collapse = " "))
        expect_true(all(diff(de) > 0), info = info)
        expect_true(all(diff(cr) > 0), info = info)
        # A clear dynamic range: the cap stands out at least twice as much as
        # the threshold, so a weak map cannot pass for a strong one.
        expect_gt(de[[length(de)]], 2 * de[[1L]])
      }
      # The threshold tint is still visibly coloured on the glass silhouette.
      expect_gt(r4_delta_e(rgb[1, , drop = FALSE], silhouette), 10)
    }
  }
})

test_that("glance ramps keep the sign hues and stay calm", {
  for (theme in c("light", "dark")) {
    stops <- neuromosaic:::.nm_glass_stops(theme)
    pos <- r4_lab(r4_hex_rgb(stops$positive))
    neg <- r4_lab(r4_hex_rgb(stops$negative))
    # Warm positive (a* > 0), cool negative (b* < 0) at every stop.
    expect_true(all(pos[, 2] > 0))
    expect_true(all(neg[, 3] < 0))
    # Moderate chroma everywhere (the r3 thumbnails peaked near 80).
    chroma <- sqrt(c(pos[, 2]^2 + pos[, 3]^2, neg[, 2]^2 + neg[, 3]^2))
    expect_lt(max(chroma), 70)
  }
  # Non-nm palettes keep their own colours.
  v <- c(-8, 6, 9)
  expect_equal(
    neuromosaic:::.nm_glass_rgb(v, "viridis", c(-10, 10), 5, "light"),
    neuromosaic:::.nm_value_rgb(v, "viridis", c(-10, 10), 5)
  )
})

# Mean contrast (CIE76 against the plate) of the coloured pixels of a written
# thumbnail, and the share of the image they cover.
r4_thumb_salience <- function(path, ground) {
  img <- png::readPNG(path)
  rgb <- matrix(img[, , 1:3], ncol = 3)
  lab <- r4_lab(rgb)
  chroma <- sqrt(lab[, 2]^2 + lab[, 3]^2)
  coloured <- chroma > 12
  de <- r4_delta_e(rgb, ground)
  c(mean = if (any(coloured)) mean(de[coloured]) else 0,
    mass = sum(de[coloured]) / length(de))
}

test_that("thumbnail salience orders maps by strength in both themes", {
  skip_if_not_installed("png")
  dims <- c(30, 34, 28)
  sp <- r4_space(dims)
  bg <- array(0, dim = dims)
  bg[3:28, 3:32, 3:26] <- 100
  grid <- expand.grid(i = seq_len(dims[1]), j = seq_len(dims[2]),
                      k = seq_len(dims[3]))
  blob <- function(peak, centres, radius) {
    v <- numeric(nrow(grid))
    for (cc in centres) {
      d2 <- (grid$i - cc[1])^2 + (grid$j - cc[2])^2 + (grid$k - cc[3])^2
      v <- pmax(v, peak * exp(-d2 / (2 * radius^2)))
    }
    v[v < 5] <- 0
    v
  }
  centres <- list(c(10, 12, 12), c(20, 22, 16), c(14, 26, 20))
  peaks <- c(7, 10, 13, 16, 19, 22)
  maps <- lapply(peaks, function(p) blob(p, centres, radius = 3.2))
  wide <- blob(13, c(centres, list(c(22, 10, 8), c(8, 28, 22))), radius = 3.2)
  sal <- list()
  for (theme in c("light", "dark")) {
    ground <- r4_grounds[[theme]][["plate"]]
    one <- function(values) {
      path <- tempfile(fileext = ".png")
      on.exit(unlink(path))
      neuromosaic:::.nm_glass_brain(
        neuroim2::NeuroVol(bg, sp), values, threshold = 5,
        limits = c(-22, 22), palette = "nm-signed", tail = "two_sided",
        continuous = FALSE, path = path, width_px = 420, theme = theme
      )
      r4_thumb_salience(path, ground)
    }
    s <- vapply(maps, one, numeric(2))
    # Stronger maps (same extent pattern, higher peak) stand out more.
    expect_true(all(diff(s["mean", ]) > 0),
                info = paste(theme, paste(round(s["mean", ], 1), collapse = " ")))
    # More extent at the same peak adds visual mass.
    w <- one(wide)
    expect_gt(w[["mass"]], s["mass", peaks == 13])
    sal[[theme]] <- s
  }
  # The weakest map is the quietest tile on both grounds.
  expect_identical(which.min(sal$light["mean", ]), 1L)
  expect_identical(which.min(sal$dark["mean", ]), 1L)
})

test_that("the glass-brain caption describes the salience encoding", {
  txt <- neuromosaic:::.nm_glass_brain_semantics
  expect_match(txt, "faint tint at the threshold", fixed = TRUE)
  expect_match(txt, "weaker maps look weaker", fixed = TRUE)
  expect_false(grepl("lightening", txt))
})

# Specimen check (opt-in): set NEUROMOSAIC_SPECIMEN_THUMBS to a directory of
# the HCP specimen's *_summary.png / *_summary-dark.png files.
test_that("specimen thumbnails: the weakest map is the quietest tile", {
  dir <- Sys.getenv("NEUROMOSAIC_SPECIMEN_THUMBS", "")
  skip_if(!nzchar(dir) || !dir.exists(dir), "specimen thumbnails not supplied")
  skip_if_not_installed("png")
  ids <- c("wm_2bk_0bk", "wm_face_avg", "wm_place_avg", "story_math",
           "tom_random", "faces_shapes", "reward_punish", "rh_avg")
  # Mean suprathreshold |z| over the three projections (from the specimen).
  strength <- c(11.90, 12.21, 11.58, 16.65, 12.10, 11.85, 7.47, 11.27)
  for (theme in c("light", "dark")) {
    suffix <- if (identical(theme, "dark")) "-dark" else ""
    s <- vapply(ids, function(id) {
      r4_thumb_salience(file.path(dir, paste0(id, "_summary", suffix, ".png")),
                        r4_grounds[[theme]][["plate"]])[["mean"]]
    }, numeric(1))
    expect_identical(names(which.min(s)), "reward_punish")
    expect_identical(names(which.max(s)), "story_math")
    expect_gt(stats::cor(s, strength), 0.9)
  }
})

# Pixel index nearest each voxel centre when `n` voxels span `n_out` pixels.
r4_centre_px <- function(n, n_out) {
  centre <- (seq_len(n) - 0.5) * n_out / n
  pmin(pmax(ceiling(centre - 1e-9), 1L), n_out)
}

test_that("kept/hidden voxel centres hold at the shipped 3.6 and 2.4 px/voxel", {
  n <- 30
  st <- matrix(0, n, n)
  st[4:26, 5:25] <- 7
  # Single-voxel holes and notches inside and on the edge of the blob.
  st[10, 10] <- 0
  st[15, 18] <- 0
  st[20, 12] <- 4.9
  st[4, 14] <- 0
  st[18, 25] <- 0
  # Single-voxel specks outside it, one just over threshold.
  st[2, 2] <- 5.05
  st[28, 28] <- 6
  st[12, 28] <- -5.2
  bg <- matrix(100, n, n)
  win <- list(floor = 4, lo = 20, hi = 100, min = 0)
  for (ppv in c(3.6, 2.4)) {
    out <- c(round(n * ppv), round(n * ppv) + 1L)
    layers <- neuromosaic:::.nm_composite_slice(
      bg, st, win, "nm-signed", c(-10, 10), threshold = 5,
      tail = "two_sided", continuous = FALSE, layers = TRUE, out = out
    )
    expect_identical(dim(layers$overlay_alpha), as.integer(out))
    pr <- r4_centre_px(n, out[[1]])
    pc <- r4_centre_px(n, out[[2]])
    a <- layers$overlay_alpha[pr, pc]
    kept <- abs(st) >= 5
    expect_true(all(a[kept] == 1), info = paste(ppv, "kept"))
    expect_true(all(a[!kept] == 0), info = paste(ppv, "hidden"))
  }
})

test_that("report figures ship their tiles 1:1, so the guarantee is in the PNG", {
  skip_if_not_installed("png")
  dims <- c(40, 44, 14)
  sp <- r4_space(dims)
  bg <- array(0, dims)
  bg[3:38, 3:42, 2:13] <- 100
  st <- array(0, dims)
  st[6:30, 8:36, 5:9] <- 7
  set.seed(4)
  holes <- sample(which(st != 0), 120)
  st[holes] <- 0
  out <- stat_montage(neuroim2::NeuroVol(bg, sp), neuroim2::NeuroVol(st, sp),
                      threshold = 5, cap = 10, zlevels = 5:9, draw = FALSE)
  parts <- out$plot$parts
  for (variant in c("wide", "narrow")) for (theme in c("light", "dark")) {
    spec <- neuromosaic:::.nm_volume_figure_variant(out$plot, theme, variant)
    path <- tempfile(fileext = ".png")
    neuromosaic:::.nm_write_figure(spec, path)
    img <- png::readPNG(path)
    unlink(path)
    i <- 2L # the first tile carries the L/R letters
    b <- spec$tile_boxes[i, ]
    tile <- neuromosaic:::.nm_layout_tile(parts, i, c(b$height, b$width))
    rgba <- grDevices::col2rgb(tile, alpha = TRUE)
    opaque <- rgba[4, ] == 255
    rows <- b$top + seq_len(b$height) - 1L
    cols <- b$left + seq_len(b$width) - 1L
    got <- t(matrix(round(img[rows, cols, 1:3] * 255), ncol = 3))
    # Exactly the composited pixels: no resampling between tile and PNG.
    expect_equal(max(abs(got[, opaque] - rgba[1:3, opaque])), 0,
                 info = paste(variant, theme))
    # And in the PNG itself: every kept voxel centre is coloured, every
    # hidden one (including the single-voxel holes) shows the underlay.
    src <- parts$sources[[i]]$stat
    pr <- r4_centre_px(nrow(src), b$height)
    pc <- r4_centre_px(ncol(src), b$width)
    px <- img[rows[pr], cols[pc], 1:3, drop = FALSE]
    lab <- r4_lab(matrix(px, ncol = 3))
    chroma <- matrix(sqrt(lab[, 2]^2 + lab[, 3]^2), nrow(src), ncol(src))
    kept <- abs(src) >= 5
    expect_gt(min(chroma[kept]), 25)
    expect_lt(max(chroma[!kept]), 8)
    expect_gt(sum(kept), 100)
  }
})
