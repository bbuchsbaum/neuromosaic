# Round-3 report figures: key titles that never leave the canvas, a centred
# key group with a legible note, section-shared slice sets, glass-brain
# thumbnails whose lightness tracks |value|, smooth overlay outlines that
# still draw exactly the kept voxels, template debris removed from the
# underlay, quiet A/P marks, and surface cap triangles from drawn values.

r3_space <- function(dims = c(16, 18, 14)) {
  neuroim2::NeuroSpace(dim = dims, spacing = c(2, 2, 2),
                       origin = c(-15, -17, -13))
}

r3_background <- function(dims = c(16, 18, 14)) {
  arr <- array(0, dim = dims)
  arr[2:(dims[1] - 1), 2:(dims[2] - 1), 2:(dims[3] - 1)] <- 100
  neuroim2::NeuroVol(arr, r3_space(dims))
}

r3_stat <- function(dims = c(16, 18, 14), value = 7) {
  arr <- array(0, dim = dims)
  arr[4:7, 4:8, 5:9] <- value
  arr[10:13, 9:13, 6:10] <- -value
  neuroim2::NeuroVol(arr, r3_space(dims))
}

r3_png_rgb <- function(path) {
  img <- png::readPNG(path)
  if (length(dim(img)) == 3L && dim(img)[[3L]] >= 3L) img[, , 1:3] else img
}

# Non-canvas ("ink") pixels of the key block: rows below the last tile.
r3_key_ink <- function(spec, rows_from) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  neuromosaic:::.nm_write_figure(spec, path)
  rgb <- r3_png_rgb(path)
  canvas <- grDevices::col2rgb(spec$canvas) / 255
  diff <- abs(rgb[, , 1] - canvas[[1]]) + abs(rgb[, , 2] - canvas[[2]]) +
    abs(rgb[, , 3] - canvas[[3]])
  ink <- diff > 0.12
  ink[seq_len(rows_from - 1L), ] <- FALSE
  ink
}

test_that("narrow figure keys keep the whole title inside the canvas", {
  skip_if_not_installed("png")
  out <- stat_montage(r3_background(), r3_stat(), threshold = 5, cap = 10,
                      zlevels = 5:12, legend_title = "Z-statistic",
                      draw = FALSE)
  for (theme in c("light", "dark")) {
    spec <- neuromosaic:::.nm_volume_figure_variant(out$plot, theme, "narrow")
    block <- neuromosaic:::.nm_key_block(14, narrow = TRUE)
    key_top <- spec$height - block$height
    ink <- r3_key_ink(spec, floor(key_top))
    cols <- which(colSums(ink) > 0)
    rows <- which(rowSums(ink) > 0)
    expect_gt(length(cols), 0)
    # Every key pixel (title, bar, ticks, note) lies inside the canvas with a
    # margin, so nothing is clipped at the left edge as in round 2.
    expect_gte(min(cols), 8, label = paste(theme, "leftmost key ink"))
    expect_lte(max(cols), spec$width - 8)
    # The title sits above the bar: its ink starts above the bar's top row.
    bar_row <- min(which(rowSums(ink[, seq(spec$width * 0.3, spec$width * 0.7)])
                         > spec$width * 0.3))
    expect_lt(min(rows), bar_row - 10)
  }
})

test_that("narrow surface strip keys keep the whole title inside the canvas", {
  skip_if_not_installed("png")
  ras <- matrix("#B0B0B0", 40, 90)
  panels <- lapply(list(c("left", "lateral"), c("left", "medial"),
                        c("right", "medial"), c("right", "lateral")),
                   function(x) {
    list(raster = ras, hemi = x[[1]], view = x[[2]],
         anterior = neuromosaic:::.nm_surface_anterior_side(x[[1]], x[[2]]))
  })
  key <- list(palette = "nm-signed", limits = c(-22.3, 22.3), threshold = 5,
              signed = TRUE, tail = "two_sided", continuous = FALSE,
              over_low = TRUE, over_high = TRUE, title = "Z-statistic",
              note = "values averaged across cortical depth")
  parts <- list(panels = panels, key = key)
  for (theme in c("light", "dark")) {
    spec <- neuromosaic:::.nm_surface_strip_layout(parts, theme = theme,
                                                   variant = "narrow")
    block <- neuromosaic:::.nm_key_block(14, narrow = TRUE)
    ink <- r3_key_ink(spec, floor(spec$height - block$height))
    cols <- which(colSums(ink) > 0)
    expect_gte(min(cols), 8)
    expect_lte(max(cols), spec$width - 8)
  }
})

test_that("the wide key group (title + bar) is centred and the note legible", {
  skip_if_not_installed("png")
  # cap above the data maximum: no triangles, so the group is title + bar.
  out <- stat_montage(r3_background(), r3_stat(), threshold = 5, cap = 10,
                      zlevels = 5:12, legend_title = "Z-statistic",
                      draw = FALSE)
  spec <- out$plot
  block <- neuromosaic:::.nm_key_block(11, narrow = FALSE)
  key_top <- floor(spec$height - block$height + block$above)
  ink <- r3_key_ink(spec, key_top)
  # Rows of the bar line (title + bar): the first rows of the key block.
  bar_rows <- key_top + seq_len(20)
  cols <- which(colSums(ink[bar_rows, , drop = FALSE]) > 0)
  centre <- (min(cols) + max(cols)) / 2
  expect_lt(abs(centre - spec$width / 2), 0.01 * spec$width)

  # Note: at least the label size (>= 11 CSS px on the 2x figure) and at least
  # 4.5:1 against the canvas in both themes.
  key <- neuromosaic:::.nm_colour_key_grob(
    "nm-signed", c(-10, 10), 5, TRUE, "two_sided", FALSE,
    title = "z", note = "threshold", fontsize = 11, note_below = TRUE
  )
  note <- Filter(function(g) identical(g$label, "threshold"), key$children)
  expect_length(note, 1L)
  expect_gte(note[[1]]$gp$fontsize, 11)
  lum <- function(hex) {
    v <- grDevices::col2rgb(hex)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  ratio <- function(a, b) {
    l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
    (l[[1]] + 0.05) / (l[[2]] + 0.05)
  }
  for (theme in c("light", "dark")) {
    th <- neuromosaic:::.nm_figure_theme(theme)
    expect_gte(ratio(th$note, th$canvas), 4.5, label = paste(theme, "note"))
  }
})

r3_manifest <- function(maps, task) {
  n <- length(maps)
  data.frame(
    map_id = paste0("m", seq_len(n)),
    analysis_id = paste0("m", seq_len(n)),
    task = task,
    effective_display_mode = "thresholded",
    effective_threshold = 5,
    effective_tail = "two_sided",
    effective_min_cluster_size = 1L,
    effective_connectivity = "18-connect",
    stringsAsFactors = FALSE
  )
}

test_that("maps of one layout section share a slice set chosen from their union", {
  dims <- c(10, 10, 40)
  make <- function(zs) {
    arr <- array(0, dim = dims)
    arr[4:6, 4:6, zs] <- 8
    neuroim2::NeuroVol(arr, neuroim2::NeuroSpace(dims, c(2, 2, 2)))
  }
  maps <- list(make(3:14), make(20:37), make(10:30))
  manifest <- r3_manifest(maps, task = c("wm", "wm", "lang"))
  masks <- list(NULL, NULL, NULL)

  z <- neuromosaic:::.montage_group_zlevels(manifest, maps, masks,
                                            layout = "task")
  expect_identical(z$m1$zlevels, z$m2$zlevels)
  expect_identical(z$m1$scope, "section")
  expect_identical(z$m1$map_ids, c("m1", "m2"))
  expect_identical(z$m1$section, "wm")
  # The shared set spans both maps' extents (union), not just the first:
  # below map 2's first slice and above map 1's last.
  expect_lt(min(z$m1$zlevels), 20)
  expect_gt(max(z$m1$zlevels), 14)
  # A single-map section chooses its own slices and says so.
  expect_identical(z$m3$scope, "map")
  expect_identical(z$m3$map_ids, "m3")

  # Per-map scope keeps each map's own slices; no layout falls back to the
  # analysis (here one map each).
  own <- neuromosaic:::.montage_group_zlevels(manifest, maps, masks,
                                              layout = "task", scope = "map")
  expect_false(identical(own$m1$zlevels, own$m2$zlevels))
  nolayout <- neuromosaic:::.montage_group_zlevels(manifest, maps, masks)
  expect_false(identical(nolayout$m1$zlevels, nolayout$m2$zlevels))

  # A manifest zlevels entry overrides one map only.
  manifest$zlevels <- list(NULL, c(2L, 4L, 6L), NULL)
  over <- neuromosaic:::.montage_group_zlevels(manifest, maps, masks,
                                               layout = "task")
  expect_identical(over$m2$zlevels, c(2L, 4L, 6L))
  expect_identical(over$m2$scope, "map")
  expect_identical(over$m1$zlevels, z$m1$zlevels)

  expect_identical(montage_policy()$slice_scope, "section")
  expect_identical(montage_policy(slice_scope = "map")$slice_scope, "map")
  expect_error(montage_policy(slice_scope = "slab"))
})

# CIE L* of 0-1 sRGB rows.
r3_lstar <- function(rgb) {
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  y <- lin %*% c(0.2126, 0.7152, 0.0722)
  f <- ifelse(y > 216 / 24389, y^(1 / 3), (24389 / 27 * y + 16) / 116)
  as.numeric(116 * f - 16)
}

test_that("glass-brain colour departs from the card monotonically with |z|", {
  # Round 4: salience, not lightness, is the invariant. On the light card the
  # stronger effect is darker; on the dark card it is lighter (see
  # test-report-figures-salience.R for contrast against the card ground).
  zs <- c(5, 6, 7, 9, 12, 15, 18, 22)
  nr <- 12
  nc <- length(zs) * 5
  pos <- matrix(0, nr, nc)
  for (k in seq_along(zs)) pos[, (k - 1) * 5 + 1:5] <- zs[[k]]
  proj <- function(img) list(img = img)
  depth <- list(img = matrix(5, nr, nc))
  for (theme in c("light", "dark")) {
    th <- neuromosaic:::.nm_figure_theme(theme)
    for (sgn in c(1, -1)) {
      p <- if (sgn > 0) {
        list(pos = proj(pos), neg = proj(pos * 0))
      } else {
        list(pos = proj(pos * 0), neg = proj(-pos))
      }
      col <- neuromosaic:::.nm_glass_panel(p, depth, threshold = 5,
                                           limits = c(-22, 22),
                                           palette = "nm-signed",
                                           tail = "two_sided",
                                           continuous = FALSE, theme = th)
      f <- nrow(col) / nr
      row <- round(nrow(col) / 2)
      L <- vapply(seq_along(zs), function(k) {
        c0 <- round(((k - 1) * 5 + 3 - 0.5) * f)
        rgb <- t(grDevices::col2rgb(col[row, c0])) / 255
        r3_lstar(rgb)
      }, numeric(1))
      dir <- if (identical(theme, "dark")) 1 else -1
      expect_true(all(dir * diff(L) > 0),
                  label = paste(theme, sgn, paste(round(L, 1), collapse = " ")))
      # Opaque: the effect pixel is fully covered, so the card never shows
      # through (no wash on light, no mud on dark).
      alpha <- grDevices::col2rgb(col[row, round(f * 3)], alpha = TRUE)[4, 1]
      expect_equal(unname(alpha), 255)
    }
  }
})

test_that("glass-brain thumbnails are written at 2x the slot and stay small", {
  skip_if_not_installed("png")
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  stat <- as.numeric(as.array(r3_stat()))
  info <- neuromosaic:::.nm_glass_brain(
    r3_background(), stat, threshold = 5, limits = c(-10, 10),
    palette = "nm-signed", tail = "two_sided", continuous = FALSE,
    path = path
  )
  expect_equal(info$width, 840)
  expect_equal(dim(png::readPNG(path))[[2]], 840L)
  expect_lt(file.size(path), 60 * 1024)
})

test_that("smooth overlay outlines still draw exactly the kept voxel centres", {
  f <- 4L
  n <- 24
  # A diagonal staircase edge plus isolated specks, holes and weak ends.
  st <- outer(seq_len(n), seq_len(n), function(i, j) ifelse(j >= i + 2, 7, 0))
  st[20, 3] <- 5.1
  st[18:20, 8:10] <- 6
  st[19, 9] <- 0
  st[5, 3] <- -5.2
  bg <- matrix(100, n, n)
  win <- list(floor = 4, lo = 20, hi = 100, min = 0)
  layers <- neuromosaic:::.nm_composite_slice(
    bg, st, win, "nm-signed", c(-10, 10), threshold = 5,
    tail = "two_sided", continuous = FALSE, f = f, layers = TRUE
  )
  a <- layers$overlay_alpha
  # The four subpixels around every voxel centre.
  around <- function(i, j) {
    r <- (i - 1) * f + c(f / 2, f / 2 + 1)
    c <- (j - 1) * f + c(f / 2, f / 2 + 1)
    as.matrix(expand.grid(r, c))
  }
  for (i in seq_len(n)) for (j in seq_len(n)) {
    v <- a[around(i, j)]
    if (abs(st[i, j]) >= 5) {
      expect_true(all(v == 1), info = paste("kept", i, j))
    } else {
      expect_true(all(v == 0), info = paste("hidden", i, j))
    }
  }
  # A shallow voxel staircase (two rows per column step) is drawn as a
  # near-straight edge: the 0.5 crossing along each subpixel row deviates far
  # less from a straight fit than the bilinear (round-2) coverage does.
  st2 <- outer(seq_len(n), seq_len(n), function(i, j) ifelse(2 * j >= i + 8, 7, 0))
  smooth <- neuromosaic:::.nm_composite_slice(
    bg, st2, win, "nm-signed", c(-10, 10), threshold = 5,
    tail = "two_sided", continuous = FALSE, f = f, layers = TRUE
  )$overlay_alpha
  crossing <- function(alpha) {
    rows <- seq(4 * f, 20 * f)
    x <- vapply(rows, function(r) {
      k <- which(alpha[r, ] >= 0.5)
      if (length(k)) min(k) else NA_real_
    }, numeric(1))
    stats::sd(stats::residuals(stats::lm(x ~ rows)))
  }
  stair <- neuromosaic:::.nm_upsample((st2 >= 5) * 1, f, "linear")
  stair_alpha <- neuromosaic:::.nm_smoothstep(stair, 0.32, 0.48)
  expect_lt(crossing(smooth), 0.5 * crossing(stair_alpha))
})

test_that("small tissue islands are dropped from the underlay, not the overlay", {
  bg <- matrix(0, 30, 30)
  bg[5:26, 5:26] <- 100   # brain
  bg[1:2, 14:15] <- 100   # a 4-voxel template fragment
  st <- matrix(0, 30, 30)
  win <- list(floor = 4, lo = 20, hi = 100, min = 0)
  col <- neuromosaic:::.nm_composite_slice(
    bg, st, win, "nm-signed", c(-10, 10), threshold = 5,
    tail = "two_sided", continuous = FALSE, f = 4L
  )
  alpha <- function(r, c) unname(grDevices::col2rgb(col[r, c], alpha = TRUE)[4, 1])
  expect_equal(alpha(4 * 1 + 2, 4 * 14 + 2), 0)       # fragment gone
  expect_equal(alpha(4 * 15 + 2, 4 * 15 + 2), 255)    # brain kept
  # A kept overlay voxel over the fragment is still drawn.
  st[1, 14] <- 8
  layers <- neuromosaic:::.nm_composite_slice(
    bg, st, win, "nm-signed", c(-10, 10), threshold = 5,
    tail = "two_sided", continuous = FALSE, f = 4L, layers = TRUE
  )
  expect_equal(layers$overlay_alpha[2, 4 * 13 + 2], 1)
  debris <- neuromosaic:::.nm_tissue_debris(bg > 4)
  expect_equal(sum(debris), 4)
})

test_that("each surface view carries one arrow mark pointing at its anterior pole", {
  ras <- matrix("#B0B0B0", 40, 90)
  combos <- list(c("left", "lateral"), c("left", "medial"),
                 c("right", "medial"), c("right", "lateral"))
  panels <- lapply(combos, function(x) {
    list(raster = ras, hemi = x[[1]], view = x[[2]],
         anterior = neuromosaic:::.nm_surface_anterior_side(x[[1]], x[[2]]))
  })
  key <- list(palette = "nm-signed", limits = c(-10, 10), threshold = 5,
              signed = TRUE, tail = "two_sided", continuous = FALSE,
              over_low = FALSE, over_high = FALSE, title = "z")
  expected <- c("left lateral" = "left", "left medial" = "right",
                "right medial" = "left", "right lateral" = "right")
  for (variant in c("wide", "narrow")) {
    spec <- neuromosaic:::.nm_surface_strip_layout(
      list(panels = panels, key = key), variant = variant
    )
    labels <- unlist(lapply(spec$grob$children, function(g) {
      if (inherits(g, "text")) g$label
    }))
    # No loose P letters and exactly one mark per view.
    expect_false(any(labels %in% c("A", "P")))
    expect_equal(sum(grepl("← A|A →", labels)), 4L)
    for (m in spec$marks) {
      side <- expected[[paste(m$hemi, m$view)]]
      expect_identical(m$anterior, side)
      expect_identical(m$glyph, if (side == "left") "← A" else "A →")
      end <- if (side == "left") m$panel_x0 else m$panel_x0 + m$panel_w
      expect_equal(m$a_x, end)
    }
  }
})

test_that("surface cap triangles come from drawn cortical vertex values", {
  overlay <- list(lh = c(3, 6, 21, 30), rh = c(-4, -8, 12, NA))
  atlas <- list(cortex_mask = list(lh = c(TRUE, TRUE, TRUE, FALSE),
                                   rh = c(TRUE, TRUE, TRUE, TRUE)))
  drawn <- neuromosaic:::.nm_surface_drawn_values(
    overlay, atlas, hemis = c("left", "right"), threshold = 5
  )
  # 30 sits on the medial wall (not drawn); 3 and -4 are below threshold.
  expect_setequal(drawn, c(6, 21, -8, 12))
  expect_false(any(drawn > 22.25))
  # Only the requested hemispheres count.
  expect_setequal(
    neuromosaic:::.nm_surface_drawn_values(overlay, atlas, "right", NULL, 5),
    c(-8, 12)
  )
  # Not a projected vertex list -> no surface values (caller falls back).
  expect_null(neuromosaic:::.nm_surface_drawn_values(NULL, atlas, "left"))
  # The projector lookup never errors, whatever neuroatlas provides.
  expect_no_error(neuromosaic:::.nm_project_surface_overlay(
    NULL, list(), "avg", "thickness", "linear", "mean", NULL, 0
  ))
})
