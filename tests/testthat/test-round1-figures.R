# Report figures: world-mm slice labels, central slice selection, display
# cluster extent shared by every view, and the report palette.

make_round1_space <- function(dims = c(12, 14, 16)) {
  neuroim2::NeuroSpace(
    dim = dims, spacing = c(2, 2, 2), origin = c(-11, -13, -20)
  )
}

# A positive blob of 27 voxels, a positive 2-voxel speck, and a negative blob.
make_round1_stat <- function() {
  sp <- make_round1_space()
  arr <- array(0, dim = dim(sp))
  arr[3:5, 3:5, 6:8] <- 6
  arr[10, 12, 12:13] <- 7
  arr[8:10, 3:5, 9:11] <- -6.5
  neuroim2::NeuroVol(arr, sp)
}

make_round1_background <- function() {
  sp <- make_round1_space()
  arr <- array(0, dim = dim(sp))
  arr[2:11, 2:13, 2:15] <- 100
  arr[5:8, 6:9, 7:10] <- 30 # a dark interior "ventricle"
  neuroim2::NeuroVol(arr, sp)
}

test_that("report montage labels slices in world millimetres", {
  stat <- make_round1_stat()
  bg <- make_round1_background()
  out <- stat_montage(bg, stat, threshold = 5, zlevels = c(6, 9, 12),
                      draw = FALSE)

  expect_s3_class(out$plot, "nm_volume_figure_spec")
  world_z <- neuroim2::grid_to_coord(
    neuroim2::space(stat), cbind(1, 1, c(6, 9, 12))
  )[, 3]
  expect_equal(world_z, c(-10, -4, 2))
  expect_identical(out$slice_labels, c("z = −10", "z = −4", "z = +2"))
  expect_equal(out$plot$world, world_z)
})

test_that("LAS data is labelled from the affine and drawn neurologically", {
  tr <- diag(c(-2, 2, 2, 1))
  tr[1:3, 4] <- c(5, -5, 30)
  sp <- neuroim2::NeuroSpace(c(6, 6, 6), spacing = c(2, 2, 2), trans = tr)
  arr <- array(0, dim = dim(sp))
  # Large i is negative x: the subject's left hemisphere.
  arr[5:6, 2:4, 2:4] <- 8
  stat <- neuroim2::NeuroVol(arr, sp)
  frame <- neuromosaic:::.nm_axis_frame(sp)
  expect_equal(neuromosaic:::.nm_axis_world(frame[[3L]], 3), 34)

  out <- stat_montage(stat, stat, threshold = 5, zlevels = 3, draw = FALSE)
  expect_identical(out$slice_labels, "z = +34")

  slice <- neuromosaic:::.nm_slice_image(arr, 3L, 3L, frame)
  expect_true(all(diff(slice$x) > 0))
  cols_with_signal <- which(colSums(slice$img > 0) > 0)
  # Subject left (negative x) lands on the image's left half.
  expect_true(all(slice$x[cols_with_signal] < 0))
  expect_true(all(cols_with_signal <= ncol(slice$img) / 2))
})

test_that("display slices are drawn from the central mass, not empty edges", {
  dims <- c(10, 10, 60)
  supra <- array(FALSE, dim = dims)
  supra[4:6, 4:6, 20:40] <- TRUE # the mass
  supra[5, 5, 1] <- TRUE # stray edge voxels
  supra[5, 5, 60] <- TRUE
  z <- neuromosaic:::.stat_montage_central_zlevels(supra, 3L, max_slices = 12L)

  expect_length(z, 12L)
  expect_false(any(z %in% c(1L, 60L)))
  expect_true(all(z >= 20L & z <= 40L))
  expect_true(all(diff(z) == diff(z)[[1L]]))
  # Every chosen slice shows signal.
  expect_true(all(vapply(z, function(k) any(supra[, , k]), logical(1))))
})

test_that("few occupied slices are all shown", {
  supra <- array(FALSE, dim = c(5, 5, 30))
  supra[2:3, 2:3, c(10, 11, 13)] <- TRUE
  expect_identical(
    neuromosaic:::.stat_montage_central_zlevels(supra, 3L, max_slices = 12L),
    c(10L, 11L, 13L)
  )
})

test_that("the display extent mask removes sub-k clusters with the manifest connectivity", {
  stat <- make_round1_stat()
  values <- as.numeric(stat)
  shown <- abs(values) >= 5
  excluded <- neuromosaic:::.montage_extent_excluded(
    values, dim(stat), shown, min_cluster_size = 5, connectivity = "18-connect"
  )
  arr <- array(excluded, dim = dim(stat))
  expect_equal(sum(excluded), 2L)
  expect_true(all(arr[10, 12, 12:13]))
  expect_false(any(arr[3:5, 3:5, 6:8]))
  expect_false(any(arr[8:10, 3:5, 9:11]))

  # A corner-touching pair is one cluster under 26- but two under 18-connectivity.
  corner <- array(0, dim = c(4, 4, 4))
  corner[1, 1, 1] <- 9
  corner[2, 2, 2] <- 9
  v <- as.numeric(corner)
  m <- v >= 5
  expect_equal(sum(neuromosaic:::.montage_extent_excluded(
    v, dim(corner), m, min_cluster_size = 2, connectivity = "26-connect"
  )), 0L)
  expect_equal(sum(neuromosaic:::.montage_extent_excluded(
    v, dim(corner), m, min_cluster_size = 2, connectivity = "18-connect"
  )), 2L)
  # Signs never merge.
  signed <- array(0, dim = c(4, 4, 4))
  signed[1, 1, 1] <- 9
  signed[2, 1, 1] <- -9
  sv <- as.numeric(signed)
  expect_equal(sum(neuromosaic:::.montage_extent_excluded(
    sv, dim(signed), abs(sv) >= 5, min_cluster_size = 2
  )), 2L)
  # No extent requested: nothing is excluded.
  expect_false(any(neuromosaic:::.montage_extent_excluded(
    values, dim(stat), shown, min_cluster_size = NULL
  )))
})

test_that("stat_montage hides sub-extent clusters but keeps the threshold count", {
  stat <- make_round1_stat()
  out <- stat_montage(stat, stat, threshold = 5, min_cluster_size = 5,
                      draw = FALSE)
  spec <- out$display_spec
  n_threshold <- sum(abs(as.numeric(stat)) >= 5)

  expect_equal(out$n_suprathreshold, n_threshold)
  expect_equal(out$n_display_voxels, n_threshold - 2L)
  expect_equal(out$n_extent_excluded, 2L)
  overlay <- as.array(out$overlay)
  expect_true(all(is.na(overlay[10, 12, 12:13])))
  expect_true(all(is.finite(overlay[3:5, 3:5, 6:8])))
  # Exported overlays hide the speck (NaN) but keep sub-threshold voxels.
  support <- array(spec$display_support_mask, dim = dim(stat))
  expect_false(any(support[10, 12, 12:13]))
  expect_true(support[1, 1, 1])
})

test_that("an extent that removes every cluster is an empty display, not an error", {
  stat <- make_round1_stat()
  out <- stat_montage(stat, stat, threshold = 5, min_cluster_size = 500,
                      draw = FALSE)
  expect_equal(out$n_display_voxels, 0L)
  expect_gt(out$n_suprathreshold, 0L)
  expect_error(
    stat_montage(stat, stat, threshold = 50, draw = FALSE),
    "No finite suprathreshold voxels"
  )
})

test_that("the static surface projects the extent-filtered map", {
  stat <- make_round1_stat()
  captured <- NULL
  draw <- function(...) {
    captured <<- list(...)
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }
  res <- surf_montage(
    stat = stat, surfatlas = make_toy_surfatlas(),
    output_file = tempfile(fileext = ".png"), threshold = 5, cap = 8,
    min_cluster_size = 5, plot_fun = draw, width = 200, height = 160,
    res = 72
  )
  payload <- as.array(captured$overlay)
  expect_true(all(is.na(payload[10, 12, 12:13])))
  expect_equal(payload[4, 4, 7], 6)
  # Sub-threshold values still reach the projection.
  expect_equal(payload[1, 1, 1], 0)
  # Report palette: ramp stops for neurosurf's split-half rasterizer, solid.
  expect_identical(
    captured$overlay_palette, neuromosaic:::.nm_threshold_stops()
  )
  expect_equal(captured$overlay_alpha_ramp, 0)
  expect_false(captured$colorbar)
})

test_that("the report palette is threshold-saturated and shared by every view", {
  cols <- nm_palette_colors(c(-20, 20), threshold = 5, n = 401L)
  values <- seq(-20, 20, length.out = 401L)
  expect_identical(cols[values == 5], "#B40426")
  expect_identical(cols[values == -5], "#1D4E9E")
  expect_identical(cols[[401L]], "#FFE08A")
  expect_identical(cols[[1L]], "#A6E3F5")
  expect_true(all(cols[abs(values) < 5] == "#C9CDD0"))
  # Never white anywhere in the drawn ramps.
  drawn <- grDevices::col2rgb(cols[abs(values) >= 5])
  expect_true(all(colMeans(drawn) < 235))

  expect_identical(
    neuromosaic:::.montage_volume_palette("diverging", "diverging"), "nm-signed"
  )
  expect_identical(
    neuromosaic:::.montage_surface_palette("diverging", "diverging"),
    "nm-signed"
  )
  expect_true(neuromosaic:::.nm_is_palette("nm-signed"))
  expect_error(nm_palette_colors(c(-1, 1), palette = "vik"), "palette")
})

test_that("the volume scene carries the report palette and cluster rules", {
  row <- data.frame(
    map_id = "m", quantity = "z", label = "Map", stringsAsFactors = FALSE
  )
  metadata <- list(
    display_mode = "thresholded", scale = "diverging", center = 0,
    limits = c(-12, 12), threshold = 4, tail = "two_sided",
    palette = "nm-signed", alpha = 0.9, alpha_mode = "binary",
    support = "analysis", units = NULL, status = "ready",
    n_display_voxels = 10L, legend_title = "z",
    connectivity = "26-connect", min_cluster_size = 15L
  )
  scene_map <- neuromosaic:::.montage_volume_scene_map(row, metadata, "a1")
  expect_identical(scene_map$display$connectivity, "26-connect")
  expect_identical(scene_map$display$min_cluster_size, 15L)
  expect_identical(
    scene_map$display$palette_colors,
    nm_palette_colors(c(-12, 12), threshold = 4)
  )
})

test_that("the figure canvas matches the page colour and draws the key", {
  skip_if_not_installed("png")
  stat <- make_round1_stat()
  bg <- make_round1_background()
  out <- stat_montage(bg, stat, threshold = 5, min_cluster_size = 5,
                      draw = FALSE)
  path <- tempfile(fileext = ".png")
  neuromosaic:::.nm_write_figure(out$plot, path)
  img <- png::readPNG(path)
  corner <- round(img[2, 2, 1:3] * 255)
  expect_equal(corner, c(0xFB, 0xFB, 0xF8))
  expect_equal(dim(img)[[2L]], out$plot$width)
  expect_equal(dim(img)[[1L]], out$plot$height)
})

test_that("the glass-brain summary is written on the page canvas", {
  skip_if_not_installed("png")
  stat <- make_round1_stat()
  bg <- make_round1_background()
  values <- as.numeric(stat)
  path <- tempfile(fileext = ".png")
  neuromosaic:::.nm_glass_brain(
    bg, values, threshold = 5, limits = c(-8, 8), palette = "nm-signed",
    tail = "two_sided", continuous = FALSE, path = path, width_px = 600
  )
  img <- png::readPNG(path)
  expect_equal(dim(img)[[2L]], 600L)
  expect_equal(round(img[1, 1, 1:3] * 255), c(0xFB, 0xFB, 0xF8))
})

test_that("enclosed low-intensity holes are filled in the underlay mask", {
  m <- matrix(FALSE, 7, 7)
  m[2:6, 2:6] <- TRUE
  m[4, 4] <- FALSE
  filled <- neuromosaic:::.nm_fill_holes(m)
  expect_true(filled[4, 4])
  expect_false(filled[1, 1])
})
