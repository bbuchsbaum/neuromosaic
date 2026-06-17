test_that("surf_montage writes PNG with supplied projection and clipped cap", {
  inputs <- make_toy_cluster_report_inputs()
  output_file <- tempfile("surface-montage-", fileext = ".png")
  captured <- new.env(parent = emptyenv())

  projection <- list(
    overlay = list(
      lh = c(0, 4.5, -6.5, NA_real_),
      rh = c(8, 0, NA_real_, -3.25)
    ),
    meta = list(
      surface_space = "fsLR-32k",
      hemis = list(
        lh = list(target_vertices = 4L, projected_vertices = 4L,
                  finite_vertices = 3L),
        rh = list(target_vertices = 4L, projected_vertices = 4L,
                  finite_vertices = 3L)
      )
    )
  )
  plot_fun <- function(..., overlay, overlay_lim, overlay_threshold) {
    captured$overlay <- overlay
    captured$overlay_lim <- overlay_lim
    captured$overlay_threshold <- overlay_threshold
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }

  result <- surf_montage(
    stat = inputs$stat_map,
    surfatlas = make_toy_surfatlas(),
    output_file = output_file,
    threshold = 3,
    cap = 4,
    projection = projection,
    plot_fun = plot_fun,
    width = 320,
    height = 220,
    res = 72
  )

  expect_s3_class(result, "surf_montage_result")
  expect_true(file.exists(result$image))
  expect_gt(file.info(result$image)$size, 0)
  expect_identical(result$surface_space, "fsLR-32k")
  expect_equal(result$threshold, 3)
  expect_equal(result$cap, 4)
  expect_gt(result$n_suprathreshold, 0)
  expect_equal(captured$overlay_lim, c(-4, 4))
  expect_equal(captured$overlay_threshold, 3)
  expect_lte(max(abs(unlist(captured$overlay)), na.rm = TRUE), 4)
  expect_lte(max(abs(unlist(result$overlay)), na.rm = TRUE), 4)
  expect_s3_class(result$diagnostics$hemi, "data.frame")
})

test_that("surf_montage rejects invalid inputs and empty overlays", {
  inputs <- make_toy_cluster_report_inputs()
  projection <- list(
    overlay = list(lh = c(0, 1), rh = c(0, 1)),
    meta = list(surface_space = "fsLR-32k", hemis = list())
  )
  plot_fun <- function(...) {
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }

  expect_error(
    surf_montage(
      stat = inputs$stat_map,
      surfatlas = list(ids = 1L),
      output_file = tempfile(fileext = ".png"),
      threshold = 3,
      projection = projection,
      plot_fun = plot_fun
    ),
    "must inherit from class 'surfatlas'"
  )

  expect_error(
    surf_montage(
      stat = inputs$stat_map,
      surfatlas = make_toy_surfatlas(),
      output_file = tempfile(fileext = ".png"),
      threshold = 0,
      projection = projection,
      plot_fun = plot_fun
    ),
    "positive number"
  )

  expect_error(
    surf_montage(
      stat = inputs$stat_map,
      surfatlas = make_toy_surfatlas(),
      output_file = tempfile(fileext = ".png"),
      threshold = 100,
      projection = projection,
      plot_fun = plot_fun
    ),
    "No finite suprathreshold voxels"
  )
})

test_that("surf_montage delegates projection to plot_brain without a hook", {
  inputs <- make_toy_cluster_report_inputs()
  captured <- new.env(parent = emptyenv())
  plot_fun <- function(..., overlay) {
    captured$overlay <- overlay
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }
  output_file <- tempfile("surface-builtin-", fileext = ".png")

  res <- surf_montage(
    stat = inputs$stat_map,
    surfatlas = make_toy_surfatlas(),
    output_file = output_file,
    threshold = 3,
    cap = 5,
    plot_fun = plot_fun,
    width = 320, height = 220, res = 72
  )

  # The raw statistic volume is handed to plot_brain, which projects it itself
  # (rather than a pre-projected lh/rh list from the broken manual path).
  expect_true(inherits(captured$overlay, "NeuroVol"))
  expect_identical(res$diagnostics$projection, "plot_brain")
  expect_null(res$overlay)
  expect_equal(res$cap, 5)
  expect_gt(res$n_suprathreshold, 0)
})

test_that("surf_montage drops wrong-signed voxels for one-sided tails", {
  inputs <- make_toy_cluster_report_inputs()  # +4.5 cluster and -5.5 cluster
  captured <- new.env(parent = emptyenv())
  plot_fun <- function(..., overlay) {
    captured$overlay <- overlay
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }

  surf_montage(
    stat = inputs$stat_map, surfatlas = make_toy_surfatlas(),
    output_file = tempfile(fileext = ".png"), threshold = 3, tail = "positive",
    plot_fun = plot_fun, width = 320, height = 220, res = 72
  )
  pos <- as.numeric(as.array(captured$overlay))
  expect_true(inherits(captured$overlay, "NeuroVol"))
  expect_false(any(pos[is.finite(pos)] < 0))   # no negative clusters leak in
  expect_true(any(pos[is.finite(pos)] > 3))     # positive suprathreshold kept

  surf_montage(
    stat = inputs$stat_map, surfatlas = make_toy_surfatlas(),
    output_file = tempfile(fileext = ".png"), threshold = 3, tail = "negative",
    plot_fun = plot_fun, width = 320, height = 220, res = 72
  )
  neg <- as.numeric(as.array(captured$overlay))
  expect_false(any(neg[is.finite(neg)] > 0))   # no positive clusters leak in
  expect_true(any(neg[is.finite(neg)] < -3))    # negative suprathreshold kept
})
