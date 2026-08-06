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

  expect_warning(
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
    ),
    "deprecated"
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
  expect_identical(result$render$style, "stat_publication")
  expect_identical(result$render$colorbar_source, "overlay")
  expect_identical(result$render$limits, c(-4, 4))
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

test_that("surf_montage renders a base panel for an all-zero map under empty='warning' (#8)", {
  inputs <- make_toy_cluster_report_inputs()
  zero_vol <- neuroim2::NeuroVol(
    array(0, dim(inputs$stat_map)), neuroim2::space(inputs$stat_map)
  )
  captured <- new.env(parent = emptyenv())
  plot_fun <- function(..., overlay_lim) {
    captured$overlay_lim <- overlay_lim
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }
  out <- tempfile("surface-empty-", fileext = ".png")

  expect_warning(
    result <- surf_montage(
      stat = zero_vol, surfatlas = make_toy_surfatlas(), output_file = out,
      threshold = 3, empty = "warning", plot_fun = plot_fun,
      width = 320, height = 220, res = 72
    ),
    "No finite suprathreshold voxels"
  )

  expect_s3_class(result, "surf_montage_result")
  expect_true(file.exists(result$image))
  expect_equal(result$n_suprathreshold, 0)
  # The default cap for an empty overlay must stay positive so overlay_lim is
  # finite and the base panel renders (not NA, which would break plot_brain).
  expect_true(all(is.finite(captured$overlay_lim)))
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

test_that("continuous montage requests publication semantics and overlay legend", {
  inputs <- make_toy_cluster_report_inputs()
  captured <- new.env(parent = emptyenv())
  plot_fun <- function(...) {
    captured$args <- list(...)
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }

  result <- surf_montage(
    stat = inputs$stat_map,
    surfatlas = make_toy_surfatlas(),
    output_file = tempfile("surface-publication-", fileext = ".png"),
    threshold = 3,
    cap = 5,
    plot_fun = plot_fun,
    width = 320,
    height = 220,
    res = 72,
    render_device = "png"
  )

  expect_identical(captured$args$style, "stat_publication")
  expect_identical(captured$args$static_backend, "cpu")
  expect_identical(captured$args$colorbar_source, "overlay")
  expect_identical(captured$args$overlay_title, "Statistic")
  expect_identical(captured$args$overlay_alpha, 0.85)
  expect_equal(captured$args$overlay_lim, c(-5, 5))
  expect_identical(captured$args$overlay_interpolation, "linear")
  expect_identical(captured$args$overlay_sampling, "thickness")
  expect_identical(captured$args$overlay_aggregate, "mean")
  expect_equal(captured$args$overlay_depth, seq(0.1, 0.9, length.out = 5L))
  expect_identical(result$diagnostics$projection_interpolation, "linear")
  expect_identical(result$diagnostics$projection_aggregate, "mean")
  expect_identical(result$render$device, "png")
  expect_identical(result$render$backend, "cpu_barycentric")
  expect_equal(result$render[c("width", "height", "res")],
               list(width = 320, height = 220, res = 72))
})

test_that("PDF output uses rasterized panels with a vector-capable device", {
  skip_if_not(capabilities("cairo"), "Cairo PDF is unavailable")
  surfatlas <- make_toy_surfatlas()
  plot_fun <- function(...) {
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_text(label = "vector label")
  }
  file <- tempfile(fileext = ".pdf")
  result <- surf_montage(
    vals = c(4, -4), surfatlas = surfatlas, output_file = file,
    threshold = 3, plot_fun = plot_fun, width = 600, height = 375,
    res = 150, render_device = "auto"
  )
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 100)
  expect_identical(result$render$device, "cairo_pdf")
})

test_that("surf_montage renders parcel-valued vals without projection (#7)", {
  surfatlas <- make_toy_surfatlas()
  captured <- new.env(parent = emptyenv())
  plot_fun <- function(surfatlas, vals, lim, palette, ...) {
    captured$vals <- vals
    captured$lim <- lim
    captured$palette <- palette
    captured$has_overlay <- "overlay" %in% names(list(...))
    ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  }

  res <- surf_montage(
    vals = c(`1` = 4.5, `2` = -5.5),
    surfatlas = surfatlas,
    output_file = tempfile("surface-parcels-", fileext = ".png"),
    threshold = 3,
    cap = 6,
    plot_fun = plot_fun,
    width = 320,
    height = 220,
    res = 72
  )

  expect_equal(captured$vals, c(`1` = 4.5, `2` = -5.5))
  expect_equal(captured$lim, c(-6, 6))
  expect_identical(captured$palette, "vik")
  expect_false(captured$has_overlay)
  expect_identical(res$diagnostics$projection, "parcel_values")
  expect_equal(res$n_suprathreshold, 2)
  expect_equal(res$vals, captured$vals)

  expect_error(
    surf_montage(
      stat = make_toy_cluster_report_inputs()$stat_map,
      vals = c(1, 2),
      surfatlas = surfatlas,
      output_file = tempfile(fileext = ".png"),
      threshold = 3,
      plot_fun = plot_fun
    ),
    "exactly one"
  )
  expect_error(
    surf_montage(
      vals = c(1, 2, 3),
      surfatlas = surfatlas,
      output_file = tempfile(fileext = ".png"),
      threshold = 3,
      plot_fun = plot_fun
    ),
    "length 2"
  )
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
