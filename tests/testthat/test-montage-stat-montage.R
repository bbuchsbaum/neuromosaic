test_that("stat_montage returns plot data and render metadata", {
  inputs <- make_toy_cluster_report_inputs()

  out <- stat_montage(
    inputs$stat_map,
    inputs$stat_map,
    threshold = 3,
    title = "Toy montage",
    draw = FALSE
  )

  expect_s3_class(out, "stat_montage_result")
  expect_false(is.null(out$plot))
  expect_gt(out$n_suprathreshold, 0)
  expect_equal(out$threshold, 3)
  expect_identical(out$tail, "two_sided")
  expect_identical(out$requested_style, "report")
  style_choices <- eval(formals(neuroim2::plot_overlay)$style)
  expect_true(
    "report" %in% style_choices,
    info = "neuroim2::plot_overlay() must expose style = 'report'"
  )
  expect_identical(out$style, "report")

  alpha_choices <- eval(formals(neuroim2::plot_overlay)$ov_alpha_mode)
  expected_alpha <- if ("soft" %in% alpha_choices) "soft" else "proportional"
  expect_identical(out$alpha_mode, expected_alpha)
})

test_that("stat_montage fails loudly on empty overlays", {
  inputs <- make_toy_cluster_report_inputs()

  expect_error(
    stat_montage(inputs$stat_map, inputs$stat_map, threshold = 100, draw = FALSE),
    "No finite suprathreshold voxels"
  )
})

test_that("stat_montage masks by tail before plotting", {
  inputs <- make_toy_cluster_report_inputs()

  positive <- stat_montage(
    inputs$stat_map,
    inputs$stat_map,
    threshold = 3,
    tail = "positive",
    draw = FALSE
  )
  pos_values <- as.numeric(positive$overlay)
  expect_true(all(pos_values[is.finite(pos_values)] >= 3))

  negative <- stat_montage(
    inputs$stat_map,
    inputs$stat_map,
    threshold = 3,
    tail = "negative",
    draw = FALSE
  )
  neg_values <- as.numeric(negative$overlay)
  expect_true(all(neg_values[is.finite(neg_values)] <= -3))
})

test_that("stat_montage clips overlays to a shared cap", {
  inputs <- make_toy_cluster_report_inputs()

  out <- stat_montage(
    inputs$stat_map,
    inputs$stat_map,
    threshold = 3,
    cap = 4,
    draw = FALSE
  )
  values <- as.numeric(out$overlay)

  expect_equal(out$cap, 4)
  expect_lte(max(abs(values), na.rm = TRUE), 4)
})

test_that("stat_montage validates inputs and can restamp via prepare_overlay", {
  inputs <- make_toy_cluster_report_inputs()
  expect_error(
    stat_montage(inputs$stat_map, inputs$stat_map, threshold = -1, draw = FALSE),
    "positive number"
  )

  stat_space <- neuroim2::NeuroSpace(
    dim = dim(neuroim2::space(inputs$stat_map)),
    spacing = c(2, 2, 2),
    origin = c(100, 100, 100)
  )
  stat <- neuroim2::NeuroVol(as.array(inputs$stat_map), stat_space)
  out <- stat_montage(
    inputs$stat_map,
    stat,
    threshold = 3,
    on_mismatch = "restamp",
    draw = FALSE
  )

  expect_identical(out$overlay_action, "restamped")
  expect_true(.same_neuro_space(neuroim2::space(out$overlay),
                                neuroim2::space(inputs$stat_map)))
})

test_that("stat_montage accepts the ramp alpha mode and validates alpha_gamma", {
  inputs <- make_toy_cluster_report_inputs()

  out <- stat_montage(
    inputs$stat_map, inputs$stat_map, threshold = 3,
    ov_alpha_mode = "ramp", draw = FALSE
  )
  alpha_choices <- eval(formals(neuroim2::plot_overlay)$ov_alpha_mode)
  expected <- if ("ramp" %in% alpha_choices) "ramp" else "proportional"
  expect_identical(out$alpha_mode, expected)

  # alpha_gamma is forwarded only when supported, but is always validated.
  expect_error(
    stat_montage(inputs$stat_map, inputs$stat_map, threshold = 3,
                 alpha_gamma = -1, draw = FALSE),
    "alpha_gamma"
  )
  expect_silent(
    stat_montage(inputs$stat_map, inputs$stat_map, threshold = 3,
                 ov_alpha_mode = "soft", alpha_gamma = 2, draw = FALSE)
  )
})
