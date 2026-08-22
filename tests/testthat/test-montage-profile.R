make_grouped_profile_manifest <- function() {
  data.frame(
    analysis_id = rep("faces", 4L),
    map_id = c("faces_z", "faces_estimate", "faces_se", "faces_reliability"),
    path = paste0(c("z", "estimate", "se", "reliability"), ".nii.gz"),
    role = c("primary", rep("auxiliary", 3L)),
    quantity = c(
      "test_statistic", "estimate", "standard_error", "mylab:reliability"
    ),
    distribution = c("z", NA, NA, NA),
    label = c("Z statistic", "Effect estimate", "Standard error", "Reliability"),
    stringsAsFactors = FALSE
  )
}

test_that("canonical grouped manifests derive semantic defaults", {
  manifest <- make_grouped_profile_manifest()

  out <- validate_manifest(
    manifest,
    check_files = FALSE,
    check_overlays = FALSE
  )

  expect_identical(out$analysis_id, rep("faces", 4L))
  expect_identical(out$role, c("primary", rep("auxiliary", 3L)))
  expect_identical(out$quantity, manifest$quantity)
  expect_identical(out$distribution, c("z", NA_character_, NA_character_, NA_character_))
  expect_identical(out$signed, c(TRUE, TRUE, FALSE, NA))
})

test_that("legacy stat_kind values normalize without making FSL vocabulary canonical", {
  legacy <- data.frame(
    map_id = c("z_map", "t_map", "beta_map", "cope_map"),
    path = paste0(c("z", "t", "beta", "cope"), ".nii.gz"),
    stat_kind = c("z", "t", "beta", "cope"),
    df = c(NA, 20, NA, NA),
    signed = TRUE,
    threshold = c(3, 2, 0.1, 0.1),
    label = c("Z", "T", "Beta", "COPE"),
    stringsAsFactors = FALSE
  )

  out <- validate_manifest(legacy, check_files = FALSE, check_overlays = FALSE)

  expect_identical(
    out$quantity,
    c("test_statistic", "test_statistic", "estimate", "estimate")
  )
  expect_identical(out$distribution, c("z", "t", NA_character_, NA_character_))
  expect_identical(out$analysis_id, out$map_id)
  expect_true(all(out$role == "primary"))
})

test_that("group validation requires one unambiguous primary", {
  manifest <- make_grouped_profile_manifest()

  none <- manifest
  none$role <- "auxiliary"
  expect_error(
    validate_manifest(none, check_files = FALSE, check_overlays = FALSE),
    "exactly one primary"
  )

  two <- manifest
  two$role[2] <- "primary"
  expect_error(
    validate_manifest(two, check_files = FALSE, check_overlays = FALSE),
    "exactly one primary"
  )

  inferred <- manifest
  inferred$role <- NULL
  out <- validate_manifest(inferred, check_files = FALSE, check_overlays = FALSE)
  expect_identical(out$role, c("primary", rep("auxiliary", 3L)))

  inconsistent <- manifest
  inconsistent$analysis_label <- c("Faces", "Other", "Faces", "Faces")
  expect_error(
    validate_manifest(inconsistent, check_files = FALSE,
                      check_overlays = FALSE),
    "analysis_label.*consistent"
  )
})

test_that("blank selector labels fall back to panel labels", {
  manifest <- make_grouped_profile_manifest()
  manifest$selector_label <- c("", NA, "SE", "Reliability")

  out <- validate_manifest(manifest, check_files = FALSE,
                           check_overlays = FALSE)

  expect_identical(
    out$selector_label,
    c("Z statistic", "Effect estimate", "SE", "Reliability")
  )
})

test_that("built-in profiles resolve display semantics and robust limits", {
  manifest <- validate_manifest(
    make_grouped_profile_manifest(),
    check_files = FALSE,
    check_overlays = FALSE
  )
  values <- list(
    c(-6, -3, 0, 3, 6),
    c(-100, -2, -1, 0, 1, 2, 100),
    c(0.1, 0.2, 0.3, 0.4, 50),
    c(-0.9, -0.2, 0.1, 0.8)
  )

  out <- resolve_montage_profiles(manifest, map_values = values)

  expect_identical(
    out$effective_display_mode,
    c("thresholded", "continuous", "continuous", "continuous")
  )
  expect_identical(
    out$effective_scale,
    c("diverging", "diverging", "sequential", "diverging")
  )
  expect_identical(
    out$effective_profile_source,
    c("builtin", "builtin", "builtin", "automatic")
  )
  expect_equal(out$effective_lower[[2]], -stats::quantile(abs(values[[2]]), 0.99,
                                                          names = FALSE))
  expect_equal(out$effective_upper[[2]], -out$effective_lower[[2]])
  expect_equal(out$effective_lower[[3]], 0)
  expect_equal(out$effective_upper[[3]], stats::quantile(values[[3]], 0.99,
                                                         names = FALSE))
  expect_true(all(is.finite(out$effective_lower)))
  expect_true(all(is.finite(out$effective_upper)))
})

test_that("custom profiles override automatic semantics without global state", {
  manifest <- make_grouped_profile_manifest()[4, , drop = FALSE]
  manifest$role <- "primary"
  manifest <- validate_manifest(manifest, check_files = FALSE,
                                check_overlays = FALSE)
  profile <- montage_map_profile(
    id = "mylab:reliability",
    label = "Split-half reliability",
    scale = "diverging",
    center = 0,
    domain = c(-1, 1),
    units = "correlation"
  )

  out <- resolve_montage_profiles(
    manifest,
    profiles = list(`mylab:reliability` = profile),
    map_values = list(c(-0.4, 0.2, 0.9))
  )

  expect_identical(out$effective_profile_source, "user")
  expect_identical(out$effective_profile_label, "Split-half reliability")
  expect_equal(out$effective_lower, -1)
  expect_equal(out$effective_upper, 1)
  expect_identical(out$effective_units, "correlation")
})

test_that("automatic limits obey positive scaling metamorphically", {
  manifest <- make_grouped_profile_manifest()[4, , drop = FALSE]
  manifest$role <- "primary"
  manifest <- validate_manifest(manifest, check_files = FALSE,
                                check_overlays = FALSE)
  values <- c(-4, -1, 0, 2, 7)

  base <- resolve_montage_profiles(manifest, map_values = list(values))
  scaled <- resolve_montage_profiles(manifest, map_values = list(10 * values))

  expect_equal(scaled$effective_lower, 10 * base$effective_lower,
               tolerance = 1e-12)
  expect_equal(scaled$effective_upper, 10 * base$effective_upper,
               tolerance = 1e-12)
})

test_that("robust limits do not collapse on a sparse nonzero map", {
  manifest <- make_grouped_profile_manifest()[2, , drop = FALSE]
  manifest$role <- "primary"
  sparse <- c(rep(0, 999), 25)

  out <- resolve_montage_profiles(manifest, map_values = list(sparse))

  expect_equal(out$effective_lower, -25)
  expect_equal(out$effective_upper, 25)
})

test_that("analysis masks define the values used for automatic limits", {
  manifest <- make_grouped_profile_manifest()[2, , drop = FALSE]
  manifest$role <- "primary"
  manifest$mask <- I(list(c(TRUE, TRUE, FALSE)))
  manifest <- validate_manifest(manifest, check_files = FALSE,
                                check_overlays = FALSE)
  out <- resolve_montage_policy(
    manifest,
    stat_maps = list(c(-1, 1, 1000))
  )

  expect_equal(out$effective_lower, -1)
  expect_equal(out$effective_upper, 1)
})

test_that("custom quantities can explicitly reuse a built-in profile", {
  manifest <- make_grouped_profile_manifest()[4, , drop = FALSE]
  manifest$role <- "primary"
  manifest$profile <- "standard_error"

  out <- resolve_montage_profiles(
    manifest,
    map_values = list(c(0.1, 0.2, 0.4))
  )

  expect_identical(out$effective_profile_id, "standard_error")
  expect_identical(out$effective_profile_source, "builtin")
  expect_identical(out$effective_scale, "sequential")
  expect_equal(out$effective_lower, 0)
})

test_that("a custom bounded domain spanning zero implies a diverging scale", {
  manifest <- make_grouped_profile_manifest()[4, , drop = FALSE]
  manifest$role <- "primary"
  manifest$profile <- "mylab:correlation"
  profile <- montage_map_profile(
    "mylab:correlation",
    domain = c(-1, 1)
  )

  out <- resolve_montage_profiles(manifest, profiles = list(profile))

  expect_identical(out$effective_scale, "diverging")
  expect_equal(c(out$effective_lower, out$effective_upper), c(-1, 1))
})

test_that("negligible negative roundoff does not flip an automatic scale", {
  manifest <- make_grouped_profile_manifest()[4, , drop = FALSE]
  manifest$role <- "primary"

  out <- resolve_montage_profiles(
    manifest,
    map_values = list(c(-1e-12, 0, 0.5, 1))
  )

  expect_identical(out$effective_scale, "sequential")
})

test_that("profiles reject invalid domains and nonnegative quantities reject negatives", {
  expect_error(
    montage_map_profile("bad", domain = c(1, 0)),
    "increasing"
  )
  expect_error(
    montage_map_profile("bad-center", scale = "diverging", center = 1),
    "center = 0"
  )

  manifest <- make_grouped_profile_manifest()[3, , drop = FALSE]
  manifest$role <- "primary"
  manifest <- validate_manifest(manifest, check_files = FALSE,
                                check_overlays = FALSE)
  expect_error(
    resolve_montage_profiles(manifest, map_values = list(c(0.1, -0.01, 0.2))),
    "nonnegative"
  )

  custom <- make_grouped_profile_manifest()[4, , drop = FALSE]
  custom$role <- "primary"
  custom$profile <- "missing:profile"
  expect_error(
    resolve_montage_profiles(custom, map_values = list(c(0, 1))),
    "Unknown montage map profile"
  )

  first <- montage_map_profile("mylab:first")
  second <- montage_map_profile("mylab:second")
  expect_error(
    resolve_montage_profiles(
      custom,
      profiles = list(`mylab:second` = first, other = second),
      map_values = list(c(0, 1))
    ),
    "names and ids must be unique"
  )

  bad_mode <- custom
  bad_mode$profile <- NULL
  bad_mode$display_mode <- "sometimes"
  expect_error(
    validate_manifest(bad_mode, check_files = FALSE, check_overlays = FALSE),
    "display_mode.*must be one of"
  )

  bad_limits <- custom
  bad_limits$profile <- NULL
  bad_limits$lower <- 2
  bad_limits$upper <- 1
  expect_error(
    validate_manifest(bad_limits, check_files = FALSE,
                      check_overlays = FALSE),
    "display limits must be increasing"
  )

  outside_domain <- manifest
  outside_domain$lower <- -1
  outside_domain$upper <- 1
  expect_error(
    resolve_montage_profiles(
      outside_domain,
      map_values = list(c(0.1, 0.2, 0.3))
    ),
    "within the profile domain"
  )

  asymmetric <- make_grouped_profile_manifest()[2, , drop = FALSE]
  asymmetric$role <- "primary"
  asymmetric$lower <- -1
  asymmetric$upper <- 2
  expect_error(
    resolve_montage_profiles(asymmetric, map_values = list(c(-1, 2))),
    "symmetric around center"
  )

  signed_sequential <- custom
  signed_sequential$profile <- NULL
  signed_sequential$signed <- TRUE
  signed_sequential$scale <- "sequential"
  expect_error(
    resolve_montage_profiles(
      signed_sequential,
      map_values = list(c(0, 1))
    ),
    "sequential scale conflicts with signed = TRUE"
  )
})

test_that("analysis groups preserve primary-first order and membership", {
  manifest <- validate_manifest(
    make_grouped_profile_manifest()[c(3, 1, 4, 2), ],
    check_files = FALSE,
    check_overlays = FALSE
  )

  groups <- neuromosaic:::.montage_analysis_groups(manifest)

  expect_named(groups, "faces")
  expect_identical(groups$faces$primary_map_id, "faces_z")
  expect_identical(groups$faces$map_ids[[1]], "faces_z")
  expect_setequal(groups$faces$map_ids, manifest$map_id)
})

test_that("policy thresholds test statistics but leaves auxiliary maps continuous", {
  manifest <- make_grouped_profile_manifest()[1:3, ]
  out <- resolve_montage_policy(
    manifest,
    montage_policy(p = 0.005),
    stat_maps = list(c(-4, 0, 4), c(-2, 0, 2), c(0.1, 0.2, 0.3))
  )

  expect_equal(out$effective_threshold[[1]], stats::qnorm(1 - 0.005 / 2))
  expect_true(all(is.na(out$effective_threshold[2:3])))
  expect_identical(
    out$effective_display_mode,
    c("thresholded", "continuous", "continuous")
  )
})

test_that("auxiliary support defaults to analysis and primary masking is explicit", {
  manifest <- make_grouped_profile_manifest()[1:2, ]
  values <- list(c(-4, -1, 0, 1, 4), c(-2, -1, 0, 1, 2))
  resolved <- resolve_montage_policy(
    manifest,
    montage_policy(p = 0.005),
    stat_maps = values
  )
  resolved$effective_threshold[[1]] <- 3

  default_masks <- neuromosaic:::.montage_render_support_masks(
    resolved, values
  )
  expect_null(default_masks[[2]])

  resolved$effective_support[[2]] <- "primary"
  primary_masks <- neuromosaic:::.montage_render_support_masks(
    resolved, values
  )
  expect_identical(primary_masks[[2]], c(TRUE, FALSE, FALSE, FALSE, TRUE))

  resolved$mask <- I(list(
    c(TRUE, FALSE, FALSE, FALSE, TRUE),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  ))
  masked_primary <- neuromosaic:::.montage_render_support_masks(
    resolved, values
  )
  expect_identical(masked_primary[[2]], c(TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("compatible continuous maps can share a robust display scale", {
  manifest <- data.frame(
    analysis_id = c("a", "a", "a"),
    map_id = c("z", "estimate_a", "estimate_b"),
    role = c("primary", "auxiliary", "auxiliary"),
    quantity = c("test_statistic", "estimate", "estimate"),
    distribution = c("z", NA, NA),
    units = c("z", "percent signal", "percent signal"),
    label = c("Z", "Estimate A", "Estimate B"),
    stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(function(row) 1, function(row) 2,
                            function(row) 3))
  values <- list(c(-4, 4), c(-1, 1), c(-8, 8))
  profiled <- resolve_montage_profiles(manifest, map_values = values)
  policy <- montage_policy(cap_within = "analysis_id")
  resolved <- resolve_montage_policy(profiled, policy, stat_maps = values)
  shared <- neuromosaic:::.apply_montage_shared_profile_limits(
    resolved, policy
  )

  expect_identical(shared$cap_key[[2]], shared$cap_key[[3]])
  expect_equal(shared$effective_lower[[2]], shared$effective_lower[[3]])
  expect_equal(shared$effective_upper[[2]], shared$effective_upper[[3]])
  expect_false(identical(shared$cap_key[[1]], shared$cap_key[[2]]))
})

test_that("shared fixed caps cannot escape a bounded profile domain", {
  manifest <- data.frame(
    analysis_id = c("a", "a"),
    map_id = c("p1", "p2"),
    role = c("primary", "auxiliary"),
    quantity = c("probability", "probability"),
    label = c("Probability 1", "Probability 2"),
    stringsAsFactors = FALSE
  )
  manifest$recipe <- I(list(function(row) 1, function(row) 2))
  values <- list(c(0.1, 0.5), c(0.2, 0.9))
  profiled <- resolve_montage_profiles(manifest, map_values = values)
  policy <- montage_policy(cap_within = "analysis_id", cap = 10)
  resolved <- resolve_montage_policy(profiled, policy, stat_maps = values)

  shared <- neuromosaic:::.apply_montage_shared_profile_limits(
    resolved, policy
  )

  expect_equal(shared$effective_lower, c(0, 0))
  expect_equal(shared$effective_upper, c(1, 1))
})
