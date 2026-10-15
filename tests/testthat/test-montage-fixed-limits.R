test_that("fixed thresholded profiles survive every report scale consumer", {
  inputs <- make_toy_cluster_report_inputs()
  limits <- c(100, 17, 1)
  maxima <- c(35, 12, 1)
  ids <- c("prevalence", "coverage", "reference")
  maps <- lapply(maxima, function(top) {
    a <- array(0, dim(inputs$stat_map))
    a[2:4, 2:4, 2:4] <- top
    neuroim2::NeuroVol(a, neuroim2::space(inputs$stat_map))
  })
  manifest <- data.frame(map_id = ids, quantity = ids, label = ids,
                         threshold = c(1, 1, 0.5), tail = "positive",
                         signed = FALSE, min_cluster_size = 1)
  manifest$stat_map <- maps
  profiles <- lapply(seq_along(ids), function(i) {
    montage_map_profile(ids[[i]], display_mode = "thresholded",
                        scale = "sequential", palette_family = "inferno",
                        limits = c(0, limits[[i]]))
  })
  # Exercise the real report scale, static key and scene builders; only the
  # expensive cortical geometry renderer is replaced with a blank plot.
  local_mocked_bindings(plot_brain = function(...) ggplot2::ggplot(),
                        .package = "neuroatlas")
  directory <- withr::local_tempdir()
  output <- file.path(directory, "mixed.qmd")
  render_montage_report(
    manifest, output_file = output, profiles = profiles,
    policy = montage_policy(cap = 50), bg = inputs$stat_map,
    surfatlas = make_toy_surfatlas(),
    surface_args = list(render_device = "png"),
    volume_args = list(zlevels = 3L),
    interactive = montage_interactive(assets = "bundle"),
    image_width = 400, image_height = 300, image_res = 72
  )
  rd <- readRDS(sub("\\.qmd$", "_report-data.rds", output))
  expect_true(all(rd$manifest$effective_limits_fixed))
  expect_length(unique(rd$manifest$cap_key), 3)
  serialized <- jsonlite::fromJSON(rd$interactive$scene_json, simplifyVector = FALSE)
  exported <- unlist(lapply(serialized$analyses, `[[`, "maps"), recursive = FALSE)
  for (i in seq_along(ids)) {
    panel <- rd$panels[[ids[[i]]]]
    expect_equal(panel$volume$limits, c(0, limits[[i]]))
    expect_equal(panel$surface$limits, c(0, limits[[i]]))
    expect_equal(panel$volume$cap, limits[[i]])
    expect_equal(panel$surface$cap, limits[[i]])
    row <- rd$manifest[i, , drop = FALSE]
    volume <- .montage_volume_scene_map(row, panel$volume, "fixture")
    expect_equal(volume$display$limits, c(0, limits[[i]]))
    exported_map <- Filter(function(x) identical(x$map_id, ids[[i]]), exported)[[1]]
    expect_equal(unlist(exported_map$display$limits), c(0, limits[[i]]))
    surface <- .montage_surface_scene_layer(
      row, panel, list(left = c(0, maxima[[i]]), right = c(0, maxima[[i]])), list()
    )
    expect_equal(surface$limits, c(0, limits[[i]]))
    expect_match(panel$summary_alt, paste0("0 to ", limits[[i]]), fixed = TRUE)
    expect_identical(as.numeric(manifest$stat_map[[i]]), as.numeric(maps[[i]]))
  }
})

test_that("different fixed ranges never share a cap or get widened", {
  manifest <- data.frame(map_id = c("fixed_a", "fixed_b", "auto"),
                         quantity = "estimate", label = "Effect",
                         lower = c(-100, -17, NA), upper = c(100, 17, NA))
  manifest$recipe <- rep(list(function(row) 1), 3)
  values <- rep(list(c(-2, 0, 2)), 3)
  policy <- montage_policy(cap_within = "quantity", cap = 50)
  resolved <- resolve_montage_policy(manifest, policy, stat_maps = values)
  expect_identical(resolved$effective_limits_fixed, c(TRUE, TRUE, FALSE))
  expect_length(unique(resolved$cap_key), 3)
  shared <- .apply_montage_shared_profile_limits(resolved, policy)
  expect_equal(shared$effective_upper[1:2], c(100, 17))
  caps <- .montage_shared_caps(shared, values, policy)
  expect_equal(unname(unlist(caps)), c(100, 17, 50))
})
