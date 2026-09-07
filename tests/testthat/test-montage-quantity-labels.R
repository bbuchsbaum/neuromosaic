test_that("quantity legends have one title and units contract", {
  expect_identical(.montage_legend()$text, "Statistic")
  expect_identical(.montage_legend("Delay coefficient", "% signal change")$text,
                   "Delay coefficient (% signal change)")
  expect_error(.montage_legend(NA_character_), "legend_title")
  expect_error(.montage_legend("Effect", c("a", "b")), "units")

  inputs <- make_toy_cluster_report_inputs()
  manifest <- data.frame(map_id = "effect", quantity = "estimate",
                         label = "Analysis title", legend_title = "Semipartial r")
  manifest$stat_map <- list(inputs$stat_map)
  profile <- montage_map_profile("estimate", label = "Coefficient", units = "a.u.")
  resolved <- resolve_montage_profiles(manifest, profiles = list(profile))
  expect_identical(resolved$effective_profile_label, "Semipartial r")
  expect_identical(resolved$effective_units, "a.u.")
  expect_identical(resolved$label, "Analysis title")
  manifest$legend_title <- NULL
  expect_identical(resolve_montage_profiles(manifest, profiles = list(profile))$
                     effective_profile_label, "Coefficient")
})

test_that("direct renderers carry custom quantity legends into real plots", {
  inputs <- make_toy_cluster_report_inputs()
  title <- "Delay coefficient for the full observation interval"
  out <- stat_montage(inputs$stat_map, inputs$stat_map, threshold = 3,
                      legend_title = title, units = "% signal change", draw = FALSE)
  expect_identical(out$display_spec$legend_title, title)
  expect_identical(out$display_spec$units, "% signal change")
  # Inspect the assembled grobs, not a mock of the renderer's arguments.
  labels <- function(g) {
    c(if (inherits(g, "text")) as.character(g$label),
      unlist(lapply(g$grobs, labels)), unlist(lapply(g$children, labels)))
  }
  text <- labels(grid::grid.grabExpr(print(out$plot)))
  expect_true(paste0(title, " (% signal change)") %in% gsub("[[:space:]]+", " ", text))
  expect_false(any(grepl("activation", text, ignore.case = TRUE)))

  # Explicit direct-render units override profile units in display metadata.
  no_units <- stat_montage(inputs$stat_map, inputs$stat_map, threshold = 3,
                           legend_title = "Custom quantity", units = NULL,
                           draw = FALSE)
  row <- resolve_montage_profiles(data.frame(
    map_id = "custom", path = "unused.nii", quantity = "estimate",
    label = "Analysis title", units = "profile units"
  ))
  metadata <- .montage_volume_display_metadata(
    no_units$display_spec, row, no_units$display_spec$support_mask
  )
  expect_null(metadata$units)
  expect_identical(metadata$legend_title, "Custom quantity")

  for (parcel in c(TRUE, FALSE)) {
    captured <- NULL
    args <- list(surfatlas = make_toy_surfatlas(), threshold = 3,
                 output_file = tempfile(fileext = ".png"),
                 legend_title = title, units = "% signal change",
                 width = 320, height = 220, res = 72,
                 plot_fun = function(...) {
                   captured <<- list(...)
                   ggplot2::ggplot() + ggplot2::geom_blank()
                 })
    if (parcel) args$vals <- c(4.5, -5.5) else args$stat <- inputs$stat_map
    result <- do.call(surf_montage, args)
    expect_identical(captured$colorbar_title, paste0(title, " (% signal change)"))
    if (!parcel) expect_identical(captured$overlay_title, captured$colorbar_title)
    expect_identical(result$render$legend_title, title)
    unlink(result$image)
  }
})

test_that("report legends preserve profile semantics across render and cache paths", {
  inputs <- make_toy_cluster_report_inputs()
  directory <- withr::local_tempdir()
  manifest <- data.frame(map_id = "effect", quantity = "estimate",
                         label = "Analysis title", legend_title = "Delay coefficient",
                         units = "% signal change")
  manifest$stat_map <- list(inputs$stat_map)
  captured <- NULL
  plot_fun <- function(...) {
    captured <<- list(...)
    ggplot2::ggplot() + ggplot2::geom_blank()
  }
  local_mocked_bindings(plot_brain = plot_fun, .package = "neuroatlas")
  for (pass in 1:2) {
    output <- file.path(directory, paste0("report-", pass, ".qmd"))
    render_montage_report(manifest, output_file = output,
                         bg = inputs$stat_map, surfatlas = make_toy_surfatlas(),
                         image_dir = file.path(directory, "images"),
                         image_width = 600, image_height = 450, image_res = 72)
    rd <- readRDS(sub("\\.qmd$", "_report-data.rds", output))
    panel <- rd$panels$effect
    expect_identical(panel$volume$legend_title, "Delay coefficient")
    expect_identical(panel$surface$legend_title, "Delay coefficient")
    expect_identical(panel$surface$units, "% signal change")
    map <- .montage_volume_scene_map(rd$manifest, panel$volume, "fixture")
    expect_identical(map$legend_title, "Delay coefficient")
    expect_identical(map$label, "Analysis title")
    layer <- .montage_surface_scene_layer(
      rd$manifest, panel, list(left = c(1, 2), right = c(3, 4)), list()
    )
    expect_identical(layer$legend$title, "Delay coefficient")
    expect_identical(layer$legend$units, "% signal change")
    if (pass == 2) expect_true(panel$surface$diagnostics$cache_hit)
  }
  expect_identical(captured$colorbar_title, "Delay coefficient (% signal change)")
})
