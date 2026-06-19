make_montage_render_manifest <- function() {
  tmpdir <- tempfile("montage-render-")
  dir.create(tmpdir, recursive = TRUE)
  map_path <- file.path(tmpdir, "stat-map.nii.gz")
  writeLines("placeholder", map_path)

  data.frame(
    map_id = c("contrast_a_model_1", "contrast_a_model_2"),
    path = map_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    tail = "two_sided",
    connectivity = "18-connect",
    min_cluster_size = 10,
    contrast = "contrast-a",
    model = c("model-1", "model-2"),
    label = c("Contrast A - model 1", "Contrast A - model 2"),
    description = c("First panel.", "Second panel."),
    stringsAsFactors = FALSE
  )
}

test_that("render_montage_report writes qmd source and sidecar data", {
  manifest <- make_montage_render_manifest()
  qmd_out <- tempfile("montage-report-", fileext = ".qmd")

  result <- render_montage_report(
    manifest,
    output_file = qmd_out,
    title = "Fixture Montage",
    layout = c("contrast", "model")
  )

  expect_true(file.exists(result))

  sidecar <- sub("\\.qmd$", "_report-data.rds", result)
  expect_true(file.exists(sidecar))

  qmd <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_match(qmd, basename(sidecar), fixed = TRUE)
  expect_match(qmd, "expects the sidecar file", fixed = TRUE)
  expect_match(qmd, "montage_report_formatters", fixed = TRUE)

  rd <- readRDS(sidecar)
  expect_equal(nrow(rd$manifest), 2)
  expect_identical(rd$params$title, "Fixture Montage")
  expect_identical(rd$params$layout, c("contrast", "model"))
  expect_identical(names(rd$panels), manifest$map_id)
})

test_that("render_montage_report renders fixture HTML through Rmd template", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for render tests")

  manifest <- make_montage_render_manifest()
  html_out <- tempfile("montage-report-", fileext = ".html")

  result <- render_montage_report(
    manifest,
    output_file = html_out,
    title = "Fixture Montage",
    layout = c("contrast", "model"),
    quiet = TRUE
  )

  expect_true(file.exists(result))
  html <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_match(html, "Fixture Montage")
  expect_match(html, "Contrast A - model 1")
  expect_match(html, "Contrast A - model 2")
  expect_match(html, "contrast-a")
})

test_that("render_montage_report resolves labeller policy and volume panels", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-render-volume-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1",
    path = stat_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    contrast = "faces",
    model = "m1",
    stringsAsFactors = FALSE
  )
  qmd_out <- file.path(tmpdir, "volume-montage.qmd")

  result <- withCallingHandlers(
    render_montage_report(
      manifest,
      output_file = qmd_out,
      bg = inputs$stat_map,
      labeller = function(entities) {
        list(title = paste("Contrast", entities$contrast))
      },
      policy = montage_policy(layout = c("contrast", "model"),
                              cap_within = "contrast"),
      image_width = 700,
      image_height = 500,
      image_res = 72
    ),
    warning = function(w) {
      if (grepl("strings not representable in native encoding",
                conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
      stop(w)
    }
  )

  sidecar <- sub("\\.qmd$", "_report-data.rds", result)
  rd <- readRDS(sidecar)
  image_path <- rd$panels$faces_m1$volume_image

  expect_true(file.exists(result))
  expect_true(file.exists(image_path))
  expect_identical(rd$manifest$label, "Contrast faces")
  expect_true("effective_threshold" %in% names(rd$manifest))
  expect_identical(rd$params$layout, c("contrast", "model"))
  expect_s3_class(rd$params$policy, "montage_policy")
  expect_gt(rd$panels$faces_m1$volume$n_suprathreshold, 0)
})

test_that("render_montage_report adds surface panels with the shared cap", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )

  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-render-surface-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1",
    path = stat_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    contrast = "faces",
    model = "m1",
    label = "Faces M1",
    stringsAsFactors = FALSE
  )
  qmd_out <- file.path(tmpdir, "surface-montage.qmd")

  testthat::local_mocked_bindings(
    surf_montage = function(stat, surfatlas, output_file, threshold, tail,
                            signed, cap, width, height, res, title,
                            subtitle, ...) {
      writeLines("surface placeholder", output_file)
      structure(
        list(
          image = normalizePath(output_file, mustWork = FALSE),
          threshold = threshold,
          tail = tail,
          signed = signed,
          cap = cap,
          n_suprathreshold = 4L,
          surface_space = "fsLR-32k",
          diagnostics = list(
            hemi = data.frame(hemi = c("lh", "rh"), finite_vertices = c(2L, 2L))
          )
        ),
        class = "surf_montage_result"
      )
    },
    .package = "neuromosaic"
  )

  result <- render_montage_report(
    manifest,
    output_file = qmd_out,
    bg = inputs$stat_map,
    surfatlas = make_toy_surfatlas(),
    image_width = 700,
    image_height = 500,
    image_res = 72
  )

  sidecar <- sub("\\.qmd$", "_report-data.rds", result)
  rd <- readRDS(sidecar)
  panel <- rd$panels$faces_m1

  expect_true(file.exists(panel$volume_image))
  expect_true(file.exists(panel$surface_image))
  expect_equal(panel$surface$surface_space, "fsLR-32k")
  expect_equal(panel$surface$cap, panel$volume$cap)
  expect_gt(panel$surface$n_suprathreshold, 0)
})

test_that("render_montage_report reuses cached surface panels by map hash", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )

  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-render-surface-cache-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1",
    path = stat_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    contrast = "faces",
    model = "m1",
    label = "Faces M1",
    stringsAsFactors = FALSE
  )
  calls <- 0L

  testthat::local_mocked_bindings(
    surf_montage = function(stat, surfatlas, output_file, threshold, tail,
                            signed, cap, width, height, res, title,
                            subtitle, ...) {
      calls <<- calls + 1L
      writeLines("surface placeholder", output_file)
      structure(
        list(
          image = normalizePath(output_file, mustWork = FALSE),
          threshold = threshold,
          tail = tail,
          signed = signed,
          cap = cap,
          n_suprathreshold = 4L,
          surface_space = "fsLR-32k",
          diagnostics = list(cache_hit = FALSE)
        ),
        class = "surf_montage_result"
      )
    },
    .package = "neuromosaic"
  )

  args <- list(
    manifest = manifest,
    output_file = file.path(tmpdir, "surface-cache.qmd"),
    surfatlas = make_toy_surfatlas(),
    image_dir = file.path(tmpdir, "images"),
    image_width = 700,
    image_height = 500,
    image_res = 72
  )
  first <- do.call(render_montage_report, args)
  second <- do.call(render_montage_report, args)

  first_rd <- readRDS(sub("\\.qmd$", "_report-data.rds", first))
  second_rd <- readRDS(sub("\\.qmd$", "_report-data.rds", second))

  expect_equal(calls, 1L)
  expect_identical(
    second_rd$panels$faces_m1$surface_image,
    first_rd$panels$faces_m1$surface_image
  )
  expect_true(second_rd$panels$faces_m1$surface$diagnostics$cache_hit)
  expect_match(basename(second_rd$panels$faces_m1$surface_image), "surface")
})

test_that("render_montage_report adds peak tables and effective-N QC", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-render-atlas-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1",
    path = stat_path,
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    min_cluster_size = 3,
    contrast = "faces",
    model = "m1",
    label = "Faces M1",
    n = 1,
    subjects = "sub-01, sub-02",
    stringsAsFactors = FALSE
  )
  qmd_out <- file.path(tmpdir, "atlas-montage.qmd")

  result <- suppressWarnings(
    render_montage_report(
      manifest,
      output_file = qmd_out,
      atlas = inputs$atlas,
      max_clusters = 5L
    )
  )

  sidecar <- sub("\\.qmd$", "_report-data.rds", result)
  rd <- readRDS(sidecar)
  panel <- rd$panels$faces_m1

  expect_true(file.exists(result))
  expect_true("atlas_label" %in% names(panel$peak_table))
  expect_gt(nrow(panel$peak_table), 0)
  expect_identical(panel$peaks$n_clusters, nrow(panel$peak_table))
  expect_equal(panel$qc$effective_n[[1]], 1)
  expect_equal(panel$qc$source_n[[1]], 2)
  expect_equal(panel$qc$dropped_n[[1]], 1)
  expect_true(panel$qc$has_dropped_subjects[[1]])
  expect_equal(rd$qc$qc_status[[1]], "dropped_subjects")

  qmd <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_match(qmd, "montage_report_formatters", fixed = TRUE)
})

test_that("render_montage_report handles a relative template and isolates intermediates (#4)", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for render tests")

  manifest <- make_montage_render_manifest()

  # Template copied to its own dir; we never write next to it (HPC read-only libs).
  template_dir <- tempfile("montage-template-")
  dir.create(template_dir, recursive = TRUE)
  template_path <- file.path(template_dir, "montage_report.Rmd")
  file.copy(
    system.file("templates", "montage_report.Rmd", package = "neuromosaic"),
    template_path,
    overwrite = TRUE
  )

  output_dir <- tempfile("montage-output-")
  dir.create(output_dir, recursive = TRUE)
  out_file <- file.path(output_dir, "report.html")

  # Render from a third directory, passing the template by a path relative to
  # it, so the absolutize-before-with_dir() fix is exercised.
  work_dir <- tempfile("montage-work-")
  dir.create(work_dir, recursive = TRUE)
  rel_template <- file.path("..", basename(template_dir), "montage_report.Rmd")

  result <- withr::with_dir(work_dir, {
    render_montage_report(
      manifest,
      output_file = out_file,
      template = rel_template,
      title = "Relative Template",
      layout = c("contrast", "model"),
      quiet = TRUE
    )
  })

  expect_true(file.exists(result))
  expect_equal(normalizePath(result), normalizePath(out_file))
  # Intermediates must not land next to the (possibly read-only) template.
  expect_length(list.files(template_dir, pattern = "knit\\.md$"), 0)
  expect_false(file.exists(file.path(template_dir, "report.knit.md")))
})

test_that("render_montage_report passes an absolute template and output-dir intermediates to rmarkdown (#4)", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )

  manifest <- make_montage_render_manifest()

  template_dir <- tempfile("montage-template-")
  dir.create(template_dir, recursive = TRUE)
  template_path <- file.path(template_dir, "montage_report.Rmd")
  file.copy(
    system.file("templates", "montage_report.Rmd", package = "neuromosaic"),
    template_path,
    overwrite = TRUE
  )

  output_dir <- tempfile("montage-output-")
  dir.create(output_dir, recursive = TRUE)
  out_file <- file.path(output_dir, "report.html")

  work_dir <- tempfile("montage-work-")
  dir.create(work_dir, recursive = TRUE)
  rel_template <- file.path("..", basename(template_dir), "montage_report.Rmd")

  captured <- NULL
  testthat::local_mocked_bindings(
    render = function(input, output_file, output_dir, intermediates_dir, ...) {
      captured <<- list(input = input, output_dir = output_dir,
                        intermediates_dir = intermediates_dir)
      invisible(input)
    },
    .package = "rmarkdown"
  )

  withr::with_dir(work_dir, {
    render_montage_report(
      manifest,
      output_file = out_file,
      template = rel_template,
      layout = c("contrast", "model"),
      quiet = TRUE
    )
  })

  expect_false(is.null(captured))
  # #4a: a relative template= is absolutized before with_dir() changes the cwd.
  expect_identical(captured$input, normalizePath(template_path))
  # #4b: intermediates go to the writable output dir, never the template dir.
  expect_identical(captured$intermediates_dir, normalizePath(output_dir))
})

test_that("render_montage_report validates output and layout contract", {
  manifest <- make_montage_render_manifest()

  expect_error(
    render_montage_report(manifest, tempfile("montage-report-", fileext = ".docx")),
    "Unsupported report extension"
  )

  expect_error(
    render_montage_report(
      manifest,
      output_file = tempfile("montage-report-", fileext = ".qmd"),
      layout = "missing_column"
    ),
    "layout.*not found"
  )
})

test_that("render_montage_report rejects qmd templates for html output", {
  manifest <- make_montage_render_manifest()

  expect_error(
    render_montage_report(
      manifest,
      output_file = tempfile("montage-report-", fileext = ".html"),
      template = system.file("templates", "montage_report.qmd",
                             package = "neuromosaic")
    ),
    "Qmd templates are only supported"
  )
})

test_that("montage report rmarkdown output formats are explicit", {
  skip_if_not_installed("rmarkdown")

  html_format <- neuromosaic:::.montage_rmarkdown_output_format("html")
  pdf_format <- neuromosaic:::.montage_rmarkdown_output_format("pdf")

  expect_s3_class(html_format, "rmarkdown_output_format")
  expect_s3_class(pdf_format, "rmarkdown_output_format")
  expect_equal(pdf_format$pandoc$latex_engine, "xelatex")
  expect_true("--number-sections" %in% pdf_format$pandoc$args)
})

test_that("montage templates share formatter helpers and base64 image strategy", {
  template <- system.file("templates", "montage_report.Rmd",
                          package = "neuromosaic")
  text <- paste(readLines(template, warn = FALSE), collapse = "\n")
  image_path <- tempfile("montage-format-image-", fileext = ".png")
  writeLines("image placeholder", image_path)
  fmt <- montage_report_formatters(is_html = TRUE)
  image_html <- paste(
    capture.output(fmt$emit_panel_image(image_path, "panel")),
    collapse = "\n"
  )

  expect_match(text, "montage_report_formatters", fixed = TRUE)
  expect_match(image_html, "data:image/png;base64", fixed = TRUE)
  expect_match(text, "results='asis'", fixed = TRUE)
})

test_that(".montage_shared_caps uses a robust quantile, not the raw maximum", {
  sp <- neuroim2::NeuroSpace(dim = c(4, 4, 4), spacing = c(2, 2, 2))
  arr <- array(0, dim = c(4, 4, 4))
  arr[1:27] <- seq(3.1, 4.5, length.out = 27)  # ordinary suprathreshold voxels
  arr[64] <- 50                                # one extreme hot voxel
  vol <- neuroim2::NeuroVol(arr, sp)

  manifest <- data.frame(
    map_id = "m", stat_kind = "z", signed = TRUE, label = "m",
    stringsAsFactors = FALSE
  )
  manifest$effective_threshold <- 3
  manifest$effective_tail <- "two_sided"
  manifest$cap_key <- "m"

  robust <- .montage_shared_caps(
    manifest, list(vol), policy = montage_policy(cap_quantile = 0.9)
  )
  expect_lt(robust[["m"]], 6)        # the lone z = 50 voxel does not set the cap
  expect_gt(robust[["m"]], 4)

  fixed <- .montage_shared_caps(
    manifest, list(vol), policy = montage_policy(cap = 8)
  )
  expect_equal(fixed[["m"]], 8)

  floored <- .montage_shared_caps(
    manifest, list(vol), policy = montage_policy(cap_quantile = 0.5, cap_floor = 10)
  )
  expect_gte(floored[["m"]], 10)
})

test_that("render_montage_report forwards volume_args and validates passthrough", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-volargs-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1", path = stat_path, stat_kind = "z", signed = TRUE,
    threshold = 3, contrast = "faces", model = "m1", label = "Faces",
    stringsAsFactors = FALSE
  )
  out <- file.path(tmpdir, "vol.qmd")

  r <- render_montage_report(
    manifest, output_file = out, bg = inputs$stat_map, render_peaks = FALSE,
    volume_args = list(cap = 4, ov_alpha_mode = "binary"),
    image_width = 360, image_height = 260, image_res = 72
  )
  rd <- readRDS(sub("\\.qmd$", "_report-data.rds", r))
  expect_equal(rd$panels$faces_m1$volume$cap, 4)        # caller cap wins
  expect_identical(rd$panels$faces_m1$volume$alpha_mode, "binary")

  expect_error(
    render_montage_report(manifest, output_file = out, bg = inputs$stat_map,
                          render_peaks = FALSE, volume_args = list(bogus = 1)),
    "cannot be forwarded"
  )
  expect_error(
    render_montage_report(manifest, output_file = out, bg = inputs$stat_map,
                          render_peaks = FALSE,
                          volume_args = list(bg = inputs$stat_map)),
    "cannot be forwarded"
  )
  expect_error(
    render_montage_report(manifest, output_file = out, bg = inputs$stat_map,
                          render_peaks = FALSE, surface_args = list(stat = 1)),
    "cannot be forwarded"
  )
})

test_that("surface cache key changes when surface_args change", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-surface-cachekey-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1", path = stat_path, stat_kind = "z", units = "z",
    signed = TRUE, threshold = 3, contrast = "faces", model = "m1",
    label = "Faces M1", stringsAsFactors = FALSE
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    surf_montage = function(stat, surfatlas, output_file, threshold, tail,
                            signed, cap, overlay_alpha, width, height, res,
                            title, subtitle, ...) {
      calls <<- calls + 1L
      writeLines("surface placeholder", output_file)
      structure(
        list(image = normalizePath(output_file, mustWork = FALSE),
             threshold = threshold, tail = tail, signed = signed, cap = cap,
             n_suprathreshold = 4L, surface_space = "fsLR-32k",
             diagnostics = list(cache_hit = FALSE)),
        class = "surf_montage_result"
      )
    },
    .package = "neuromosaic"
  )

  base <- list(
    manifest = manifest, surfatlas = make_toy_surfatlas(),
    image_dir = file.path(tmpdir, "images"),
    image_width = 700, image_height = 500, image_res = 72
  )
  out1 <- do.call(render_montage_report, c(base, list(
    output_file = file.path(tmpdir, "a.qmd"),
    surface_args = list(overlay_alpha = 0.4)
  )))
  out2 <- do.call(render_montage_report, c(base, list(
    output_file = file.path(tmpdir, "b.qmd"),
    surface_args = list(overlay_alpha = 0.9)
  )))
  rd1 <- readRDS(sub("\\.qmd$", "_report-data.rds", out1))
  rd2 <- readRDS(sub("\\.qmd$", "_report-data.rds", out2))

  expect_equal(calls, 2L)  # different overlay_alpha is not a cache hit
  expect_false(identical(
    basename(rd1$panels$faces_m1$surface_image),
    basename(rd2$panels$faces_m1$surface_image)
  ))
})

test_that("render_montage_report tolerates an empty contrast by default (#8)", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-empty-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-null_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "null_map", path = stat_path, stat_kind = "z", signed = TRUE,
    threshold = 1e6,                       # nothing survives -> empty panel
    contrast = "null", model = "m1", label = "Null contrast",
    stringsAsFactors = FALSE
  )
  out <- file.path(tmpdir, "empty.qmd")

  # Report-level default empty = "warning": warns, but still builds the bundle
  # and renders the base panel instead of aborting the whole report.
  w <- capture_warnings(
    r <- suppressMessages(render_montage_report(
      manifest, output_file = out, bg = inputs$stat_map, render_peaks = FALSE,
      image_width = 360, image_height = 260, image_res = 72
    ))
  )
  expect_true(any(grepl("suprathreshold", w)))
  expect_true(file.exists(r))
  rd <- readRDS(sub("\\.qmd$", "_report-data.rds", r))
  expect_equal(rd$panels$null_map$volume$n_suprathreshold, 0)

  # empty = "error" restores the strict per-map abort.
  expect_error(
    suppressMessages(suppressWarnings(render_montage_report(
      manifest, output_file = out, bg = inputs$stat_map, render_peaks = FALSE,
      empty = "error",
      image_width = 360, image_height = 260, image_res = 72
    ))),
    "suprathreshold"
  )
})

test_that("render_montage_report announces qmd source instead of rendering (#10)", {
  manifest <- make_montage_render_manifest()
  qmd_out <- tempfile("montage-report-", fileext = ".qmd")

  expect_message(
    render_montage_report(
      manifest, output_file = qmd_out, layout = c("contrast", "model")
    ),
    "quarto render"
  )
})

test_that("montage pdf output honors a custom latex_engine (#12)", {
  skip_if_not_installed("rmarkdown")

  pdf_format <- neuromosaic:::.montage_rmarkdown_output_format("pdf", "pdflatex")
  expect_equal(pdf_format$pandoc$latex_engine, "pdflatex")
  # Default is unchanged.
  expect_equal(
    neuromosaic:::.montage_rmarkdown_output_format("pdf")$pandoc$latex_engine,
    "xelatex"
  )
})

test_that("render_montage_report validates latex_engine (#12)", {
  manifest <- make_montage_render_manifest()
  expect_error(
    render_montage_report(
      manifest, tempfile("montage-report-", fileext = ".pdf"),
      latex_engine = c("a", "b")
    ),
    "latex_engine"
  )
})

test_that("render_montage_report renders an all-zero map under default empty='warning' (#8)", {
  # The FDR pre-masking workflow produces a literally all-zero map when nothing
  # survives; its derived cap is 0, which must not abort under empty = "warning".
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-allzero-")
  dir.create(tmpdir, recursive = TRUE)
  zero_vol <- neuroim2::NeuroVol(
    array(0, dim(inputs$stat_map)), neuroim2::space(inputs$stat_map)
  )
  stat_path <- file.path(tmpdir, "contrast-null_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(zero_vol, stat_path)
  manifest <- data.frame(
    map_id = "all_zero", path = stat_path, stat_kind = "z", signed = TRUE,
    threshold = 3, contrast = "null", model = "m1", label = "All zero",
    stringsAsFactors = FALSE
  )
  out <- file.path(tmpdir, "allzero.qmd")

  w <- capture_warnings(
    r <- suppressMessages(render_montage_report(
      manifest, output_file = out, bg = inputs$stat_map, render_peaks = FALSE,
      image_width = 360, image_height = 260, image_res = 72
    ))
  )
  expect_true(any(grepl("suprathreshold", w)))
  expect_true(file.exists(r))
  rd <- readRDS(sub("\\.qmd$", "_report-data.rds", r))
  expect_equal(rd$panels$all_zero$volume$n_suprathreshold, 0)
})

test_that("render_montage_report empty policy reaches the labeller path (#8)", {
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-labeller-empty-")
  dir.create(tmpdir, recursive = TRUE)
  manifest <- data.frame(
    map_id = "faces_m1", stat_kind = "z", signed = TRUE, threshold = 1e6,
    contrast = "faces", stringsAsFactors = FALSE
  )
  # An in-memory stat_map list-column makes validate_manifest run overlay QC via
  # apply_montage_labeller(); empty = "warning" must reach that call too.
  manifest$stat_map <- I(list(inputs$stat_map))
  out <- file.path(tmpdir, "labeller.qmd")

  w <- capture_warnings(
    r <- suppressMessages(render_montage_report(
      manifest, output_file = out,
      labeller = function(entities) {
        list(title = paste("Contrast", entities$contrast))
      }
    ))
  )
  expect_true(any(grepl("suprathreshold", w)))
  expect_true(file.exists(r))
})

test_that("render_montage_report surface cache honors a later empty='error' (#8)", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE),
    "testthat::local_mocked_bindings() is required for this test"
  )
  inputs <- make_toy_cluster_report_inputs()
  tmpdir <- tempfile("montage-surface-empty-cache-")
  dir.create(tmpdir, recursive = TRUE)
  stat_path <- file.path(tmpdir, "contrast-faces_model-m1_stat-z.nii.gz")
  neuroim2::write_vol(inputs$stat_map, stat_path)
  manifest <- data.frame(
    map_id = "faces_m1", path = stat_path, stat_kind = "z", signed = TRUE,
    threshold = 1e6,                       # nothing survives -> empty panel
    contrast = "faces", model = "m1", label = "Faces M1",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    surf_montage = function(stat, surfatlas, output_file, ...) {
      writeLines("surface placeholder", output_file)
      structure(
        list(image = normalizePath(output_file, mustWork = FALSE),
             threshold = 1e6, tail = "two_sided", signed = TRUE, cap = 1,
             n_suprathreshold = 0L, surface_space = "fsLR-32k",
             diagnostics = list(cache_hit = FALSE)),
        class = "surf_montage_result"
      )
    },
    .package = "neuromosaic"
  )

  args <- list(
    manifest = manifest, surfatlas = make_toy_surfatlas(),
    image_dir = file.path(tmpdir, "images"),
    image_width = 320, image_height = 220, image_res = 72
  )
  # Warning mode caches the empty panel PNG.
  suppressWarnings(suppressMessages(do.call(render_montage_report, c(
    args, list(output_file = file.path(tmpdir, "a.qmd"), empty = "warning")
  ))))
  # A later strict request must still abort, even though the PNG is cached and
  # `empty` is excluded from the cache key.
  expect_error(
    suppressMessages(do.call(render_montage_report, c(
      args, list(output_file = file.path(tmpdir, "b.qmd"), empty = "error")
    ))),
    "suprathreshold"
  )
})
