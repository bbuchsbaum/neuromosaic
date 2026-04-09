test_that("cluster_report renders HTML with relative figure paths", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for render tests")

  inputs <- example_cluster_inputs()
  out <- tempfile("cluster-report-", fileext = ".html")

  result <- cluster_report(
    stat_map = inputs$stat_map,
    data_source = inputs$data_source,
    atlas = inputs$atlas,
    design = inputs$design,
    threshold = 3.0,
    formulas = list("Condition x Time" = value ~ condition * time),
    output_file = out,
    min_cluster_size = 5,
    quiet = TRUE,
    brain_slices = TRUE
  )

  expect_true(file.exists(result$report_path))

  html <- paste(readLines(result$report_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("//tmp/", html, fixed = TRUE))
  expect_false(grepl("\\(axial", html, fixed = TRUE))
  expect_false(grepl("$sagittal", html, fixed = TRUE))
  expect_match(html, "Brain View")
})

test_that("cluster_report renders stat-map-only HTML reports", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for render tests")

  inputs <- make_toy_cluster_report_inputs()
  out <- tempfile("cluster-report-table-", fileext = ".html")

  result <- suppressWarnings(
    cluster_report(
      stat_map = inputs$stat_map,
      data_source = NULL,
      atlas = inputs$atlas,
      threshold = 3.0,
      output_file = out,
      min_cluster_size = 3,
      quiet = TRUE,
      brain_slices = FALSE
    )
  )

  expect_true(file.exists(result$report_path))

  html <- paste(readLines(result$report_path, warn = FALSE), collapse = "\n")
  expect_match(html, "Sample-level signal plots are omitted")
  expect_match(html, "MNI Coordinate Table")
})

test_that("render_cluster_report writes qmd source and sidecar data", {
  inputs <- make_toy_cluster_report_inputs()
  qmd_out <- tempfile("cluster-report-", fileext = ".qmd")

  result <- suppressWarnings(
    cluster_report(
      stat_map = inputs$stat_map,
      data_source = NULL,
      atlas = inputs$atlas,
      threshold = 3.0,
      output_file = qmd_out,
      min_cluster_size = 3,
      quiet = TRUE,
      brain_slices = FALSE
    )
  )

  expect_true(file.exists(result$report_path))

  sidecar <- sub("\\.qmd$", "_report-data.rds", result$report_path)
  expect_true(file.exists(sidecar))

  qmd <- paste(readLines(result$report_path, warn = FALSE), collapse = "\n")
  expect_match(qmd, basename(sidecar), fixed = TRUE)
  expect_match(qmd, "expects the sidecar file", fixed = TRUE)
  expect_match(qmd, "Render from this directory", fixed = TRUE)

  rd <- readRDS(sidecar)
  expect_true(is.list(rd))
  expect_true("cluster_table" %in% names(rd))
})

test_that("render_cluster_report rejects unsupported output extensions", {
  inputs <- make_toy_cluster_report_inputs()
  report_data <- suppressWarnings(
    cluster_report(
      stat_map = inputs$stat_map,
      data_source = NULL,
      atlas = inputs$atlas,
      threshold = 3.0,
      output_file = NULL,
      min_cluster_size = 3,
      brain_slices = FALSE
    )
  )

  expect_error(
    render_cluster_report(report_data, tempfile("cluster-report-", fileext = ".docx")),
    "Unsupported report extension"
  )
})

test_that("render_cluster_report rejects qmd templates for html output", {
  inputs <- make_toy_cluster_report_inputs()
  report_data <- suppressWarnings(
    cluster_report(
      stat_map = inputs$stat_map,
      data_source = NULL,
      atlas = inputs$atlas,
      threshold = 3.0,
      output_file = NULL,
      min_cluster_size = 3,
      brain_slices = FALSE
    )
  )

  expect_error(
    render_cluster_report(
      report_data,
      output_file = tempfile("cluster-report-", fileext = ".html"),
      template = system.file("templates", "cluster_report.qmd", package = "neuromosaic")
    ),
    "Qmd templates are only supported"
  )
})

test_that("render_cluster_report errors cleanly when rmarkdown is unavailable", {
  inputs <- make_toy_cluster_report_inputs()
  report_data <- suppressWarnings(
    cluster_report(
      stat_map = inputs$stat_map,
      data_source = NULL,
      atlas = inputs$atlas,
      threshold = 3.0,
      output_file = NULL,
      min_cluster_size = 3,
      brain_slices = FALSE
    )
  )

  render_without_rmarkdown <- render_cluster_report
  environment(render_without_rmarkdown) <- list2env(
    list(requireNamespace = function(pkg, quietly = TRUE) {
      if (identical(pkg, "rmarkdown")) {
        return(FALSE)
      }
      base::requireNamespace(pkg, quietly = quietly)
    }),
    parent = environment(render_cluster_report)
  )

  expect_error(
    render_without_rmarkdown(
      report_data,
      output_file = tempfile("cluster-report-", fileext = ".html")
    ),
    "Package 'rmarkdown' is required to render reports"
  )
})
