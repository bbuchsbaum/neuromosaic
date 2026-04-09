#' Render a Cluster Report
#'
#' Renders a parameterized Rmd template to PDF or HTML, embedding all cluster
#' tables and time-course plots.
#'
#' @param report_data A list assembled by [cluster_report()] containing:
#'   `cluster_table`, `cluster_parcels`, `mni_table`, `plots`, `params`.
#' @param output_file Path for the rendered output. Extension determines format
#'   (`.pdf`, `.html`, or `.qmd`). `.qmd` output writes a companion
#'   `_report-data.rds` sidecar in the same directory.
#' @param template Path to a custom Rmd/Qmd template. If `NULL`, uses the
#'   bundled template matching `output_file`.
#' @param quiet Logical; suppress render progress messages? Default `TRUE`.
#'
#' @details Quarto source output depends on a sidecar `_report-data.rds` file.
#'   Keep that file in the same directory as the generated `.qmd`, and render
#'   from that directory unless you edit the embedded `readRDS()` path.
#'
#' @return The path to the rendered report (invisibly).
#' @export
render_cluster_report <- function(report_data,
                                  output_file = "cluster_report.pdf",
                                  template = NULL,
                                  quiet = TRUE) {
  ext <- tolower(tools::file_ext(output_file))
  if (!ext %in% c("html", "pdf", "qmd")) {
    stop(
      "Unsupported report extension '.", ext,
      "'. Use '.html', '.pdf', or '.qmd'.",
      call. = FALSE
    )
  }

  if (is.null(template)) {
    template_name <- if (ext == "qmd") "cluster_report.qmd" else "cluster_report.Rmd"
    template <- system.file("templates", template_name, package = "neuromosaic")
    if (!nzchar(template)) {
      stop("Bundled template not found. Is neuromosaic installed?",
           call. = FALSE)
    }
  }

  template_ext <- tolower(tools::file_ext(template))
  if (ext %in% c("html", "pdf") && identical(template_ext, "qmd")) {
    stop(
      "Qmd templates are only supported when `output_file` ends in '.qmd'.",
      call. = FALSE
    )
  }

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  output_file <- file.path(output_dir, basename(output_file))

  if (ext == "qmd") {
    return(invisible(.write_cluster_report_qmd(
      report_data = report_data,
      output_file = output_file,
      template = template
    )))
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render reports.", call. = FALSE)
  }

  output_format <- if (ext == "html") "html_document" else "pdf_document"
  # Render from the target directory so HTML figure assets use relative paths.
  withr::with_dir(output_dir, {
    rmarkdown::render(
      input = template,
      output_file = basename(output_file),
      output_dir = ".",
      output_format = output_format,
      params = list(report_data = report_data),
      envir = new.env(parent = globalenv()),
      quiet = quiet
    )
  })

  invisible(normalizePath(output_file, mustWork = FALSE))
}

.write_cluster_report_qmd <- function(report_data, output_file, template) {
  lines <- readLines(template, warn = FALSE)
  sidecar <- paste0(
    tools::file_path_sans_ext(basename(output_file)),
    "_report-data.rds"
  )
  sidecar_path <- file.path(dirname(output_file), sidecar)

  saveRDS(report_data, sidecar_path)
  lines <- gsub("__REPORT_DATA_FILE__", sidecar, lines, fixed = TRUE)
  note_lines <- c(
    paste0(
      "<!-- NOTE: This .qmd expects the sidecar file '", sidecar,
      "' to stay in the same directory. -->"
    ),
    "<!-- Render from this directory unless you edit the embedded readRDS() path. -->",
    ""
  )
  yaml_delims <- which(trimws(lines) == "---")
  if (length(yaml_delims) >= 2L) {
    lines <- append(lines, note_lines, after = yaml_delims[2])
  } else {
    lines <- c(note_lines, lines)
  }
  writeLines(lines, output_file)

  invisible(normalizePath(output_file, mustWork = FALSE))
}
