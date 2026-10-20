# Fixed, synthetic complete-report size specimen. No source data or network.
# RGL_USE_NULL=TRUE Rscript tools/benchmark-report-assets.R /path/to/output
pkgload::load_all(quiet = TRUE)
benchmark_report_assets <- function(destination, width = 880L, height = 240L, map_count = 8L) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  destination <- normalizePath(destination)
  source_dir <- file.path(destination, "source")
  dir.create(source_dir, showWarnings = FALSE)
  set.seed(20261020)
  images <- vapply(c("light", "dark", "narrow", "narrow-dark"), function(variant) {
    path <- file.path(source_dir, paste0(variant, ".png"))
    image_width <- if (grepl("narrow", variant)) round(width * 400 / 880) else width
    # Dense synthetic pixels prevent a trivially compressible blank-image result.
    pixels <- array(runif(image_width * height * 3L), c(height, image_width, 3L))
    png::writePNG(pixels, path)
    path
  }, character(1))
  manifest <- data.frame(map_id = paste0("map", seq_len(map_count)), quantity = "test_statistic",
                         distribution = "z", signed = TRUE, threshold = 3,
                         label = paste("Synthetic map", seq_len(map_count)),
                         path = "unused.nii.gz")
  panels <- stats::setNames(lapply(manifest$map_id, function(id) list(
    volume_image = images[["light"]], volume_image_dark = images[["dark"]],
    volume_image_narrow = images[["narrow"]],
    volume_image_narrow_dark = images[["narrow-dark"]]
  )), manifest$map_id)
  packaging <- if ("assets" %in% names(formals(render_montage_report))) {
    c("embed", "bundle")
  } else "auto"
  rows <- lapply(packaging, function(mode) {
    output <- file.path(destination, mode, "index.html")
    call <- list(manifest = manifest, output_file = output, panels = panels,
                 materialize_recipes = FALSE, check_files = FALSE)
    if (mode != "auto") call$assets <- mode
    do.call(render_montage_report, call)
    if (exists("report_size", mode = "function")) {
      size <- report_size(output)
      total <- size$total_bytes
      companions <- nrow(size$files) - 1L
    } else {
      total <- file.size(output)
      companions <- 0L
    }
    data.frame(mode = mode, html_bytes = file.size(output),
               required_bytes = total, companion_files = companions,
               source_figure_bytes = sum(file.size(images)))
  })
  result <- do.call(rbind, rows)
  write.csv(result, file.path(destination, "sizes.csv"), row.names = FALSE)
  print(result, row.names = FALSE)
  cat("Artifacts:", destination, "\n")
  if (exists("write_report_bundle", mode = "function") && "bundle" %in% packaging) {
    write_report_bundle(file.path(destination, "bundle", "index.html"),
                        file.path(destination, "relocated"))
  }
  invisible(result)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  benchmark_report_assets(if (length(args)) args[[1]] else tempfile("report-assets-"))
}
