# Reproduce representative interactive-report asset sizes.
# Run from the repository root:
#   RGL_USE_NULL=TRUE Rscript -e 'source("tools/benchmark-interactive-assets.R")'

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required.")
}
pkgload::load_all(getwd(), quiet = TRUE)

benchmark_grid <- function(label, dimensions, spacing) {
  count <- prod(dimensions)
  space <- neuroim2::NeuroSpace(
    dim = dimensions,
    spacing = rep(spacing, 3L),
    origin = -dimensions * spacing / 2
  )
  output <- tempfile(paste0("neuromosaic-benchmark-", label, "-"))
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)

  materialize <- function(name, kind, values) {
    asset <- neuromosaic:::.montage_materialize_volume_asset(
      neuroim2::NeuroVol(array(values, dim = dimensions), space),
      kind = kind,
      output_dir = output,
      compression = "gzip"
    )$asset
    rm(values)
    invisible(gc())
    data.frame(
      grid = label,
      asset = name,
      voxels = count,
      uncompressed_bytes = asset$uncompressed_bytes,
      gzip_bytes = asset$compressed_bytes,
      base64_bytes = 4 * ceiling(asset$compressed_bytes / 3),
      stringsAsFactors = FALSE
    )
  }

  index <- seq_len(count)
  rows <- list(
    materialize(
      "background", "background",
      round(70 + 25 * sin(index / 1901) + 10 * cos(index / 3271), 3)
    ),
    materialize(
      "z-statistic", "overlay",
      round(5 * sin(index / 701) * cos(index / 1103), 3)
    ),
    materialize(
      "estimate", "overlay",
      round(0.8 * sin(index / 997) + 0.2 * cos(index / 4093), 4)
    ),
    materialize(
      "standard-error", "overlay",
      round(0.12 + 0.05 * abs(sin(index / 1237)), 4)
    )
  )
  do.call(rbind, rows)
}

results <- rbind(
  benchmark_grid("MNI-like 2 mm", c(91L, 109L, 91L), 2),
  benchmark_grid("MNI-like 1 mm", c(182L, 218L, 182L), 1)
)
totals <- aggregate(
  results[c("uncompressed_bytes", "gzip_bytes", "base64_bytes")],
  by = list(grid = results$grid), sum
)
for (field in c("uncompressed_bytes", "gzip_bytes", "base64_bytes")) {
  totals[[sub("_bytes$", "_mib", field)]] <- round(totals[[field]] / 1024^2, 2)
}
print(results, row.names = FALSE)
print(totals[c("grid", "uncompressed_mib", "gzip_mib", "base64_mib")],
      row.names = FALSE)
