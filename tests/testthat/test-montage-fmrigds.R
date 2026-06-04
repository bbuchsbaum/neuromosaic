make_toy_fmrigds_group <- function(n_contrast = 6L) {
  skip_if_not_installed("fmrigds")

  sp <- neuroim2::NeuroSpace(dim = c(3, 3, 3), spacing = c(2, 2, 2),
                             origin = c(-2, -2, -2))
  make_vol <- function(value) {
    arr <- array(value, dim = c(3, 3, 3))
    arr[2, 2, 2] <- value + 3
    neuroim2::NeuroVol(arr, sp)
  }

  design <- expand.grid(
    model = c("m1", "m2"),
    variant = c("v1", "v2"),
    stringsAsFactors = FALSE
  )
  design$map_subject <- paste(design$model, design$variant, sep = "_")
  contrasts <- paste0("contrast", seq_len(n_contrast))

  beta <- stats::setNames(vector("list", nrow(design)), design$map_subject)
  for (i in seq_len(nrow(design))) {
    beta[[i]] <- stats::setNames(lapply(seq_along(contrasts), function(j) {
      make_vol(i * 10 + j)
    }), contrasts)
  }

  col_data <- design[, c("model", "variant"), drop = FALSE]
  rownames(col_data) <- design$map_subject
  suppressWarnings(fmrigds::gds_from_neurovol_nested(
    beta = beta,
    col_data = col_data
  ))
}

test_that("fmrigds_render_manifest adapts 6x2x2 group maps", {
  skip_if_not_installed("fmrigds")

  gds <- make_toy_fmrigds_group()
  out_dir <- tempfile("fmrigds-render-")

  manifest <- fmrigds_render_manifest(
    gds,
    assay = "beta",
    materialize_dir = out_dir,
    map_id_cols = c("contrast", "model", "variant"),
    label_cols = c("contrast", "model", "variant"),
    threshold = 3,
    min_cluster_size = 3L
  )

  expect_s3_class(manifest, "data.frame")
  expect_equal(nrow(manifest), 24L)
  expect_equal(length(unique(manifest$contrast)), 6L)
  expect_equal(length(unique(manifest$model)), 2L)
  expect_equal(length(unique(manifest$variant)), 2L)
  expect_equal(length(unique(manifest$map_id)), 24L)
  expect_true(all(file.exists(manifest$path)))
  expect_true(all(dirname(manifest$path) == normalizePath(out_dir)))
  expect_identical(manifest$level, rep("group", 24L))
  expect_identical(manifest$stat_kind, rep("beta", 24L))
})

test_that("fmrigds_render_manifest validates GDS assay inputs", {
  skip_if_not_installed("fmrigds")

  gds <- make_toy_fmrigds_group(n_contrast = 1L)

  expect_error(
    fmrigds_render_manifest(gds, assay = "missing"),
    "not found"
  )
  expect_error(
    fmrigds_render_manifest(list(), assay = "beta"),
    "GDS object"
  )
})
