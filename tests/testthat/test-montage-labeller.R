make_labeller_manifest <- function(include_label = FALSE) {
  manifest <- data.frame(
    map_id = c("map_a", "map_b"),
    path = c("a.nii.gz", "b.nii.gz"),
    stat_kind = "z",
    units = "z",
    signed = TRUE,
    threshold = 3,
    contrast = c("faces", "places"),
    model = c("m1", "m1"),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_label)) {
    manifest$label <- c("Faces", "Places")
  }
  manifest
}

test_that("apply_montage_labeller fills labels from a function contract", {
  manifest <- make_labeller_manifest()
  labelled <- apply_montage_labeller(
    manifest,
    labeller = function(entities) {
      list(
        title = paste(entities$contrast, entities$model),
        short = entities$contrast,
        description = paste("Model", entities$model),
        legend_semantics = paste("warm =", entities$contrast)
      )
    }
  )

  expect_identical(labelled$label, c("faces m1", "places m1"))
  expect_identical(labelled$short, c("faces", "places"))
  expect_identical(labelled$description, c("Model m1", "Model m1"))
  expect_identical(labelled$legend_semantics,
                   c("warm = faces", "warm = places"))
})

test_that("apply_montage_labeller fills labels from a map_id keyed table", {
  manifest <- make_labeller_manifest()
  labels <- data.frame(
    map_id = c("map_a", "map_b"),
    title = c("Face response", "Place response"),
    description = c("Face panel", "Place panel"),
    stringsAsFactors = FALSE
  )

  labelled <- apply_montage_labeller(manifest, labels)

  expect_identical(labelled$label, c("Face response", "Place response"))
  expect_identical(labelled$description, c("Face panel", "Place panel"))
})

test_that("apply_montage_labeller NULL path enforces label coverage", {
  manifest <- make_labeller_manifest(include_label = TRUE)
  expect_s3_class(apply_montage_labeller(manifest), "data.frame")

  missing <- make_labeller_manifest(include_label = FALSE)
  expect_error(
    apply_montage_labeller(missing),
    "missing required column.*label"
  )
})

test_that("apply_montage_labeller fails when function output lacks a title", {
  manifest <- make_labeller_manifest()

  expect_error(
    apply_montage_labeller(manifest, function(entities) list(short = "x")),
    "non-empty 'title' or 'label'"
  )
})

test_that("apply_montage_labeller validates labeller inputs", {
  manifest <- make_labeller_manifest()

  expect_error(
    apply_montage_labeller(manifest, labeller = "bad"),
    "NULL, a function, or a data frame"
  )
  expect_error(
    apply_montage_labeller(
      manifest,
      labeller = function(entities) list(title = "x"),
      entity_cols = "missing"
    ),
    "entity_cols.*missing"
  )
  expect_error(
    apply_montage_labeller(
      manifest,
      data.frame(label = "x", stringsAsFactors = FALSE)
    ),
    "map_id"
  )
})
