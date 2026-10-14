test_that("map sets resolve channels and reject ambiguous membership", {
  inputs <- make_toy_cluster_report_inputs()
  manifest <- data.frame(map_id = c("a_t", "a_est", "a_se", "b_t", "b_est", "b_se"),
    label = c("A t", "A estimate", "A se", "B t", "B estimate", "B se"),
    analysis_id = c("a", "a", "a", "b", "b", "b"),
    role = c("primary", "auxiliary", "auxiliary", "primary", "auxiliary", "auxiliary"),
    quantity = c("test_statistic", "estimate", "standard_error",
                 "test_statistic", "estimate", "standard_error"),
    distribution = c("t", NA, NA, "t", NA, NA), threshold = 2, df = 20,
    stringsAsFactors = FALSE)
  manifest$stat_map <- I(rep(list(inputs$stat_map), 6))
  members <- data.frame(map_id = manifest$map_id, member_id = rep(c("s1", "s2"), each = 3),
    model = "m", contrast = "c", stringsAsFactors = FALSE)
  out <- validate_montage_map_sets(manifest, montage_map_set("subjects", members,
    inputs$stat_map, member_kind = "subject"))
  expect_identical(out$sets$subjects$members$channel,
                   c("t", "estimate", "standard_error", "t", "estimate", "standard_error"))
  expect_identical(out$sets$subjects$common_channels[[1]], c("t", "estimate"))
  expect_error(montage_map_set("dup", c("a_t", "a_t"), inputs$stat_map), "more than once")
})

test_that("loaded sets retain finite zero and enforce a binary explicit mask", {
  inputs <- make_toy_cluster_report_inputs()
  zero <- inputs$stat_map; zero[] <- 0
  manifest <- data.frame(map_id = "zero", label = "zero", analysis_id = "zero", role = "primary",
    quantity = "test_statistic", distribution = "z", threshold = 2, stringsAsFactors = FALSE)
  manifest$stat_map <- I(list(zero))
  mask <- neuroim2::NeuroVol(array(1, dim(zero)), neuroim2::space(zero))
  out <- validate_montage_map_sets(manifest, montage_map_set("zero", "zero", mask,
    template = inputs$stat_map), load_maps = TRUE)
  expect_true(all(out$sets$zero$members$data[[1]] == 0))
  expect_gt(out$sets$zero$payload_bytes, 0)
  expect_identical(out$sets$zero$payload_bytes,
                   4 * (length(zero) + length(inputs$stat_map)) + length(mask))
  bad <- neuroim2::NeuroVol(array(.5, dim(zero)), neuroim2::space(zero))
  expect_error(validate_montage_map_sets(manifest, montage_map_set("bad", "zero", bad,
    template = inputs$stat_map), load_maps = TRUE), "binary")
})

test_that("map-set semantics reject orphan SE and incompatible declared units", {
  inputs <- make_toy_cluster_report_inputs()
  manifest <- data.frame(
    map_id = c("a", "b"), label = c("a", "b"), analysis_id = c("a", "b"),
    role = "primary", quantity = "estimate", units = c("beta", "percent"),
    stringsAsFactors = FALSE
  )
  manifest$stat_map <- I(list(inputs$stat_map, inputs$stat_map))
  members <- data.frame(map_id = c("a", "b"), member_id = c("s1", "s2"),
                        model = "m", contrast = "c")
  expect_error(validate_montage_map_sets(manifest,
    montage_map_set("units", members, inputs$stat_map)), "incompatible declared units")
  manifest$quantity <- c("test_statistic", "standard_error")
  manifest$distribution <- c("z", NA_character_)
  members$member_id <- "s1"
  expect_error(validate_montage_map_sets(manifest,
    montage_map_set("se", members, inputs$stat_map)), "lacks an estimate")
})

test_that("loading uses analysis masks, rejects recipes, overflow, and invalid atlas labels", {
  inputs <- make_toy_cluster_report_inputs()
  masked <- inputs$stat_map
  masked[] <- 1
  analysis_mask <- neuroim2::NeuroVol(array(c(1, rep(0, length(masked) - 1)), dim(masked)),
                                      neuroim2::space(masked))
  manifest <- data.frame(map_id = "map", label = "map", analysis_id = "a",
    role = "primary", quantity = "test_statistic", distribution = "z", threshold = 2)
  manifest$stat_map <- I(list(masked))
  manifest$mask <- I(list(analysis_mask))
  comparison_mask <- neuroim2::NeuroVol(array(1, dim(masked)), neuroim2::space(masked))
  out <- validate_montage_map_sets(manifest, montage_map_set("masked", "map",
    comparison_mask, template = inputs$stat_map), load_maps = TRUE)
  expect_true(is.na(out$sets$masked$members$data[[1]][[2]]))
  shifted_space <- neuroim2::NeuroSpace(dim = dim(masked), spacing = c(2, 2, 2),
                                        origin = c(-4, -5, -5))
  shifted_mask <- neuroim2::NeuroVol(array(1, dim(masked)), shifted_space)
  expect_error(validate_montage_map_sets(manifest, montage_map_set("shifted", "map",
    shifted_mask, template = inputs$stat_map), load_maps = TRUE), "does not match")

  recipe_manifest <- manifest[, setdiff(names(manifest), "stat_map"), drop = FALSE]
  recipe_manifest$recipe <- I(list(function(row) stop("must not run")))
  expect_error(validate_montage_map_sets(recipe_manifest, montage_map_set("recipe", "map",
    comparison_mask, template = inputs$stat_map), load_maps = TRUE), "deferred recipes")

  overflow <- masked
  overflow[] <- 1e39
  manifest$stat_map <- I(list(overflow))
  expect_error(validate_montage_map_sets(manifest, montage_map_set("overflow", "map",
    comparison_mask, template = inputs$stat_map), load_maps = TRUE), "Float32 conversion")

  manifest$stat_map <- I(list(masked))
  labels <- neuroim2::NeuroVol(array(2, dim(masked)), neuroim2::space(masked))
  atlas <- list(name = "A", labels = labels,
                regions = data.frame(id = 1L, label = "one"))
  expect_error(validate_montage_map_sets(manifest, montage_map_set("atlas", "map",
    comparison_mask, template = inputs$stat_map, atlas = atlas), load_maps = TRUE),
    "do not cover")
})

test_that("Space accepts logical covariates as categories and rejects even tiny affine shear", {
  inputs <- make_toy_cluster_report_inputs()
  set <- montage_map_set("covariates", c("s1", "s2"), inputs$stat_map,
    covariates = data.frame(member_id = c("s1", "s2"), included = c(TRUE, NA)))
  expect_identical(set$covariates$included, c("TRUE", NA_character_))
  affine <- neuroim2::trans(neuroim2::space(inputs$stat_map))
  affine[1, 2] <- 1e-7
  grid <- neuroim2::NeuroSpace(dim(inputs$stat_map), spacing = c(2, 2, 2),
                               origin = c(-5, -5, -5), trans = affine)
  volume <- neuroim2::NeuroVol(array(1, dim(inputs$stat_map)), grid)
  expect_error(.montage_map_set_ras_geometry(volume, "map", "sheared"), "axis-aligned")
})

test_that("atlas factors preserve numeric IDs rather than factor codes", {
  toy <- make_toy_cluster_report_inputs()
  atlas <- list(name = "Regions", labels = toy$stat_map,
    regions = data.frame(id = factor(c("0", "1")), label = c("Background", "Target")))
  set <- montage_map_set("atlas", "map", toy$stat_map, atlas = atlas)
  expect_identical(set$atlas$regions$id, c(0L, 1L))
  expect_identical(set$atlas$regions$label[set$atlas$regions$id == 1L], "Target")
  atlas$regions$id <- factor(c("background", "1"))
  expect_error(montage_map_set("atlas", "map", toy$stat_map, atlas = atlas), "integer ids")
})

test_that("Float32 finite endpoints are accepted and overflow is rejected", {
  max32 <- (2 - 2^-23) * 2^127
  expect_identical(.montage_map_set_float32(c(-max32, 0, max32, NA_real_), "boundary"),
                  c(-max32, 0, max32, NA_real_))
  expect_error(.montage_map_set_float32(max32 * 2, "overflow"), "overflows")
})
