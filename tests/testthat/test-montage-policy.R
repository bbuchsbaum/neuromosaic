make_policy_manifest <- function() {
  data.frame(
    map_id = c("map_t", "map_z"),
    path = c("map-t.nii.gz", "map-z.nii.gz"),
    stat_kind = c("t", "z"),
    df = c(20, NA),
    units = c("t", "z"),
    signed = TRUE,
    p = c(0.01, NA),
    tail = c("two_sided", NA),
    connectivity = c("26-connect", NA),
    min_cluster_size = c(5, NA),
    contrast = c("faces", "faces"),
    model = c("m1", "m2"),
    label = c("T map", "Z map"),
    stringsAsFactors = FALSE
  )
}

test_that("montage_policy constructs validated default policy objects", {
  policy <- montage_policy(
    p = 0.005,
    tail = "positive",
    connectivity = "6-connect",
    min_cluster_size = 3,
    cap_within = c("contrast"),
    layout = c("contrast", "model")
  )

  expect_s3_class(policy, "montage_policy")
  expect_identical(policy$tail, "positive")
  expect_identical(policy$connectivity, "6-connect")
  expect_identical(policy$min_cluster_size, 3L)
  expect_true(is.function(policy$threshold_fun))
})

test_that("montage_policy validates constructor inputs", {
  expect_error(montage_policy(p = 2), "between 0 and 1")
  expect_error(montage_policy(min_cluster_size = 0), "positive integer")
  expect_error(montage_policy(cap_within = 1), "character vector")
  expect_error(montage_policy(layout_fun = "not-a-function"), "function")
  expect_error(montage_policy(threshold = -1), "positive number")
})

test_that("resolve_montage_policy applies defaults and row overrides", {
  manifest <- make_policy_manifest()
  policy <- montage_policy(
    p = 0.005,
    tail = "negative",
    connectivity = "18-connect",
    min_cluster_size = 10,
    cap_within = "contrast",
    layout = c("contrast", "model")
  )

  out <- resolve_montage_policy(manifest, policy)

  expect_equal(out$effective_threshold[[1]], stats::qt(1 - 0.01 / 2, df = 20))
  expect_equal(out$effective_threshold[[2]], stats::qnorm(1 - 0.005))
  expect_identical(out$effective_tail, c("two_sided", "negative"))
  expect_identical(out$effective_connectivity, c("26-connect", "18-connect"))
  expect_identical(out$effective_min_cluster_size, c(5L, 10L))
  expect_identical(out$cap_key, c("faces", "faces"))
  expect_s3_class(attr(out, "montage_policy"), "montage_policy")
})

test_that("resolve_montage_policy respects explicit thresholds", {
  manifest <- make_policy_manifest()
  manifest$threshold <- c(7, 8)

  out <- resolve_montage_policy(manifest, montage_policy(p = 0.001))

  expect_identical(out$effective_threshold, c(7, 8))
})

test_that("montage_policy supports custom threshold functions", {
  manifest <- make_policy_manifest()
  policy <- montage_policy(
    threshold = function(stat_kind, df, p, tail) {
      rep(42, length(stat_kind))
    }
  )

  out <- resolve_montage_policy(manifest, policy)

  expect_identical(out$effective_threshold, c(42, 42))
})

test_that("resolve_montage_policy validates declared cap and layout columns", {
  manifest <- make_policy_manifest()

  expect_error(
    resolve_montage_policy(manifest, montage_policy(cap_within = "missing")),
    "cap_within.*missing"
  )
  expect_error(
    resolve_montage_policy(manifest, montage_policy(layout = "missing")),
    "layout.*missing"
  )
})
