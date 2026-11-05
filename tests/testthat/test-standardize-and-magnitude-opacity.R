test_that("nm_standardize_map scales by the robust spread and keeps zero", {
  sp <- neuroim2::NeuroSpace(c(5, 5, 4), c(2, 2, 2))
  set.seed(1)
  arr <- array(stats::rnorm(100, mean = 1, sd = 2), c(5, 5, 4))
  arr[1, 1, 1] <- 0
  vol <- neuroim2::NeuroVol(arr, sp)
  z <- nm_standardize_map(vol, surface = list(lh = c(2, -4)))
  v <- arr[arr != 0]
  expect_equal(z$scale, 1.4826 * stats::median(abs(v - stats::median(v))))
  expect_equal(as.numeric(z$volume), as.numeric(arr) / z$scale)
  expect_equal(as.numeric(z$volume)[1], 0)
  expect_equal(z$surface$lh, c(2, -4) / z$scale)
  expect_identical(z$method, "robust_sd")
  expect_equal(nm_standardize_map(vol, method = "sd")$scale, stats::sd(v))
  flat <- neuroim2::NeuroVol(array(1, c(5, 5, 4)), sp)
  expect_error(nm_standardize_map(flat), "no spread")
})

test_that("magnitude opacity maps alpha modes to the browser rule", {
  expect_null(.montage_magnitude_opacity("binary"))
  ramp <- .montage_magnitude_opacity("ramp")
  expect_identical(ramp, list(low = 0, high = "range", gamma = 1, floor = 0.6))
  # Unthresholded: linear from zero to the range end, per sign.
  a <- .montage_magnitude_alpha(c(0, 2, -1, 9, NA), ramp, c(-4, 4))
  expect_equal(a, c(0, 0.5, 0.25, 1, 0))
  # The rule itself handles a mask: hidden below the edge, floor at the edge.
  a <- .montage_magnitude_alpha(c(1.5, 2, 3), ramp, c(-4, 4), c(-1, 2))
  expect_equal(a, c(0, 0.6, 0.8))
  # Thresholded report maps stay opaque above threshold: no fade.
  expect_null(.montage_magnitude_opacity("ramp", threshold = 2.3))
  # A cap below the soft knee falls back to the data maximum.
  expect_type(.montage_magnitude_opacity("soft", 0, cap = 0.01, magnitudes = 1:100), "list")
  set.seed(2)
  mags <- abs(stats::rnorm(500))
  soft <- .montage_magnitude_opacity("soft", threshold = 0, cap = 3, magnitudes = mags)
  expect_equal(soft$low, stats::median(mags))
  expect_true(soft$gamma >= 1.5 && soft$gamma <= 5)
  expect_identical(soft$high, "range")
})
