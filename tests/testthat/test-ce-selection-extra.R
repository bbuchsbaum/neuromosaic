# Additional tests for ce_selection.R pure helpers

test_that(".keep_peak_for_tail two_sided accepts both signs above threshold", {
  expect_true(neuromosaic:::.keep_peak_for_tail(5.0, 3.0, "two_sided"))
  expect_true(neuromosaic:::.keep_peak_for_tail(-5.0, 3.0, "two_sided"))
  expect_false(neuromosaic:::.keep_peak_for_tail(2.0, 3.0, "two_sided"))
  expect_false(neuromosaic:::.keep_peak_for_tail(-2.0, 3.0, "two_sided"))
})

test_that(".keep_peak_for_tail positive only accepts positive", {
  expect_true(neuromosaic:::.keep_peak_for_tail(5.0, 3.0, "positive"))
  expect_false(neuromosaic:::.keep_peak_for_tail(-5.0, 3.0, "positive"))
})

test_that(".keep_peak_for_tail negative only accepts negative", {
  expect_true(neuromosaic:::.keep_peak_for_tail(-5.0, 3.0, "negative"))
  expect_false(neuromosaic:::.keep_peak_for_tail(5.0, 3.0, "negative"))
})

test_that(".sign_label_from_peak returns correct labels", {
  expect_equal(neuromosaic:::.sign_label_from_peak(3.5), "positive")
  expect_equal(neuromosaic:::.sign_label_from_peak(-3.5), "negative")
  expect_equal(neuromosaic:::.sign_label_from_peak(0), "unsigned")
})

test_that(".normalize_sphere_centers handles matrix input", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 3, byrow = TRUE)
  res <- neuromosaic:::.normalize_sphere_centers(m)
  expect_true(is.matrix(res))
  expect_equal(nrow(res), 2L)
  expect_equal(ncol(res), 3L)
})

test_that(".normalize_sphere_centers handles numeric vector input", {
  res <- neuromosaic:::.normalize_sphere_centers(c(10, 20, 30))
  expect_true(is.matrix(res))
  expect_equal(nrow(res), 1L)
  expect_equal(ncol(res), 3L)
})

test_that(".normalize_sphere_centers returns NULL for NULL input", {
  expect_null(neuromosaic:::.normalize_sphere_centers(NULL))
})

test_that(".normalize_sphere_centers errors on wrong dimensions", {
  expect_error(
    neuromosaic:::.normalize_sphere_centers(c(1, 2))
  )
})
