# Tests for utility functions in ce_compute.R

# -- .safe_numeric / .safe_integer ---------------------------------------------

test_that(".safe_numeric returns numeric for valid input", {
  expect_equal(neuromosaic:::.safe_numeric("3.14"), 3.14)
  expect_equal(neuromosaic:::.safe_numeric(42), 42)
})

test_that(".safe_numeric returns default for invalid input", {
  expect_equal(neuromosaic:::.safe_numeric("abc"), NA_real_)
  expect_equal(neuromosaic:::.safe_numeric("abc", 0), 0)
  expect_equal(neuromosaic:::.safe_numeric(NULL, -1), -1)
  expect_equal(neuromosaic:::.safe_numeric(NA, 99), 99)
})

test_that(".safe_integer returns integer for valid input", {
  expect_equal(neuromosaic:::.safe_integer("7"), 7L)
  expect_equal(neuromosaic:::.safe_integer(5L), 5L)
})

test_that(".safe_integer returns default for invalid input", {
  expect_equal(neuromosaic:::.safe_integer("xyz"), NA_integer_)
  expect_equal(neuromosaic:::.safe_integer(NULL, 0L), 0L)
})

# -- .empty_cluster_table / .empty_cluster_parcels -----------------------------

test_that(".empty_cluster_table returns zero-row tibble with correct schema", {
  tbl <- neuromosaic:::.empty_cluster_table()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_true(all(c("cluster_id", "sign", "component_id", "n_voxels",
                    "peak_x", "peak_y", "peak_z", "max_stat",
                    "peak_coord") %in% names(tbl)))
})

test_that(".empty_cluster_parcels returns zero-row tibble with correct schema", {
  tbl <- neuromosaic:::.empty_cluster_parcels()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_true(all(c("cluster_id", "sign", "parcel_id", "parcel_label",
                    "n_voxels", "frac", "peak_stat", "max_pos",
                    "min_neg") %in% names(tbl)))
})

# -- LRU cache helpers ---------------------------------------------------------

test_that(".cache_get works with LRU cache objects", {
  cache <- neuromosaic:::.new_lru_cache(max_entries = 10L)
  cache$set("k1", 42)
  expect_equal(neuromosaic:::.cache_get(cache, "k1"), 42)
  expect_null(neuromosaic:::.cache_get(cache, "missing"))
})

test_that(".cache_get works with environment cache", {
  env <- new.env(parent = emptyenv())
  assign("k1", 42, envir = env)
  expect_equal(neuromosaic:::.cache_get(env, "k1"), 42)
  expect_null(neuromosaic:::.cache_get(env, "missing"))
})

test_that(".cache_get returns NULL for NULL cache", {
  expect_null(neuromosaic:::.cache_get(NULL, "k1"))
})

test_that(".cache_set and .cache_exists work with LRU cache", {
  cache <- neuromosaic:::.new_lru_cache(max_entries = 10L)
  expect_false(neuromosaic:::.cache_exists(cache, "x"))
  neuromosaic:::.cache_set(cache, "x", 99)
  expect_true(neuromosaic:::.cache_exists(cache, "x"))
  expect_equal(neuromosaic:::.cache_get(cache, "x"), 99)
})

test_that(".cache_set and .cache_exists work with environment cache", {
  env <- new.env(parent = emptyenv())
  expect_false(neuromosaic:::.cache_exists(env, "y"))
  neuromosaic:::.cache_set(env, "y", 77)
  expect_true(neuromosaic:::.cache_exists(env, "y"))
})

test_that(".cache_clear empties LRU cache", {
  cache <- neuromosaic:::.new_lru_cache(max_entries = 10L)
  cache$set("a", 1)
  cache$set("b", 2)
  expect_equal(cache$size(), 2L)
  neuromosaic:::.cache_clear(cache)
  expect_equal(cache$size(), 0L)
})

test_that(".cache_clear empties environment cache", {
  env <- new.env(parent = emptyenv())
  assign("a", 1, envir = env)
  neuromosaic:::.cache_clear(env)
  expect_equal(length(ls(env)), 0L)
})

# -- .grid_to_linear_index ----------------------------------------------------

test_that(".grid_to_linear_index computes correct indices", {
  grid <- matrix(c(1L, 1L, 1L,
                   2L, 1L, 1L,
                   1L, 2L, 1L), ncol = 3, byrow = TRUE)
  dims <- c(5L, 5L, 5L)
  idx <- neuromosaic:::.grid_to_linear_index(grid, dims)
  expect_equal(idx[1], 1L)   # (1,1,1) = 1
  expect_equal(idx[2], 2L)   # (2,1,1) = 2
  expect_equal(idx[3], 6L)   # (1,2,1) = 1 + 5 = 6
})

# -- .normalize_sample_table --------------------------------------------------

test_that(".normalize_sample_table adds .sample_index", {
  tbl <- neuromosaic:::.normalize_sample_table(
    sample_table = data.frame(condition = c("A", "B")),
    n_samples = 2L
  )
  expect_true(".sample_index" %in% names(tbl))
  expect_equal(tbl$.sample_index, 1:2)
})

test_that(".normalize_sample_table creates default table when NULL", {
  tbl <- neuromosaic:::.normalize_sample_table(
    sample_table = NULL,
    n_samples = 5L
  )
  expect_equal(nrow(tbl), 5L)
  expect_true(".sample_index" %in% names(tbl))
})
