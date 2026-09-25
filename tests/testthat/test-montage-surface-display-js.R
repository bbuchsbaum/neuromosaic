# The browser-side helpers that format numbers and choose colour-bar ticks for
# the interactive surface widget are plain functions; exercise them in V8.
surface_display_js <- function() {
  skip_if_not_installed("V8")
  path <- system.file("htmlwidgets", "lib", "neuromosaic-surface", "display.js",
                      package = "neuromosaic")
  ctx <- V8::v8()
  ctx$eval("var globalThis = this;")
  ctx$source(path)
  ctx
}

test_that("surface numbers use three significant figures and a true minus", {
  ctx <- surface_display_js()
  fmt <- function(x, minus = TRUE) {
    ctx$call("NeuroMosaicSurfaceDisplay.formatNumber", x, list(minus = minus))
  }
  expect_identical(fmt(7.2243567609787), "7.22")
  expect_identical(fmt(-7.2243567609787), "−7.22")
  expect_identical(fmt(-7.2243567609787, minus = FALSE), "-7.22")
  expect_identical(fmt(3.1), "3.1")
  expect_identical(fmt(0.85), "0.85")
  expect_identical(fmt(1234.5), "1235")
  expect_identical(fmt(0), "0")
  expect_identical(fmt(12), "12")
  expect_identical(ctx$call("NeuroMosaicSurfaceDisplay.formatNumber", "abc"), "")
})

test_that("colour-bar ticks keep the range ends and fit the threshold between", {
  ctx <- surface_display_js()
  ticks <- function(range, mask, gap = 0.16) {
    unlist(ctx$call("NeuroMosaicSurfaceDisplay.legendTicks", range, mask, gap))
  }
  expect_equal(ticks(c(-7.22, 7.22), c(-3.1, 3.1)), c(-7.22, -3.1, 3.1, 7.22))
  # A narrow bar keeps the scale (the ends) and drops crowded inner ticks.
  expect_equal(ticks(c(-7.22, 7.22), c(-3.1, 3.1), 0.3), c(-7.22, 7.22))
  # Without a mask a diverging range marks zero.
  expect_equal(ticks(c(-7.22, 7.22), c(0, 0)), c(-7.22, 0, 7.22))
  # A mask edge at the range end is not duplicated.
  expect_equal(ticks(c(0, 1), c(0, 0.5)), c(0, 0.5, 1))
  expect_length(ticks(c(1, 1), c(0, 0)), 0)
})

test_that("hover regions read as networks with the atlas parcel as detail", {
  ctx <- surface_display_js()
  describe <- function(id, entry = NULL, atlas = "Schaefer-100-7networks") {
    ctx$call("NeuroMosaicSurfaceDisplay.describeParcel", id, entry, atlas)
  }
  vis <- describe(3, list(label = "Vis_3", full = "LH_Vis_3", network = "Vis"))
  expect_identical(vis$name, "Visual network")
  expect_identical(vis$detail, "LH_Vis_3 · Schaefer-100-7networks")
  pfc <- describe(80, list(label = "Default_PFC_2", full = "RH_Default_PFC_2",
                           network = "Default"))
  expect_identical(pfc$name, "Default network, prefrontal")
  expect_identical(
    describe(10, list(label = "DefaultA_pCunPCC_3", network = "DefaultA"))$name,
    "Default A network, precuneus / PCC"
  )
  expect_identical(describe(0)$name, "Medial wall")
  # Unknown atlases keep their own labels.
  expect_identical(describe(5, list(label = "V1"), "Glasser")$name, "V1")
})
