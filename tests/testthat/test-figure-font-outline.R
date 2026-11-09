test_that("figure family ignores bitmap-only faces such as X11 Helvetica", {
  fonts <- data.frame(
    family = c("Helvetica", "Helvetica", "Arial", "DejaVu Sans"),
    path = c("/usr/share/fonts/75dpi/helvR12-ISO8859-1.pcf.gz",
             "/usr/share/fonts/100dpi/helvBO12.pcf.gz",
             "/usr/share/fonts/msttcore/arial.ttf",
             "/usr/share/fonts/dejavu/DejaVuSans.ttf"),
    stringsAsFactors = FALSE
  )
  expect_identical(.nm_pick_figure_family(fonts)[[1]], "Arial")
  expect_length(.nm_pick_figure_family(fonts[1:2, ]), 0)
  fonts$path[1] <- "/System/Library/Fonts/Helvetica.ttc"
  expect_identical(.nm_pick_figure_family(fonts)[[1]], "Helvetica")
  mac <- data.frame(family = "Avenir Next", path = "/System/Library/Fonts/Avenir Next.ttc")
  expect_identical(.nm_pick_figure_family(rbind(fonts, mac))[[1]], "Avenir Next")
  expect_length(.nm_pick_figure_family(NULL), 0)
  expect_length(.nm_pick_figure_family(data.frame(x = 1)), 0)
})
