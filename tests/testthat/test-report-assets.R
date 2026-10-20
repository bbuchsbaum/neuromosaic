report_asset_fixture <- function(root = tempfile("report-export-")) {
  skip_if_not_installed("png")
  dir.create(root, recursive = TRUE)
  images <- vapply(seq_len(4L), function(i) {
    path <- file.path(root, paste0("source-", i, ".png"))
    png::writePNG(array(seq(0, 1, length.out = 16L * 12L * 3L) / i,
                        c(12L, 16L, 3L)), path)
    path
  }, character(1))
  copy <- file.path(root, "identical-different-name.png")
  file.copy(images[[1]], copy)
  manifest <- data.frame(map_id = c("first", "second"),
    quantity = "test_statistic", distribution = "z", signed = TRUE,
    threshold = 3, label = c("First map", "Second map"), path = "unused.nii.gz")
  panels <- stats::setNames(lapply(seq_len(2L), function(i) list(
    volume_image = if (i == 1L) images[[1]] else copy,
    volume_image_dark = images[[2]], volume_image_narrow = images[[3]],
    volume_image_narrow_dark = images[[4]]
  )), manifest$map_id)
  list(root = root, images = images, manifest = manifest, panels = panels)
}

render_asset_fixture <- function(fx, mode, extension = "html") {
  output <- file.path(fx$root, mode, paste0("index.", extension))
  render_montage_report(fx$manifest, output, panels = fx$panels,
    materialize_recipes = FALSE, check_files = FALSE, assets = mode)
}

test_that("bundle packaging deduplicates figures without changing any variant", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("xml2")
  skip_if_not(rmarkdown::pandoc_available())
  fx <- report_asset_fixture()
  embed <- render_asset_fixture(fx, "embed")
  bundle <- render_asset_fixture(fx, "bundle")
  a <- report_size(embed); b <- report_size(bundle)
  expect_true(a$complete); expect_true(b$complete)
  figures <- b$files[b$files$category == "figures", ]
  expect_equal(nrow(figures), 4L)
  expect_setequal(figures$sha256, vapply(fx$images, function(p) {
    digest::digest(file = p, algo = "sha256", serialize = FALSE)
  }, character(1)))
  expect_equal(b$total_bytes, sum(b$files$bytes))
  expect_equal(b$total_bytes, b$html_bytes + b$companion_bytes)
  expect_gt(nrow(a$embedded), 4L)
  expect_true(anyDuplicated(a$embedded$sha256) > 0L)
  doc <- xml2::read_html(bundle)
  expect_equal(length(xml2::xml_find_all(doc, "//figure[contains(@class, 'nm-figure-volume')]//source")), 6L)
  expect_equal(length(xml2::xml_find_all(doc, "//figure[contains(@class, 'nm-figure-volume')]/noscript/img")), 2L)
  expect_false(any(grepl("neuroimjs|surfview", b$files$path)))
})

test_that("a staged bundle includes only verified required files and relocates", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("xml2")
  skip_if_not(rmarkdown::pandoc_available())
  fx <- report_asset_fixture()
  output <- render_asset_fixture(fx, "bundle")
  writeLines("unrelated source/cache", file.path(dirname(output), "unrelated.nii"))
  dir.create(file.path(dirname(output), "index_files", "obsolete"))
  writeLines("obsolete", file.path(dirname(output), "index_files", "obsolete", "old.js"))
  destination <- file.path(fx$root, "relocated")
  copied <- write_report_bundle(output, destination)
  size <- report_size(copied)
  expect_true(size$complete)
  expect_setequal(list.files(destination, recursive = TRUE), size$files$path)
  expect_identical(size$files$sha256, report_size(output)$files$sha256)
  expect_false(file.exists(file.path(destination, "unrelated.nii")))
  expect_true(file.exists(file.path(dirname(output), "unrelated.nii")))
  expect_error(write_report_bundle(output, destination), "new or empty")
  figure <- size$files$path[size$files$category == "figures"][[1]]
  unlink(file.path(destination, figure))
  expect_error(report_size(copied), "No such file|does not exist|cannot find|path")
})

test_that("content-addressed copies reject corruption rather than overwriting it", {
  fx <- report_asset_fixture()
  directory <- file.path(fx$root, "copies")
  name <- .report_copy_asset(fx$images[[1]], directory)
  writeBin(charToRaw("tampered"), file.path(directory, name))
  expect_error(.report_copy_asset(fx$images[[1]], directory), "content hash")
  expect_identical(readBin(file.path(directory, name), "raw", n = 8L), charToRaw("tampered"))
})

test_that("packaging validation happens before materialization", {
  expect_error(render_montage_report(NULL, "test.pdf", assets = "bundle"), "HTML/QMD")
  expect_error(render_montage_report(NULL, "test.html", space = list(), assets = "embed"), "Space requires")
  expect_error(render_cluster_report(NULL, "test.pdf", assets = "embed"), "HTML/QMD")
  expect_error(render_montage_report(NULL, assets = "invalid"), "arg")
})

test_that("QMD preserves explicit packaging and relative figure directories", {
  fx <- report_asset_fixture()
  for (mode in c("bundle", "embed")) {
    output <- suppressMessages(render_asset_fixture(fx, mode, "qmd"))
    lines <- readLines(output)
    delimiters <- which(lines == "---")
    header <- yaml::yaml.load(paste(lines[seq.int(delimiters[[1]] + 1L,
                                                  delimiters[[2]] - 1L)], collapse = "\n"))
    expect_identical(header$format$html$`embed-resources`, mode == "embed")
    expect_true(any(grepl(paste0("embed-resources: ", tolower(as.character(mode == "embed"))),
                          lines, fixed = TRUE)))
    expect_true(any(grepl("nm-report-assets", lines, fixed = TRUE)))
    rd <- readRDS(sub("\\.qmd$", "_report-data.rds", output))
    if (mode == "bundle") expect_identical(rd$figure_assets$dir, "index_files/figures")
  }
})

test_that("inventory distinguishes legacy, remote, and escaping resource references", {
  skip_if_not_installed("xml2")
  root <- tempfile("resource-refs-"); dir.create(root)
  path <- file.path(root, "index.html")
  writeLines('<html><script src="https://example.invalid/runtime.js"></script></html>', path)
  size <- report_size(path)
  expect_false(size$complete)
  expect_identical(size$remote_resources, "https://example.invalid/runtime.js")
  expect_error(write_report_bundle(path, file.path(root, "copy")), "incomplete")
  outside <- tempfile("outside-"); writeLines("outside", outside)
  writeLines(paste0('<img src="../', basename(outside), '">'), path)
  expect_error(report_size(path), "outside the report directory")
})

test_that("CSS dependencies are included without unrelated files", {
  skip_if_not_installed("xml2")
  root <- tempfile("css-assets-"); dir.create(root)
  dir.create(file.path(root, "css")); dir.create(file.path(root, "fonts"))
  writeBin(as.raw(1:12), file.path(root, "fonts", "test.woff2"))
  writeLines("@font-face {src:url('../fonts/test.woff2')}\nbody{color:red}",
             file.path(root, "css", "style.css"))
  path <- file.path(root, "index.html")
  writeLines('<link rel="stylesheet" href="css/style.css">', path)
  .report_append_registry(path, character())
  size <- report_size(path)
  expect_true(size$complete)
  expect_setequal(size$files$path, c("index.html", "css/style.css", "fonts/test.woff2"))
})

test_that("resource inventories retain commas in filenames and resolve srcsets", {
  skip_if_not_installed("xml2")
  root <- tempfile("srcset-assets-"); dir.create(root)
  writeBin(as.raw(1:4), file.path(root, "a,b.png"))
  writeBin(as.raw(5:8), file.path(root, "large.png"))
  path <- file.path(root, "index.html")
  writeLines('<img src="a,b.png"><source srcset="large.png 2x">', path)
  .report_append_registry(path, character())
  expect_setequal(report_size(path)$files$path, c("index.html", "a,b.png", "large.png"))
})

test_that("size diagnostics do not truncate large embedded payloads", {
  skip_if_not_installed("xml2")
  path <- tempfile(fileext = ".html")
  payload <- strrep("A", 11L * 1024L * 1024L)
  writeLines(paste0('<script type="application/octet-stream">', payload, '</script>'), path)
  .report_append_registry(path, character())
  size <- report_size(path)
  expect_true(size$complete)
  expect_equal(size$embedded$encoded_bytes, nchar(payload, type = "bytes"))
  expect_equal(size$embedded$binary_bytes, nchar(payload, type = "bytes") * 3 / 4)
})

test_that("cluster reports forward packaging and retain template options", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("xml2")
  skip_if_not(rmarkdown::pandoc_available())
  toy <- make_toy_cluster_report_inputs()
  root <- tempfile("cluster-assets-"); dir.create(root)
  out <- file.path(root, "index.html")
  result <- suppressWarnings(cluster_report(toy$stat_map, atlas = toy$atlas,
    threshold = 3, min_cluster_size = 3, output_file = out,
    brain_slices = FALSE, assets = "bundle"))
  size <- report_size(result$report_path)
  expect_true(size$complete)
  expect_gt(size$companion_bytes, 0)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "flatly|bootstrap", perl = TRUE)
})
