# GIFTI triangles are 0-based, and so is neurosurf::SurfaceGeometry(). The
# reader used to add 1, which pushed the largest index past the vertex count,
# made the constructor fail, and returned NULL (issue #42).

# A minimal ASCII-encoded GIFTI surface: a tetrahedron. Coordinates are
# non-integer because the reader tells coordinates from faces that way.
write_test_gifti <- function(coords, faces) {
  arr <- function(m, intent, type) {
    sprintf(
      paste0(
        "<DataArray Intent=\"%s\" DataType=\"%s\" ",
        "ArrayIndexingOrder=\"RowMajorOrder\" Dimensionality=\"2\" ",
        "Dim0=\"%d\" Dim1=\"3\" Encoding=\"ASCII\" Endian=\"LittleEndian\" ",
        "ExternalFileName=\"\" ExternalFileOffset=\"\">",
        "<MetaData/><Data>%s</Data></DataArray>"
      ),
      intent, type, nrow(m), paste(t(m), collapse = " ")
    )
  }
  path <- tempfile(fileext = ".surf.gii")
  writeLines(
    paste0(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<GIFTI Version=\"1.0\" NumberOfDataArrays=\"2\"><MetaData/><LabelTable/>",
      arr(coords, "NIFTI_INTENT_POINTSET", "NIFTI_TYPE_FLOAT32"),
      arr(faces, "NIFTI_INTENT_TRIANGLE", "NIFTI_TYPE_INT32"),
      "</GIFTI>"
    ),
    path
  )
  path
}

tetra_coords <- rbind(
  c(0.5, 0.1, 0.2),
  c(10.25, 0, 0),
  c(0, 10.5, 0),
  c(0, 0, 10.75)
)
tetra_faces <- rbind(c(0, 1, 2), c(0, 1, 3), c(0, 2, 3), c(1, 2, 3))

test_that("GIFTI faces stay 0-based for SurfaceGeometry()", {
  skip_if_not_installed("gifti")
  path <- write_test_gifti(tetra_coords, tetra_faces)

  geom <- neuromosaic:::.read_gifti_surface_geometry(path, hemi = "L")

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(neurosurf::coords(geom)), 4L)
  expect_equal(geom@hemi, "left")
  # neurosurf stores faces 1-based internally; they must be the file's
  # triangles, not shifted by one.
  expect_equal(
    unname(neurosurf::faces(geom)),
    unname(matrix(as.integer(tetra_faces + 1L), ncol = 3L))
  )
})

test_that("unambiguously 1-based faces are shifted to 0-based", {
  expect_equal(
    neuromosaic:::.gifti_faces_zero_based(tetra_faces + 1L, n_vert = 4L),
    matrix(as.integer(tetra_faces), ncol = 3L)
  )
  # 0-based input is left alone, including when index 1 is the smallest
  # value but the largest stays below the vertex count.
  expect_equal(
    neuromosaic:::.gifti_faces_zero_based(tetra_faces, n_vert = 4L),
    matrix(as.integer(tetra_faces), ncol = 3L)
  )
  partial <- rbind(c(1, 2, 3), c(1, 2, 3))
  expect_equal(
    neuromosaic:::.gifti_faces_zero_based(partial, n_vert = 4L),
    matrix(as.integer(partial), ncol = 3L)
  )
})

test_that("a surface that cannot be built warns with the file name", {
  skip_if_not_installed("gifti")
  bad <- tetra_faces
  bad[1, 1] <- 7 # beyond the four vertices
  path <- write_test_gifti(tetra_coords, bad)

  expect_warning(
    geom <- neuromosaic:::.read_gifti_surface_geometry(path, hemi = "R"),
    basename(path),
    fixed = TRUE
  )
  expect_null(geom)
})

test_that("the TemplateFlow fsaverage5 white surface loads with 10,242 vertices", {
  skip_if_not_installed("gifti")
  path <- file.path(
    Sys.getenv("TEMPLATEFLOW_HOME", path.expand("~/Library/Caches/templateflow")),
    "tpl-fsaverage", "tpl-fsaverage_hemi-L_den-10k_white.surf.gii"
  )
  skip_if_not(file.exists(path) && file.size(path) > 0, "fsaverage5 white surface not cached")

  geom <- neuromosaic:::.read_gifti_surface_geometry(path, hemi = "L")

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(neurosurf::coords(geom)), 10242L)
})

test_that("the fallback warning says inflated geometry gives wrong anatomy", {
  msg <- neuromosaic:::.overlay_fallback_message("white", "lh", "inflated")
  expect_match(msg, "white surface for hemi 'lh'", fixed = TRUE)
  expect_match(msg, "atlas's own inflated geometry", fixed = TRUE)
  expect_match(msg, "wrong anatomy", fixed = TRUE)
  expect_match(
    neuromosaic:::.overlay_fallback_message("pial", "rh", NA_character_),
    "own display geometry",
    fixed = TRUE
  )
})
