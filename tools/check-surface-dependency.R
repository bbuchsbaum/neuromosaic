# Run from the package root: RGL_USE_NULL=TRUE Rscript tools/check-surface-dependency.R
# Install the declared neurosurf remote into a fresh target library. Other
# dependencies may be resolved from the caller's configured libraries.
Sys.setenv(RGL_USE_NULL = "TRUE")
lib <- tempfile("neuromosaic-surface-library-", tmpdir = dirname(tempdir()))
dir.create(lib)
.libPaths(c(lib, .libPaths()))
description <- read.dcf("DESCRIPTION")
remotes <- trimws(strsplit(description[1, "Remotes"], ",", fixed = TRUE)[[1]])
remote <- remotes[grepl("^bbuchsbaum/neurosurf@", remotes)]
stopifnot(length(remote) == 1L)
remotes::install_github(remote, lib = lib, dependencies = NA, upgrade = "never")
stopifnot(normalizePath(find.package("neurosurf")) == normalizePath(file.path(lib, "neurosurf")))
stopifnot(identical(utils::packageDescription("neurosurf", lib.loc = lib)$RemoteSha,
                    sub(".*@", "", remote)))
tarball <- pkgbuild::build(".", dest_path = dirname(lib), vignettes = FALSE, manual = FALSE)
status <- system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(tarball)))
stopifnot(status == 0L)
# Retain the isolated library for independent consumer checks.
stopifnot(normalizePath(find.package("neuromosaic")) == normalizePath(file.path(lib, "neuromosaic")))
library(neuromosaic)
result <- testthat::test_file("tests/testthat/test-neurosurf-contract.R", stop_on_failure = TRUE)
cat("Installed and verified:", remote, "in", lib, "\n")
