# The cerebellar reference is opt-in (unset = no fetch); pin it off anyway so
# no test depends on a user's session options. (A setup file, not a helper:
# pkgload::load_all() also sources helpers, which would leak into builds.)
options(neuromosaic.cerebellum_reference = FALSE)
