# Mixed quantities deliberately remain below their fixed display maxima.
pkgload::load_all(quiet = TRUE)
artifact <- file.path(getwd(), "e2e", ".artifacts")
dir.create(artifact, recursive = TRUE, showWarnings = FALSE)
dims <- c(10L, 10L, 8L)
grid <- neuroim2::NeuroSpace(dims, spacing = c(2, 2, 2), origin = c(-10, -10, -8))
vol <- function(x) neuroim2::NeuroVol(array(x, dims), grid)
ijk <- arrayInd(seq_len(prod(dims)), dims)
radius <- rowSums(sweep(ijk, 2, c(5, 5, 4))^2)
bg <- vol(pmax(0, 100 - radius * 3))
ids <- c("prevalence", "coverage", "reference")
upper <- c(100, 17, 1)
maxima <- c(35, 12, 1)
manifest <- data.frame(
  analysis_id = "mixed", map_id = ids, role = c("primary", "auxiliary", "auxiliary"),
  quantity = ids, label = c("Prevalence", "Coverage", "Binary map"),
  threshold = c(1, 1, 0.5), signed = FALSE, tail = "positive", min_cluster_size = 1
)
manifest$stat_map <- lapply(maxima, function(top) vol(ifelse(radius < 12, top, 0)))
profiles <- lapply(seq_along(ids), function(i) montage_map_profile(
  ids[[i]], display_mode = "thresholded", scale = "sequential",
  palette_family = "inferno", limits = c(0, upper[[i]])
))
render_montage_report(
  manifest, output_file = file.path(artifact, "fixed-scales.html"), bg = bg,
  profiles = profiles, policy = montage_policy(cap = 50),
  title = "Independent fixed display scales", render_peaks = FALSE,
  interactive = montage_interactive(assets = "bundle"),
  volume_args = list(zlevels = 3:5), quiet = TRUE
)
