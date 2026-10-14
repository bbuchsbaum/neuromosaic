# Small deterministic subject maps; no downloads or participant data.
pkgload::load_all(quiet = TRUE)
artifact <- file.path(getwd(), "e2e", ".artifacts")
dir.create(artifact, recursive = TRUE, showWarnings = FALSE)
dims <- c(10L, 10L, 8L)
grid <- neuroim2::NeuroSpace(dims, spacing = c(2, 2, 2), origin = c(-10, -10, -8))
vol <- function(values) neuroim2::NeuroVol(array(values, dims), grid)
ijk <- arrayInd(seq_len(prod(dims)), dims)
radius <- rowSums(sweep(ijk, 2, c(5, 5, 4))^2)
mask <- vol(as.numeric(radius < 22))
bg <- vol(pmax(0, 100 - radius * 3))
subjects <- paste0("sub-0", 1:4)
manifest <- do.call(rbind, lapply(seq_along(subjects), function(i) {
  subject <- subjects[[i]]
  data.frame(map_id = paste(subject, c("t", "beta", "se"), sep = "_"),
    analysis_id = subject, analysis_label = paste("Faces:", subject),
    role = c("primary", "auxiliary", "auxiliary"),
    quantity = c("test_statistic", "estimate", "standard_error"),
    distribution = c("t", NA, NA), df = c(30, NA, NA),
    threshold = c(2, NA, NA), label = c("T statistic", "Estimate", "Standard error"),
    units = c(NA, "percent", "percent"),
    selector_label = c("T", "Estimate", "SE"))
}))
manifest$stat_map <- I(unlist(lapply(seq_along(subjects), function(i) {
  values <- sin(ijk[, 1] * .4 + i * .4) * cos(ijk[, 2] * .3) +
    sin(ijk[, 3] * .7 + i) * .6
  values[seq_len(50)] <- 0 # genuine shared finite zeros, within explicit support
  list(vol(values * 5), vol(values * .8), vol(rep(.2 + i * .01, prod(dims))))
}), recursive = FALSE))
members <- data.frame(map_id = manifest$map_id,
                      member_id = rep(subjects, each = 3), model = "first-level",
                      contrast = "faces")
sets <- list(montage_map_set("faces", members, mask,
  label = "Faces: all subjects", member_kind = "subject",
  atlas = list(name = "Synthetic halves", labels = vol(ifelse(radius < 22,
    ifelse(ijk[, 1] < 5, 1, 2), 0)),
    regions = data.frame(id = c(1L, 2L), label = c("Left half", "Right half"))),
  covariates = data.frame(member_id = subjects, age = c(24, 30, 32, 28),
    included = c(TRUE, FALSE, TRUE, NA))),
  montage_map_set("subset", members[members$member_id != "sub-04", ], mask,
    label = "Faces: first three subjects", member_kind = "subject"))
config <- montage_space(sets, statistic = "t", height = 650)
for (extension in c("html", "qmd")) {
  render_montage_report(manifest,
    output_file = file.path(artifact, paste0("space-report.", extension)),
    title = "Subject maps: Space integration", bg = bg,
    intro = "A deterministic synthetic example for the Space integration.",
    interactive = montage_interactive(assets = "bundle"), space = config,
    render_peaks = FALSE, image_width = 480, image_height = 300, image_res = 72,
    cache_dir = file.path(artifact, "space-cache"),
    image_dir = file.path(artifact, "space-images"), quiet = TRUE)
}
single <- manifest[manifest$quantity == "test_statistic", ]
single_members <- members[members$map_id %in% single$map_id, ]
render_montage_report(single,
  output_file = file.path(artifact, "space-single.html"),
  title = "Single-map subject reports", bg = bg,
  space = montage_space(montage_map_set("single", single_members, mask,
    member_kind = "subject")), render_peaks = FALSE,
  image_width = 480, image_height = 300, image_res = 72,
  cache_dir = file.path(artifact, "space-cache"),
  image_dir = file.path(artifact, "space-images"), quiet = TRUE)
