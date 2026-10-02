# Report figures: the shared "nm-signed" palette, the static volume montage
# (the report's figure of record), its horizontal colour key, the display
# cluster-extent mask, and the per-map glass-brain summary thumbnail.
#
# Everything here draws with grid on top of plain numeric arrays, so the
# figure does not depend on neuroim2's slice-label or legend conventions.

# Palette -----------------------------------------------------------------

# Threshold-saturated diverging palette shared by every report view. The
# first stop is the colour at |value| = threshold and the last stop the colour
# at the display cap; nothing below threshold is ever drawn.
.nm_signed_stops <- list(
  positive = c("#B40426", "#E8552C", "#F59E4C", "#FFE08A"),
  negative = c("#1D4E9E", "#2F7FC9", "#4FB3E0", "#A6E3F5")
)

# Colour of the never-drawn |value| < threshold band in exported colour
# vectors (e.g. for the interactive key); neutral so it never reads as signal.
.nm_signed_gap_colour <- "#C9CDD0"

.nm_palette_names <- c("nm-signed", "nm-positive", "nm-negative")

.nm_is_palette <- function(palette) {
  is.character(palette) && length(palette) == 1L && !is.na(palette) &&
    palette %in% .nm_palette_names
}

.nm_ramp_rgb <- function(stops, t) {
  t <- pmin(pmax(as.numeric(t), 0), 1)
  out <- matrix(NA_real_, nrow = length(t), ncol = 3L)
  ok <- is.finite(t)
  if (any(ok)) {
    out[ok, ] <- grDevices::colorRamp(stops, space = "Lab")(t[ok]) / 255
  }
  out
}

# RGB (0-1) for values under a palette. `threshold` is the |value| at which the
# nm ramps start (0 for continuous maps); values outside `limits` are clipped.
.nm_value_rgb <- function(values, palette, limits, threshold = 0) {
  values <- as.numeric(values)
  out <- matrix(NA_real_, nrow = length(values), ncol = 3L)
  ok <- is.finite(values)
  if (!any(ok)) return(out)
  lo <- limits[[1L]]
  hi <- limits[[2L]]
  v <- pmin(pmax(values[ok], lo), hi)
  if (.nm_is_palette(palette)) {
    thr <- if (is.finite(threshold) && threshold > 0) threshold else 0
    cap <- max(abs(limits))
    span <- max(cap - thr, .Machine$double.eps)
    t <- (abs(v) - thr) / span
    rgb <- matrix(NA_real_, nrow = length(v), ncol = 3L)
    pos <- v >= 0
    if (identical(palette, "nm-negative")) pos[] <- FALSE
    if (identical(palette, "nm-positive")) pos[] <- TRUE
    if (any(pos)) rgb[pos, ] <- .nm_ramp_rgb(.nm_signed_stops$positive, t[pos])
    if (any(!pos)) rgb[!pos, ] <- .nm_ramp_rgb(.nm_signed_stops$negative, t[!pos])
    out[ok, ] <- rgb
    return(out)
  }
  pal <- neuroim2::resolve_cmap(palette, 256L)
  idx <- 1L + floor((v - lo) / max(hi - lo, .Machine$double.eps) * 255)
  idx <- pmin(pmax(idx, 1L), 256L)
  out[ok, ] <- t(grDevices::col2rgb(pal[idx])) / 255
  out
}

#' Report Palette Colours
#'
#' Returns a colour vector for the report's threshold-saturated diverging
#' palette (`"nm-signed"`), sampled evenly across `limits`. Values whose
#' magnitude is below `threshold` fall in a neutral band that the report never
#' draws; from the threshold to the cap the positive ramp runs dark red to pale
#' yellow and the negative ramp dark blue to pale cyan, so effects just above
#' threshold are already saturated.
#'
#' @param limits Finite increasing display limits, usually `c(-cap, cap)`.
#' @param threshold Non-negative magnitude where the ramps start.
#' @param n Number of colours.
#' @param palette One of `"nm-signed"`, `"nm-positive"`, `"nm-negative"`.
#'
#' @return A character vector of `n` hex colours.
#' @export
nm_palette_colors <- function(limits, threshold = 0, n = 256L,
                              palette = "nm-signed") {
  limits <- .validate_montage_limits(limits)
  if (!.nm_is_palette(palette)) {
    stop("'palette' must be one of: ", paste(.nm_palette_names, collapse = ", "),
         ".", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold < 0) {
    stop("'threshold' must be a non-negative number.", call. = FALSE)
  }
  n <- as.integer(n)
  values <- seq(limits[[1L]], limits[[2L]], length.out = n)
  rgb <- .nm_value_rgb(values, palette, limits, threshold)
  cols <- grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L])
  if (threshold > 0) cols[abs(values) < threshold] <- .nm_signed_gap_colour
  cols
}

# Ramp stops ordered low value -> high value for renderers that split a
# palette into negative and positive halves and stretch each half over
# [threshold, cap] themselves (neurosurf's CPU rasterizer).
.nm_threshold_stops <- function(palette = "nm-signed", tail = "two_sided") {
  neg <- rev(.nm_signed_stops$negative)
  pos <- .nm_signed_stops$positive
  if (identical(palette, "nm-positive") || identical(tail, "positive")) {
    return(pos)
  }
  if (identical(palette, "nm-negative") || identical(tail, "negative")) {
    return(neg)
  }
  c(neg, pos)
}

# Display cluster extent ----------------------------------------------------

# Suprathreshold display voxels that belong to clusters smaller than
# `min_cluster_size` (sign-separated, manifest connectivity). These are the
# clusters the report's cluster table excludes, so no view may draw them.
.montage_extent_excluded <- function(values,
                                     dims,
                                     display_mask,
                                     min_cluster_size,
                                     connectivity = "18-connect") {
  values <- as.numeric(values)
  excluded <- rep(FALSE, length(values))
  k <- suppressWarnings(as.integer(min_cluster_size))
  if (length(k) != 1L || is.na(k) || k <= 1L) return(excluded)
  if (length(dims) != 3L || prod(dims) != length(values)) return(excluded)
  connectivity <- match.arg(
    connectivity, c("18-connect", "26-connect", "6-connect")
  )
  display_mask <- as.logical(display_mask)
  display_mask[is.na(display_mask)] <- FALSE
  for (sgn in c(1, -1)) {
    mask <- display_mask & is.finite(values) & (sgn * values) > 0
    if (!any(mask)) next
    comps <- neuroim2::conn_comp_3D(
      array(mask, dim = dims), connect = connectivity
    )
    size <- as.integer(comps$size)
    excluded <- excluded | (mask & size < k)
  }
  excluded
}

# Slice geometry ------------------------------------------------------------

# Maps each array axis to its dominant world axis and step. Report spaces are
# axis-aligned (MNI templates); an oblique affine is reduced to its dominant
# axes, which only affects the drawn pixel aspect, never the labels.
.nm_axis_frame <- function(space) {
  dims <- dim(space)[seq_len(3L)]
  origin <- neuroim2::grid_to_coord(space, matrix(1, nrow = 1L, ncol = 3L))
  frame <- lapply(seq_len(3L), function(a) {
    unit <- matrix(1, nrow = 1L, ncol = 3L)
    unit[1L, a] <- 2
    step <- as.numeric(neuroim2::grid_to_coord(space, unit) - origin)
    world <- which.max(abs(step))
    list(world = world, step = step[[world]],
         start = as.numeric(origin)[[world]], n = dims[[a]])
  })
  frame
}

.nm_axis_world <- function(frame_axis, index) {
  frame_axis$start + (as.numeric(index) - 1) * frame_axis$step
}

# Slice `index` along array axis `along` as an image matrix oriented for
# display: rows top-to-bottom = vertical world axis descending, columns
# left-to-right = horizontal world axis ascending (neurological convention:
# subject left on image left).
.nm_slice_image <- function(arr, along, index, frame) {
  retained <- setdiff(seq_len(3L), along)
  mat <- switch(
    along,
    arr[index, , , drop = TRUE],
    arr[, index, , drop = TRUE],
    arr[, , index, drop = TRUE]
  )
  mat <- matrix(mat, nrow = frame[[retained[[1L]]]]$n)
  worlds <- vapply(frame[retained], `[[`, numeric(1), "world")
  # Horizontal axis = the retained axis with the lower world index
  # (x before y before z).
  if (worlds[[1L]] > worlds[[2L]]) {
    mat <- t(mat)
    retained <- rev(retained)
  }
  h <- frame[[retained[[1L]]]]
  v <- frame[[retained[[2L]]]]
  if (h$step < 0) mat <- mat[rev(seq_len(nrow(mat))), , drop = FALSE]
  if (v$step < 0) mat <- mat[, rev(seq_len(ncol(mat))), drop = FALSE]
  # Image: rows = vertical descending, cols = horizontal ascending.
  img <- t(mat)[rev(seq_len(ncol(mat))), , drop = FALSE]
  hw <- sort(.nm_axis_world(h, seq_len(h$n)))
  vw <- sort(.nm_axis_world(v, seq_len(v$n)))
  list(img = img, x = hw, y = rev(vw),
       h_world = h$world, v_world = v$world,
       dx = abs(h$step), dy = abs(v$step))
}

# Separable linear interpolation weights: `n` source samples to `n_out`
# (default `n * f`) output pixel centres; `n_out` need not be a multiple of
# `n`, so a tile can be composited directly at its display size.
.nm_interp_weights <- function(n, f, n_out = round(n * f)) {
  pos <- (seq_len(n_out) - 0.5) * n / n_out + 0.5
  pos <- pmin(pmax(pos, 1), n)
  lo <- floor(pos)
  hi <- pmin(lo + 1L, n)
  w <- pos - lo
  m <- matrix(0, nrow = n_out, ncol = n)
  m[cbind(seq_along(pos), lo)] <- 1 - w
  m[cbind(seq_along(pos), hi)] <- m[cbind(seq_along(pos), hi)] + w
  m
}

# Separable Catmull-Rom (cubic convolution, a = -0.5) weights on the same
# subpixel grid, edges replicated. The kernel interpolates (1 at its own
# sample, 0 at every other), so a voxel-centre value is reproduced exactly,
# while level sets are C1-smooth instead of following the voxel grid.
.nm_cubic_weights <- function(n, f, n_out = round(n * f)) {
  pos <- (seq_len(n_out) - 0.5) * n / n_out + 0.5
  pos <- pmin(pmax(pos, 1), n)
  base <- floor(pos)
  m <- matrix(0, nrow = n_out, ncol = n)
  kern <- function(x) {
    x <- abs(x)
    ifelse(x <= 1, 1.5 * x^3 - 2.5 * x^2 + 1,
           ifelse(x < 2, -0.5 * x^3 + 2.5 * x^2 - 4 * x + 2, 0))
  }
  for (k in -1:2) {
    idx <- base + k
    w <- kern(pos - idx)
    idx <- pmin(pmax(idx, 1L), n)
    cells <- cbind(seq_along(pos), idx)
    m[cells] <- m[cells] + w
  }
  m
}

# Resample a matrix by `f` per axis, or to exactly `out` = c(rows, cols).
.nm_upsample <- function(img, f, method = c("linear", "nearest", "cubic"),
                         out = NULL) {
  method <- match.arg(method)
  if (is.null(out)) {
    if (f <= 1L) return(img)
    out <- round(dim(img) * f)
  }
  out <- as.integer(out)
  if (identical(method, "nearest")) {
    pick <- function(n, n_out) {
      pmin(n, floor((seq_len(n_out) - 0.5) * n / n_out) + 1L)
    }
    return(img[pick(nrow(img), out[[1L]]), pick(ncol(img), out[[2L]]),
               drop = FALSE])
  }
  weights <- if (identical(method, "cubic")) .nm_cubic_weights else
    .nm_interp_weights
  wr <- weights(nrow(img), n_out = out[[1L]])
  wc <- weights(ncol(img), n_out = out[[2L]])
  wr %*% img %*% t(wc)
}

# Slice selection -----------------------------------------------------------

# Evenly spaced slices across the central mass of the display voxels (5th to
# 95th percentile of their positions along `along`), so near-empty edge slices
# are never chosen. The spacing is a whole number of voxels; with 2 mm voxels
# the labels step in even millimetres.
.stat_montage_central_zlevels <- function(supra_arr, along, max_slices = 12L,
                                          probs = c(0.05, 0.95)) {
  along <- as.integer(along)
  dims <- dim(supra_arr)
  n <- dims[[along]]
  counts <- vapply(seq_len(n), function(index) {
    sum(switch(
      along,
      supra_arr[index, , ],
      supra_arr[, index, ],
      supra_arr[, , index]
    ), na.rm = TRUE)
  }, numeric(1))
  occupied <- which(counts > 0)
  if (length(occupied) == 0L) {
    return(unique(round(seq(1, n, length.out = min(max_slices, n)))))
  }
  if (length(occupied) <= max_slices) return(occupied)
  cdf <- cumsum(counts) / sum(counts)
  lo <- which(cdf >= probs[[1L]])[[1L]]
  hi <- which(cdf >= probs[[2L]])[[1L]]
  if (hi - lo + 1L < max_slices) {
    centre <- (lo + hi) / 2
    lo <- max(1L, floor(centre - (max_slices - 1L) / 2))
    hi <- min(n, lo + max_slices - 1L)
    lo <- max(1L, hi - max_slices + 1L)
    return(seq.int(lo, hi))
  }
  step <- max(1L, floor((hi - lo) / (max_slices - 1L)))
  span <- step * (max_slices - 1L)
  start <- round((lo + hi) / 2 - span / 2)
  start <- min(max(1L, start), n - span)
  as.integer(start + step * (seq_len(max_slices) - 1L))
}

# Formatting ----------------------------------------------------------------

.nm_minus <- "\u2212"

.nm_format_signed <- function(x, digits = 0L, plus = TRUE) {
  txt <- formatC(abs(x), format = "f", digits = digits)
  sign <- ifelse(x < 0 & round(abs(x), digits) > 0, .nm_minus,
                 ifelse(plus & round(x, digits) > 0, "+", ""))
  paste0(sign, txt)
}

# Tick label: whole numbers stay whole, otherwise one decimal.
.nm_format_tick <- function(x) {
  vapply(x, function(v) {
    digits <- if (abs(v - round(v)) < 0.05) 0L else 1L
    .nm_format_signed(v, digits = digits, plus = FALSE)
  }, character(1))
}

.nm_slice_label <- function(axis_world, world_value) {
  paste0(c("x", "y", "z")[[axis_world]], " = ",
         .nm_format_signed(round(world_value), digits = 0L))
}

# Themes and type -----------------------------------------------------------

.nm_canvas <- "#FBFBF8"
# Dark figure canvas: the report page's dark `--nm-page` token, so dark
# figures sit flush on the dark page with no plate.
.nm_canvas_dark <- "#0F1214"

.nm_figure_theme <- function(theme = c("light", "dark")) {
  theme <- match.arg(theme)
  if (identical(theme, "dark")) {
    return(list(
      name = "dark", canvas = .nm_canvas_dark, ink = "#D9DFE2",
      label = "#BCC5CA", muted = "#98A3AA", note = "#AEB8BE",
      tick = "#7D888F",
      gap_fill = "#1A1F23", hatch = "#3B444A", rim = "#6F7980"
    ))
  }
  list(
    name = "light", canvas = .nm_canvas, ink = "#22292D",
    label = "#3A4247", muted = "#5F6A72", note = "#4C565D",
    tick = "#7A848B",
    gap_fill = "#EFEFEB", hatch = "#C4C8C5", rim = "#8E959A"
  )
}

.nm_font_env <- new.env(parent = emptyenv())

# One family for every word in the report figures: the page's sans (Avenir
# Next on macOS) when the ragg device can resolve it, else the device "sans".
# `medium` is a registered 500-weight variant used for the few emphasised
# marks (key title, L/R); everything else is regular.
.nm_figure_fonts <- function() {
  if (!is.null(.nm_font_env$fonts)) return(.nm_font_env$fonts)
  fonts <- list(regular = "sans", medium = "sans", medium_face = 2L)
  if (requireNamespace("ragg", quietly = TRUE) &&
      requireNamespace("systemfonts", quietly = TRUE)) {
    families <- tryCatch(
      unique(systemfonts::system_fonts()$family),
      error = function(e) character()
    )
    pick <- intersect(
      c("Avenir Next", "Avenir", "Inter", "Segoe UI", "Helvetica Neue",
        "Helvetica", "Arial"),
      families
    )
    if (length(pick)) {
      fonts$regular <- pick[[1L]]
      registered <- tryCatch({
        systemfonts::register_variant(
          "NM Figure Medium", pick[[1L]], weight = "medium"
        )
        TRUE
      }, error = function(e) FALSE)
      fonts$medium <- if (registered) "NM Figure Medium" else pick[[1L]]
      fonts$medium_face <- if (registered) 1L else 2L
    }
  }
  .nm_font_env$fonts <- fonts
  fonts
}

# Registered system fonts only resolve on the ragg devices. Figures built for
# .nm_open_png() use them; code that draws onto another device (pdf, base
# png) builds its grobs inside .nm_with_device_fonts().
.nm_plain_fonts <- function() {
  isTRUE(.nm_font_env$force_plain) || !requireNamespace("ragg", quietly = TRUE)
}

.nm_with_device_fonts <- function(code) {
  name <- names(grDevices::dev.cur())
  old <- .nm_font_env$force_plain
  .nm_font_env$force_plain <- !grepl("^agg", name %||% "")
  on.exit(.nm_font_env$force_plain <- old, add = TRUE)
  force(code)
}

.nm_gp_text <- function(size, col, medium = FALSE) {
  fonts <- if (.nm_plain_fonts()) {
    list(regular = "sans", medium = "sans", medium_face = 2L)
  } else {
    .nm_figure_fonts()
  }
  if (isTRUE(medium)) {
    grid::gpar(fontsize = size, col = col, fontfamily = fonts$medium,
               fontface = fonts$medium_face)
  } else {
    grid::gpar(fontsize = size, col = col, fontfamily = fonts$regular,
               fontface = 1L)
  }
}

# Underlay ------------------------------------------------------------------

# Display-only clean-up of a slice's tissue mask: 8-connected islands
# smaller than `max_voxels` and than `min_fraction` of the slice's largest
# island are template debris (e.g. a ~20-voxel fragment below the frontal pole
# of MNI152NLin6Asym at z = -32 that joins the brain only through other
# slices). They are dropped from the underlay so they never print as a dark
# speck; overlay voxels are untouched (a kept voxel over debris is still drawn).
.nm_tissue_debris <- function(body, max_voxels = 30L, min_fraction = 0.05) {
  out <- matrix(FALSE, nrow(body), ncol(body))
  if (!any(body) || nrow(body) < 2L || ncol(body) < 2L) return(out)
  comps <- tryCatch(
    neuroim2::conn_comp_3D(array(body, dim = c(dim(body), 1L)),
                           connect = "26-connect"),
    error = function(e) NULL
  )
  if (is.null(comps)) return(out)
  index <- matrix(as.integer(comps$index), nrow(body))
  size <- matrix(as.integer(comps$size), nrow(body))
  if (!any(index > 0L)) return(out)
  largest <- max(size[index > 0L])
  index > 0L & size < max_voxels & size < min_fraction * largest
}

.nm_underlay_dark <- c(0.20, 0.215, 0.23)
.nm_underlay_light <- c(0.784, 0.796, 0.804) # #C8CBCD

.nm_underlay_window <- function(bg_values) {
  v <- bg_values[is.finite(bg_values)]
  if (!length(v)) return(list(floor = 0, lo = 0, hi = 1, min = -1))
  rng <- range(v)
  floor <- rng[[1L]] + 0.04 * diff(rng)
  inside <- v[v > floor]
  if (length(inside) < 10L) inside <- v
  # The low end sits at the 8th percentile of in-brain intensity so the thin
  # partial-volume rim at the skull-strip edge never reads as a black line.
  q <- stats::quantile(inside, c(0.08, 0.995), names = FALSE)
  if (!(q[[2L]] > q[[1L]])) q <- rng
  list(floor = floor, lo = q[[1L]], hi = q[[2L]], min = rng[[1L]])
}

# Separable Gaussian smoothing (row and column operators), edge-normalised.
.nm_blur_operator <- function(n, sigma) {
  i <- seq_len(n)
  d <- outer(i, i, "-")
  k <- exp(-d^2 / (2 * sigma^2))
  k[abs(d) > ceiling(3 * sigma)] <- 0
  k / rowSums(k)
}

.nm_blur <- function(img, sigma) {
  if (!is.finite(sigma) || sigma <= 0) return(img)
  .nm_blur_operator(nrow(img), sigma) %*% img %*%
    t(.nm_blur_operator(ncol(img), sigma))
}

.nm_smoothstep <- function(x, lo, hi) {
  t <- pmin(pmax((x - lo) / (hi - lo), 0), 1)
  t * t * (3 - 2 * t)
}

# Mean of the 3 x 3 neighbourhood over pixels flagged by `weight_mask`.
.nm_neighbour_mean <- function(values, weight_mask) {
  s <- matrix(0, nrow(values), ncol(values))
  n <- s
  v <- ifelse(weight_mask, values, 0)
  for (dr in -1:1) for (dc in -1:1) {
    s <- s + .nm_shift_num(v, dr, dc)
    n <- n + .nm_shift_num(weight_mask * 1, dr, dc)
  }
  list(mean = ifelse(n > 0, s / pmax(n, 1), NA_real_), n = n)
}

.nm_shift_num <- function(m, dr, dc) {
  out <- matrix(0, nrow(m), ncol(m))
  rs <- seq_len(nrow(m)) - dr
  cs <- seq_len(ncol(m)) - dc
  okr <- rs >= 1L & rs <= nrow(m)
  okc <- cs >= 1L & cs <= ncol(m)
  out[okr, okc] <- m[rs[okr], cs[okc]]
  out
}

# Composite one slice to an RGBA colour matrix (transparent outside the
# brain, so one tile serves light and dark canvases). `bg_img` and `stat_img`
# are display-oriented images; `stat_img` holds the statistic with hidden
# voxels (sub-threshold, sub-extent, outside support) set to 0 (thresholded)
# or NA (continuous).
#
# Visibility is decided at native resolution, never from interpolated values:
# a voxel is kept iff it passes the display rule, and the overlay coverage is
# the smoothed kept mask corrected at every voxel centre
# (.nm_smooth_coverage). The pixels nearest every kept voxel's centre are
# therefore fully coloured and those nearest a hidden voxel's centre never
# are. Report figures composite each tile at its shipped pixel size and draw
# it 1:1, so this holds in the PNG itself. Colour inside the coverage is the
# kept-only (normalised) interpolation of the value, clamped to >= threshold,
# so edges never fade into the sub-threshold band.
.nm_composite_slice <- function(bg_img, stat_img, window, palette, limits,
                                threshold, tail, continuous, f = 4L,
                                alpha = 0.95, layers = FALSE, out = NULL) {
  # `out` = c(rows, cols) composites straight at the shipped tile size (the
  # report figures do), so nothing resamples the tile afterwards.
  out <- as.integer(out %||% round(dim(bg_img) * f))
  fr <- out[[1L]] / nrow(bg_img)
  fc <- out[[2L]] / ncol(bg_img)
  f <- (fr + fc) / 2
  bg_native <- ifelse(is.finite(bg_img), bg_img, window$min)
  # Tissue mask with interior holes (ventricles, which fall below the floor
  # in skull-stripped templates) filled, so only true background is clear.
  body <- .nm_fill_holes(bg_native > window$floor)
  debris <- .nm_tissue_debris(body)
  if (any(debris)) {
    bg_native[debris] <- window$min
    body <- body & !debris
  }
  bg_native[body & bg_native <= window$floor] <- window$floor
  # Rim voxels (body voxels touching background) take the brighter of their
  # own value and their interior neighbours' mean; background pixels next to
  # the brain take the brain's edge value, so interpolation never pulls the
  # silhouette towards black.
  inner <- body & .nm_shift(body, 1, 0) & .nm_shift(body, -1, 0) &
    .nm_shift(body, 0, 1) & .nm_shift(body, 0, -1)
  rim <- body & !inner
  nb <- .nm_neighbour_mean(bg_native, inner)
  lift <- rim & is.finite(nb$mean)
  bg_native[lift] <- pmax(bg_native[lift], nb$mean[lift])
  out_nb <- .nm_neighbour_mean(bg_native, body)
  pad <- !body & is.finite(out_nb$mean)
  bg_native[pad] <- out_nb$mean[pad]

  bg_up <- .nm_upsample(bg_native, out = out)
  t <- (bg_up - window$lo) / max(window$hi - window$lo, .Machine$double.eps)
  t <- pmin(pmax(t, 0), 1)^0.9
  r <- .nm_underlay_dark[[1L]] + t * (.nm_underlay_light[[1L]] - .nm_underlay_dark[[1L]])
  g <- .nm_underlay_dark[[2L]] + t * (.nm_underlay_light[[2L]] - .nm_underlay_dark[[2L]])
  b <- .nm_underlay_dark[[3L]] + t * (.nm_underlay_light[[3L]] - .nm_underlay_dark[[3L]])
  # Smooth silhouette: the upsampled tissue mask is Gaussian-smoothed at
  # display resolution and cut at its half level with a ~1.5 px soft edge, so
  # the outline follows the anatomy instead of the voxel grid.
  cover <- .nm_blur2(.nm_upsample(body * 1, out = out), 0.7 * fr, 0.7 * fc)
  a <- .nm_smoothstep(cover, 0.42, 0.58)
  # A hairline rim just inside the silhouette, darker than the edge grey.
  edge <- .nm_smoothstep(cover, 0.44, 0.52) * (1 - .nm_smoothstep(cover, 0.56, 0.68))
  r <- r * (1 - 0.30 * edge)
  g <- g * (1 - 0.30 * edge)
  b <- b * (1 - 0.30 * edge)

  ov_alpha <- matrix(0, nrow(bg_up), ncol(bg_up))
  ov_value <- matrix(NA_real_, nrow(bg_up), ncol(bg_up))
  if (continuous) {
    ov <- .nm_upsample(stat_img, method = "nearest", out = out)
    keep <- is.finite(ov)
    ov_alpha[keep] <- 1
    ov_value[keep] <- ov[keep]
  } else {
    stat0 <- ifelse(is.finite(stat_img), stat_img, 0)
    signs <- switch(tail, positive = 1, negative = -1, c(1, -1))
    best_cover <- matrix(0, nrow(bg_up), ncol(bg_up))
    for (sgn in signs) {
      kept <- (sgn * stat0) >= threshold
      if (!any(kept)) next
      cov <- .nm_smooth_coverage(kept, f, out = out)
      lin <- .nm_upsample(kept * 1, out = out)
      num <- .nm_upsample(ifelse(kept, stat0, 0), out = out)
      val <- num / pmax(lin, 1e-9)
      val <- sgn * pmax(sgn * val, threshold)
      # Where the cubic edge reaches past the bilinear support, carry the
      # nearest kept value outward.
      far <- lin < 1e-6
      if (any(far)) {
        near <- .nm_upsample(ifelse(kept, stat0, NA_real_), method = "nearest",
                             out = out)
        val[far] <- ifelse(is.finite(near[far]), near[far], sgn * threshold)
      }
      take <- cov > best_cover & cov > 0.47
      ov_alpha[take] <- .nm_smoothstep(cov[take], 0.47, 0.53)
      ov_value[take] <- val[take]
      best_cover[take] <- cov[take]
    }
  }
  show <- ov_alpha > 0
  if (any(show)) {
    rgb <- .nm_value_rgb(ov_value[show], palette, limits, threshold)
    w <- alpha * ov_alpha[show]
    r[show] <- (1 - w) * r[show] + w * rgb[, 1L]
    g[show] <- (1 - w) * g[show] + w * rgb[, 2L]
    b[show] <- (1 - w) * b[show] + w * rgb[, 3L]
    a[show] <- pmax(a[show], ov_alpha[show])
  }
  col <- grDevices::rgb(pmin(pmax(r, 0), 1), pmin(pmax(g, 0), 1),
                        pmin(pmax(b, 0), 1), pmin(pmax(a, 0), 1))
  col <- matrix(col, nrow = nrow(bg_up), ncol = ncol(bg_up))
  if (isTRUE(layers)) {
    return(list(col = col, overlay_alpha = ov_alpha, overlay_value = ov_value,
                brain_alpha = a, f = f))
  }
  col
}

# Display coverage of a kept-voxel mask at `f` pixels per voxel (or at
# exactly `out` = c(rows, cols) pixels, for a tile composited at its shipped
# size): the bilinear mask smoothed at display resolution (so outlines are
# curves, not voxel staircases), then corrected voxel by voxel so that the
# field is at least `hi` at every kept voxel centre and at most `lo` at every
# hidden one (checked at the pixels whose centres lie within half a pixel of
# the voxel centre: 1 or 2 per axis). Cut at 0.5 (soft edge `lo`-`hi`, about
# half a pixel), every kept voxel centre is therefore drawn and no hidden
# voxel centre ever is: smoothing never adds or drops a voxel. Composited at
# the shipped size, that holds in the shipped pixels, not only on a finer
# raster that is later resampled.
.nm_smooth_coverage <- function(kept, f, sigma = 0.9, hi = 0.53, lo = 0.47,
                                exact = TRUE, out = NULL) {
  if (is.null(out)) {
    if (f <= 1L) return(kept * 1)
    out <- round(dim(kept) * f)
  }
  out <- as.integer(out)
  fr <- out[[1L]] / nrow(kept)
  fc <- out[[2L]] / ncol(kept)
  base <- .nm_upsample(kept * 1, out = out)
  g <- .nm_blur2(base, sigma * fr, sigma * fc)
  # Thumbnails (exact = FALSE) keep the plain smoothed field: a projection's
  # one-voxel teeth are noise at that size, not structure.
  if (!isTRUE(exact)) return(g)
  # Pixels whose centres lie within half a pixel of each voxel centre, as two
  # index vectors per axis (equal where only one pixel qualifies), and the
  # bilinear weight of a voxel's own correction at the farther of them.
  near_px <- function(n, n_out) {
    centre <- (seq_len(n) - 0.5) * n_out / n # in pixel units
    a <- pmin(pmax(ceiling(centre - 1e-9), 1L), n_out)
    b <- pmin(pmax(floor(centre + 1 + 1e-9), 1L), n_out)
    b <- ifelse(abs(b - 0.5 - centre) <= 0.5 + 1e-9, b, a)
    w <- .nm_interp_weights(n, n_out = n_out)
    list(idx = list(a, b),
         w = pmin(w[cbind(a, seq_len(n))], w[cbind(b, seq_len(n))]))
  }
  pr <- near_px(nrow(kept), out[[1L]])
  pc <- near_px(ncol(kept), out[[2L]])
  w0 <- outer(pr$w, pc$w)
  for (iter in seq_len(8L)) {
    lo_c <- hi_c <- NULL
    for (r in pr$idx) for (cc in pc$idx) {
      v <- g[r, cc, drop = FALSE]
      lo_c <- if (is.null(lo_c)) v else pmin(lo_c, v)
      hi_c <- if (is.null(hi_c)) v else pmax(hi_c, v)
    }
    res <- ifelse(kept, pmax(0, hi - lo_c), pmin(0, lo - hi_c))
    if (!any(abs(res) > 1e-9)) break
    g <- g + .nm_upsample(res / w0 * 1.02, out = out)
  }
  g
}

# Gaussian smoothing with separate row and column widths.
.nm_blur2 <- function(img, sigma_r, sigma_c) {
  if (is.finite(sigma_r) && sigma_r > 0) {
    img <- .nm_blur_operator(nrow(img), sigma_r) %*% img
  }
  if (is.finite(sigma_c) && sigma_c > 0) {
    img <- img %*% t(.nm_blur_operator(ncol(img), sigma_c))
  }
  img
}

# Fill enclosed holes of a logical image: background is whatever connects to
# the image border (4-connected) through FALSE pixels.
.nm_fill_holes <- function(mask) {
  mask[is.na(mask)] <- FALSE
  if (!any(mask)) return(mask)
  free <- !mask
  outside <- matrix(FALSE, nrow(mask), ncol(mask))
  outside[c(1L, nrow(mask)), ] <- free[c(1L, nrow(mask)), ]
  outside[, c(1L, ncol(mask))] <- outside[, c(1L, ncol(mask))] |
    free[, c(1L, ncol(mask))]
  repeat {
    grown <- outside | .nm_shift(outside, 1, 0) | .nm_shift(outside, -1, 0) |
      .nm_shift(outside, 0, 1) | .nm_shift(outside, 0, -1)
    grown <- grown & free
    if (identical(grown, outside)) break
    outside <- grown
  }
  !outside
}

# Colour key ----------------------------------------------------------------

# Horizontal key: only the drawn ranges carry colour; the hidden
# |value| < threshold band is hatched, and triangles mark values beyond the cap.
# Sizes are in points at the figure's resolution (`scale` converts when the
# device resolution differs from the 144-dpi report figures). `note_below`
# sets the note on its own line under the ticks (narrow figures).
.nm_colour_key_grob <- function(palette, limits, threshold, signed, tail,
                                continuous, over_low = FALSE, over_high = FALSE,
                                title = "z", note = NULL,
                                fontsize = 11, theme = .nm_figure_theme(),
                                scale = 1, note_below = FALSE,
                                title_above = FALSE,
                                ink = NULL, muted = NULL) {
  fontsize <- fontsize * scale
  ink <- ink %||% theme$ink
  muted <- muted %||% theme$note %||% theme$muted
  bp <- function(v) grid::unit(v * scale, "bigpts")
  lo <- limits[[1L]]
  hi <- limits[[2L]]
  thr <- if (!continuous && is.finite(threshold)) threshold else 0
  span <- hi - lo
  xs <- function(v) (v - lo) / span
  n <- 200L
  segs <- list()
  if (!continuous && thr > 0) {
    if (signed && tail != "positive" && lo < -thr) {
      segs[[length(segs) + 1L]] <- seq(lo, -thr, length.out = n)
    }
    if (tail != "negative" && hi > thr) {
      segs[[length(segs) + 1L]] <- seq(thr, hi, length.out = n)
    }
  } else {
    segs[[1L]] <- seq(lo, hi, length.out = n * 2L)
  }

  bar_h <- bp(11)
  bar_y <- grid::unit(1, "npc") - 0.5 * bar_h
  children <- list()
  for (vals in segs) {
    rgb <- .nm_value_rgb(vals, palette, limits, thr)
    cols <- grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L])
    x0 <- xs(min(vals))
    w <- xs(max(vals)) - x0
    children[[length(children) + 1L]] <- grid::rasterGrob(
      matrix(cols, nrow = 1L), x = grid::unit(x0, "npc"), y = bar_y,
      width = grid::unit(w, "npc"), height = bar_h,
      just = c("left", "centre"), interpolate = TRUE
    )
  }
  if (!continuous && thr > 0) {
    g0 <- if (signed && tail != "positive") xs(-thr) else xs(max(lo, 0))
    g1 <- if (tail != "negative") xs(thr) else xs(min(hi, 0))
    if (g1 > g0) {
      children[[length(children) + 1L]] <- grid::rectGrob(
        x = grid::unit(g0, "npc"), y = bar_y,
        width = grid::unit(g1 - g0, "npc"),
        height = bar_h, just = c("left", "centre"),
        gp = grid::gpar(fill = theme$gap_fill, col = NA)
      )
      # Diagonal hatch, drawn in a clipping viewport over the gap.
      k <- seq(-1, 1, by = 0.05)
      hatch <- grid::segmentsGrob(
        x0 = grid::unit(k, "npc"), x1 = grid::unit(k, "npc") + bar_h,
        y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
        gp = grid::gpar(col = theme$hatch, lwd = 0.9 * scale)
      )
      children[[length(children) + 1L]] <- grid::gTree(
        children = grid::gList(hatch),
        vp = grid::viewport(
          x = grid::unit(g0, "npc"), y = bar_y,
          width = grid::unit(g1 - g0, "npc"),
          height = bar_h, just = c("left", "centre"), clip = "on"
        )
      )
    }
  }
  tri <- function(side, value) {
    rgb <- .nm_value_rgb(value, palette, limits, thr)
    col <- grDevices::rgb(rgb[1L, 1L], rgb[1L, 2L], rgb[1L, 3L])
    x <- if (side == "low") 0 else 1
    dir <- if (side == "low") -1 else 1
    grid::polygonGrob(
      x = grid::unit.c(grid::unit(x, "npc"),
                       grid::unit(x, "npc") + bp(dir * 9),
                       grid::unit(x, "npc")),
      y = grid::unit.c(bar_y + 0.5 * bar_h, bar_y, bar_y - 0.5 * bar_h),
      gp = grid::gpar(fill = col, col = NA)
    )
  }
  if (over_high) children[[length(children) + 1L]] <- tri("high", hi)
  if (over_low) children[[length(children) + 1L]] <- tri("low", lo)

  ticks <- if (!continuous && thr > 0) {
    v <- c(if (signed && tail != "positive") c(lo, -thr),
           if (tail != "negative") c(thr, hi))
    v[v >= lo & v <= hi]
  } else {
    unique(c(lo, if (lo < 0 && hi > 0) 0, hi))
  }
  ticks <- unique(ticks)
  tick_x <- grid::unit(xs(ticks), "npc")
  tick_top <- bar_y - 0.5 * bar_h
  children[[length(children) + 1L]] <- grid::segmentsGrob(
    x0 = tick_x, x1 = tick_x, y0 = tick_top, y1 = tick_top - bp(3.5),
    gp = grid::gpar(col = theme$tick, lwd = 0.8 * scale)
  )
  children[[length(children) + 1L]] <- grid::textGrob(
    .nm_format_tick(ticks), x = tick_x, y = tick_top - bp(5),
    just = c("centre", "top"), gp = .nm_gp_text(fontsize, ink)
  )
  # The title sits left of the bar (a fixed gap that clears a low cap
  # triangle, so keys with and without triangles line up), or above the bar,
  # left-aligned with its origin, where the width is too tight for both.
  children[[length(children) + 1L]] <- if (isTRUE(title_above)) {
    grid::textGrob(
      title, x = grid::unit(0, "npc"), y = bar_y + 0.5 * bar_h + bp(5),
      just = c("left", "bottom"), gp = .nm_gp_text(fontsize, ink, medium = TRUE)
    )
  } else {
    grid::textGrob(
      title, x = grid::unit(0, "npc") - bp(.nm_key_title_gap),
      y = bar_y + bp(fontsize * 0.12), just = c("right", "centre"),
      gp = .nm_gp_text(fontsize, ink, medium = TRUE)
    )
  }
  if (!is.null(note) && nzchar(note)) {
    children[[length(children) + 1L]] <- if (isTRUE(note_below)) {
      grid::textGrob(
        note, x = grid::unit(0.5, "npc"), y = tick_top - bp(5 + fontsize * 1.5),
        just = c("centre", "top"), gp = .nm_gp_text(fontsize, muted)
      )
    } else {
      grid::textGrob(
        note, x = grid::unit(1, "npc") + bp(if (over_high) 19 else 11),
        y = bar_y, just = c("left", "centre"),
        gp = .nm_gp_text(fontsize - 0.5, muted)
      )
    }
  }
  grid::gTree(children = do.call(grid::gList, children))
}

# Gap (points) between a left-hand key title and the bar origin.
.nm_key_title_gap <- 17

# Width in device pixels of `text` set at `size` points on a `res`-dpi figure
# in the figure family (the device's own metrics when ragg/systemfonts are
# available, else an Avenir-like estimate).
.nm_text_width_px <- function(text, size, res = 144, medium = FALSE) {
  if (!length(text) || !nzchar(text)) return(0)
  if (!.nm_plain_fonts() && requireNamespace("systemfonts", quietly = TRUE)) {
    fonts <- .nm_figure_fonts()
    bold <- isTRUE(medium) && identical(fonts$medium_face, 2L)
    args <- list(text,
                 family = if (isTRUE(medium)) fonts$medium else fonts$regular,
                 size = size, res = res)
    # systemfonts >= 1.2.4 takes `weight`; older releases take `bold`.
    if ("weight" %in% names(formals(systemfonts::string_width))) {
      args$weight <- if (bold) "bold" else "normal"
    } else {
      args$bold <- bold
    }
    w <- tryCatch(do.call(systemfonts::string_width, args),
                  error = function(e) NA_real_)
    if (length(w) == 1L && is.finite(w) && w > 0) return(as.numeric(w))
  }
  nchar(text) * size * 0.56 * res / 72
}

# Shared horizontal placement of the colour key, so the volume and surface
# figures of one map line up exactly. The bar has a fixed share of the figure
# width; the title + gap + bar group is centred on the figure (wide: title on
# the bar's left) or the bar itself is centred (narrow: title above it). The
# note always sits centred under the bar, so the group stays centred.
# Returns fractions of the figure width.
.nm_key_geometry <- function(narrow = FALSE, title = NULL, fontsize = NULL,
                             width_px = NULL, res = 144) {
  w <- if (isTRUE(narrow)) 0.80 else 0.42
  if (isTRUE(narrow) || is.null(title) || is.null(width_px)) {
    return(list(x0 = (1 - w) / 2, w = w, title_above = isTRUE(narrow)))
  }
  fontsize <- fontsize %||% 11
  lead <- .nm_text_width_px(title, fontsize, res, medium = TRUE) +
    .nm_key_title_gap * res / 72
  x0 <- (width_px - (lead + w * width_px)) / 2 + lead
  list(x0 = x0 / width_px, w = w, title_above = FALSE)
}

# Vertical budget (px) of the key block below the tiles: the space above the
# bar (room for an above-bar title), and the block's total height.
.nm_key_block <- function(fs, narrow = FALSE) {
  above <- if (isTRUE(narrow)) fs * 2 * 2.1 else 30
  list(above = above, height = above + fs * 2 * 5.0)
}

# Volume montage figure -----------------------------------------------------

.nm_figure_crop <- function(slices) {
  xr <- NULL
  yr <- NULL
  for (s in slices) {
    keep <- s$inside
    if (!any(keep)) next
    cols <- which(colSums(keep) > 0)
    rows <- which(rowSums(keep) > 0)
    xr <- range(c(xr, s$x[cols]))
    yr <- range(c(yr, s$y[rows]))
  }
  if (is.null(xr)) {
    s <- slices[[1L]]
    xr <- range(s$x)
    yr <- range(s$y)
  }
  pad_x <- max(2, 0.03 * diff(xr))
  pad_y <- max(2, 0.03 * diff(yr))
  list(x = c(xr[[1L]] - pad_x, xr[[2L]] + pad_x),
       y = c(yr[[1L]] - pad_y, yr[[2L]] + pad_y))
}

.nm_crop_image <- function(s, crop) {
  cols <- which(s$x >= crop$x[[1L]] & s$x <= crop$x[[2L]])
  rows <- which(s$y >= crop$y[[1L]] & s$y <= crop$y[[2L]])
  list(img = s$img[rows, cols, drop = FALSE], x = s$x[cols], y = s$y[rows])
}

# Build the report montage. The tiles (RGBA, transparent outside the brain)
# are composited once; `.nm_volume_layout()` arranges them for a theme and
# width, so light/dark and wide/narrow variants share identical pixels.
# Returns the wide light layout with the tiles attached for variants.
.nm_volume_figure <- function(background,
                              stat_values,
                              zlevels,
                              along,
                              threshold,
                              limits,
                              palette,
                              signed,
                              tail,
                              continuous,
                              legend_text = "z",
                              note = NULL,
                              ncol = NULL,
                              width_px = 1760,
                              res = 144,
                              alpha = 0.95) {
  space <- neuroim2::space(background)
  frame <- .nm_axis_frame(space)
  bg_arr <- as.array(background)
  stat_arr <- array(as.numeric(stat_values), dim = dim(bg_arr))
  window <- .nm_underlay_window(as.numeric(bg_arr))
  zlevels <- as.integer(zlevels)
  along <- as.integer(along)

  slices <- lapply(zlevels, function(z) {
    bg <- .nm_slice_image(bg_arr, along, z, frame)
    st <- .nm_slice_image(stat_arr, along, z, frame)
    bg$stat <- st$img
    bg$inside <- (is.finite(bg$img) & bg$img > window$floor) |
      (is.finite(st$img) & st$img != 0)
    bg$label <- .nm_slice_label(
      frame[[along]]$world, .nm_axis_world(frame[[along]], z)
    )
    bg
  })
  crop <- .nm_figure_crop(slices)
  aspect <- diff(crop$y) / diff(crop$x)

  # Tiles are composited by the layout at their shipped pixel size (one per
  # figure size, shared by the light and dark themes); see .nm_layout_tile().
  sources <- lapply(slices, function(s) {
    cropped_bg <- .nm_crop_image(s, crop)
    cropped_st <- .nm_crop_image(list(img = s$stat, x = s$x, y = s$y), crop)
    stat_img <- cropped_st$img
    if (!continuous) stat_img[!is.finite(stat_img)] <- 0
    list(bg = cropped_bg$img, stat = stat_img)
  })

  finite_stat <- stat_arr[is.finite(stat_arr) & stat_arr != 0]
  parts <- list(
    sources = sources,
    render = list(window = window, palette = palette, limits = limits,
                  threshold = threshold, tail = tail, continuous = continuous,
                  alpha = alpha),
    tile_cache = new.env(parent = emptyenv()),
    labels = vapply(slices, `[[`, character(1), "label"),
    aspect = aspect,
    lr = frame[[along]]$world != 1L,
    key = list(
      palette = palette, limits = limits, threshold = threshold,
      signed = signed, tail = tail, continuous = continuous,
      over_low = length(finite_stat) > 0 &&
        min(finite_stat) < limits[[1L]] - 1e-9,
      over_high = length(finite_stat) > 0 &&
        max(finite_stat) > limits[[2L]] + 1e-9,
      title = legend_text, note = note
    )
  )
  spec <- .nm_volume_layout(parts, ncol = ncol, width_px = width_px,
                            res = res)
  spec$world <- vapply(zlevels, function(z) {
    .nm_axis_world(frame[[along]], z)
  }, numeric(1))
  spec$crop <- crop
  spec$parts <- parts
  spec
}

# Arrange composited tiles into a figure. `variant = "narrow"` is the phone
# layout: four columns, larger type, the key across the width and its note on
# its own line.
.nm_volume_layout <- function(parts, ncol = NULL, width_px = 1760, res = 144,
                              theme = "light",
                              variant = c("wide", "narrow")) {
  variant <- match.arg(variant)
  th <- .nm_figure_theme(theme)
  narrow <- identical(variant, "narrow")
  n <- length(parts$sources)
  ncol <- ncol %||% if (narrow) min(4L, n) else min(6L, n)
  ncol <- max(1L, min(as.integer(ncol), n))
  nrow <- ceiling(n / ncol)

  # Type sizes in points at 144 dpi (2 px per pt): 11 pt = 22 px = 11 CSS px
  # when the 2x PNG is shown at half size. The narrow figure is shown at
  # roughly 45% on a phone, so its type is set larger.
  fs <- if (narrow) 14 else 11
  px <- function(v) grid::unit(v * 72 / res, "bigpts")
  pad <- if (narrow) 18 else 28
  gutter <- if (narrow) 10 else 14
  label_h <- fs * 2 * 1.45
  block <- .nm_key_block(fs, narrow)
  key_h <- block$height
  # Whole-pixel tiles at whole-pixel positions, drawn 1:1 (no resampling), so
  # the kept/hidden voxel-centre guarantee holds in the shipped PNG.
  tile_w <- floor((width_px - 2 * pad - (ncol - 1) * gutter) / ncol)
  tile_h <- round(tile_w * parts$aspect)
  xpad <- floor((width_px - ncol * tile_w - (ncol - 1) * gutter) / 2)
  height_px <- ceiling(
    pad + nrow * (label_h + tile_h) + (nrow - 1) * gutter + key_h
  )

  kids <- list(grid::rectGrob(gp = grid::gpar(fill = th$canvas, col = NA)))
  # Pixel box of each tile in the PNG (1-based, from the top-left corner).
  boxes <- data.frame(left = integer(n), top = integer(n),
                      width = rep(as.integer(tile_w), n),
                      height = rep(as.integer(tile_h), n))
  for (i in seq_len(n)) {
    row <- (i - 1L) %/% ncol
    col <- (i - 1L) %% ncol
    x0 <- xpad + col * (tile_w + gutter)
    y_top <- pad + row * (label_h + tile_h + gutter)
    boxes$left[[i]] <- as.integer(x0) + 1L
    boxes$top[[i]] <- as.integer(round(y_top + label_h)) + 1L
    kids[[length(kids) + 1L]] <- grid::rasterGrob(
      .nm_layout_tile(parts, i, c(tile_h, tile_w)),
      x = px(x0), y = px(height_px - round(y_top + label_h)),
      width = px(tile_w), height = px(tile_h),
      just = c("left", "top"), interpolate = FALSE
    )
    kids[[length(kids) + 1L]] <- grid::textGrob(
      parts$labels[[i]], x = px(x0 + 2),
      y = grid::unit(1, "npc") - px(y_top + label_h - fs * 0.6),
      just = c("left", "bottom"),
      # Slice labels in the medium weight, a point larger on the wide figure
      # (12 CSS px), so they read as firmly as the captions beneath.
      gp = .nm_gp_text(if (narrow) fs else fs + 1, th$label, medium = TRUE)
    )
    if (i == 1L && isTRUE(parts$lr)) {
      # Orientation (neurological: subject left on image left), set in the
      # first tile's lower corners, which the brain's outline leaves empty.
      low <- grid::unit(1, "npc") - px(y_top + label_h + tile_h - 4)
      lr <- .nm_gp_text(fs, th$muted, medium = TRUE)
      kids <- c(kids, list(
        grid::textGrob("L", x = px(x0 + 4), y = low,
                       just = c("left", "bottom"), gp = lr),
        grid::textGrob("R", x = px(x0 + tile_w - 4), y = low,
                       just = c("right", "bottom"), gp = lr)
      ))
    }
  }

  key_args <- parts$key
  geo <- .nm_key_geometry(narrow, title = key_args$title, fontsize = fs,
                          width_px = width_px, res = res)
  key <- .nm_colour_key_grob(
    key_args$palette, key_args$limits, key_args$threshold, key_args$signed,
    key_args$tail, key_args$continuous,
    over_low = key_args$over_low, over_high = key_args$over_high,
    title = key_args$title, note = key_args$note, fontsize = fs,
    theme = th, note_below = TRUE, title_above = geo$title_above
  )
  key_top <- height_px - key_h + block$above
  key$vp <- grid::viewport(
    x = px(width_px * geo$x0), y = grid::unit(1, "npc") - px(key_top),
    width = px(width_px * geo$w), height = px(fs * 2 * 3),
    just = c("left", "top")
  )
  kids[[length(kids) + 1L]] <- key

  structure(
    list(grob = grid::gTree(children = do.call(grid::gList, kids),
                            cl = "nm_volume_figure"),
         width = width_px, height = height_px, res = res,
         labels = parts$labels, ncol = ncol, nrow = nrow,
         theme = th$name, variant = variant, canvas = th$canvas,
         tile_boxes = boxes),
    class = "nm_volume_figure_spec"
  )
}

# One composited tile at exactly `out` = c(rows, cols) pixels, cached per
# size (themes share tiles: they are transparent outside the brain).
.nm_layout_tile <- function(parts, i, out) {
  out <- as.integer(out)
  key <- paste(i, out[[1L]], out[[2L]])
  cache <- parts$tile_cache
  if (is.environment(cache) && !is.null(cache[[key]])) return(cache[[key]])
  rd <- parts$render
  src <- parts$sources[[i]]
  tile <- .nm_composite_slice(
    src$bg, src$stat, rd$window, rd$palette, rd$limits, rd$threshold,
    rd$tail, rd$continuous, alpha = rd$alpha, out = out
  )
  if (is.environment(cache)) cache[[key]] <- tile
  tile
}

# Light/dark x wide/narrow variants of a report figure built by
# .nm_volume_figure(). Narrow figures are 800 px wide (shown at ~360 CSS px).
.nm_volume_figure_variant <- function(spec, theme = "light",
                                      variant = "wide") {
  out <- .nm_volume_layout(
    spec$parts, ncol = if (identical(variant, "narrow")) NULL else spec$ncol,
    width_px = if (identical(variant, "narrow")) 800 else spec$width,
    res = spec$res, theme = theme, variant = variant
  )
  out$world <- spec$world
  out$crop <- spec$crop
  out
}

#' @export
print.nm_volume_figure_spec <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x$grob)
  invisible(x)
}

.nm_open_png <- function(path, width, height, res, background = .nm_canvas) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(path, width = width, height = height, units = "px",
                  res = res, background = background)
  } else {
    grDevices::png(path, width = width, height = height, res = res,
                   bg = background)
  }
}

.nm_write_figure <- function(figure, path) {
  .nm_open_png(path, figure$width, figure$height, figure$res,
               background = figure$canvas %||% .nm_canvas)
  tryCatch({
    grid::grid.newpage()
    grid::grid.draw(figure$grob)
  }, finally = grDevices::dev.off())
  invisible(path)
}

# Glass-brain thumbnails (no colour key, shown small) take a 256-colour
# palette (no dithering, sRGB; the canvas colour stays exact), which halves
# their bytes. Figures with a colour key stay full colour: a palette would
# band the key's ramp. Opt out with options(neuromosaic.figure_quantize = FALSE).
.nm_quantize_png <- function(path, colours = 256L) {
  if (!isTRUE(getOption("neuromosaic.figure_quantize", TRUE)) ||
      !requireNamespace("magick", quietly = TRUE) ||
      !identical(tolower(tools::file_ext(path)), "png") || !file.exists(path)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    img <- magick::image_read(path)
    q <- magick::image_quantize(img, max = colours, colorspace = "sRGB",
                                dither = FALSE)
    tmp <- paste0(path, ".q.png")
    magick::image_write(q, tmp, format = "png8")
    if (file.exists(tmp) && file.size(tmp) < file.size(path)) {
      file.rename(tmp, path)
    } else {
      unlink(tmp)
    }
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

# Glass-brain summary -------------------------------------------------------

# Logical matrix shifted by (dr, dc) rows/cols, padded with FALSE.
.nm_shift <- function(m, dr, dc) {
  out <- matrix(FALSE, nrow(m), ncol(m))
  rs <- seq_len(nrow(m)) - dr
  cs <- seq_len(ncol(m)) - dc
  okr <- rs >= 1L & rs <= nrow(m)
  okc <- cs >= 1L & cs <= ncol(m)
  out[okr, okc] <- m[rs[okr], cs[okc]]
  out
}

# Re-embed a projected 2-D array (over the retained axes) as a one-slice
# volume and orient it like a slice image.
.nm_projection_image <- function(proj, d, along, frame) {
  vol <- array(0, dim = replace(d, along, 1L))
  if (along == 1L) vol[1L, , ] <- proj
  if (along == 2L) vol[, 1L, ] <- proj
  if (along == 3L) vol[, , 1L] <- proj
  frame[[along]]$n <- 1L
  .nm_slice_image(vol, along, 1L, frame)
}

# Per-sign maximum projections along array axis `along`: the largest positive
# and the most negative value on each line of sight (0 where none), so the
# summary can show the winning sign and still mark the one hidden behind it.
.nm_signed_projections <- function(arr, along, frame) {
  keep <- setdiff(seq_len(3L), along)
  pos <- arr
  pos[!is.finite(pos) | pos < 0] <- 0
  neg <- arr
  neg[!is.finite(neg) | neg > 0] <- 0
  d <- dim(arr)
  list(
    pos = .nm_projection_image(apply(pos, keep, max), d, along, frame),
    neg = .nm_projection_image(apply(neg, keep, min), d, along, frame)
  )
}

# Signed maximum-|value| projection (kept for callers and tests): the value of
# larger magnitude on each line of sight.
.nm_signed_mip <- function(arr, along, frame) {
  p <- .nm_signed_projections(arr, along, frame)
  img <- ifelse(abs(p$neg$img) > p$pos$img, p$neg$img, p$pos$img)
  p$pos$img <- img
  p$pos
}

.nm_mean_projection <- function(arr, along, frame, floor) {
  inside <- is.finite(arr) & arr > floor
  cnt <- apply(inside, setdiff(seq_len(3L), along), sum)
  .nm_projection_image(ifelse(cnt > 0, cnt, NA_real_), dim(arr), along, frame)
}

# The three summary views and their display orientation. Sagittal is shown
# with anterior on the LEFT (as in the interactive viewer); coronal and axial
# are neurological (subject left on image left), axial with anterior at top.
.nm_glass_views <- function(frame) {
  world_axis <- vapply(frame, `[[`, numeric(1), "world")
  list(
    list(name = "sagittal", along = which(world_axis == 1)[[1L]], flip = TRUE),
    list(name = "coronal", along = which(world_axis == 2)[[1L]], flip = FALSE),
    list(name = "axial", along = which(world_axis == 3)[[1L]], flip = FALSE)
  )
}

# Plain-language projection semantics of the summary thumbnails, for captions.
.nm_glass_brain_semantics <- paste(
  "Glass brain: three maximum-|z| projections (sagittal with anterior on the",
  "left; coronal and axial with the left hemisphere on the left, axial",
  "anterior at the top). Each pixel shows the strongest effect along its line",
  "of sight, so an opposite-sign effect behind it is hidden. Colour strength",
  "rises with |z| on one report-wide scale, from a faint tint at the",
  "threshold to the strongest tint at the colour cap (red positive, blue",
  "negative), so weaker maps look weaker; the figures carry exact values."
)

# Glass silhouette tones per theme: a flat fill with a faint depth shade and a
# 1 px contour, so effects (opaque, on the shared palette) carry all colour.
.nm_glass_tones <- function(theme_name) {
  if (identical(theme_name, "dark")) {
    list(lo = c(0.165, 0.192, 0.208), hi = c(0.204, 0.235, 0.251),
         line = c(0.345, 0.384, 0.408))
  } else {
    list(lo = c(0.918, 0.925, 0.918), hi = c(0.862, 0.871, 0.867),
         line = c(0.663, 0.690, 0.706))
  }
}

# Glance ramps for the glass-brain thumbnails, ordered threshold -> cap.
# A thumbnail is read for salience, not for value: an effect must stand out
# from the card more the stronger it is, on either ground. The figure-of-record
# palette cannot do that on a light card (its threshold red is the darkest,
# most saturated colour and its cap yellow nearly matches the card), so the
# thumbnails use per-theme ramps with the same hues (warm positive, cool
# negative) whose contrast with the card ground rises with |value|: pale tint
# -> deep colour on light, dim -> bright on dark. Chroma stays moderate so the
# cards sit below the figures in the page hierarchy.
.nm_glass_stops <- function(theme_name = "light") {
  if (identical(theme_name, "dark")) {
    return(list(
      positive = c("#5A2E2A", "#93402F", "#CC6A3E", "#EDA55C", "#F6D48C"),
      negative = c("#253752", "#2E5A8C", "#3F86BF", "#70B5DC", "#B2DEEE")
    ))
  }
  list(
    positive = c("#F2CABB", "#EA9C82", "#D9684E", "#BC3A33", "#962030"),
    negative = c("#CCDAEE", "#9BBBE0", "#6595CC", "#3768AC", "#1E4486")
  )
}

# RGB (0-1) of glass-brain effect values on the theme's glance ramp; palettes
# other than the nm family keep their own colours.
.nm_glass_rgb <- function(values, palette, limits, threshold,
                          theme_name = "light") {
  if (!.nm_is_palette(palette)) {
    return(.nm_value_rgb(values, palette, limits, threshold))
  }
  values <- as.numeric(values)
  thr <- if (is.finite(threshold) && threshold > 0) threshold else 0
  cap <- max(abs(limits))
  t <- (pmin(abs(values), cap) - thr) / max(cap - thr, .Machine$double.eps)
  # A mild power keeps the many moderate effects of a dense map quiet and lets
  # the strong ones carry the tile (still strictly monotone).
  t <- pmax(t, 0)^1.3
  stops <- .nm_glass_stops(theme_name)
  pos <- values >= 0
  if (identical(palette, "nm-negative")) pos[] <- FALSE
  if (identical(palette, "nm-positive")) pos[] <- TRUE
  out <- matrix(NA_real_, nrow = length(values), ncol = 3L)
  if (any(pos)) out[pos, ] <- .nm_ramp_rgb(stops$positive, t[pos])
  if (any(!pos)) out[!pos, ] <- .nm_ramp_rgb(stops$negative, t[!pos])
  out
}

# One composited glass-brain panel (RGBA matrix, transparent background).
# Effects are opaque colour on the theme's glance ramp (.nm_glass_stops), so
# their contrast with the card rises monotonically with |value| from the
# threshold to the cap, on light and dark cards alike.
.nm_glass_panel <- function(proj, depth, threshold, limits, palette, tail,
                            continuous, theme, f = 4L) {
  dimg <- depth$img
  present <- is.finite(dimg) & dimg > 0
  dmax <- if (any(present)) {
    stats::quantile(dimg[present], 0.98, names = FALSE)
  } else {
    1
  }
  dens <- ifelse(present, pmin(dimg / max(dmax, 1), 1), 0)
  dens_up <- .nm_upsample(dens, f, "linear")
  body <- .nm_blur(.nm_upsample(present * 1, f, "linear"), 0.6 * f)
  a <- .nm_smoothstep(body, 0.42, 0.58)
  outline <- .nm_smoothstep(body, 0.40, 0.48) *
    (1 - .nm_smoothstep(body, 0.52, 0.62))
  glass <- .nm_glass_tones(theme$name)
  shade <- dens_up^1.5
  ch <- lapply(1:3, function(k) {
    v <- glass$lo[[k]] + shade * (glass$hi[[k]] - glass$lo[[k]])
    v * (1 - outline) + glass$line[[k]] * outline
  })
  a <- pmax(a, outline)

  thr <- if (continuous) 0 else threshold
  fills <- list()
  signs <- if (continuous) "pos" else switch(
    tail, positive = "pos", negative = "neg", c("pos", "neg")
  )
  for (s in signs) {
    img <- proj[[s]]$img
    sgn <- if (identical(s, "pos")) 1 else -1
    kept <- if (continuous) abs(img) > 0 else (sgn * img) >= thr
    if (!any(kept)) next
    # Smoothed footprint, cut a little below half so one-voxel-wide
    # structures survive the smoothing.
    cov <- .nm_smooth_coverage(kept, f, sigma = 0.6, exact = FALSE) + 0.12
    lin <- .nm_upsample(kept * 1, f, "linear")
    val <- .nm_upsample(ifelse(kept, img, 0), f, "linear") / pmax(lin, 1e-9)
    far <- lin < 1e-6
    if (any(far)) {
      near <- .nm_upsample(ifelse(kept, img, NA_real_), f, "nearest")
      val[far] <- ifelse(is.finite(near[far]), near[far], sgn * thr)
    }
    val <- sgn * pmax(sgn * val, thr)
    fills[[s]] <- list(cov = cov, val = val, sgn = sgn)
  }
  winner <- NULL
  if (length(fills) == 2L) {
    mag_p <- ifelse(fills$pos$cov > 0.4, abs(fills$pos$val), 0)
    mag_n <- ifelse(fills$neg$cov > 0.4, abs(fills$neg$val), 0)
    winner <- ifelse(mag_n > mag_p, "neg", "pos")
  }
  for (s in names(fills)) {
    fl <- fills[[s]]
    on <- fl$cov > 0.4
    if (!is.null(winner)) on <- on & winner == s
    if (!any(on)) next
    w <- .nm_smoothstep(fl$cov[on], 0.4, 0.6)
    rgb <- .nm_glass_rgb(fl$val[on], palette, limits, thr, theme$name)
    for (k in 1:3) ch[[k]][on] <- (1 - w) * ch[[k]][on] + w * rgb[, k]
    a[on] <- pmax(a[on], w)
  }
  matrix(grDevices::rgb(pmin(pmax(ch[[1L]], 0), 1), pmin(pmax(ch[[2L]], 0), 1),
                        pmin(pmax(ch[[3L]], 0), 1), pmin(pmax(a, 0), 1)),
         nrow = nrow(dens_up))
}

# Three-view glass brain (sagittal, coronal, axial) of the extent-filtered
# display map on the report-wide colour scale, cropped to the brain and
# written at about twice its display size. Returns the panel geometry.
.nm_glass_brain <- function(background, stat_values, threshold, limits,
                            palette, tail, continuous, path,
                            width_px = 840, res = 144, theme = "light") {
  th <- .nm_figure_theme(theme)
  space <- neuroim2::space(background)
  frame <- .nm_axis_frame(space)
  bg_arr <- as.array(background)
  stat_arr <- array(as.numeric(stat_values), dim = dim(bg_arr))
  stat_arr[!is.finite(stat_arr)] <- 0
  window <- .nm_underlay_window(as.numeric(bg_arr))

  panels <- lapply(.nm_glass_views(frame), function(view) {
    proj <- .nm_signed_projections(stat_arr, view$along, frame)
    depth <- .nm_mean_projection(bg_arr, view$along, frame, window$floor)
    # Crop to the projected brain (plus any signal) with a 2-voxel margin.
    inside <- (is.finite(depth$img) & depth$img > 0) |
      proj$pos$img != 0 | proj$neg$img != 0
    rows <- which(rowSums(inside) > 0)
    cols <- which(colSums(inside) > 0)
    if (!length(rows)) rows <- seq_len(nrow(inside))
    if (!length(cols)) cols <- seq_len(ncol(inside))
    rows <- seq(max(1L, min(rows) - 2L), min(nrow(inside), max(rows) + 2L))
    cols <- seq(max(1L, min(cols) - 2L), min(ncol(inside), max(cols) + 2L))
    if (isTRUE(view$flip)) cols <- rev(cols)
    sub <- function(img) img[rows, cols, drop = FALSE]
    proj$pos$img <- sub(proj$pos$img)
    proj$neg$img <- sub(proj$neg$img)
    depth$img <- sub(depth$img)
    list(proj = proj, depth = depth, name = view$name,
         w = length(cols) * proj$pos$dx, h = length(rows) * proj$pos$dy)
  })
  pad <- width_px / 70
  gap <- width_px / 30
  widths <- vapply(panels, `[[`, numeric(1), "w")
  heights <- vapply(panels, `[[`, numeric(1), "h")
  scale <- (width_px - 2 * pad - 2 * gap) / sum(widths)
  height_px <- ceiling(max(heights) * scale + 2 * pad)
  px <- function(v) grid::unit(v * 72 / res, "bigpts")

  kids <- list(grid::rectGrob(gp = grid::gpar(fill = th$canvas, col = NA)))
  x <- pad
  for (p in panels) {
    col <- .nm_glass_panel(p$proj, p$depth, threshold, limits, palette, tail,
                           continuous, th)
    w <- p$w * scale
    h <- p$h * scale
    kids[[length(kids) + 1L]] <- grid::rasterGrob(
      col, x = px(x), y = px(height_px / 2), width = px(w), height = px(h),
      just = c("left", "centre"), interpolate = TRUE
    )
    x <- x + w + gap
  }
  .nm_open_png(path, width_px, height_px, res, background = th$canvas)
  tryCatch({
    grid::grid.newpage()
    grid::grid.draw(grid::gTree(children = do.call(grid::gList, kids)))
  }, finally = grDevices::dev.off())
  .nm_quantize_png(path, colours = 128L)
  invisible(list(path = path, width = width_px, height = height_px,
                 views = vapply(panels, `[[`, character(1), "name")))
}
