# Magnitude-graded overlay opacity shared by the interactive views.
#
# Static slices take their opacity from neuroim2::plot_overlay(ov_alpha_mode).
# The slice viewer and the cortical surface receive the same rule as a
# renderer-neutral `magnitude_opacity` list, evaluated live in the browser as
# the reader moves the threshold or colour range:
#
#   start = max(low, threshold on that side); end = colour-range end
#   alpha = 0 below start, else floor' + (1 - floor') * ((m - start) / (end - start))^gamma
#
# with floor' = floor only when start > 0. The fade is for unthresholded
# (continuous) maps: thresholded report figures stay opaque above threshold
# and encode magnitude by colour alone, so a positive threshold returns NULL.
# "ramp"/"proportional" match
# neuroim2's linear ramp (floor 0.6 above a threshold, none without one);
# "soft" reuses neuroim2's self-tuning knee and exponent, fitted once in R from
# the map's own magnitudes so every view applies identical parameters.

.montage_alpha_modes_graded <- c("ramp", "proportional", "soft")

.montage_magnitude_opacity <- function(alpha_mode,
                                       threshold = 0,
                                       cap = NULL,
                                       magnitudes = NULL,
                                       gamma = NULL) {
  if (is.null(alpha_mode) || length(alpha_mode) != 1L || is.na(alpha_mode) ||
      !alpha_mode %in% .montage_alpha_modes_graded) {
    return(NULL)
  }
  thresh <- suppressWarnings(as.numeric(threshold))
  thresh <- if (length(thresh) == 1L && is.finite(thresh) && thresh > 0) thresh else 0
  if (thresh > 0) return(NULL)
  if (!identical(alpha_mode, "soft")) {
    return(list(low = 0, high = "range", gamma = 1, floor = 0.6))
  }
  mags <- abs(as.numeric(magnitudes))
  mags <- mags[is.finite(mags) & mags > 0]
  cap <- suppressWarnings(as.numeric(cap))
  cap <- if (length(cap) == 1L && is.finite(cap) && cap > thresh) cap else NULL
  params <- .montage_soft_alpha_params(mags, thresh, cap, gamma)
  list(
    # Above a report threshold the knee is the threshold itself, which the
    # browser already applies live; keep `low` for unthresholded maps only.
    low = if (thresh > 0) 0 else as.numeric(params$lo),
    high = "range",
    gamma = as.numeric(params$gamma),
    floor = as.numeric(params$alpha_floor %||% 0)
  )
}

# neuroim2's own fit when available, so static and interactive views agree.
.montage_soft_alpha_params <- function(mags, thresh, cap, gamma) {
  fit <- tryCatch(
    utils::getFromNamespace("soft_alpha_params", "neuroim2"),
    error = function(e) NULL
  )
  knee <- if (thresh > 0) thresh else if (length(mags)) stats::median(mags) else 0
  # A colour cap at or below the knee leaves nothing to ramp over; let the
  # fit use the data maximum instead.
  if (!is.null(cap) && cap <= knee) cap <- NULL
  if (is.function(fit)) {
    out <- tryCatch(fit(mags, thresh = thresh, cap = cap, gamma = gamma),
                    error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  hi <- cap %||% if (length(mags)) max(mags) else knee + 1
  if (!is.finite(hi) || hi <= knee) hi <- knee + 1
  if (is.null(gamma)) {
    supra <- mags[mags > knee]
    gamma <- if (length(supra) >= 10L) {
      t_med <- min(max(stats::median((supra - knee) / (hi - knee)), 0.001), 0.999)
      min(max(log(0.2) / log(t_med), 1.5), 5)
    } else {
      2
    }
  }
  list(lo = knee, hi = hi, gamma = gamma, alpha_floor = 0)
}

# Vectorised twin of the browser rule (surfview DataLayer magnitudeOpacity and
# NeuroMosaicVolumeDisplay.magnitudeAlpha): opacity multipliers for `values`.
.montage_magnitude_alpha <- function(values, spec, range, threshold = c(0, 0)) {
  v <- as.numeric(values)
  if (is.null(spec)) return(ifelse(is.finite(v), 1, 0))
  positive <- v >= 0
  m <- abs(v)
  threshold <- as.numeric(threshold)
  masked <- length(threshold) == 2L && all(is.finite(threshold)) &&
    threshold[[2L]] > threshold[[1L]]
  edge <- if (masked) {
    pmax(0, ifelse(positive, threshold[[2L]], -threshold[[1L]]))
  } else {
    0
  }
  start <- pmax(as.numeric(spec$low %||% 0), edge)
  high <- spec$high %||% "range"
  end <- if (identical(high, "range")) {
    side <- ifelse(positive, range[[2L]], -range[[1L]])
    ifelse(side > 0, side, max(abs(range)))
  } else {
    as.numeric(high)
  }
  floor <- ifelse(start > 0, as.numeric(spec$floor %||% 0), 0)
  gamma <- as.numeric(spec$gamma %||% 1)
  t <- pmin(1, pmax(0, (m - start) / pmax(end - start, .Machine$double.eps)))
  a <- floor + (1 - floor) * t^gamma
  a[!(end > start)] <- 1
  a[!is.finite(v) | m < start | m == 0] <- 0
  a
}

# Per-row parameters, fitted once from the row's volume (cached by path) so the
# slice viewer and the surface use the same knee and exponent.
.montage_row_magnitude_opacity <- local({
  cache <- new.env(parent = emptyenv())
  function(row, alpha_mode, threshold, cap, fallback_values = NULL) {
    if (is.null(alpha_mode) || !identical(alpha_mode, "soft")) {
      return(.montage_magnitude_opacity(alpha_mode, threshold, cap))
    }
    path <- tryCatch(as.character(row$path[[1L]]), error = function(e) NA_character_)
    key <- paste(path, threshold, cap, sep = "\r")
    if (!is.null(cache[[key]])) return(cache[[key]])
    mags <- NULL
    if (length(path) == 1L && !is.na(path) && file.exists(path)) {
      mags <- tryCatch(as.numeric(neuroim2::read_vol(path)), error = function(e) NULL)
    }
    if (is.null(mags)) mags <- unlist(fallback_values, use.names = FALSE)
    out <- .montage_magnitude_opacity(alpha_mode, threshold, cap, mags)
    if (length(path) == 1L && !is.na(path)) cache[[key]] <- out
    out
  }
})

#' Magnitude-graded opacity for an unthresholded map
#'
#' Returns the fade rule report views apply when a map profile uses a graded
#' `alpha_mode`, for callers that render static figures themselves (for
#' example [surf_montage()] with precomputed vertex values). Weak values become
#' transparent over the anatomy; nothing is hidden. Thresholded maps keep
#' opaque colour above threshold, so a positive `threshold` returns `NULL`.
#'
#' `"ramp"` and `"proportional"` fade linearly from zero to the colour-range
#' end; `"soft"` fits neuroim2's self-tuning knee and exponent to the map's
#' magnitudes, so weak values fall away faster. `"binary"` returns `NULL`.
#'
#' @param alpha_mode One of `"binary"`, `"ramp"`, `"proportional"`, `"soft"`.
#' @param values Map values (volume or vertices) used to fit the `"soft"` rule.
#' @param cap Colour-range end (magnitude), used by the `"soft"` fit.
#' @param threshold Display threshold; positive values disable the fade.
#' @return `NULL` or a list with `low`, `high`, `gamma`, `floor`, accepted by
#'   [surf_montage()] (`magnitude_opacity`) and
#'   `neurosurf::surface_layer(magnitude_opacity =)`.
#' @examples
#' nm_magnitude_opacity("ramp")
#' nm_magnitude_opacity("soft", values = abs(stats::rnorm(1000)), cap = 3)
#' @export
nm_magnitude_opacity <- function(alpha_mode, values = NULL, cap = NULL, threshold = 0) {
  .montage_magnitude_opacity(alpha_mode, threshold = threshold, cap = cap,
                             magnitudes = values)
}
