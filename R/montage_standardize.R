#' Express an unthresholded map in z units
#'
#' Divides a map by its spatial spread so that unthresholded maps of
#' different quantities (betas, correlations, contrasts) share one readable
#' scale and one threshold vocabulary. Zero is preserved: the map is scaled,
#' not centred, so the sign and the null value keep their meaning.
#'
#' The spread is estimated over the map's support (finite, non-zero voxels, or
#' `mask`). `"robust_sd"` (default) uses \eqn{1.4826 \times} the median
#' absolute deviation about the median, which ignores a minority of strong
#' effects; `"sd"` uses the ordinary standard deviation. Matching surface
#' values (for example the same map projected to a cortical surface) are
#' divided by the same volume-derived scale, so every view shows identical
#' units.
#'
#' This is a display scaling, not a statistical test: the result is a
#' standardized effect map, not a z-statistic with a null distribution. Use it
#' for unthresholded maps, and say so in the caption (the returned `scale` and
#' `method` support that).
#'
#' @param x A `NeuroVol`.
#' @param mask Optional logical `NeuroVol` or array restricting the voxels used
#'   to estimate the spread. Defaults to finite, non-zero voxels.
#' @param surface Optional list of numeric vertex vectors (for example
#'   `list(lh = ..., rh = ...)`) scaled by the same factor.
#' @param method `"robust_sd"` or `"sd"`.
#'
#' @return A list with `volume` (the scaled `NeuroVol`), `surface` (scaled
#'   vertex values or `NULL`), `scale` (the divisor, in the input's units),
#'   `method`, and `n` (voxels used).
#' @examples
#' sp <- neuroim2::NeuroSpace(c(4, 4, 4), c(2, 2, 2))
#' vol <- neuroim2::NeuroVol(array(stats::rnorm(64, sd = 3), c(4, 4, 4)), sp)
#' z <- nm_standardize_map(vol)
#' z$scale
#' @export
nm_standardize_map <- function(x, mask = NULL, surface = NULL,
                               method = c("robust_sd", "sd")) {
  method <- match.arg(method)
  if (!methods::is(x, "NeuroVol")) {
    stop("'x' must be a NeuroVol.", call. = FALSE)
  }
  arr <- as.array(x)
  support <- if (is.null(mask)) {
    is.finite(arr) & arr != 0
  } else {
    m <- as.array(mask)
    if (!identical(dim(m), dim(arr))) {
      stop("'mask' must have the same dimensions as 'x'.", call. = FALSE)
    }
    as.logical(m) & is.finite(arr)
  }
  values <- arr[support]
  if (length(values) < 3L) {
    stop("Too few supported voxels to estimate a spread.", call. = FALSE)
  }
  scale <- switch(
    method,
    robust_sd = stats::mad(values, center = stats::median(values), constant = 1.4826),
    sd = stats::sd(values)
  )
  if (!is.finite(scale) || scale <= 0) {
    stop("The map has no spread on its support; it cannot be standardized.",
         call. = FALSE)
  }
  scaled <- arr / scale
  if (!is.null(surface)) {
    if (!is.list(surface) || !all(vapply(surface, is.numeric, logical(1)))) {
      stop("'surface' must be a list of numeric vertex vectors.", call. = FALSE)
    }
    surface <- lapply(surface, function(v) v / scale)
  }
  list(
    volume = neuroim2::NeuroVol(scaled, neuroim2::space(x)),
    surface = surface,
    scale = scale,
    method = method,
    n = length(values)
  )
}
