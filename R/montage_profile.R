#' Define Display Semantics for a Montage Map Quantity
#'
#' A map profile describes how a scientific quantity should be displayed. It
#' is an ordinary value object: pass profiles directly to
#' [resolve_montage_profiles()] or [render_montage_report()] rather than
#' registering global state.
#'
#' @param id Stable profile identifier. Using a namespaced identifier such as
#'   `"mylab:reliability"` is recommended for project-specific quantities.
#' @param label Human-facing quantity label used by static and interactive
#'   legends. This is distinct from the manifest's analysis/panel `label`.
#'   A manifest `legend_title` overrides this label for an individual map.
#' @param display_mode Either `"thresholded"` or `"continuous"`.
#' @param scale Either `"diverging"` or `"sequential"`.
#' @param center Numeric center for a diverging scale. The current volume and
#'   surface contract supports zero-centered diverging scales.
#' @param domain Optional legal value domain. Infinite endpoints are allowed.
#' @param limits Optional fixed finite display limits. When omitted, limits are
#'   derived robustly from map values and clipped to `domain`.
#' @param palette_family Optional palette name or family understood by the
#'   renderer.
#' @param alpha_mode Overlay opacity mode.
#' @param support Whether the map is shown over the full `"analysis"` support
#'   or only the `"primary"` map's suprathreshold support.
#' @param units Optional colorbar units.
#'
#' @return A `montage_map_profile` object.
#' @export
montage_map_profile <- function(id,
                                label = id,
                                display_mode = NULL,
                                scale = NULL,
                                center = NULL,
                                domain = NULL,
                                limits = NULL,
                                palette_family = NULL,
                                alpha_mode = NULL,
                                support = NULL,
                                units = NULL) {
  .profile_scalar_character(id, "id", required = TRUE)
  .profile_scalar_character(label, "label", required = TRUE)
  display_mode <- .profile_choice(
    display_mode, c("thresholded", "continuous"), "display_mode"
  )
  scale <- .profile_choice(scale, c("diverging", "sequential"), "scale")
  support <- .profile_choice(support, c("analysis", "primary"), "support")
  .profile_scalar_character(palette_family, "palette_family")
  .profile_scalar_character(alpha_mode, "alpha_mode")
  .profile_scalar_character(units, "units")
  if (!is.null(center) && (!is.numeric(center) || length(center) != 1L ||
                           !is.finite(center))) {
    stop("'center' must be NULL or a finite numeric scalar.", call. = FALSE)
  }
  if (!is.null(center) && !isTRUE(all.equal(as.numeric(center), 0))) {
    stop("Diverging montage profiles currently require center = 0.",
         call. = FALSE)
  }
  domain <- .validate_profile_range(domain, "domain", finite = FALSE)
  limits <- .validate_profile_range(limits, "limits", finite = TRUE)
  if (!is.null(domain) && !is.null(limits) &&
      (limits[[1L]] < domain[[1L]] || limits[[2L]] > domain[[2L]])) {
    stop("'limits' must lie within 'domain'.", call. = FALSE)
  }

  structure(
    list(
      id = trimws(id),
      label = trimws(label),
      display_mode = display_mode,
      scale = scale,
      center = center,
      domain = domain,
      limits = limits,
      palette_family = palette_family,
      alpha_mode = alpha_mode,
      support = support,
      units = units
    ),
    class = "montage_map_profile"
  )
}

#' Resolve Montage Map Profiles
#'
#' Resolves display defaults for every manifest row. Precedence is explicit
#' manifest columns, then a caller-supplied profile, then a built-in quantity
#' profile, and finally data-driven automatic semantics. Automatic limits use
#' robust 99th-percentile ranges rather than raw extrema.
#'
#' @param manifest A montage render manifest.
#' @param profiles Optional list of [montage_map_profile()] objects. Names may
#'   match a manifest `profile` value or a `quantity`; profile `id` values are
#'   also recognized.
#' @param map_values Optional list with one numeric map vector per manifest row.
#'   Named lists are aligned by `map_id`. Values are used only to resolve and
#'   validate display ranges; no map data are changed.
#'
#' @return The normalized manifest with `effective_*` profile columns.
#' @export
resolve_montage_profiles <- function(manifest, profiles = NULL,
                                     map_values = NULL) {
  manifest <- validate_manifest(
    manifest,
    check_files = FALSE,
    check_overlays = FALSE
  )
  profiles <- .normalize_montage_profiles(profiles)
  values <- .normalize_montage_profile_values(map_values, manifest)

  resolved <- lapply(seq_len(nrow(manifest)), function(i) {
    .resolve_montage_profile_row(manifest, i, profiles, values[[i]])
  })
  manifest$effective_display_mode <- vapply(
    resolved, `[[`, character(1), "display_mode"
  )
  manifest$effective_scale <- vapply(resolved, `[[`, character(1), "scale")
  manifest$effective_center <- vapply(
    resolved,
    function(x) x$center %||% NA_real_,
    numeric(1)
  )
  manifest$effective_lower <- vapply(resolved, `[[`, numeric(1), "lower")
  manifest$effective_upper <- vapply(resolved, `[[`, numeric(1), "upper")
  manifest$effective_domain_lower <- vapply(
    resolved, `[[`, numeric(1), "domain_lower"
  )
  manifest$effective_domain_upper <- vapply(
    resolved, `[[`, numeric(1), "domain_upper"
  )
  manifest$effective_profile_source <- vapply(
    resolved, `[[`, character(1), "source"
  )
  manifest$effective_profile_id <- vapply(resolved, `[[`, character(1), "id")
  manifest$effective_profile_label <- vapply(
    resolved, `[[`, character(1), "label"
  )
  manifest$effective_palette_family <- vapply(
    resolved, `[[`, character(1), "palette_family"
  )
  manifest$effective_alpha_mode <- vapply(
    resolved, `[[`, character(1), "alpha_mode"
  )
  manifest$effective_support <- vapply(
    resolved, `[[`, character(1), "support"
  )
  manifest$effective_units <- vapply(resolved, `[[`, character(1), "units")
  manifest$effective_signed <- vapply(resolved, `[[`, logical(1), "signed")
  manifest
}

.resolve_montage_profile_row <- function(manifest, row, profiles, values) {
  quantity <- manifest$quantity[[row]]
  explicit_profile <- .profile_row_character(manifest, row, "profile")
  if (!is.null(explicit_profile)) {
    user_profile <- profiles[[explicit_profile]]
    builtin <- .builtin_montage_profile(explicit_profile)
    profile <- user_profile %||% builtin
    if (is.null(profile)) {
      stop(
        "Unknown montage map profile '", explicit_profile, "' for map_id '",
        manifest$map_id[[row]], "'.",
        call. = FALSE
      )
    }
    source <- if (!is.null(user_profile)) "user" else "builtin"
  } else {
    user_profile <- profiles[[quantity]]
    builtin <- .builtin_montage_profile(quantity)
    profile <- user_profile %||% builtin
    source <- if (!is.null(user_profile)) {
      "user"
    } else if (!is.null(builtin)) {
      "builtin"
    } else {
      "automatic"
    }
  }
  if (is.null(profile)) {
    profile <- montage_map_profile(
      id = quantity,
      label = .profile_title(quantity),
      display_mode = "continuous"
    )
  }

  finite_values <- .finite_montage_values(values)
  .validate_profile_value_domain(
    finite_values, profile$domain, quantity, manifest$map_id[[row]]
  )
  explicit_display_mode <- .profile_row_character(
    manifest, row, "display_mode"
  )
  explicit_scale <- .profile_row_character(manifest, row, "scale")
  explicit_support <- .profile_row_character(manifest, row, "support")
  display_mode <- .profile_choice(
    explicit_display_mode,
    c("thresholded", "continuous"),
    "display_mode"
  ) %||%
    profile$display_mode %||% "continuous"
  scale <- .profile_choice(
    explicit_scale,
    c("diverging", "sequential"),
    "scale"
  ) %||% profile$scale
  center <- .profile_row_numeric(manifest, row, "center") %||% profile$center
  signed <- manifest$signed[[row]]
  if (is.null(scale)) {
    domain_spans_zero <- !is.null(profile$domain) &&
      profile$domain[[1L]] < 0 && profile$domain[[2L]] > 0
    negative_tolerance <- sqrt(.Machine$double.eps) * max(
      1, abs(finite_values)
    )
    has_negative_values <- length(finite_values) > 0L &&
      any(finite_values < -negative_tolerance)
    scale <- if (isTRUE(signed) ||
                 (!isFALSE(signed) &&
                  (has_negative_values || domain_spans_zero))) {
      "diverging"
    } else {
      "sequential"
    }
  }
  if (identical(scale, "diverging")) {
    if (isFALSE(signed)) {
      stop(
        "A diverging scale conflicts with signed = FALSE for map_id '",
        manifest$map_id[[row]], "'.",
        call. = FALSE
      )
    }
    center <- center %||% 0
    signed <- TRUE
  } else {
    if (isTRUE(signed)) {
      stop(
        "A sequential scale conflicts with signed = TRUE for map_id '",
        manifest$map_id[[row]], "'.",
        call. = FALSE
      )
    }
    signed <- FALSE
  }
  if (!is.null(center) && !identical(scale, "diverging")) {
    stop(
      "A display 'center' is only valid with a diverging scale for map_id '",
      manifest$map_id[[row]], "'.",
      call. = FALSE
    )
  }
  if (!is.null(center) && !isTRUE(all.equal(as.numeric(center), 0))) {
    stop(
      "Diverging maps currently require center = 0 for map_id '",
      manifest$map_id[[row]], "'.",
      call. = FALSE
    )
  }

  limits <- profile$limits
  explicit_lower <- .profile_row_numeric(manifest, row, "lower")
  explicit_upper <- .profile_row_numeric(manifest, row, "upper")
  if (!is.null(explicit_lower) || !is.null(explicit_upper)) {
    limits <- c(explicit_lower %||% NA_real_, explicit_upper %||% NA_real_)
    source <- "explicit"
  }
  if (!is.null(explicit_display_mode) ||
      !is.null(explicit_scale) ||
      !is.null(.profile_row_numeric(manifest, row, "center")) ||
      !is.null(.profile_row_character(manifest, row, "palette_family")) ||
      !is.null(.profile_row_character(manifest, row, "alpha_mode")) ||
      !is.null(explicit_support)) {
    source <- "explicit"
  }
  limits <- .resolve_profile_limits(
    values = finite_values,
    scale = scale,
    center = center,
    domain = profile$domain,
    limits = limits
  )
  if (!is.null(profile$domain) &&
      (limits[[1L]] < profile$domain[[1L]] ||
       limits[[2L]] > profile$domain[[2L]])) {
    stop(
      "Display limits must lie within the profile domain for map_id '",
      manifest$map_id[[row]], "'.",
      call. = FALSE
    )
  }
  if (identical(scale, "diverging") &&
      !isTRUE(all.equal(
        center - limits[[1L]], limits[[2L]] - center,
        tolerance = sqrt(.Machine$double.eps)
      ))) {
    stop(
      "Diverging display limits must be symmetric around center for map_id '",
      manifest$map_id[[row]], "'.",
      call. = FALSE
    )
  }

  palette <- .profile_row_character(manifest, row, "palette_family") %||%
    profile$palette_family %||%
    if (identical(scale, "diverging")) "diverging" else "sequential"
  alpha <- .profile_row_character(manifest, row, "alpha_mode") %||%
    profile$alpha_mode %||%
    if (identical(display_mode, "thresholded")) "soft" else "binary"
  support <- .profile_choice(
    explicit_support, c("analysis", "primary"), "support"
  ) %||%
    profile$support %||% "analysis"
  units <- .profile_row_character(manifest, row, "units") %||%
    profile$units %||% ""

  list(
    id = profile$id,
    label = .profile_row_character(manifest, row, "legend_title") %||% profile$label,
    display_mode = display_mode,
    scale = scale,
    center = center,
    lower = limits[[1L]],
    upper = limits[[2L]],
    domain_lower = profile$domain[[1L]] %||% -Inf,
    domain_upper = profile$domain[[2L]] %||% Inf,
    palette_family = palette,
    alpha_mode = alpha,
    support = support,
    units = units,
    signed = isTRUE(signed),
    source = source
  )
}

.builtin_montage_profile <- function(quantity) {
  switch(
    quantity,
    test_statistic = montage_map_profile(
      quantity, "Test statistic", "thresholded", "diverging", 0,
      palette_family = "diverging", alpha_mode = "soft"
    ),
    estimate = montage_map_profile(
      quantity, "Estimate", "continuous", "diverging", 0,
      palette_family = "diverging", alpha_mode = "binary"
    ),
    standard_error = montage_map_profile(
      quantity, "Standard error", "continuous", "sequential",
      domain = c(0, Inf), palette_family = "sequential", alpha_mode = "binary"
    ),
    variance = montage_map_profile(
      quantity, "Variance", "continuous", "sequential",
      domain = c(0, Inf), palette_family = "sequential", alpha_mode = "binary"
    ),
    probability = montage_map_profile(
      quantity, "Probability", "continuous", "sequential",
      domain = c(0, 1), limits = c(0, 1), palette_family = "sequential",
      alpha_mode = "binary"
    ),
    proportion = montage_map_profile(
      quantity, "Proportion", "continuous", "sequential",
      domain = c(0, 1), limits = c(0, 1), palette_family = "sequential",
      alpha_mode = "binary"
    ),
    r_squared = montage_map_profile(
      quantity, "R-squared", "continuous", "sequential",
      domain = c(0, 1), limits = c(0, 1), palette_family = "sequential",
      alpha_mode = "binary"
    ),
    NULL
  )
}

.normalize_montage_profiles <- function(profiles) {
  if (is.null(profiles)) {
    return(list())
  }
  if (inherits(profiles, "montage_map_profile")) {
    profiles <- list(profiles)
  }
  if (!is.list(profiles) ||
      !all(vapply(profiles, inherits, logical(1), "montage_map_profile"))) {
    stop("'profiles' must contain montage_map_profile objects.", call. = FALSE)
  }
  ids <- vapply(profiles, `[[`, character(1), "id")
  supplied_names <- names(profiles)
  if (is.null(supplied_names)) {
    supplied_names <- rep("", length(profiles))
  }
  supplied_names <- trimws(supplied_names)
  has_name <- !is.na(supplied_names) & nzchar(supplied_names)
  keys <- ifelse(has_name, supplied_names, ids)
  aliases <- c(keys, ids)
  owners <- rep(seq_along(profiles), 2L)
  ambiguous <- vapply(split(owners, aliases), function(x) {
    length(unique(x)) > 1L
  }, logical(1))
  if (any(ambiguous)) {
    stop("Profile names and ids must be unique.", call. = FALSE)
  }
  out <- stats::setNames(profiles, keys)
  for (i in seq_along(profiles)) {
    if (is.null(out[[ids[[i]]]])) {
      out[[ids[[i]]]] <- profiles[[i]]
    }
  }
  out
}

.normalize_montage_profile_values <- function(map_values, manifest) {
  n <- nrow(manifest)
  if (is.null(map_values)) {
    return(rep(list(numeric()), n))
  }
  if (!is.list(map_values)) {
    if (n == 1L) {
      map_values <- list(map_values)
    } else {
      stop("'map_values' must be a list with one entry per map.", call. = FALSE)
    }
  }
  if (!is.null(names(map_values)) && all(manifest$map_id %in% names(map_values))) {
    map_values <- unname(map_values[manifest$map_id])
  }
  if (length(map_values) != n) {
    stop("'map_values' must have one entry per manifest row.", call. = FALSE)
  }
  map_values
}

.finite_montage_values <- function(values) {
  if (is.null(values)) {
    return(numeric())
  }
  values <- suppressWarnings(as.numeric(values))
  values[is.finite(values)]
}

.validate_profile_value_domain <- function(values, domain, quantity, map_id) {
  if (length(values) == 0L || is.null(domain)) {
    return(invisible(TRUE))
  }
  tolerance <- sqrt(.Machine$double.eps)
  bad <- values < domain[[1L]] - tolerance | values > domain[[2L]] + tolerance
  if (any(bad)) {
    qualifier <- if (is.finite(domain[[1L]]) && domain[[1L]] >= 0) {
      "nonnegative values within"
    } else {
      "values within"
    }
    stop(
      "Quantity '", quantity, "' requires ", qualifier, " its domain for ",
      "map_id '", map_id, "'.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.resolve_profile_limits <- function(values, scale, center, domain, limits) {
  if (!is.null(limits) && all(is.finite(limits))) {
    return(limits)
  }
  if (is.null(limits) && !is.null(domain) && all(is.finite(domain))) {
    return(domain)
  }
  if (identical(scale, "diverging")) {
    center <- center %||% 0
    distances <- abs(values - center)
    cap <- if (length(distances) > 0L) {
      as.numeric(stats::quantile(distances, 0.99, names = FALSE))
    } else {
      1
    }
    if ((!is.finite(cap) || cap <= 0) && any(distances > 0)) {
      cap <- as.numeric(stats::quantile(
        distances[distances > 0], 0.99, names = FALSE
      ))
    }
    if (!is.finite(cap) || cap <= 0) cap <- 1
    derived <- c(center - cap, center + cap)
  } else if (length(values) > 0L) {
    lower <- if (!is.null(domain) && is.finite(domain[[1L]])) {
      domain[[1L]]
    } else {
      as.numeric(stats::quantile(values, 0.01, names = FALSE))
    }
    upper <- as.numeric(stats::quantile(values, 0.99, names = FALSE))
    if ((!is.finite(upper) || upper <= lower) && any(values > lower)) {
      upper <- as.numeric(stats::quantile(
        values[values > lower], 0.99, names = FALSE
      ))
    }
    if (!is.finite(upper) || upper <= lower) upper <- lower + 1
    derived <- c(lower, upper)
  } else if (!is.null(domain) && all(is.finite(domain))) {
    derived <- domain
  } else {
    derived <- c(0, 1)
  }
  if (!is.null(domain)) {
    derived[[1L]] <- max(derived[[1L]], domain[[1L]])
    derived[[2L]] <- min(derived[[2L]], domain[[2L]])
  }
  if (!is.null(limits)) {
    supplied <- is.finite(limits)
    derived[supplied] <- limits[supplied]
  }
  if (!all(is.finite(derived)) || derived[[1L]] >= derived[[2L]]) {
    stop("Resolved display limits must be finite and increasing.", call. = FALSE)
  }
  derived
}

.profile_row_character <- function(manifest, row, field) {
  if (!field %in% names(manifest)) return(NULL)
  value <- as.character(manifest[[field]][[row]])
  if (.missing_character(value)) NULL else trimws(value)
}

.profile_row_numeric <- function(manifest, row, field) {
  if (!field %in% names(manifest)) return(NULL)
  value <- suppressWarnings(as.numeric(manifest[[field]][[row]]))
  if (length(value) != 1L || is.na(value)) NULL else value
}

.profile_scalar_character <- function(x, field, required = FALSE) {
  if (is.null(x) && !required) return(invisible(TRUE))
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("'", field, "' must be a non-empty character scalar.", call. = FALSE)
  }
  invisible(TRUE)
}

.profile_choice <- function(x, choices, field) {
  if (is.null(x)) return(NULL)
  .profile_scalar_character(x, field, required = TRUE)
  x <- tolower(trimws(x))
  if (!x %in% choices) {
    stop("'", field, "' must be one of: ", paste(choices, collapse = ", "),
         ".", call. = FALSE)
  }
  x
}

.validate_profile_range <- function(x, field, finite) {
  if (is.null(x)) return(NULL)
  if (!is.numeric(x) || length(x) != 2L || anyNA(x) ||
      (isTRUE(finite) && any(!is.finite(x))) || x[[1L]] >= x[[2L]]) {
    stop("'", field, "' must be an increasing numeric pair", if (finite) {
      " of finite values."
    } else {
      "."
    }, call. = FALSE)
  }
  as.numeric(x)
}

.profile_title <- function(x) {
  tools::toTitleCase(gsub("[_:]", " ", x))
}

# One quantity/units contract shared by every renderer. Panel and selector titles
# describe the analysis; they are deliberately not the color-scale authority.
.montage_legend <- function(legend_title = NULL, units = NULL) {
  .profile_scalar_character(legend_title, "legend_title")
  .profile_scalar_character(units, "units")
  title <- legend_title %||% "Statistic"
  list(title = title, units = units,
       text = if (is.null(units)) title else paste0(title, " (", units, ")"))
}

.montage_row_legend <- function(row) {
  .montage_legend(
    .profile_row_character(row, 1L, "effective_profile_label"),
    .profile_row_character(row, 1L, "effective_units")
  )
}
