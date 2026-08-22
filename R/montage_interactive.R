# Interactive volume-report contracts.

.montage_volume_scene_name <- "org.neuromosaic.volume-scene"
.montage_volume_scene_version <- 1L
.montage_interactive_controls <- c("threshold", "palette", "opacity")

#' Configure Optional Interactive Volume Views
#'
#' Defines the author-facing contract for progressive, browser-based volume
#' exploration in a montage report. The object contains only interaction and
#' packaging choices: scientific display defaults continue to come from the
#' manifest, map profiles, montage policy, and their resolved display spec.
#'
#' Interactivity is opt-in. Passing `NULL` as the `interactive` argument to a
#' report renderer preserves the existing static-only behavior. When enabled,
#' the static panel remains the authoritative fallback for PDF, print,
#' JavaScript-disabled browsers, unsupported WebGL, and load failures. A custom
#' report template is responsible for emitting the standard interactive host
#' hooks. With `map_selector = "none"`, static variants remain expanded and an
#' interactive launcher identifies the map it opens.
#'
#' Reader changes to threshold or display range, palette, and opacity are local
#' exploratory state. They do not alter static figures, QC, clusters, peak
#' tables, or report provenance, and the browser UI must provide an exact reset
#' to the report-resolved defaults. Controls are map-local for the page session;
#' selecting another map and returning restores that map's exploratory state.
#' Hiding, leaving the page, or explicitly disposing the viewer releases its
#' browser resources. Reopening starts from the currently selected map's report
#' defaults.
#'
#' The browser translates the report's numeric limits, inclusive tail threshold,
#' palette family, units, and global alpha. Static intensity-dependent alpha
#' modes (for example a soft alpha ramp) are represented by uniform browser
#' opacity because neuroimjs exposes one layer opacity. The static montage is
#' therefore the authoritative rendering; semantic parity, not pixel identity,
#' is the version 1 contract.
#'
#' Interactive volume geometry is fail-closed. Backgrounds, maps, and affected
#' masks are compared on dimensions, voxel spacing, world origin, axis
#' orientation, and the complete affine using a combined absolute/relative
#' tolerance of `1e-6`. The interactive path never restamps, interpolates, or
#' resamples data; callers must align inputs before report construction.
#'
#' @param view Interactive view type. Version 1 supports only `"orthogonal"`.
#' @param assets Asset packaging mode. `"bundle"` writes relative companion
#'   assets suitable for an HTTP static site; `"embed"` stores compressed data
#'   in the HTML for direct `file://` use.
#' @param compression Volume-asset compression. `"gzip"` is the portable,
#'   lossless default; `"none"` is intended for diagnostics.
#' @param initial_position Initial coordinate strategy when `world_coord` is
#'   `NULL`: the tail-aware primary-map peak or the field-of-view centre.
#' @param world_coord Optional finite numeric `(x, y, z)` world coordinate. When
#'   supplied it overrides `initial_position` with an explicit starting point.
#' @param controls Character vector selecting reader display controls. Supported
#'   values are `"threshold"` (or range for continuous maps), `"palette"`, and
#'   `"opacity"`. Use `character()` for navigation-only viewing.
#' @param max_embed_mb Positive size budget, in MiB, checked against the actual
#'   base64 payload before the HTML report is emitted. It does not affect bundle
#'   mode.
#' @param cache_maps Positive integer maximum number of decoded overlay maps
#'   retained by the browser runtime per active viewer.
#'
#' @return A `montage_interactive` value object.
#' @export
montage_interactive <- function(view = "orthogonal",
                                assets = c("bundle", "embed"),
                                compression = c("gzip", "none"),
                                initial_position = c("primary_peak", "center"),
                                world_coord = NULL,
                                controls = c("threshold", "palette", "opacity"),
                                max_embed_mb = 25,
                                cache_maps = 2L) {
  view <- match.arg(view, "orthogonal")
  assets <- match.arg(assets)
  compression <- match.arg(compression)
  initial_position <- match.arg(initial_position)
  world_coord <- .montage_interactive_world_coord(world_coord)
  controls <- .montage_interactive_control_set(controls)
  max_embed_mb <- .montage_interactive_positive_number(
    max_embed_mb, "max_embed_mb"
  )
  cache_maps <- .montage_interactive_positive_integer(cache_maps, "cache_maps")

  structure(
    list(
      view = view,
      assets = assets,
      compression = compression,
      initial_position = if (is.null(world_coord)) {
        initial_position
      } else {
        "explicit"
      },
      world_coord = world_coord,
      controls = controls,
      max_embed_mb = max_embed_mb,
      cache_maps = cache_maps,
      static_fallback = TRUE,
      engine = "neuroimjs"
    ),
    class = "montage_interactive"
  )
}

#' @export
print.montage_interactive <- function(x, ...) {
  cat(
    "<montage_interactive>",
    "\n  view: ", x$view,
    "\n  assets: ", x$assets, " (", x$compression, ")",
    "\n  initial position: ", x$initial_position,
    "\n  controls: ",
    if (length(x$controls)) paste(x$controls, collapse = ", ") else "none",
    "\n  static fallback: yes\n",
    sep = ""
  )
  invisible(x)
}

#' VolumeScene Version 1 Schema
#'
#' Returns the browser-neutral contract used to connect neuromosaic report data
#' to a JavaScript volume renderer. It deliberately describes generic analyses,
#' maps, quantities, assets, geometry, and display defaults; software-specific
#' source paths, R objects, recipes, and FSL-only metric names are not part of
#' the scene.
#'
#' A scene is an ordinary JSON-safe list with class `montage_volume_scene`.
#' [validate_montage_volume_scene()] performs structural and referential checks.
#' The schema is renderer-neutral even though the first runtime is neuroimjs.
#'
#' @return A data frame with `object`, `field`, `required`, `type`, and `role`.
#' @export
montage_volume_scene_schema <- function() {
  data.frame(
    object = c(
      rep("scene", 9), rep("engine", 3), rep("view", 4),
      rep("packaging", 3), rep("background", 1), rep("analysis", 5),
      rep("map", 8), rep("status", 2), rep("display", 11),
      rep("asset", 13), rep("location", 2), rep("geometry", 6)
    ),
    field = c(
      "schema", "schema_version", "engine", "view", "packaging",
      "background", "analyses", "assets", "provenance",
      "name", "version", "adapter_version",
      "type", "controls", "cache_maps", "static_fallback",
      "mode", "compression", "max_embed_bytes",
      "asset_id",
      "analysis_id", "primary_map_id", "initial_world_coord",
      "zlevel_bookmarks", "maps",
      "map_id", "quantity", "label", "units", "asset_id", "display",
      "status", "selector_label",
      "state", "n_display_voxels",
      "mode", "scale", "center", "limits", "threshold", "tail",
      "palette", "alpha", "alpha_mode", "support", "units",
      "asset_id", "kind", "location", "encoding", "datatype",
      "compression", "hash_algorithm", "hash_scope", "hash",
      "compressed_bytes", "uncompressed_bytes", "geometry",
      "transformations",
      "kind", "ref",
      "dimensions", "spacing", "origin", "orientation", "affine",
      "fingerprint"
    ),
    required = c(
      rep(TRUE, 9), rep(TRUE, 3), rep(TRUE, 4), rep(TRUE, 3), TRUE,
      rep(TRUE, 5),
      c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
      rep(TRUE, 2),
      c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
      rep(TRUE, 13),
      rep(TRUE, 2),
      rep(TRUE, 6)
    ),
    type = c(
      "character", "integer", "object", "object", "object", "object",
      "array", "array", "object",
      "character", "character", "character",
      "character", "character[]", "integer", "logical",
      "character", "character", "integer",
      "character",
      "character", "character", "numeric[3]", "numeric[]", "array",
      "character", "character", "character", "character|null", "character",
      "object", "object", "character|null",
      "character", "integer",
      "character", "character", "numeric|null", "numeric[2]",
      "numeric|null", "character", "character", "numeric", "character",
      "character", "character|null",
      "character", "character", "object", "character", "character",
      "character", "character", "character", "character", "integer",
      "integer", "object", "character[]",
      "character", "character",
      "integer[3]", "numeric[3]", "numeric[3]", "character",
      "numeric[16]", "character"
    ),
    role = c(
      "stable schema name", "schema major version", "runtime identity",
      "viewer behavior", "asset delivery contract", "background asset link",
      "analysis groups", "content-addressed assets", "sanitized provenance",
      "runtime package", "exact runtime version", "exact adapter version",
      "orthogonal viewer", "enabled exploratory controls", "decoded map bound",
      "authoritative static fallback",
      "bundle or embedded", "gzip or none", "embedded byte guard",
      "background asset reference",
      "stable group key", "default map", "initial world coordinate",
      "static montage slice bookmarks", "associated maps",
      "stable map key", "generic or namespaced quantity", "reader label",
      "value units", "volume asset reference", "resolved report defaults",
      "empty/readiness state", "short selector label",
      "ready or empty", "displayable voxel count",
      "thresholded or continuous", "diverging or sequential", "scale centre",
      "finite display limits", "display threshold", "tail semantics",
      "canonical palette", "overlay opacity", "opacity behavior",
      "analysis or primary support", "legend units",
      "content key", "background overlay or mask", "relative or embedded ref",
      "nifti", "declared storage datatype", "gzip or none",
      "hash algorithm", "uncompressed hash scope", "content digest",
      "stored bytes", "decoded bytes", "spatial contract", "declared data changes",
      "relative or embedded", "safe local reference",
      "voxel dimensions", "voxel spacing", "world origin",
      "axis orientation codes", "4 by 4 affine", "canonical geometry digest"
    ),
    stringsAsFactors = FALSE
  )
}

#' Validate a VolumeScene
#'
#' Validates the versioned, JSON-safe scene passed to the browser runtime.
#' Validation is intentionally fail-closed: assets must be locally addressable,
#' references must resolve, geometry must be explicit, map and analysis keys must
#' be unique, and executable or source-path-bearing R values are rejected.
#'
#' @param scene A list representing VolumeScene version 1.
#'
#' @return `scene`, invisibly, with class `montage_volume_scene`.
#' @export
validate_montage_volume_scene <- function(scene) {
  if (!is.list(scene) || is.data.frame(scene)) {
    stop("'scene' must be a list.", call. = FALSE)
  }
  .montage_scene_assert_json_safe(scene, "scene")
  .montage_scene_require_fields(
    scene,
    c("schema", "schema_version", "engine", "view", "packaging",
      "background", "analyses", "assets", "provenance"),
    "scene"
  )
  if (!identical(scene$schema, .montage_volume_scene_name)) {
    stop("Unsupported VolumeScene schema name.", call. = FALSE)
  }
  if (!identical(scene$schema_version, .montage_volume_scene_version)) {
    stop(
      "Unsupported VolumeScene version: ", scene$schema_version %||% "<missing>",
      ". Expected ", .montage_volume_scene_version, ".",
      call. = FALSE
    )
  }

  .montage_scene_validate_engine(scene$engine)
  .montage_scene_validate_view(scene$view)
  .montage_scene_validate_packaging(scene$packaging)

  if (!is.list(scene$assets) || length(scene$assets) == 0L) {
    stop("'scene$assets' must contain at least one asset.", call. = FALSE)
  }
  assets <- lapply(seq_along(scene$assets), function(i) {
    .montage_scene_validate_asset(scene$assets[[i]], i)
  })
  .montage_scene_validate_asset_packaging(assets, scene$packaging)
  asset_ids <- vapply(assets, `[[`, character(1), "asset_id")
  .montage_scene_assert_unique(asset_ids, "asset_id")
  names(assets) <- asset_ids

  .montage_scene_require_fields(scene$background, "asset_id", "background")
  background_id <- .montage_scene_scalar_character(
    scene$background$asset_id, "background$asset_id"
  )
  if (!background_id %in% asset_ids) {
    stop("Background references an unknown asset_id.", call. = FALSE)
  }
  if (!identical(assets[[background_id]]$kind, "background")) {
    stop("Background asset must have kind 'background'.", call. = FALSE)
  }
  .montage_scene_validate_shared_geometry(assets, background_id)

  if (!is.list(scene$analyses) || length(scene$analyses) == 0L) {
    stop("'scene$analyses' must contain at least one analysis.", call. = FALSE)
  }
  analyses <- lapply(seq_along(scene$analyses), function(i) {
    .montage_scene_validate_analysis(
      scene$analyses[[i]], i, asset_ids,
      geometry = assets[[background_id]]$geometry
    )
  })
  analysis_ids <- vapply(analyses, `[[`, character(1), "analysis_id")
  .montage_scene_assert_unique(analysis_ids, "analysis_id")
  map_ids <- unlist(lapply(analyses, function(x) {
    vapply(x$maps, `[[`, character(1), "map_id")
  }), use.names = FALSE)
  .montage_scene_assert_unique(map_ids, "map_id")

  class(scene) <- unique(c("montage_volume_scene", class(scene)))
  invisible(scene)
}

#' @export
print.montage_volume_scene <- function(x, ...) {
  maps <- sum(vapply(x$analyses, function(a) length(a$maps), integer(1)))
  cat(
    "<montage_volume_scene v", x$schema_version, ">",
    "\n  analyses: ", length(x$analyses),
    "\n  maps: ", maps,
    "\n  assets: ", length(x$assets),
    "\n  engine: ", x$engine$name, " ", x$engine$version,
    "\n  packaging: ", x$packaging$mode, " (", x$packaging$compression, ")\n",
    sep = ""
  )
  invisible(x)
}

.new_montage_volume_scene <- function(interactive,
                                      background,
                                      analyses,
                                      assets,
                                      engine,
                                      provenance = list()) {
  if (!inherits(interactive, "montage_interactive")) {
    stop("'interactive' must be created by montage_interactive().",
         call. = FALSE)
  }
  # JSON array fields must remain arrays even when their R lists arrive with
  # analysis/map/asset names for convenient lookup. jsonlite serializes named
  # lists as objects, which would violate VolumeScene and break browser `.find`.
  analyses <- unname(lapply(analyses, function(analysis) {
    if (is.list(analysis) && is.list(analysis$maps)) {
      analysis$maps <- unname(analysis$maps)
    }
    analysis
  }))
  assets <- unname(assets)
  scene <- list(
    schema = .montage_volume_scene_name,
    schema_version = .montage_volume_scene_version,
    engine = engine,
    view = list(
      type = interactive$view,
      controls = interactive$controls,
      cache_maps = interactive$cache_maps,
      static_fallback = interactive$static_fallback
    ),
    packaging = list(
      mode = interactive$assets,
      compression = interactive$compression,
      max_embed_bytes = round(interactive$max_embed_mb * 1024^2)
    ),
    background = background,
    analyses = analyses,
    assets = assets,
    provenance = provenance
  )
  validate_montage_volume_scene(scene)
}

.montage_interactive_world_coord <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.numeric(x) || length(x) != 3L || anyNA(x) || any(!is.finite(x))) {
    stop("'world_coord' must be NULL or three finite numbers.", call. = FALSE)
  }
  as.numeric(x)
}

.montage_interactive_control_set <- function(x) {
  if (is.null(x)) x <- character()
  if (!is.character(x) || anyNA(x)) {
    stop("'controls' must be a character vector.", call. = FALSE)
  }
  x <- unique(trimws(x))
  bad <- setdiff(x, .montage_interactive_controls)
  if (length(bad)) {
    stop(
      "Unsupported interactive control(s): ", paste(bad, collapse = ", "),
      ". Supported controls: ",
      paste(.montage_interactive_controls, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}

.montage_interactive_positive_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("'", name, "' must be a positive finite number.", call. = FALSE)
  }
  as.numeric(x)
}

.montage_interactive_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop("'", name, "' must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

.montage_scene_validate_engine <- function(x) {
  .montage_scene_require_fields(x, c("name", "version", "adapter_version"),
                                "engine")
  if (!identical(.montage_scene_scalar_character(x$name, "engine$name"),
                 "neuroimjs")) {
    stop("VolumeScene v1 requires engine$name = 'neuroimjs'.", call. = FALSE)
  }
  .montage_scene_scalar_character(x$version, "engine$version")
  .montage_scene_scalar_character(x$adapter_version, "engine$adapter_version")
  invisible(x)
}

.montage_scene_validate_view <- function(x) {
  .montage_scene_require_fields(
    x, c("type", "controls", "cache_maps", "static_fallback"), "view"
  )
  if (!identical(x$type, "orthogonal")) {
    stop("VolumeScene v1 supports only an orthogonal view.", call. = FALSE)
  }
  .montage_interactive_control_set(x$controls)
  .montage_interactive_positive_integer(x$cache_maps, "view$cache_maps")
  if (!isTRUE(x$static_fallback)) {
    stop("VolumeScene requires static_fallback = TRUE.", call. = FALSE)
  }
  invisible(x)
}

.montage_scene_validate_packaging <- function(x) {
  .montage_scene_require_fields(
    x, c("mode", "compression", "max_embed_bytes"), "packaging"
  )
  .montage_scene_choice(x$mode, c("bundle", "embed"), "packaging$mode")
  .montage_scene_choice(
    x$compression, c("gzip", "none"), "packaging$compression"
  )
  .montage_scene_nonnegative_integer(
    x$max_embed_bytes, "packaging$max_embed_bytes", positive = TRUE
  )
  invisible(x)
}

.montage_scene_validate_asset <- function(x, i) {
  label <- paste0("assets[[", i, "]]")
  .montage_scene_require_fields(
    x,
    c("asset_id", "kind", "location", "encoding", "datatype",
      "compression", "hash_algorithm", "hash_scope", "hash",
      "compressed_bytes", "uncompressed_bytes", "geometry",
      "transformations"),
    label
  )
  x$asset_id <- .montage_scene_scalar_character(x$asset_id,
                                                 paste0(label, "$asset_id"))
  x$kind <- .montage_scene_choice(
    x$kind, c("background", "overlay", "mask"), paste0(label, "$kind")
  )
  .montage_scene_validate_location(x$location, paste0(label, "$location"))
  if (!identical(x$encoding, "nifti")) {
    stop("'", label, "$encoding' must be 'nifti'.", call. = FALSE)
  }
  datatype <- .montage_scene_choice(
    x$datatype, c("float64", "uint8"), paste0(label, "$datatype")
  )
  if (identical(x$kind, "mask") && !identical(datatype, "uint8")) {
    stop("Mask VolumeScene assets require datatype = 'uint8'.",
         call. = FALSE)
  }
  if (!identical(x$kind, "mask") && !identical(datatype, "float64")) {
    stop("Background and overlay VolumeScene assets require datatype = 'float64'.",
         call. = FALSE)
  }
  .montage_scene_choice(
    x$compression, c("gzip", "none"), paste0(label, "$compression")
  )
  hash_algorithm <- .montage_scene_scalar_character(
    x$hash_algorithm, paste0(label, "$hash_algorithm")
  )
  if (!identical(hash_algorithm, "md5")) {
    stop("VolumeScene v1 assets require hash_algorithm = 'md5'.",
         call. = FALSE)
  }
  if (!identical(x$hash_scope, "uncompressed")) {
    stop("'", label, "$hash_scope' must be 'uncompressed'.", call. = FALSE)
  }
  hash <- .montage_scene_scalar_character(x$hash, paste0(label, "$hash"))
  if (!grepl("^[0-9a-f]{32}$", hash)) {
    stop("'", label, "$hash' must be a lowercase MD5 digest.",
         call. = FALSE)
  }
  .montage_scene_nonnegative_integer(
    x$compressed_bytes, paste0(label, "$compressed_bytes"), positive = TRUE
  )
  .montage_scene_nonnegative_integer(
    x$uncompressed_bytes, paste0(label, "$uncompressed_bytes"), positive = TRUE
  )
  .montage_scene_validate_geometry(x$geometry, paste0(label, "$geometry"))
  if (!is.character(x$transformations) || anyNA(x$transformations)) {
    stop("'", label, "$transformations' must be a character vector.",
         call. = FALSE)
  }
  x
}

.montage_scene_validate_asset_packaging <- function(assets, packaging) {
  expected_location <- if (identical(packaging$mode, "embed")) {
    "embedded"
  } else {
    "relative"
  }
  for (asset in assets) {
    if (!identical(asset$compression, packaging$compression)) {
      stop(
        "Asset '", asset$asset_id,
        "' compression does not match scene packaging.",
        call. = FALSE
      )
    }
    if (!identical(asset$location$kind, expected_location)) {
      stop(
        "Asset '", asset$asset_id, "' must use a ", expected_location,
        " location for packaging mode '", packaging$mode, "'.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.montage_scene_validate_location <- function(x, label) {
  .montage_scene_require_fields(x, c("kind", "ref"), label)
  kind <- .montage_scene_choice(x$kind, c("relative", "embedded"),
                                paste0(label, "$kind"))
  ref <- .montage_scene_scalar_character(x$ref, paste0(label, "$ref"))
  if (identical(kind, "relative")) {
    unsafe <- grepl("^[A-Za-z][A-Za-z0-9+.-]*:", ref) ||
      grepl("^[/~]", ref) || grepl("^[A-Za-z]:[/\\\\]", ref) ||
      any(strsplit(gsub("\\\\", "/", ref), "/", fixed = TRUE)[[1]] == "..")
    if (unsafe) {
      stop("Relative asset references must be local and path-safe: ", ref,
           call. = FALSE)
    }
  } else if (!grepl("^[A-Za-z][A-Za-z0-9_.:-]*$", ref)) {
    stop("Embedded asset references must be safe HTML identifiers.",
         call. = FALSE)
  }
  invisible(x)
}

.montage_scene_validate_geometry <- function(x, label) {
  .montage_scene_require_fields(
    x,
    c("dimensions", "spacing", "origin", "orientation", "affine",
      "fingerprint"),
    label
  )
  dims <- x$dimensions
  if (!is.numeric(dims) || length(dims) != 3L || anyNA(dims) ||
      any(!is.finite(dims)) || any(dims < 1) || any(dims != floor(dims))) {
    stop("'", label, "$dimensions' must be three positive integers.",
         call. = FALSE)
  }
  spacing <- x$spacing
  if (!is.numeric(spacing) || length(spacing) != 3L || anyNA(spacing) ||
      any(!is.finite(spacing)) || any(spacing <= 0)) {
    stop("'", label, "$spacing' must be three positive finite numbers.",
         call. = FALSE)
  }
  origin <- x$origin
  if (!is.numeric(origin) || length(origin) != 3L || anyNA(origin) ||
      any(!is.finite(origin))) {
    stop("'", label, "$origin' must be three finite numbers.",
         call. = FALSE)
  }
  orientation <- .montage_scene_scalar_character(
    x$orientation, paste0(label, "$orientation")
  )
  if (!grepl("^[RL][AP][SI]$", orientation)) {
    stop("'", label, "$orientation' must be three R/L, A/P, S/I axis codes.",
         call. = FALSE)
  }
  affine <- x$affine
  if (!is.numeric(affine) || length(affine) != 16L || anyNA(affine) ||
      any(!is.finite(affine))) {
    stop("'", label, "$affine' must contain 16 finite numbers.",
         call. = FALSE)
  }
  fingerprint <- .montage_scene_scalar_character(
    x$fingerprint, paste0(label, "$fingerprint")
  )
  expected <- .montage_geometry_fingerprint(x)
  if (!identical(fingerprint, expected)) {
    stop("'", label, "$fingerprint' does not match its geometry fields.",
         call. = FALSE)
  }
  invisible(x)
}

.montage_scene_validate_shared_geometry <- function(assets, background_id) {
  reference <- assets[[background_id]]$geometry
  for (asset_id in names(assets)) {
    candidate <- assets[[asset_id]]$geometry
    mismatch <- .montage_geometry_mismatches(reference, candidate)
    if (length(mismatch) ||
        !identical(reference$fingerprint, candidate$fingerprint)) {
      stop(
        "Volume asset '", asset_id,
        "' does not match background geometry: ",
        paste(unique(c(mismatch, "fingerprint")), collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.montage_scene_validate_analysis <- function(x, i, asset_ids, geometry) {
  label <- paste0("analyses[[", i, "]]")
  .montage_scene_require_fields(
    x,
    c("analysis_id", "primary_map_id", "initial_world_coord",
      "zlevel_bookmarks", "maps"),
    label
  )
  x$analysis_id <- .montage_scene_scalar_character(
    x$analysis_id, paste0(label, "$analysis_id")
  )
  x$primary_map_id <- .montage_scene_scalar_character(
    x$primary_map_id, paste0(label, "$primary_map_id")
  )
  initial_world_coord <- .montage_interactive_world_coord(
    x$initial_world_coord
  )
  if (!.montage_world_coord_in_geometry(initial_world_coord, geometry)) {
    stop("'", label, "$initial_world_coord' lies outside the background field of view.",
         call. = FALSE)
  }
  if (!is.numeric(x$zlevel_bookmarks) || anyNA(x$zlevel_bookmarks) ||
      any(!is.finite(x$zlevel_bookmarks))) {
    stop("'", label, "$zlevel_bookmarks' must be finite numeric values.",
         call. = FALSE)
  }
  if (length(x$zlevel_bookmarks) &&
      any(x$zlevel_bookmarks < 1 |
          x$zlevel_bookmarks > geometry$dimensions[[3L]])) {
    stop("'", label, "$zlevel_bookmarks' must index background z slices.",
         call. = FALSE)
  }
  if (!is.list(x$maps) || length(x$maps) == 0L) {
    stop("'", label, "$maps' must contain at least one map.", call. = FALSE)
  }
  x$maps <- lapply(seq_along(x$maps), function(j) {
    .montage_scene_validate_map(
      x$maps[[j]], paste0(label, "$maps[[", j, "]]"), asset_ids
    )
  })
  map_ids <- vapply(x$maps, `[[`, character(1), "map_id")
  .montage_scene_assert_unique(map_ids, paste0(label, " map_id"))
  if (!x$primary_map_id %in% map_ids) {
    stop("'", label, "$primary_map_id' is not present in its maps.",
         call. = FALSE)
  }
  x
}

.montage_scene_validate_map <- function(x, label, asset_ids) {
  .montage_scene_require_fields(
    x,
    c("map_id", "quantity", "label", "asset_id", "display", "status"),
    label
  )
  x$map_id <- .montage_scene_scalar_character(x$map_id,
                                               paste0(label, "$map_id"))
  .montage_scene_scalar_character(x$quantity, paste0(label, "$quantity"))
  .montage_scene_scalar_character(x$label, paste0(label, "$label"))
  if (!is.null(x$units)) {
    .montage_scene_scalar_character(x$units, paste0(label, "$units"))
  }
  if (!is.null(x$selector_label)) {
    .montage_scene_scalar_character(
      x$selector_label, paste0(label, "$selector_label")
    )
  }
  x$asset_id <- .montage_scene_scalar_character(x$asset_id,
                                                 paste0(label, "$asset_id"))
  if (!x$asset_id %in% asset_ids) {
    stop("'", label, "$asset_id' references an unknown asset.", call. = FALSE)
  }
  .montage_scene_validate_display(x$display, paste0(label, "$display"))
  .montage_scene_validate_status(x$status, paste0(label, "$status"))
  x
}

.montage_scene_validate_status <- function(x, label) {
  .montage_scene_require_fields(x, c("state", "n_display_voxels"), label)
  .montage_scene_choice(x$state, c("ready", "empty"), paste0(label, "$state"))
  .montage_scene_nonnegative_integer(
    x$n_display_voxels, paste0(label, "$n_display_voxels")
  )
  if (identical(x$state, "empty") && x$n_display_voxels != 0L) {
    stop("An empty map must have n_display_voxels = 0.", call. = FALSE)
  }
  invisible(x)
}

.montage_scene_validate_display <- function(x, label) {
  .montage_scene_require_fields(
    x,
    c("mode", "scale", "center", "limits", "threshold", "tail", "palette",
      "alpha", "alpha_mode", "support", "units"),
    label
  )
  mode <- .montage_scene_choice(
    x$mode, c("thresholded", "continuous"), paste0(label, "$mode")
  )
  scale <- .montage_scene_choice(
    x$scale, c("diverging", "sequential"), paste0(label, "$scale")
  )
  if (identical(scale, "diverging")) {
    if (!is.numeric(x$center) || length(x$center) != 1L ||
        !is.finite(x$center) || x$center != 0) {
      stop("Diverging VolumeScene displays require center = 0.", call. = FALSE)
    }
  } else if (!is.null(x$center)) {
    stop("Sequential VolumeScene displays require center = NULL.",
         call. = FALSE)
  }
  if (!is.numeric(x$limits) || length(x$limits) != 2L || anyNA(x$limits) ||
      any(!is.finite(x$limits)) || x$limits[[1]] >= x$limits[[2]]) {
    stop("'", label, "$limits' must be a finite increasing pair.",
         call. = FALSE)
  }
  if (identical(mode, "thresholded")) {
    if (!is.numeric(x$threshold) || length(x$threshold) != 1L ||
        !is.finite(x$threshold) || x$threshold <= 0) {
      stop("Thresholded VolumeScene maps require a positive threshold.",
           call. = FALSE)
    }
  } else if (!is.null(x$threshold)) {
    stop("Continuous VolumeScene maps require threshold = NULL.",
         call. = FALSE)
  }
  .montage_scene_choice(
    x$tail, c("two_sided", "positive", "negative"), paste0(label, "$tail")
  )
  .montage_scene_scalar_character(x$palette, paste0(label, "$palette"))
  if (!is.numeric(x$alpha) || length(x$alpha) != 1L || !is.finite(x$alpha) ||
      x$alpha < 0 || x$alpha > 1) {
    stop("'", label, "$alpha' must be between zero and one.", call. = FALSE)
  }
  .montage_scene_scalar_character(x$alpha_mode,
                                   paste0(label, "$alpha_mode"))
  .montage_scene_choice(
    x$support, c("analysis", "primary"), paste0(label, "$support")
  )
  if (!is.null(x$units)) {
    .montage_scene_scalar_character(x$units, paste0(label, "$units"))
  }
  invisible(x)
}

.montage_scene_require_fields <- function(x, fields, label) {
  if (!is.list(x) || is.data.frame(x)) {
    stop("'", label, "' must be an object/list.", call. = FALSE)
  }
  missing <- setdiff(fields, names(x))
  if (length(missing)) {
    stop(
      "'", label, "' is missing required field(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(x)
}

.montage_scene_scalar_character <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("'", label, "' must be a non-empty character scalar.", call. = FALSE)
  }
  trimws(x)
}

.montage_scene_choice <- function(x, choices, label) {
  x <- .montage_scene_scalar_character(x, label)
  if (!x %in% choices) {
    stop(
      "'", label, "' must be one of: ", paste(choices, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}

.montage_scene_nonnegative_integer <- function(x, label, positive = FALSE) {
  lower <- if (isTRUE(positive)) 1 else 0
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x != floor(x)) {
    stop(
      "'", label, "' must be ", if (positive) "a positive" else "a non-negative",
      " integer.",
      call. = FALSE
    )
  }
  as.integer(x)
}

.montage_scene_assert_unique <- function(x, label) {
  if (anyDuplicated(x)) {
    stop("VolumeScene ", label, " values must be unique.", call. = FALSE)
  }
  invisible(TRUE)
}

.montage_scene_assert_json_safe <- function(x, label) {
  if (is.null(x)) return(invisible(TRUE))
  if (is.function(x) || is.environment(x) || is.language(x) ||
      isS4(x) || is.data.frame(x) || is.raw(x) || is.complex(x)) {
    stop("'", label, "' contains a value that cannot enter VolumeScene JSON.",
         call. = FALSE)
  }
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms)) {
      if (length(nms) != length(x) || anyNA(nms) || any(!nzchar(nms)) ||
          anyDuplicated(nms)) {
        stop("'", label, "' has invalid or duplicate object field names.",
             call. = FALSE)
      }
      banned <- c("path", "source_path", "source_paths", "recipe", "stat_map",
                  "parcel_values")
      bad <- nms[tolower(nms) %in% banned]
      if (length(bad)) {
        stop(
          "VolumeScene forbids source-bearing field(s): ",
          paste(bad, collapse = ", "), ".",
          call. = FALSE
        )
      }
    }
    for (i in seq_along(x)) {
      child <- if (is.null(nms)) {
        paste0(label, "[[", i, "]]")
      } else {
        paste0(label, "$", nms[[i]])
      }
      .montage_scene_assert_json_safe(x[[i]], child)
    }
    return(invisible(TRUE))
  }
  if (!is.atomic(x) || is.factor(x) || (is.object(x) && !is.null(class(x)))) {
    stop("'", label, "' contains a non-JSON R value.", call. = FALSE)
  }
  if (anyNA(x) || (is.numeric(x) && any(!is.finite(x)))) {
    stop("'", label, "' contains missing or non-finite JSON values.",
         call. = FALSE)
  }
  invisible(TRUE)
}
