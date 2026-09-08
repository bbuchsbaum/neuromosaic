# First-class interactive surface-report contracts.

.montage_surface_projection_modes <- c("auto", "none")
.montage_surface_control_names <- c(
  "threshold", "range", "palette", "opacity"
)
.montage_surface_asset_modes <- c("bundle", "embed")
.montage_surface_compressions <- c("gzip", "none")
.montage_surface_adapter_version <- "1.0.0"
.montage_surface_display_asset <- file.path(
  "htmlwidgets", "lib", "neuromosaic-surface", "display.js"
)

#' Configure Interactive Surface Views
#'
#' Defines an optional interactive surface rendering for montage reports. The
#' static surface montage remains the authoritative report figure; this object
#' only controls how the same resolved maps are represented by
#' `neurosurf::SurfaceScene` in HTML.
#'
#' By default, parcel-valued manifest rows are expanded through the supplied
#' surface atlas and volumetric rows are projected with the same thickness,
#' interpolation, aggregation, and smoothing defaults as [surf_montage()].
#' Calling `montage_surface()` is therefore the explicit opt-in to
#' volume-to-surface projection. Set `projection = "none"` to reject volume
#' rows, or supply `values` containing already projected vertex values.
#'
#' @param geometry Optional `SurfaceGeometry`, `SurfaceSet`, or named list with
#'   `left`/`right` (or `lh`/`rh`) geometries. `NULL` uses the geometry carried
#'   by `surfatlas` in [render_montage_report()]. A supplied geometry must have
#'   the same hemisphere topology as that atlas.
#' @param values Optional named list keyed by manifest `map_id`. Each value is
#'   either a numeric vector for a unilateral scene or a named left/right list
#'   of per-vertex values. Values are interpreted as already projected and
#'   support-masked, but not thresholded, so browser threshold exploration can
#'   retain the original vertex values. Every report map must be present.
#' @param curvature Optional unilateral numeric vector or named left/right list
#'   with one value per vertex. By default anatomy is resolved from the atlas
#'   or matching white geometry, as in the static renderer.
#' @param anatomy_style Folding contrast: auto selects binary for the
#'   freesurfer preset and continuous otherwise; none disables the underlay.
#' @param anatomy_midpoint,anatomy_invert Anatomical dark/light boundary and
#'   polarity passed to [neurosurf::normalize_surface_anatomy()].
#' @param projection Whether volumetric manifest rows should be projected
#'   automatically or rejected. Projection is never inferred unless this
#'   configuration object is supplied to the report renderer.
#' @param fun,sampling,interpolation,aggregate,depth,surface_smooth_fwhm
#'   Projection settings passed to [neurosurf::vol_to_surf()]. Defaults match
#'   the publication surface path. `depth = NULL` resolves to five samples from
#'   0.1 through 0.9 for linear thickness sampling.
#' @param opacity Optional browser-layer opacity. `NULL` inherits the effective
#'   static surface opacity for each map.
#' @param assets Asset packaging mode. `"bundle"` writes relative,
#'   content-addressed companion files suitable for an HTTP static site;
#'   `"embed"` stores the same compressed assets once in the HTML for direct
#'   `file://` use.
#' @param compression Surface typed-array compression. `"gzip"` is the
#'   portable, lossless default; `"none"` is useful for diagnostics.
#' @param max_embed_mb Positive size budget, in MiB, checked against the actual
#'   base64 payload before embedded HTML or QMD report data are emitted. It does
#'   not affect bundle mode.
#' @param controls Browser display controls. Any subset of `"threshold"`,
#'   `"range"`, `"palette"`, and `"opacity"`; all are enabled by default.
#'   Controls are exploratory, retain map-local state, and reset to the exact
#'   report-authored defaults without rebuilding the scene or camera.
#' @param preset A neurosurf/surfview visual preset.
#' @param height Widget height as a positive number or CSS size string.
#' @param fallback Optional plain-text failure message. Static surface figures
#'   remain present separately in the report.
#' @param alt_text Optional alternative text. When omitted it is generated per
#'   analysis group.
#'
#' @return A validated `montage_surface` value object.
#' @export
montage_surface <- function(geometry = NULL,
                            values = NULL,
                            curvature = NULL,
                            projection = c("auto", "none"),
                            fun = c("avg", "nn", "mode"),
                            sampling = c("thickness", "midpoint",
                                         "normal_line"),
                            interpolation = c("linear", "nearest", "legacy"),
                            aggregate = c("mean", "closest", "mode"),
                            depth = NULL,
                            surface_smooth_fwhm = 0,
                            opacity = NULL,
                            assets = c("bundle", "embed"),
                            compression = c("gzip", "none"),
                            max_embed_mb = 25,
                            controls = .montage_surface_control_names,
                            preset = "paper-light",
                            height = "600px",
                            fallback = NULL,
                            alt_text = NULL,
                            anatomy_style = c("auto", "continuous", "binary", "none"),
                            anatomy_midpoint = NULL,
                            anatomy_invert = NULL) {
  anatomy_style <- match.arg(anatomy_style)
  neurosurf::normalize_surface_anatomy(c(-1, 1), midpoint = anatomy_midpoint,
                                      invert = anatomy_invert %||% FALSE)
  projection <- match.arg(projection)
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)
  interpolation <- match.arg(interpolation)
  aggregate <- match.arg(aggregate)
  assets <- match.arg(assets)
  compression <- match.arg(compression)
  if (identical(interpolation, "linear") && identical(aggregate, "mode")) {
    stop("aggregate = 'mode' is invalid with linear interpolation.",
         call. = FALSE)
  }
  if (!is.numeric(surface_smooth_fwhm) ||
      length(surface_smooth_fwhm) != 1L ||
      !is.finite(surface_smooth_fwhm) || surface_smooth_fwhm < 0) {
    stop("'surface_smooth_fwhm' must be a non-negative number.",
         call. = FALSE)
  }
  if (!is.null(depth) && (!is.numeric(depth) || !length(depth) ||
                          anyNA(depth) || any(!is.finite(depth)) ||
                          any(depth < 0 | depth > 1))) {
    stop("'depth' must be NULL or finite values between zero and one.",
         call. = FALSE)
  }
  if (!is.null(opacity) && (!is.numeric(opacity) || length(opacity) != 1L ||
                            !is.finite(opacity) || opacity < 0 || opacity > 1)) {
    stop("'opacity' must be NULL or a number between zero and one.",
         call. = FALSE)
  }
  max_embed_mb <- .montage_interactive_positive_number(
    max_embed_mb, "max_embed_mb"
  )
  if (!is.character(controls) || anyNA(controls) ||
      any(!controls %in% .montage_surface_control_names) ||
      anyDuplicated(controls)) {
    stop(
      "'controls' must be a unique subset of: ",
      paste(.montage_surface_control_names, collapse = ", "), ".",
      call. = FALSE
    )
  }
  .montage_surface_string(preset, "preset", required = TRUE)
  .montage_surface_string(fallback, "fallback")
  .montage_surface_string(alt_text, "alt_text")
  if (!((is.numeric(height) && length(height) == 1L && is.finite(height) &&
         height > 0) ||
        (is.character(height) && length(height) == 1L && !is.na(height) &&
         nzchar(trimws(height))))) {
    stop("'height' must be a positive number or one CSS size string.",
         call. = FALSE)
  }
  if (!is.null(values)) {
    if (!is.list(values) || !length(values) || is.null(names(values)) ||
        any(is.na(names(values)) | !nzchar(trimws(names(values)))) ||
        anyDuplicated(names(values))) {
      stop("'values' must be a non-empty uniquely named list keyed by map_id.",
           call. = FALSE)
    }
  }
  if (!is.null(geometry)) {
    .normalize_montage_surface_geometry(geometry, field = "geometry")
  }

  structure(
    list(
      geometry = geometry,
      values = values,
      curvature = curvature,
      anatomy_style = anatomy_style,
      anatomy_midpoint = anatomy_midpoint,
      anatomy_invert = anatomy_invert,
      projection = projection,
      fun = fun,
      sampling = sampling,
      interpolation = interpolation,
      aggregate = aggregate,
      depth = if (is.null(depth) && identical(sampling, "thickness") &&
                  identical(interpolation, "linear")) {
        seq(0.1, 0.9, length.out = 5L)
      } else {
        depth
      },
      surface_smooth_fwhm = as.numeric(surface_smooth_fwhm),
      opacity = if (is.null(opacity)) NULL else as.numeric(opacity),
      assets = assets,
      compression = compression,
      max_embed_mb = max_embed_mb,
      controls = controls,
      preset = trimws(preset),
      height = height,
      fallback = fallback,
      alt_text = alt_text
    ),
    class = "montage_surface"
  )
}

.montage_surface_string <- function(x, field, required = FALSE) {
  if (is.null(x) && !isTRUE(required)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !nzchar(trimws(x))) {
    stop("'", field, "' must be one non-empty string.", call. = FALSE)
  }
  invisible(NULL)
}

.normalize_montage_surface_geometry <- function(geometry,
                                                field = "geometry") {
  if (methods::is(geometry, "SurfaceGeometry") ||
      methods::is(geometry, "SurfaceSet")) {
    resolved <- .montage_resolve_surface_geometry(geometry)
    hemi <- .montage_surface_hemi(resolved@hemi, field)
    return(stats::setNames(list(resolved), hemi))
  }
  if (!is.list(geometry) || !length(geometry) || length(geometry) > 2L ||
      is.null(names(geometry)) || any(!nzchar(names(geometry)))) {
    stop(
      "'", field,
      "' must be a surface geometry or a named one/two-hemisphere list.",
      call. = FALSE
    )
  }
  normalized_names <- vapply(
    names(geometry), .montage_surface_hemi, character(1), field = field
  )
  if (anyDuplicated(normalized_names)) {
    stop("'", field, "' contains duplicate hemisphere names.", call. = FALSE)
  }
  result <- lapply(seq_along(geometry), function(i) {
    candidate <- geometry[[i]]
    if (!methods::is(candidate, "SurfaceGeometry") &&
        !methods::is(candidate, "SurfaceSet")) {
      stop("Every '", field, "' entry must be a surface geometry.",
           call. = FALSE)
    }
    resolved <- .montage_resolve_surface_geometry(candidate)
    declared <- .montage_surface_hemi(resolved@hemi, field)
    if (!identical(declared, normalized_names[[i]])) {
      stop(
        "'", field, "' entry '", names(geometry)[[i]],
        "' declares hemisphere '", resolved@hemi, "'.",
        call. = FALSE
      )
    }
    resolved
  })
  names(result) <- normalized_names
  result[c("left", "right")[c("left", "right") %in% names(result)]]
}

.montage_resolve_surface_geometry <- function(x) {
  resolved <- if (methods::is(x, "SurfaceSet")) {
    neurosurf::get_surface(x)
  } else {
    x
  }
  .montage_repair_surface_geometry(resolved)
}

.montage_repair_surface_geometry <- function(geometry) {
  if (is.null(geometry) || !methods::is(geometry, "SurfaceGeometry")) {
    return(geometry)
  }
  valid <- tryCatch({
    methods::validObject(geometry)
    TRUE
  }, error = function(e) FALSE)
  if (valid) return(geometry)

  mesh <- tryCatch(geometry@mesh, error = function(e) NULL)
  if (is.null(mesh) || is.null(mesh$vb) || is.null(mesh$it)) return(geometry)
  vertices <- t(mesh$vb[1:3, , drop = FALSE])
  faces <- t(mesh$it) - 1L
  storage.mode(faces) <- "integer"
  hemi <- tryCatch(geometry@hemi, error = function(e) "left")
  if (length(hemi) != 1L || is.na(hemi) || !nzchar(hemi)) hemi <- "left"
  tryCatch(
    neurosurf::SurfaceGeometry(vertices, faces, hemi = hemi),
    error = function(e) geometry
  )
}

.montage_surface_hemi <- function(x, field = "geometry") {
  key <- tolower(trimws(as.character(x)))
  if (length(key) == 1L && key %in% c("l", "lh", "left")) return("left")
  if (length(key) == 1L && key %in% c("r", "rh", "right")) return("right")
  stop("'", field, "' hemisphere must be left/lh or right/rh.",
       call. = FALSE)
}

.prepare_montage_surface_report <- function(manifest,
                                            panels,
                                            surfatlas,
                                            surface,
                                            surface_args = list()) {
  if (is.null(surface)) return(NULL)
  if (!inherits(surface, "montage_surface")) {
    stop("'surface' must be NULL or created by montage_surface().",
         call. = FALSE)
  }
  if (!inherits(surfatlas, "surfatlas")) {
    stop("Interactive surfaces require a 'surfatlas' object.", call. = FALSE)
  }
  if (!.has_surface_geometry(surfatlas)) {
    stop(
      "Interactive surfaces require surfatlas$lh_atlas/rh_atlas geometry.",
      call. = FALSE
    )
  }
  map_ids <- as.character(manifest$map_id)
  missing_fallback <- map_ids[!vapply(map_ids, function(map_id) {
    panel <- panels[[map_id]] %||% list()
    path <- panel[["surface_image"]] %||% panel[["surface_image_path"]]
    is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
  }, logical(1))]
  if (length(missing_fallback)) {
    stop(
      "Interactive surfaces require a static surface fallback for map_id: ",
      paste(missing_fallback, collapse = ", "), ".",
      call. = FALSE
    )
  }

  atlas_geometry <- .montage_surface_atlas_geometry(surfatlas)
  geometry <- if (is.null(surface$geometry)) {
    atlas_geometry
  } else {
    .normalize_montage_surface_geometry(surface$geometry)
  }
  .validate_montage_surface_topology(
    geometry, atlas_geometry, context = "interactive surface geometry"
  )
  anatomy <- .montage_surface_anatomy(surfatlas, geometry, surface, surface_args)

  custom_values <- surface$values
  if (!is.null(custom_values)) {
    missing <- setdiff(map_ids, names(custom_values))
    extra <- setdiff(names(custom_values), map_ids)
    if (length(missing) || length(extra)) {
      stop(
        "'surface$values' must match manifest map_id values exactly",
        if (length(missing)) paste0("; missing: ", paste(missing, collapse = ", ")) else "",
        if (length(extra)) paste0("; unknown: ", paste(extra, collapse = ", ")) else "",
        ".", call. = FALSE
      )
    }
    custom_values <- custom_values[map_ids]
  }

  sources <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_surface_source_args(manifest, i)
  })
  source_values <- lapply(sources, function(source) source$vals %||% source$stat)
  support_masks <- .montage_render_support_masks(manifest, source_values)
  projected <- vector("list", nrow(manifest))
  provenance <- vector("list", nrow(manifest))

  for (i in seq_len(nrow(manifest))) {
    map_id <- map_ids[[i]]
    panel_surface <- panels[[map_id]][["surface"]] %||% list()
    display_mode <- panel_surface$display_mode %||%
      manifest$effective_display_mode[[i]]
    tail <- panel_surface$tail %||% manifest$effective_tail[[i]]
    support_mask <- surface_args$support_mask %||% support_masks[[i]]
    if (!is.null(custom_values)) {
      projected[[i]] <- .normalize_montage_surface_values(
        custom_values[[map_id]], geometry, map_id
      )
      projected[[i]] <- .montage_surface_apply_tail(
        projected[[i]], display_mode, tail
      )
      provenance[[i]] <- list(source = "provided_vertex_values")
      next
    }

    if (!is.null(sources[[i]]$vals)) {
      parcel_values <- .surface_parcel_values(sources[[i]]$vals, surfatlas)
      support <- .normalize_montage_support_mask(
        support_mask, length(parcel_values)
      )
      parcel_values[!support] <- NA_real_
      parcel_values <- .montage_surface_apply_tail(
        list(values = parcel_values), display_mode, tail
      )$values
      projected[[i]] <- .montage_surface_expand_parcels(
        parcel_values, surfatlas, geometry, map_id
      )
      provenance[[i]] <- list(
        source = "parcel_values",
        parcel_ids = as.character(surfatlas$ids)
      )
      next
    }

    if (identical(surface$projection, "none")) {
      stop(
        "map_id '", map_id,
        "' is volumetric but montage_surface(projection = 'none') forbids ",
        "volume-to-surface projection.", call. = FALSE
      )
    }
    projected_result <- .montage_surface_project_volume(
      stat = sources[[i]]$stat,
      support_mask = support_mask,
      display_mode = display_mode,
      tail = tail,
      surfatlas = surfatlas,
      geometry = geometry,
      config = .montage_surface_projection_config(surface, surface_args),
      map_id = map_id
    )
    projected[[i]] <- projected_result$values
    provenance[[i]] <- projected_result$provenance
  }
  names(projected) <- map_ids
  names(provenance) <- map_ids

  groups <- .montage_analysis_groups(manifest)
  scenes <- lapply(groups, function(group) {
    group_ids <- as.character(group$map_ids)
    layers <- lapply(group_ids, function(map_id) {
      i <- match(map_id, map_ids)
      .montage_surface_scene_layer(
        row = manifest[i, , drop = FALSE],
        panel = panels[[map_id]],
        values = projected[[map_id]],
        provenance = provenance[[map_id]],
        opacity = surface$opacity %||% if (surface$preset == "freesurfer") 1 else NULL,
        heat = surface$preset == "freesurfer"
      )
    })
    layer_ids <- vapply(layers, `[[`, character(1), "name")
    map_to_layer <- stats::setNames(layer_ids, group_ids)
    primary_layer <- unname(map_to_layer[[group$primary_map_id]])
    analysis_label <- .montage_surface_analysis_label(manifest, group)
    fallback <- surface$fallback %||% paste0(
      "Interactive surface unavailable for ", analysis_label,
      "; use the static surface montage in this section."
    )
    alt_text <- surface$alt_text %||% paste0(
      "Interactive bilateral cortical surface for ", analysis_label, "."
    )
    scene_args <- c(
      geometry,
      list(
        layers = layers,
        curvature = anatomy$curvature,
        selected_layer = primary_layer,
        id = paste0(
          "neuromosaic-surface-",
          substr(.montage_md5_text(as.character(group$analysis_id)), 1L, 12L)
        ),
        metadata = list(
          analysis_id = as.character(group$analysis_id),
          map_to_layer = as.list(map_to_layer)
        ),
        provenance = list(
          package = "neuromosaic",
          anatomy = anatomy$provenance,
          projection_is_display_only = TRUE
        ),
        fallback = fallback,
        alt_text = alt_text,
        preset = surface$preset,
        mode = "report",
        asset_mode = "inline"
      )
    )
    scene <- do.call(neurosurf::surface_scene, scene_args)
    structure(
      list(
        analysis_id = as.character(group$analysis_id),
        scene = scene,
        map_to_layer = map_to_layer,
        layer_to_map = stats::setNames(group_ids, layer_ids),
        primary_map_id = as.character(group$primary_map_id),
        height = surface$height
      ),
      class = c("montage_surface_group", "list")
    )
  })
  names(scenes) <- names(groups)

  structure(
    list(
      scenes = scenes,
      preset = surface$preset,
      height = surface$height,
      projection = surface$projection,
      packaging = list(
        mode = surface$assets,
        compression = surface$compression,
        max_embed_mb = surface$max_embed_mb
      ),
      controls = surface$controls
    ),
    class = c("montage_surface_report", "list")
  )
}

.package_montage_surface_report <- function(surface, output_file) {
  if (is.null(surface)) return(NULL)
  if (!inherits(surface, "montage_surface_report")) {
    stop("Interactive surface report data are malformed.", call. = FALSE)
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
      is.na(output_file) || !nzchar(output_file)) {
    stop("'output_file' must be one non-empty path.", call. = FALSE)
  }

  raw_dir <- tempfile("neuromosaic-surface-raw-")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(raw_dir, recursive = TRUE), add = TRUE)
  manifests <- lapply(surface$scenes, function(group) {
    unclass(neurosurf::surface_scene_manifest(
      group$scene, asset_mode = "directory", asset_dir = raw_dir
    ))
  })

  descriptors <- list()
  for (manifest in manifests) {
    for (asset_id in names(manifest$assets)) {
      candidate <- manifest$assets[[asset_id]]
      existing <- descriptors[[asset_id]]
      if (!is.null(existing) &&
          (!identical(existing$sha256, candidate$sha256) ||
           !identical(existing$role, candidate$role) ||
           !identical(existing$dtype, candidate$dtype) ||
           !identical(as.integer(existing$shape),
                      as.integer(candidate$shape)))) {
        stop("Conflicting surface asset descriptor: ", asset_id, ".",
             call. = FALSE)
      }
      descriptors[[asset_id]] <- candidate
    }
  }

  packaging <- surface$packaging
  output_dir <- dirname(output_file)
  report_stem <- tools::file_path_sans_ext(basename(output_file))
  companion_name <- paste0(report_stem, "_files")
  embedded <- identical(packaging$mode, "embed")
  packaged_dir <- if (embedded) {
    tempfile("neuromosaic-surface-embedded-")
  } else {
    file.path(output_dir, companion_name, "interactive", "surfaces")
  }
  dir.create(packaged_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(packaged_dir)) {
    stop("Could not create interactive surface asset directory: ",
         packaged_dir, call. = FALSE)
  }
  if (embedded) on.exit(unlink(packaged_dir, recursive = TRUE), add = TRUE)
  ref_prefix <- if (embedded) "" else .montage_surface_web_path(file.path(
    companion_name, "interactive", "surfaces"
  ))

  records <- list()
  payloads <- list()
  browser_descriptors <- list()
  for (asset_id in names(descriptors)) {
    descriptor <- descriptors[[asset_id]]
    raw_path <- file.path(raw_dir, descriptor$uri)
    packaged <- .montage_materialize_surface_asset(
      raw_path = raw_path,
      descriptor = descriptor,
      output_dir = packaged_dir,
      compression = packaging$compression
    )
    payload_id <- paste0(
      "nm-surface-asset-", descriptor$role, "-", descriptor$sha256
    )
    ref <- if (embedded) {
      paste0("nm-surface-asset:", payload_id)
    } else {
      paste0(ref_prefix, "/", basename(packaged$path))
    }
    browser_descriptor <- descriptor
    browser_descriptor$uri <- ref
    browser_descriptor$encoding <- NULL
    browser_descriptor$data <- NULL
    browser_descriptor$metadata <- utils::modifyList(
      browser_descriptor$metadata %||% list(),
      list(neuromosaicPackaging = list(
        compression = packaging$compression,
        compressedByteLength = packaged$compressed_bytes
      ))
    )
    browser_descriptors[[asset_id]] <- browser_descriptor
    records[[asset_id]] <- list(
      asset_id = asset_id,
      role = descriptor$role,
      dtype = descriptor$dtype,
      shape = as.integer(descriptor$shape),
      location = list(
        kind = if (embedded) "embedded" else "relative",
        ref = if (embedded) payload_id else ref
      ),
      encoding = "surfview-typed-array-v1",
      compression = packaging$compression,
      hash_algorithm = "sha256",
      hash_scope = "uncompressed",
      hash = descriptor$sha256,
      compressed_bytes = packaged$compressed_bytes,
      uncompressed_bytes = as.numeric(descriptor$byteLength),
      transformations = .montage_surface_asset_transformations(
        descriptor$role, packaging$compression
      )
    )
    if (embedded) {
      bytes <- readBin(
        packaged$path, what = "raw", n = file.info(packaged$path)$size
      )
      payloads[[asset_id]] <- list(
        id = payload_id,
        asset_id = asset_id,
        compression = packaging$compression,
        base64 = jsonlite::base64_enc(bytes)
      )
    }
  }

  embedded_bytes <- if (length(payloads)) {
    sum(vapply(
      payloads, function(payload) nchar(payload$base64, type = "bytes"),
      numeric(1)
    ))
  } else {
    0
  }
  if (embedded) {
    budget <- round(packaging$max_embed_mb * 1024^2)
    if (embedded_bytes > budget) {
      compressed_bytes <- sum(vapply(
        records, `[[`, numeric(1), "compressed_bytes"
      ))
      uncompressed_bytes <- sum(vapply(
        records, `[[`, numeric(1), "uncompressed_bytes"
      ))
      stop(
        "Interactive surface assets require ",
        format(round(uncompressed_bytes / 1024^2, 2), nsmall = 2),
        " MiB raw, ",
        format(round(compressed_bytes / 1024^2, 2), nsmall = 2),
        " MiB ", packaging$compression, ", and ",
        format(round(embedded_bytes / 1024^2, 2), nsmall = 2),
        " MiB base64, exceeding max_embed_mb = ",
        packaging$max_embed_mb,
        ". Use assets = 'bundle' or raise the explicit budget.",
        call. = FALSE
      )
    }
  }

  runtime <- .montage_surface_runtime_info()
  for (i in seq_along(manifests)) {
    manifest <- manifests[[i]]
    manifest$assets <- browser_descriptors[names(manifest$assets)]
    manifest$provenance <- utils::modifyList(
      manifest$provenance %||% list(),
      list(
        packaging = list(
          mode = packaging$mode,
          compression = packaging$compression,
          hashAlgorithm = "sha256",
          hashScope = "uncompressed"
        ),
        recoverableVertexData = TRUE
      )
    )
    group <- surface$scenes[[i]]
    group$manifest <- manifest
    group$manifest_id <- paste0(
      "nm-surface-scene-",
      .montage_md5_text(.montage_surface_manifest_json(manifest))
    )
    group$layer_count <- length(group$scene@layers)
    group$fallback <- group$scene@fallback
    group$alt_text <- group$scene@alt_text
    group$preset <- group$scene@preset
    group$bilateral_group <- if (all(
      c("left", "right") %in% names(group$scene@geometries)
    )) {
      list(
        id = "bilateral",
        leftSurfaceId = "left",
        rightSurfaceId = "right"
      )
    } else {
      NULL
    }
    group$scene <- NULL
    surface$scenes[[i]] <- group
  }

  role <- vapply(records, `[[`, character(1), "role")
  surface$assets <- records
  surface$payloads <- payloads
  surface$summary <- list(
    asset_count = length(records),
    geometry_asset_count = sum(role %in% c("vertices", "faces", "curvature")),
    value_asset_count = sum(role %in% c("values", "indices")),
    compressed_bytes = sum(vapply(
      records, `[[`, numeric(1), "compressed_bytes"
    )),
    uncompressed_bytes = sum(vapply(
      records, `[[`, numeric(1), "uncompressed_bytes"
    )),
    embedded_bytes = embedded_bytes,
    packaging = packaging$mode,
    compression = packaging$compression
  )
  surface$runtime <- runtime
  surface
}

.montage_materialize_surface_asset <- function(raw_path,
                                               descriptor,
                                               output_dir,
                                               compression) {
  if (!file.exists(raw_path)) {
    stop("Missing serialized surface asset: ", basename(raw_path), ".",
         call. = FALSE)
  }
  actual <- digest::digest(
    file = raw_path, algo = "sha256", serialize = FALSE
  )
  if (!identical(actual, descriptor$sha256)) {
    stop("Serialized surface asset checksum mismatch: ", descriptor$id, ".",
         call. = FALSE)
  }
  role <- gsub("[^a-z0-9-]", "-", descriptor$role)
  suffix <- if (identical(compression, "gzip")) ".bin.gz" else ".bin"
  file_name <- paste0("sha256-", descriptor$sha256, ".", role, suffix)
  final_path <- file.path(output_dir, file_name)
  staged <- tempfile(
    paste0(".", descriptor$sha256, "-"),
    tmpdir = output_dir,
    fileext = suffix
  )
  on.exit(unlink(staged), add = TRUE)
  if (identical(compression, "gzip")) {
    .montage_gzip_file(raw_path, staged)
  } else if (!file.copy(raw_path, staged, overwrite = FALSE)) {
    stop("Failed to stage uncompressed interactive surface asset.",
         call. = FALSE)
  }
  if (!file.exists(staged)) {
    stop("Failed to stage interactive surface asset.", call. = FALSE)
  }
  if (file.exists(final_path)) {
    existing <- .montage_surface_asset_sha256(final_path, compression)
    if (!identical(existing, descriptor$sha256)) {
      stop(
        "Refusing to overwrite an existing surface asset whose content does ",
        "not match its content-addressed name: ", final_path,
        call. = FALSE
      )
    }
    unlink(staged)
  } else if (!file.rename(staged, final_path)) {
    stop("Failed to atomically install interactive surface asset.",
         call. = FALSE)
  }
  list(
    path = normalizePath(final_path, mustWork = TRUE),
    compressed_bytes = as.numeric(file.info(final_path)$size)
  )
}

.montage_surface_asset_sha256 <- function(path, compression) {
  if (identical(compression, "none")) {
    return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
  }
  expanded <- tempfile("neuromosaic-surface-expanded-", fileext = ".bin")
  on.exit(unlink(expanded), add = TRUE)
  input <- gzfile(path, open = "rb")
  output <- file(expanded, open = "wb")
  on.exit(try(close(input), silent = TRUE), add = TRUE)
  on.exit(try(close(output), silent = TRUE), add = TRUE)
  repeat {
    chunk <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (!length(chunk)) break
    writeBin(chunk, output)
  }
  close(input)
  close(output)
  digest::digest(file = expanded, algo = "sha256", serialize = FALSE)
}

.montage_surface_asset_transformations <- function(role, compression) {
  role_transformation <- switch(
    role,
    vertices = "surface_coordinates_to_float32_le",
    faces = "one_based_faces_to_zero_based_uint32_le",
    curvature = "curvature_to_float32_le_preserving_nan",
    values = "vertex_values_to_float32_le_preserving_nan",
    indices = "one_based_indices_to_zero_based_uint32_le",
    "typed_array_little_endian"
  )
  c(role_transformation, paste0("compression_", compression))
}

.montage_surface_web_path <- function(path) {
  gsub("\\\\", "/", path)
}

.montage_surface_manifest_json <- function(manifest) {
  json <- jsonlite::toJSON(
    manifest,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  )
  .montage_json_script_escape(as.character(json))
}

.montage_surface_runtime_info <- function() {
  yaml_path <- system.file("htmlwidgets", "surfwidget.yaml", package = "neurosurf")
  if (!nzchar(yaml_path) || !file.exists(yaml_path)) {
    stop("neurosurf surfwidget runtime metadata are unavailable.",
         call. = FALSE)
  }
  spec <- yaml::read_yaml(yaml_path)
  dependencies <- spec$dependencies %||% list()
  matches <- vapply(
    dependencies,
    function(dependency) identical(dependency$name, "surfview"),
    logical(1)
  )
  if (sum(matches) != 1L) {
    stop("neurosurf must declare exactly one surfview runtime dependency.",
         call. = FALSE)
  }
  dependency <- dependencies[[which(matches)]]
  script <- dependency$script
  if (is.list(script)) script <- unlist(script, use.names = FALSE)
  if (!is.character(script) || length(script) != 1L || !nzchar(script)) {
    stop("neurosurf surfview runtime script metadata are malformed.",
         call. = FALSE)
  }
  root <- system.file(dependency$src, package = "neurosurf")
  path <- file.path(root, script)
  if (!nzchar(root) || !file.exists(path)) {
    stop("neurosurf surfview runtime is unavailable.", call. = FALSE)
  }
  list(
    name = "surfview",
    version = as.character(dependency$version),
    neurosurf_version = as.character(utils::packageVersion("neurosurf")),
    script = script,
    source = as.character(dependency$src),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE),
    adapter_version = .montage_surface_adapter_version,
    adapter_sha256 = digest::digest(
      file = .montage_surface_display_path(),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

.montage_surface_display_path <- function() {
  installed <- system.file(.montage_surface_display_asset,
                           package = "neuromosaic")
  if (nzchar(installed)) return(installed)
  development <- file.path("inst", .montage_surface_display_asset)
  if (file.exists(development)) return(normalizePath(development))
  stop("Bundled interactive surface display helper not found.", call. = FALSE)
}

.montage_surface_runtime_dependency <- function(runtime) {
  root <- system.file(runtime$source, package = "neurosurf")
  path <- file.path(root, runtime$script)
  if (!nzchar(root) || !file.exists(path)) {
    stop("The recorded neurosurf surfview runtime is unavailable.",
         call. = FALSE)
  }
  actual <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  if (!identical(actual, runtime$sha256)) {
    stop(
      "The installed surfview runtime does not match the report's recorded ",
      "runtime hash.", call. = FALSE
    )
  }
  htmltools::htmlDependency(
    name = runtime$name,
    version = runtime$version,
    src = c(file = root),
    script = runtime$script
  )
}

.montage_surface_atlas_geometry <- function(surfatlas) {
  result <- list(
    left = surfatlas$lh_atlas@geometry,
    right = surfatlas$rh_atlas@geometry
  )
  result <- lapply(result, .montage_repair_surface_geometry)
  .normalize_montage_surface_geometry(result, field = "surfatlas geometry")
}

.validate_montage_surface_topology <- function(actual, expected, context) {
  if (!identical(names(actual), names(expected))) {
    stop("Hemisphere coverage differs for ", context, ".", call. = FALSE)
  }
  for (hemi in names(expected)) {
    actual_coords <- neurosurf::coords(actual[[hemi]])
    expected_coords <- neurosurf::coords(expected[[hemi]])
    actual_faces <- neurosurf::faces(actual[[hemi]])
    expected_faces <- neurosurf::faces(expected[[hemi]])
    if (nrow(actual_coords) != nrow(expected_coords) ||
        !identical(dim(actual_faces), dim(expected_faces)) ||
        !identical(as.integer(actual_faces), as.integer(expected_faces))) {
      stop(
        "Topology mismatch for ", context, " (", hemi, " hemisphere).",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.normalize_montage_surface_values <- function(values, geometry, map_id) {
  if (is.numeric(values) && length(geometry) == 1L) {
    values <- stats::setNames(list(values), names(geometry))
  }
  if (!is.list(values) || is.null(names(values)) ||
      any(!nzchar(names(values)))) {
    stop(
      "Surface values for map_id '", map_id,
      "' must be numeric for one hemisphere or a named hemisphere list.",
      call. = FALSE
    )
  }
  names(values) <- vapply(
    names(values), .montage_surface_hemi, character(1), field = "surface values"
  )
  if (anyDuplicated(names(values)) || !setequal(names(values), names(geometry))) {
    stop(
      "Surface values for map_id '", map_id,
      "' must match geometry hemispheres: ",
      paste(names(geometry), collapse = ", "), ".", call. = FALSE
    )
  }
  values <- values[names(geometry)]
  for (hemi in names(geometry)) {
    expected <- nrow(neurosurf::coords(geometry[[hemi]]))
    if (!is.numeric(values[[hemi]]) || length(values[[hemi]]) != expected) {
      stop(
        "Surface values for map_id '", map_id, "' have ",
        length(values[[hemi]]), " values for ", expected, " ", hemi,
        " vertices.", call. = FALSE
      )
    }
    values[[hemi]] <- as.numeric(values[[hemi]])
  }
  values
}

.montage_surface_apply_tail <- function(values, display_mode, tail) {
  if (!identical(display_mode, "thresholded")) return(values)
  lapply(values, function(x) {
    value_names <- names(x)
    x <- as.numeric(x)
    names(x) <- value_names
    if (identical(tail, "positive")) x[x < 0] <- NA_real_
    if (identical(tail, "negative")) x[x > 0] <- NA_real_
    x
  })
}

.montage_surface_expand_parcels <- function(values,
                                            surfatlas,
                                            geometry,
                                            map_id) {
  ids <- names(values)
  result <- lapply(names(geometry), function(hemi) {
    key <- if (identical(hemi, "left")) "lh_atlas" else "rh_atlas"
    atlas_hemi <- surfatlas[[key]]
    labels <- as.character(atlas_hemi@data)
    expected <- nrow(neurosurf::coords(geometry[[hemi]]))
    if (length(labels) != expected) {
      stop(
        "Surface atlas labels for map_id '", map_id, "' have ",
        length(labels), " entries for ", expected, " ", hemi,
        " vertices.", call. = FALSE
      )
    }
    unname(values[match(labels, ids)])
  })
  names(result) <- names(geometry)
  result
}

.montage_surface_projection_config <- function(surface, surface_args) {
  fields <- c(
    "fun", "sampling", "interpolation", "aggregate", "depth",
    "surface_smooth_fwhm", "surface_space", "density_override",
    "resolution_override"
  )
  base <- surface[intersect(fields, names(surface))]
  overrides <- surface_args[intersect(fields, names(surface_args))]
  utils::modifyList(base, overrides)
}

.montage_surface_project_volume <- function(stat,
                                            support_mask,
                                            display_mode,
                                            tail,
                                            surfatlas,
                                            geometry,
                                            config,
                                            map_id) {
  stat <- .load_overlay_neurovol(stat, paste0("stat for map_id '", map_id, "'"))
  arr <- as.array(stat)
  raw <- as.numeric(arr)
  support <- .normalize_montage_support_mask(support_mask, length(raw))
  raw[!support] <- NA_real_
  if (identical(display_mode, "thresholded") && identical(tail, "positive")) {
    raw[raw < 0] <- NA_real_
  }
  if (identical(display_mode, "thresholded") && identical(tail, "negative")) {
    raw[raw > 0] <- NA_real_
  }
  projection_vol <- neuroim2::NeuroVol(
    array(raw, dim = dim(arr)), space = neuroim2::space(stat)
  )

  out <- lapply(names(geometry), function(hemi) {
    short <- if (identical(hemi, "left")) "lh" else "rh"
    pair <- .resolve_overlay_surface_pair(
      surfatlas = surfatlas,
      hemi = short,
      space_override = config$surface_space,
      density_override = config$density_override,
      resolution_override = config$resolution_override
    )
    pair_geometry <- list(
      display = geometry[[hemi]],
      white = .montage_repair_surface_geometry(pair$white),
      pial = .montage_repair_surface_geometry(pair$pial)
    )
    display_faces <- neurosurf::faces(pair_geometry$display)
    display_n <- nrow(neurosurf::coords(pair_geometry$display))
    for (kind in c("white", "pial")) {
      candidate <- pair_geometry[[kind]]
      if (nrow(neurosurf::coords(candidate)) != display_n ||
          !identical(dim(neurosurf::faces(candidate)), dim(display_faces)) ||
          !identical(
            as.integer(neurosurf::faces(candidate)), as.integer(display_faces)
          )) {
        stop(
          "Projection ", kind, " topology does not match ", hemi,
          " display geometry for map_id '", map_id, "'.", call. = FALSE
        )
      }
    }
    projected <- neurosurf::vol_to_surf(
      surf_wm = pair_geometry$white,
      surf_pial = pair_geometry$pial,
      vol = projection_vol,
      fun = config$fun %||% "avg",
      sampling = config$sampling %||% "thickness",
      interpolation = config$interpolation %||% "linear",
      aggregate = config$aggregate %||% "mean",
      depth = config$depth,
      surface_smooth_fwhm = config$surface_smooth_fwhm %||% 0,
      fill = NA_real_
    )
    values <- .surface_values_to_numeric(projected)
    if (is.null(values) || length(values) != display_n) {
      stop(
        "Projection produced ", length(values), " values for ", display_n,
        " ", hemi, " vertices for map_id '", map_id, "'.", call. = FALSE
      )
    }
    as.numeric(values)
  })
  names(out) <- names(geometry)
  list(
    values = out,
    provenance = list(
      source = "volume_projection",
      surface_space = surfatlas$surface_space %||% config$surface_space %||%
        "unknown",
      fun = config$fun %||% "avg",
      sampling = config$sampling %||% "thickness",
      interpolation = config$interpolation %||% "linear",
      aggregate = config$aggregate %||% "mean",
      depth = config$depth,
      surface_smooth_fwhm = config$surface_smooth_fwhm %||% 0
    )
  )
}

.montage_surface_scene_layer <- function(row,
                                         panel,
                                         values,
                                         provenance,
                                         opacity = NULL,
                                         heat = FALSE) {
  map_id <- as.character(row$map_id[[1L]])
  metadata <- panel[["surface"]] %||% list()
  limits <- metadata$limits
  if (is.null(limits) || length(limits) != 2L || any(!is.finite(limits))) {
    limits <- c(row$effective_lower[[1L]], row$effective_upper[[1L]])
  }
  threshold <- metadata$threshold %||% row$effective_threshold[[1L]]
  display_mode <- metadata$display_mode %||% row$effective_display_mode[[1L]]
  tail <- metadata$tail %||% row$effective_tail[[1L]]
  threshold_pair <- .montage_surface_threshold_pair(
    display_mode, threshold, tail, limits
  )
  palette <- metadata$palette %||% .montage_surface_palette(
    row$effective_palette_family[[1L]], row$effective_scale[[1L]]
  )
  if (heat) palette <- if (identical(row$effective_scale[[1L]], "diverging")) {
    "surface-heat"
  } else {
    "surface-heat-positive"
  }
  layer_opacity <- opacity %||% metadata$alpha %||% 0.85
  legend <- if (!is.null(metadata$legend_title)) {
    .montage_legend(metadata$legend_title, metadata$units)
  } else {
    .montage_row_legend(row)
  }
  units <- legend$units
  layer_id <- paste0("map-", substr(.montage_md5_text(map_id), 1L, 16L))

  neurosurf::surface_layer(
    name = layer_id,
    values = values,
    colormap = .montage_surface_browser_palette(
      palette, row$effective_scale[[1L]]
    ),
    limits = as.numeric(limits),
    opacity = as.numeric(layer_opacity),
    units = units,
    legend = list(
      title = legend$title,
      units = units,
      visible = TRUE
    ),
    metadata = list(
      analysis_id = as.character(row$analysis_id[[1L]]),
      map_id = map_id,
      selector_label = .profile_row_character(row, 1L, "selector_label") %||%
        as.character(row$label[[1L]]),
      quantity = as.character(row$quantity[[1L]]),
      display_mode = display_mode,
      tail = tail,
      report_threshold = if (identical(display_mode, "continuous")) {
        NA_real_
      } else {
        as.numeric(threshold)
      },
      display_only = TRUE
    ),
    provenance = provenance,
    threshold = threshold_pair
  )
}

.montage_surface_threshold_pair <- function(display_mode,
                                            threshold,
                                            tail,
                                            limits) {
  if (identical(display_mode, "continuous") || is.null(threshold) ||
      length(threshold) != 1L || is.na(threshold)) {
    return(NULL)
  }
  threshold <- abs(as.numeric(threshold))
  if (identical(tail, "positive")) {
    return(c(min(limits[[1L]], threshold), threshold))
  }
  if (identical(tail, "negative")) {
    return(c(-threshold, max(limits[[2L]], -threshold)))
  }
  c(-threshold, threshold)
}

.montage_surface_browser_palette <- function(palette, scale) {
  if (is.character(palette) && length(palette) > 1L && !anyNA(palette)) {
    return(palette)
  }
  fallback <- if (identical(scale, "diverging")) "RdBu" else "inferno"
  if (!is.character(palette) || length(palette) != 1L || is.na(palette)) {
    return(fallback)
  }
  key <- tolower(trimws(palette))
  aliases <- c(
    "vik" = "RdBu", "cork" = "RdBu", "blue-red" = "RdBu",
    "bluered" = "RdBu", "rdbu" = "RdBu",
    "sequential" = "inferno", "diverging" = "RdBu",
    "gray" = "greys", "grey" = "greys"
  )
  if (key %in% names(aliases)) return(unname(aliases[[key]]))
  supported <- c(
    "surface-heat", "surface-heat-positive",
    "bone", "copper", "greys", "greens", "picnic", "portland",
    "blackbody", "jet", "hot", "cool", "spring", "summer", "autumn",
    "winter", "hsv", "rainbow", "viridis", "inferno", "magma",
    "plasma", "warm", "rainbow-soft", "glasbey"
  )
  if (key %in% supported) return(key)
  fallback
}

.montage_surface_anatomy <- function(surfatlas, geometry, surface, surface_args) {
  style <- surface$anatomy_style %||% "auto"
  if (style == "none") return(list(curvature = NULL, provenance = list(style = "none")))
  if (style == "auto") {
    style <- surface_args$anatomy_style %||%
      if (surface$preset == "freesurfer") "binary" else "continuous"
    if (style == "publication") style <- "continuous"
  }
  metric <- surface$curvature %||% surface_args$anatomy_metric
  if (!is.null(metric)) {
    metric <- .normalize_montage_surface_values(metric, geometry, "anatomy")
  }
  provenance <- list()
  curvature <- lapply(names(geometry), function(hemi) {
    resolved <- neuroatlas::surface_anatomy(
      surfatlas, hemi, metric = if (is.null(metric)) NULL else metric[[hemi]],
      source = surface_args$anatomy_metric_source
    )
    midpoint <- surface$anatomy_midpoint %||% surface_args$anatomy_midpoint
    invert <- surface$anatomy_invert %||% surface_args$anatomy_invert %||% FALSE
    provenance[[hemi]] <<- c(resolved$provenance, list(
      style = style,
      midpoint = midpoint %||% stats::median(resolved$metric),
      invert = invert
    ))
    neurosurf::normalize_surface_anatomy(resolved$metric, style, midpoint, invert)
  })
  names(curvature) <- names(geometry)
  list(curvature = curvature, provenance = provenance)
}

.montage_surface_analysis_label <- function(manifest, group) {
  rows <- match(group$map_ids, manifest$map_id)
  if ("analysis_label" %in% names(manifest)) {
    labels <- as.character(manifest$analysis_label[rows])
    labels <- labels[!is.na(labels) & nzchar(labels)]
    if (length(labels)) return(labels[[1L]])
  }
  as.character(group$analysis_id)
}

#' Interactive Surface Report Hooks
#'
#' Stable emitters for custom R Markdown or Quarto templates that want the same
#' optional, map-synchronized surface viewer as the bundled montage templates.
#' Emit the document bridge once, then emit the matching host after each static
#' analysis group. Static surface images remain the report default and fallback.
#'
#' @param surface The `surface` member of prepared montage report data, or
#'   `NULL` for a static-only report.
#' @param is_html Logical; interactive output is emitted only for HTML.
#'
#' @return A list with `emit_document()` and `emit_host(analysis_id)` functions.
#' @export
montage_surface_report_hooks <- function(surface = NULL, is_html = FALSE) {
  if (!is.null(surface) &&
      !inherits(surface, "montage_surface_report")) {
    stop(
      "'surface' must be NULL or prepared montage surface data.",
      call. = FALSE
    )
  }
  list(
    emit_document = function() {
      cat(.montage_surface_document_html(
        surface, is_html = isTRUE(is_html)
      ), sep = "")
      invisible(NULL)
    },
    emit_host = function(analysis_id) {
      host <- .montage_surface_host_tag(
        surface,
        analysis_id = as.character(analysis_id),
        is_html = isTRUE(is_html)
      )
      if (is.null(host)) return(invisible(NULL))
      rendered <- knitr::knit_print(host)
      metadata <- attr(rendered, "knit_meta", exact = TRUE)
      if (length(metadata)) knitr::knit_meta_add(metadata)
      cat(as.character(rendered), sep = "")
      invisible(NULL)
    }
  )
}

.montage_surface_host_tag <- function(surface, analysis_id, is_html = TRUE) {
  if (is.null(surface) || !isTRUE(is_html)) return(NULL)
  ids <- vapply(surface$scenes, `[[`, character(1), "analysis_id")
  if (!analysis_id %in% ids) {
    stop("Interactive surface report has no analysis_id '", analysis_id,
         "'.", call. = FALSE)
  }
  group <- surface$scenes[[match(analysis_id, ids)]]
  map_to_layer <- as.list(group$map_to_layer)
  layer_to_map <- as.list(group$layer_to_map)
  labels <- lapply(names(map_to_layer), function(map_id) {
    layer <- group$manifest$layers[[map_to_layer[[map_id]]]]
    as.character(layer$metadata$selector_label %||%
                   layer$legend$title %||% layer$label %||% map_id)
  })
  names(labels) <- names(map_to_layer)
  map_json <- .montage_json_script_escape(as.character(jsonlite::toJSON(
    map_to_layer, auto_unbox = TRUE, null = "null"
  )))
  reverse_json <- .montage_json_script_escape(as.character(jsonlite::toJSON(
    layer_to_map, auto_unbox = TRUE, null = "null"
  )))
  label_json <- .montage_json_script_escape(as.character(jsonlite::toJSON(
    labels, auto_unbox = TRUE, null = "null"
  )))
  control_json <- .montage_json_script_escape(as.character(jsonlite::toJSON(
    unname(surface$controls), auto_unbox = FALSE, null = "null"
  )))
  primary_label <- labels[[group$primary_map_id]] %||% group$primary_map_id
  safe_id <- paste0(
    "nm-surface-",
    substr(.montage_md5_text(as.character(analysis_id)), 1L, 12L)
  )
  bilateral_json <- .montage_json_script_escape(as.character(jsonlite::toJSON(
    group$bilateral_group, auto_unbox = TRUE, null = "null"
  )))
  summary <- surface$summary
  payload_size <- if (identical(summary$packaging, "embed")) {
    summary$embedded_bytes
  } else {
    summary$compressed_bytes
  }
  payload_size_text <- if (payload_size < 0.01 * 1024^2) {
    "<0.01"
  } else {
    format(round(payload_size / 1024^2, 2), nsmall = 2, trim = TRUE)
  }
  disclosure <- paste0(
    "This report includes recoverable per-vertex surface data (",
    summary$asset_count, " unique typed-array assets; ",
    payload_size_text, " MiB ",
    if (identical(summary$packaging, "embed")) "embedded" else "bundled",
    "). Geometry is content-addressed once and shared across analysis views."
  )

  tag <- htmltools::tags$section(
    class = "nm-surface-interactive",
    `data-nm-surface-host` = "",
    `data-nm-surface-analysis` = analysis_id,
    `data-nm-surface-map` = group$primary_map_id,
    `data-nm-surface-map-to-layer` = map_json,
    `data-nm-surface-layer-to-map` = reverse_json,
    `data-nm-surface-labels` = label_json,
    `data-nm-surface-controls-enabled` = control_json,
    `data-nm-surface-manifest` = group$manifest_id,
    `data-nm-surface-compression` = surface$summary$compression,
    `data-nm-surface-preset` = group$preset,
    `data-nm-surface-bilateral` = bilateral_json,
    `data-nm-surface-modified` = "false",
    htmltools::tags$details(
      class = "nm-surface-disclosure",
      htmltools::tags$summary("Explore surface interactively"),
      htmltools::tags$p(
        class = "nm-surface-target",
        "Interactive map: ",
        htmltools::tags$strong(
          `data-nm-surface-target` = "",
          primary_label
        )
      ),
      htmltools::tags$p(
        class = "nm-surface-status",
        `data-nm-surface-status` = "",
        role = "status",
        "Open this view to load the interactive surface."
      ),
      .montage_surface_control_tags(surface$controls),
      htmltools::tags$div(
        id = safe_id,
        class = "surfwidget nm-surface-widget",
        role = "group",
        `aria-label` = group$alt_text,
        style = paste0("height:", htmltools::validateCssUnit(group$height)),
        htmltools::tags$div(
          class = "nm-surface-author-fallback",
          `data-nm-surface-author-fallback` = "",
          hidden = "",
          group$fallback
        )
      ),
      htmltools::tags$p(
        class = "nm-surface-note",
        "Browser map, camera, threshold, palette, and opacity changes are ",
        "exploratory; the static montage above remains authoritative."
      ),
      htmltools::tags$p(
        class = "nm-surface-disclosure",
        htmltools::tags$strong("Data disclosure. "),
        disclosure
      )
    ),
    htmltools::tags$noscript(
      htmltools::tags$p(
        "JavaScript is disabled; use the static surface montage above."
      )
    )
  )
  htmltools::attachDependencies(
    tag, .montage_surface_runtime_dependency(surface$runtime)
  )
}

.montage_surface_control_tags <- function(controls) {
  if (!length(controls)) return(NULL)
  fields <- list()
  if ("palette" %in% controls) {
    fields <- c(fields, list(htmltools::tags$label(
      class = "nm-surface-control-field",
      "Palette",
      htmltools::tags$select(
        `data-nm-surface-palette` = "",
        `aria-label` = "Surface colormap"
      )
    )))
  }
  if ("range" %in% controls) {
    fields <- c(fields, list(htmltools::tags$fieldset(
      class = "nm-surface-control-pair",
      htmltools::tags$legend("Display range"),
      htmltools::tags$label(
        "Low",
        htmltools::tags$input(
          type = "number", step = "any",
          `data-nm-surface-range-low` = "",
          `aria-label` = "Surface range low"
        )
      ),
      htmltools::tags$label(
        "High",
        htmltools::tags$input(
          type = "number", step = "any",
          `data-nm-surface-range-high` = "",
          `aria-label` = "Surface range high"
        )
      )
    )))
  }
  if ("threshold" %in% controls) {
    fields <- c(fields, list(htmltools::tags$fieldset(
      class = "nm-surface-control-pair",
      htmltools::tags$legend("Threshold mask"),
      htmltools::tags$label(
        "Low",
        htmltools::tags$input(
          type = "number", step = "any",
          `data-nm-surface-threshold-low` = "",
          `aria-label` = "Surface threshold low"
        )
      ),
      htmltools::tags$label(
        "High",
        htmltools::tags$input(
          type = "number", step = "any",
          `data-nm-surface-threshold-high` = "",
          `aria-label` = "Surface threshold high"
        )
      )
    )))
  }
  if ("opacity" %in% controls) {
    fields <- c(fields, list(htmltools::tags$label(
      class = "nm-surface-control-field nm-surface-opacity-field",
      "Opacity",
      htmltools::tags$input(
        type = "range", min = "0", max = "1", step = "0.05",
        `data-nm-surface-opacity` = "",
        `aria-label` = "Surface opacity"
      ),
      htmltools::tags$output(`data-nm-surface-opacity-value` = "")
    )))
  }
  htmltools::tags$section(
    class = "nm-surface-display-controls",
    `data-nm-surface-display-controls` = "",
    `aria-label` = "Surface display controls",
    hidden = "",
    fields,
    htmltools::tags$button(
      type = "button",
      `data-nm-surface-reset-display` = "",
      "Reset map display"
    )
  )
}

.montage_surface_document_html <- function(surface, is_html = TRUE) {
  if (is.null(surface) || !isTRUE(is_html)) return("")
  if (!inherits(surface, "montage_surface_report")) {
    stop("Interactive surface report data are malformed.", call. = FALSE)
  }
  manifest_html <- vapply(surface$scenes, function(group) {
    paste0(
      '<script type="application/json" id="',
      .montage_html_escape(group$manifest_id), '">',
      .montage_surface_manifest_json(group$manifest),
      "</script>\n"
    )
  }, character(1))
  payload_html <- vapply(surface$payloads, function(payload) {
    paste0(
      '<script type="application/octet-stream" id="',
      .montage_html_escape(payload$id), '" data-compression="',
      .montage_html_escape(payload$compression), '">',
      payload$base64, "</script>\n"
    )
  }, character(1))
  display_runtime <- .montage_read_inline_asset(
    .montage_surface_display_path()
  )
  if (grepl("</script", tolower(display_runtime), fixed = TRUE)) {
    stop("Bundled surface display helper contains an unsafe script sequence.",
         call. = FALSE)
  }
  paste0(
    paste(manifest_html, collapse = ""),
    paste(payload_html, collapse = ""),
    "<script data-nm-surface-display>\n",
    display_runtime,
    "\n</script>\n",
    "<style data-nm-surface-bridge>\n",
    ".nm-surface-interactive{border:1px solid #d7dee8;border-radius:7px;margin:1rem 0 2rem;background:#f8fafc;}\n",
    ".nm-surface-disclosure>summary{cursor:pointer;color:#284858;font-weight:700;padding:.8rem 1rem;}\n",
    ".nm-surface-disclosure[open]>summary{border-bottom:1px solid #d7dee8;}\n",
    ".nm-surface-target,.nm-surface-status,.nm-surface-note{margin:.7rem 1rem;color:#425466;}\n",
    ".nm-surface-status{font-size:.92rem;}\n",
    ".nm-surface-display-controls{align-items:end;background:#eef4f6;border-block:1px solid #d7dee8;display:grid;gap:.65rem;grid-template-columns:repeat(auto-fit,minmax(145px,1fr));padding:.75rem 1rem;}\n",
    ".nm-surface-display-controls[hidden]{display:none;}\n",
    ".nm-surface-control-field{display:grid;font-size:.78rem;font-weight:700;gap:.2rem;}\n",
    ".nm-surface-control-field select,.nm-surface-control-pair input{background:white;border:1px solid #aebbc7;border-radius:4px;font:inherit;padding:.3rem .4rem;width:100%;}\n",
    ".nm-surface-control-pair{border:0;display:grid;gap:.2rem;grid-template-columns:1fr 1fr;margin:0;padding:0;}\n",
    ".nm-surface-control-pair legend{font-size:.78rem;font-weight:700;grid-column:1/-1;padding:0;}\n",
    ".nm-surface-control-pair label{font-size:.72rem;font-weight:600;}\n",
    ".nm-surface-opacity-field{grid-template-columns:1fr auto;}\n",
    ".nm-surface-opacity-field>input{grid-column:1/2;width:100%;}\n",
    ".nm-surface-opacity-field>output{font-variant-numeric:tabular-nums;grid-column:2/3;grid-row:2;}\n",
    ".nm-surface-display-controls button{background:white;border:1px solid #8294a5;border-radius:4px;cursor:pointer;font:inherit;font-weight:650;padding:.42rem .6rem;}\n",
    ".nm-surface-display-controls :focus-visible{outline:3px solid #86b8c0;outline-offset:2px;}\n",
    ".nm-surface-widget{background:#fbfbf8;min-height:260px;max-width:100%;position:relative;}\n",
    ".nm-surface-author-fallback{background:#fff7ed;border:1px solid #fed7aa;color:#7c2d12;margin:1rem;padding:1rem;}\n",
    ".nm-surface-note{font-size:.85rem;}\n",
    ".nm-surface-disclosure{font-size:.78rem;margin:.6rem 1rem 1rem;color:#596a78;}\n",
    ".nm-view-router{align-items:center;border:1px solid #d7dee8;border-radius:7px;display:flex;flex-wrap:wrap;gap:.45rem;margin:1rem 0;padding:.65rem .85rem;}\n",
    ".nm-view-router legend{color:#425466;font-size:.78rem;font-weight:700;padding:0 .25rem;}\n",
    ".nm-view-router label{align-items:center;background:#edf2f5;border:1px solid #c9d3dc;border-radius:999px;cursor:pointer;display:flex;font-weight:650;gap:.3rem;padding:.32rem .7rem;}\n",
    ".nm-view-router label:has(input:checked){background:#2f6f73;border-color:#2f6f73;color:white;}\n",
    ".nm-view-router input:focus-visible{outline:3px solid #86b8c0;outline-offset:3px;}\n",
    "@media(max-width:560px){.nm-surface-display-controls{grid-template-columns:1fr 1fr}.nm-surface-control-field{grid-column:1/-1;}}\n",
    "@media print{.nm-surface-interactive,.nm-view-router{display:none!important;}}\n",
    "</style>\n",
    "<script data-nm-surface-bridge>\n",
    "(function(){'use strict';\n",
    "var display=window.NeuroMosaicSurfaceDisplay;\n",
    "function parse(host,name){try{return JSON.parse(host.getAttribute(name)||'{}');}catch(error){return {};}}\n",
    "function reportGroup(analysisId){var groups=document.querySelectorAll('[data-nm-map-group]');for(var i=0;i<groups.length;i+=1){if(groups[i].getAttribute('data-nm-map-group')===analysisId)return groups[i];}return null;}\n",
    "function setStatus(host,text){var node=host.querySelector('[data-nm-surface-status]');if(node)node.textContent=text;}\n",
    "function setTarget(host,mapId){host.setAttribute('data-nm-surface-map',mapId);var labels=parse(host,'data-nm-surface-labels');var node=host.querySelector('[data-nm-surface-target]');if(node)node.textContent=labels[mapId]||mapId;}\n",
    "function requestReportMap(host,mapId){var analysisId=host.getAttribute('data-nm-surface-analysis');var group=reportGroup(analysisId);if(group)group.dispatchEvent(new CustomEvent('nm-map-request',{detail:{analysisId:analysisId,mapId:mapId},bubbles:true}));}\n",
    "function selectMap(host,mapId){var mapping=parse(host,'data-nm-surface-map-to-layer');var layerId=mapping[mapId];if(!layerId)return;setTarget(host,mapId);var handle=host.__nmSurfaceHandle;if(handle){try{handle.selectLayer(layerId);}catch(error){setStatus(host,'Interactive surface could not select this map: '+error.message);}}}\n",
    "function base64Bytes(text){var binary=window.atob(text.replace(/\\s/g,''));var bytes=new Uint8Array(binary.length);for(var i=0;i<binary.length;i+=1)bytes[i]=binary.charCodeAt(i);return bytes;}\n",
    "function decompress(bytes,compression){if(compression==='none')return Promise.resolve(bytes);if(compression!=='gzip')return Promise.reject(new Error('Unsupported surface asset compression: '+compression));if(typeof window.DecompressionStream!=='function')return Promise.reject(new Error('This browser cannot decompress embedded surface data.'));var stream=new Blob([bytes]).stream().pipeThrough(new DecompressionStream('gzip'));return new Response(stream).arrayBuffer().then(function(buffer){return new Uint8Array(buffer);});}\n",
    "function surfaceFetch(input,init){var url=typeof input==='string'?input:input.url;var prefix='nm-surface-asset:';if(url.indexOf(prefix)===0){var payloadId=decodeURIComponent(url.slice(prefix.length));var node=document.getElementById(payloadId);if(!node)return Promise.resolve(new Response('',{status:404,statusText:'Missing embedded surface asset'}));var compression=node.getAttribute('data-compression')||'none';return decompress(base64Bytes(node.textContent||''),compression).then(function(bytes){return new Response(bytes,{status:200,headers:{'content-type':'application/octet-stream'}});});}return window.fetch(input,init).then(function(response){if(!response.ok||!/\\.gz(?:$|[?#])/.test(url))return response;return response.arrayBuffer().then(function(buffer){return decompress(new Uint8Array(buffer),'gzip');}).then(function(bytes){return new Response(bytes,{status:200,headers:{'content-type':'application/octet-stream'}});});});}\n",
    "function showFallback(host,error){var fallback=host.querySelector('[data-nm-surface-author-fallback]');var widget=host.querySelector('.nm-surface-widget');if(widget)widget.setAttribute('data-nm-surface-failed','true');if(fallback){fallback.hidden=false;if(error)fallback.textContent=fallback.textContent+' '+error.message;}setStatus(host,'Interactive surface unavailable; use the static montage above.'+(error?' '+error.message:''));}\n",
    "function installRouter(host){var analysisId=host.getAttribute('data-nm-surface-analysis');if(document.querySelector('[data-nm-view-router=\"'+CSS.escape(analysisId)+'\"]'))return;var volume=document.querySelector('[data-nm-volume-analysis=\"'+CSS.escape(analysisId)+'\"]');var router=document.createElement('fieldset');router.className='nm-view-router';router.setAttribute('data-nm-view-router',analysisId);var legend=document.createElement('legend');legend.textContent='Analysis view';router.appendChild(legend);var name='nm-view-'+analysisId.replace(/[^A-Za-z0-9_-]/g,'-');function add(value,label){var wrapper=document.createElement('label');var input=document.createElement('input');input.type='radio';input.name=name;input.value=value;input.checked=value==='static';wrapper.append(input,document.createTextNode(label));router.appendChild(wrapper);return input;}var choices=[add('static','Static')];if(volume)choices.push(add('slices','Slices'));choices.push(add('surface','Surface'));function choose(view){host.hidden=view!=='surface';if(volume)volume.hidden=view!=='slices';if(view==='surface'){var details=host.querySelector('details');if(details)details.open=true;mount(host).catch(function(){});}if(view==='slices'&&volume){var toggle=volume.querySelector('[data-nm-volume-toggle]');if(toggle&&toggle.getAttribute('aria-expanded')!=='true')toggle.click();}}choices.forEach(function(choice){choice.addEventListener('change',function(){if(choice.checked)choose(choice.value);});});var anchor=volume&&volume.compareDocumentPosition(host)&Node.DOCUMENT_POSITION_FOLLOWING?volume:host;anchor.parentNode.insertBefore(router,anchor);choose('static');}\n",
    "function layerFor(snapshot,layerId){if(!snapshot)return null;for(var i=0;i<snapshot.surfaces.length;i+=1){var layers=snapshot.surfaces[i].layers;for(var j=0;j<layers.length;j+=1){if(layers[j].id===layerId)return layers[j];}}return null;}\n",
    "function addresses(snapshot,layerId){var out=[];(snapshot.surfaces||[]).forEach(function(surface){if(surface.layers.some(function(layer){return layer.id===layerId;}))out.push({surfaceId:surface.id,layerId:layerId});});return out;}\n",
    "function pair(value){return display.pair(value);}\n",
    "function currentLayerId(host){return display.layerIdForMap(parse(host,'data-nm-surface-map-to-layer'),host.getAttribute('data-nm-surface-map'));}\n",
    "function defaultFor(host,layer){return host.__nmSurfaceDisplayStore.capture(layer);}\n",
    "function setNumber(host,selector,value){var input=host.querySelector(selector);if(input&&Number.isFinite(value))input.value=String(value);}\n",
    "function renderControls(host,snapshot,layerId){var panel=host.querySelector('[data-nm-surface-display-controls]');if(!panel)return;var layer=layerFor(snapshot,layerId);if(!layer)return;panel.hidden=false;var defaults=defaultFor(host,layer);var scalar=layer.scalarMapping;var palette=host.querySelector('[data-nm-surface-palette]');if(palette&&scalar){var options=(scalar.availableColorMaps||[]).filter(function(option){return option.availability&&option.availability.enabled;});var signature=options.map(function(option){return option.id+'\\u0000'+option.label;}).join('\\u0001');if(palette.getAttribute('data-options')!==signature){palette.setAttribute('data-options',signature);palette.replaceChildren.apply(palette,options.map(function(option){var node=document.createElement('option');node.value=option.id;node.textContent=option.label;return node;}));}palette.value=scalar.colorMap.id;}if(scalar){setNumber(host,'[data-nm-surface-range-low]',scalar.displayRange.value[0]);setNumber(host,'[data-nm-surface-range-high]',scalar.displayRange.value[1]);setNumber(host,'[data-nm-surface-threshold-low]',scalar.maskInterval.value[0]);setNumber(host,'[data-nm-surface-threshold-high]',scalar.maskInterval.value[1]);}var opacity=host.querySelector('[data-nm-surface-opacity]');if(opacity)opacity.value=String(layer.opacity);var opacityValue=host.querySelector('[data-nm-surface-opacity-value]');if(opacityValue)opacityValue.value=Number(layer.opacity).toFixed(2);var modified=display.isModified(layer,defaults);host.setAttribute('data-nm-surface-modified',modified?'true':'false');setStatus(host,modified?'Interactive surface display modified; changes are exploratory only.':'Interactive surface ready.');}\n",
    "function reportResult(host,result){if(result&&result.ok)return true;setStatus(host,'Interactive surface display change was rejected: '+((result&&result.message)||'unknown error'));return false;}\n",
    "function applyScalar(host,update){var handle=host.__nmSurfaceHandle;var target=handle&&handle.controlTarget;if(!target)return false;var snapshot=target.getSnapshot();var layerId=currentLayerId(host);return addresses(snapshot,layerId).every(function(address){return reportResult(host,target.updateScalarMapping(address,update));});}\n",
    "function applyOpacity(host,value){var handle=host.__nmSurfaceHandle;var target=handle&&handle.controlTarget;if(!target)return false;var snapshot=target.getSnapshot();var layerId=currentLayerId(host);return addresses(snapshot,layerId).every(function(address){return reportResult(host,target.setLayerOpacity(address,value));});}\n",
    "function numericPair(host,lowSelector,highSelector){var value=display.orderedFinitePair(host.querySelector(lowSelector).value,host.querySelector(highSelector).value);if(!value)setStatus(host,'Surface display bounds must be finite and ordered.');return value;}\n",
    "function bindDisplayControls(host){var palette=host.querySelector('[data-nm-surface-palette]');if(palette)palette.addEventListener('change',function(){applyScalar(host,{colorMapId:palette.value});});var rangeInputs=host.querySelectorAll('[data-nm-surface-range-low],[data-nm-surface-range-high]');rangeInputs.forEach(function(input){input.addEventListener('change',function(){var value=numericPair(host,'[data-nm-surface-range-low]','[data-nm-surface-range-high]');if(value)applyScalar(host,{displayRange:value});});});var thresholdInputs=host.querySelectorAll('[data-nm-surface-threshold-low],[data-nm-surface-threshold-high]');thresholdInputs.forEach(function(input){input.addEventListener('change',function(){var value=numericPair(host,'[data-nm-surface-threshold-low]','[data-nm-surface-threshold-high]');if(value)applyScalar(host,{maskInterval:value});});});var opacity=host.querySelector('[data-nm-surface-opacity]');if(opacity)opacity.addEventListener('input',function(){var value=Number(opacity.value);if(Number.isFinite(value))applyOpacity(host,value);});var reset=host.querySelector('[data-nm-surface-reset-display]');if(reset)reset.addEventListener('click',function(){var handle=host.__nmSurfaceHandle;var target=handle&&handle.controlTarget;if(!target)return;var layerId=currentLayerId(host);var defaults=host.__nmSurfaceDisplayStore.get(layerId);if(!defaults)return;applyScalar(host,{colorMapId:defaults.colorMapId,displayRange:defaults.displayRange,maskInterval:defaults.maskInterval});applyOpacity(host,defaults.opacity);});}\n",
    "function connectHandle(host,handle){host.__nmSurfaceHandle=handle;var widget=host.querySelector('.surfwidget');if(widget)widget.__surfviewHandle=handle;return Promise.resolve(handle.ready).then(function(){selectMap(host,host.getAttribute('data-nm-surface-map'));var target=handle.controlTarget;if(!target||typeof target.subscribe!=='function'){setStatus(host,'Interactive surface ready.');return;}var reverse=parse(host,'data-nm-surface-layer-to-map');host.__nmSurfaceSubscription=target.subscribe(function(snapshot){var exclusive=snapshot&&snapshot.capabilities&&snapshot.capabilities.exclusiveMap;var layerId=exclusive&&exclusive.displayedLayerId;var mapId=layerId&&reverse[layerId];if(mapId&&mapId!==host.getAttribute('data-nm-surface-map')){setTarget(host,mapId);requestReportMap(host,mapId);}if(layerId)renderControls(host,snapshot,layerId);});renderControls(host,target.getSnapshot(),currentLayerId(host));setStatus(host,'Interactive surface ready.');});}\n",
    "function mount(host){if(host.__nmSurfaceMount)return host.__nmSurfaceMount;setStatus(host,'Loading interactive surface data...');host.__nmSurfaceMount=Promise.resolve().then(function(){if(!window.surfview||typeof window.surfview.mountSurfView!=='function')throw new Error('The surfview runtime did not load.');var manifestNode=document.getElementById(host.getAttribute('data-nm-surface-manifest'));if(!manifestNode)throw new Error('The surface scene manifest is missing.');var manifest=JSON.parse(manifestNode.textContent||'{}');manifest.selectedLayer=currentLayerId(host)||manifest.selectedLayer;var widget=host.querySelector('.surfwidget');var options={lazy:false,preset:host.getAttribute('data-nm-surface-preset')||'paper-light',controls:true,mode:'report',baseUrl:document.baseURI,fetcher:surfaceFetch,onError:function(error){showFallback(host,error);}};var bilateral=parse(host,'data-nm-surface-bilateral');if(bilateral&&bilateral.id)options.bilateralGroup=bilateral;var handle=window.surfview.mountSurfView(widget,manifest,options);return connectHandle(host,handle);}).catch(function(error){showFallback(host,error);throw error;});return host.__nmSurfaceMount;}\n",
    "function bind(host){if(host.__nmSurfaceBound)return;host.__nmSurfaceBound=true;if(!display){showFallback(host,new Error('The surface display helper did not load.'));return;}host.__nmSurfaceDisplayStore=display.createDefaultStore();bindDisplayControls(host);var details=host.querySelector('details');if(details)details.addEventListener('toggle',function(){if(!details.open)return;mount(host).then(function(){if(host.__nmSurfaceHandle)window.requestAnimationFrame(function(){host.__nmSurfaceHandle.resize();});}).catch(function(){});});installRouter(host);if(details&&details.open&&!host.hidden)mount(host).catch(function(){});}\n",
    "function initialize(){var hosts=document.querySelectorAll('[data-nm-surface-host]');hosts.forEach(bind);document.addEventListener('nm-map-change',function(event){var detail=event.detail||{};hosts.forEach(function(host){if(host.getAttribute('data-nm-surface-analysis')===detail.analysisId)selectMap(host,detail.mapId);});});window.addEventListener('pagehide',function(){hosts.forEach(function(host){var sub=host.__nmSurfaceSubscription;if(sub&&typeof sub.unsubscribe==='function')sub.unsubscribe();var handle=host.__nmSurfaceHandle;if(handle&&typeof handle.dispose==='function')handle.dispose();});},{once:true});}\n",
    "if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',initialize,{once:true});else initialize();\n",
    "}());\n",
    "</script>\n"
  )
}
