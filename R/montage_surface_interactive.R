# First-class interactive surface-report contracts.

.montage_surface_projection_modes <- c("auto", "none")
.montage_surface_control_names <- c(
  "threshold", "range", "palette", "opacity"
)
.montage_surface_asset_modes <- c("bundle", "embed")
.montage_surface_compressions <- c("gzip", "none")
.montage_surface_adapter_version <- "2.0.0"
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
#' Hovering (or tapping) the surface reports the map value, the hemisphere,
#' and, when the surface atlas labels every displayed vertex, the atlas
#' parcel and its network (for example "Visual network", `RH_Vis_7`). The
#' parcel lookup is embedded once per report as compressed per-vertex ids.
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
#' @param preset A neurosurf/surfview visual preset. The default `"report"`
#'   pairs a two-tone sulcal underlay with a camera-attached key light and
#'   heat colours (red-yellow, blue-cyan) for thresholded statistics.
#' @param layout How the two hemispheres are posed. `"anatomical"` keeps one
#'   coherent brain in RAS position that rotates as a whole; `"split"` rotates
#'   each hemisphere into the chosen view and places them side by side.
#' @param view Initial whole-brain view for the anatomical layout. The default
#'   `"oblique"` is the Overview pose set by `oblique`.
#' @param oblique Overview camera angle for the anatomical layout: `"auto"`
#'   chooses, per displayed map, the oblique angle that faces the most
#'   suprathreshold cortex; a numeric `c(azimuth, elevation)` in degrees fixes
#'   it (azimuth 0 = anterior, 90 = right, 180 = posterior, -90 = left).
#' @param anatomy Computed underlay used when no `curvature` is supplied:
#'   `"sulcal_depth"` (a white-to-inflated sulcal-depth proxy with broad
#'   gyral/sulcal contrast) or `"curvature"` (white-surface mean curvature, as
#'   in the static montage).
#' @param height Canvas height as a positive number of pixels or a CSS size
#'   string. `NULL` (default) uses a responsive 16:10 stage (square on phones).
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
                            preset = "report",
                            layout = c("anatomical", "split"),
                            view = c("oblique", "left", "right", "dorsal",
                                     "ventral", "anterior", "posterior",
                                     "left-medial", "right-medial"),
                            anatomy = c("sulcal_depth", "curvature"),
                            oblique = "auto",
                            height = NULL,
                            fallback = NULL,
                            alt_text = NULL,
                            anatomy_style = c("auto", "continuous", "binary", "none"),
                            anatomy_midpoint = NULL,
                            anatomy_invert = NULL) {
  anatomy_style <- match.arg(anatomy_style)
  layout <- match.arg(layout)
  view <- match.arg(view)
  anatomy <- match.arg(anatomy)
  if (!(identical(oblique, "auto") ||
        (is.numeric(oblique) && length(oblique) == 2L &&
         all(is.finite(oblique)) && abs(oblique[[2L]]) <= 90))) {
    stop("'oblique' must be \"auto\" or c(azimuth, elevation) in degrees ",
         "with |elevation| <= 90.", call. = FALSE)
  }
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
  if (!(is.null(height) ||
        (is.numeric(height) && length(height) == 1L && is.finite(height) &&
         height > 0) ||
        (is.character(height) && length(height) == 1L && !is.na(height) &&
         nzchar(trimws(height))))) {
    stop("'height' must be NULL, a positive number, or one CSS size string.",
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
      layout = layout,
      view = view,
      anatomy = anatomy,
      oblique = if (is.numeric(oblique)) as.numeric(oblique) else "auto",
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
  parcels <- .montage_surface_parcel_lookup(surfatlas, geometry)

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
        opacity = surface$opacity %||%
          if (surface$preset %in% c("freesurfer", "report")) 1 else NULL,
        heat = if (identical(surface$preset, "freesurfer")) {
          TRUE
        } else if (identical(surface$preset, "report")) {
          "thresholded"
        } else {
          FALSE
        }
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
      layout = surface$layout %||% "split",
      view = surface$view %||% "oblique",
      oblique = surface$oblique %||% "auto",
      height = surface$height,
      projection = surface$projection,
      packaging = list(
        mode = surface$assets,
        compression = surface$compression,
        max_embed_mb = surface$max_embed_mb
      ),
      controls = surface$controls,
      parcels = parcels
    ),
    class = c("montage_surface_report", "list")
  )
}

# Per-vertex atlas parcel ids and a label table, so the browser readout can
# name the region under the pointer. NULL when the atlas does not describe the
# displayed vertices one-to-one.
.montage_surface_parcel_lookup <- function(surfatlas, geometry) {
  hemis <- c(left = "lh_atlas", right = "rh_atlas")
  ids <- list()
  for (side in intersect(names(geometry), names(hemis))) {
    atlas_hemi <- surfatlas[[hemis[[side]]]]
    values <- tryCatch(as.integer(round(atlas_hemi@data)), error = function(e) NULL)
    vertex_count <- ncol(geometry[[side]]@mesh$vb)
    if (is.null(values) || length(values) != vertex_count || anyNA(values) ||
        any(values < 0L) || any(values > 65535L)) {
      return(NULL)
    }
    ids[[side]] <- values
  }
  table_ids <- suppressWarnings(as.integer(surfatlas$ids))
  if (!length(ids) || !length(table_ids) || anyNA(table_ids)) return(NULL)
  field <- function(name) {
    value <- surfatlas[[name]]
    if (length(value) == length(table_ids)) as.character(value) else rep(NA_character_, length(table_ids))
  }
  list(
    atlas = as.character(surfatlas$name %||% "atlas")[[1L]],
    ids = ids,
    table = data.frame(
      id = table_ids,
      label = field("labels"),
      full = field("orig_labels"),
      network = field("network"),
      stringsAsFactors = FALSE
    )
  )
}

# Encode the parcel lookup as gzip-compressed little-endian uint16 payloads
# plus one JSON descriptor. Always embedded: it is small and atlas-level.
.package_montage_surface_parcels <- function(parcels) {
  if (is.null(parcels)) return(NULL)
  payloads <- list()
  refs <- list()
  for (side in names(parcels$ids)) {
    raw_path <- tempfile("nm-parcels-", fileext = ".bin")
    gz_path <- tempfile("nm-parcels-", fileext = ".bin.gz")
    on.exit(unlink(c(raw_path, gz_path)), add = TRUE)
    writeBin(as.integer(parcels$ids[[side]]), raw_path, size = 2L, endian = "little")
    .montage_gzip_file(raw_path, gz_path)
    bytes <- readBin(gz_path, what = "raw", n = file.info(gz_path)$size)
    id <- paste0("nm-surface-parcels-", side)
    payloads[[side]] <- list(id = id, compression = "gzip",
                             base64 = jsonlite::base64_enc(bytes))
    refs[[side]] <- id
  }
  table <- parcels$table
  entries <- lapply(seq_len(nrow(table)), function(i) {
    Filter(function(value) !is.na(value), list(
      label = table$label[[i]], full = table$full[[i]], network = table$network[[i]]
    ))
  })
  names(entries) <- as.character(table$id)
  list(
    payloads = payloads,
    descriptor = list(atlas = parcels$atlas, payloads = refs, parcels = entries)
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
  surface$parcel_assets <- .package_montage_surface_parcels(surface$parcels)
  surface$parcels <- NULL
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
    # The adapter is the display helper plus the bridge script and styles.
    adapter_sha256 = digest::digest(
      paste(
        .montage_read_inline_asset(.montage_surface_display_path()),
        .montage_surface_bridge_assets()$js,
        .montage_surface_bridge_assets()$css,
        sep = "\n"
      ),
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
  if (isTRUE(heat) ||
      (identical(heat, "thresholded") && identical(display_mode, "thresholded"))) {
    palette <- if (identical(row$effective_scale[[1L]], "diverging")) {
      "surface-heat"
    } else {
      "surface-heat-positive"
    }
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
      source = surface_args$anatomy_metric_source,
      type = surface$anatomy %||% "curvature"
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
    if (identical(summary$packaging, "embed")) "Includes " else "Loads ",
    payload_size_text, " MB of per-vertex data."
  )
  legends <- lapply(names(map_to_layer), function(map_id) {
    layer <- group$manifest$layers[[map_to_layer[[map_id]]]]
    list(
      title = as.character(layer$legend$title %||% layer$label %||% map_id),
      units = as.character(layer$legend$units %||% layer$units %||% "")
    )
  })
  names(legends) <- names(map_to_layer)
  legend_json <- .montage_json_script_escape(as.character(jsonlite::toJSON(
    legends, auto_unbox = TRUE, null = "null"
  )))
  map_ids <- names(map_to_layer)
  title <- if (length(map_ids) > 1L) {
    htmltools::tags$select(
      class = "nm-sv-map nm-sv-title",
      `data-nm-surface-map-select` = "",
      `aria-label` = "Map shown on the surface",
      lapply(map_ids, function(map_id) {
        htmltools::tags$option(
          value = map_id,
          selected = if (identical(map_id, group$primary_map_id)) NA else NULL,
          labels[[map_id]]
        )
      })
    )
  } else {
    htmltools::tags$p(
      class = "nm-sv-title",
      `data-nm-surface-target` = "",
      primary_label
    )
  }

  tag <- htmltools::tags$section(
    class = "nm-surface-interactive",
    `data-nm-surface-host` = "",
    `data-nm-surface-analysis` = analysis_id,
    `data-nm-surface-map` = group$primary_map_id,
    `data-nm-surface-map-to-layer` = map_json,
    `data-nm-surface-layer-to-map` = reverse_json,
    `data-nm-surface-labels` = label_json,
    `data-nm-surface-legends` = legend_json,
    `data-nm-surface-controls-enabled` = control_json,
    `data-nm-surface-manifest` = group$manifest_id,
    `data-nm-surface-compression` = surface$summary$compression,
    `data-nm-surface-preset` = group$preset,
    `data-nm-surface-layout` = surface$layout %||% "split",
    `data-nm-surface-view` = surface$view %||% "oblique",
    `data-nm-surface-oblique` = if (is.numeric(surface$oblique)) {
      paste(format(surface$oblique, trim = TRUE), collapse = ",")
    } else {
      "auto"
    },
    `data-nm-surface-ground` = "#FBFBF8",
    `data-nm-surface-bilateral` = bilateral_json,
    `data-nm-surface-modified` = "false",
    `aria-label` = paste0("Interactive cortical surface: ", primary_label),
    htmltools::tags$div(
      class = "nm-sv-frame",
    htmltools::tags$div(
      class = "nm-sv-stage",
      style = if (!is.null(group$height)) {
        paste0("aspect-ratio:auto;max-height:none;height:",
               htmltools::validateCssUnit(group$height))
      },
      tabindex = "0",
      role = "group",
      `aria-label` = paste0("3D surface viewer: ", primary_label),
      `aria-describedby` = paste0(safe_id, "-help"),
      htmltools::tags$div(
        id = safe_id,
        class = "surfwidget nm-surface-widget",
        role = "img",
        `aria-label` = group$alt_text,
        htmltools::tags$div(
          class = "nm-surface-author-fallback",
          `data-nm-surface-author-fallback` = "",
          hidden = "",
          group$fallback
        )
      ),
      htmltools::tags$div(
        class = "nm-sv-overlay nm-sv-head",
        title,
        htmltools::tags$p(
          class = "nm-sv-meta",
          htmltools::tags$span(`data-nm-surface-meta` = ""),
          htmltools::tags$span(
            class = "nm-sv-modified",
            `data-nm-surface-modified` = "",
            hidden = "",
            "Display changed"
          )
        )
      ),
      htmltools::tags$div(
        class = "nm-sv-overlay nm-sv-actions",
        if (length(surface$controls)) {
          htmltools::tags$button(
            type = "button", class = "nm-sv-icon",
            `data-nm-surface-display-toggle` = "",
            `aria-expanded` = "false",
            `aria-controls` = paste0(safe_id, "-display"),
            title = "Colour, range and threshold",
            `aria-label` = "Colour, range and threshold",
            .montage_surface_icon("sliders")
          )
        },
        htmltools::tags$button(
          type = "button", class = "nm-sv-icon",
          `data-nm-surface-reset-view` = "",
          title = "Reset view", `aria-label` = "Reset view",
          .montage_surface_icon("reset")
        ),
        htmltools::tags$button(
          type = "button", class = "nm-sv-icon",
          `data-nm-surface-export` = "",
          title = "Save PNG", `aria-label` = "Save view as PNG",
          .montage_surface_icon("download")
        )
      ),
      .montage_surface_control_tags(surface$controls, paste0(safe_id, "-display")),
      htmltools::tags$div(
        class = "nm-sv-overlay nm-sv-legend",
        `data-nm-surface-legend` = "",
        hidden = "",
        htmltools::tags$p(
          class = "nm-sv-legend-title",
          htmltools::tags$span(`data-nm-legend-title` = ""),
          htmltools::tags$span(`data-nm-legend-units` = "")
        ),
        htmltools::tags$canvas(`aria-hidden` = "true"),
        htmltools::tags$div(class = "nm-sv-ticks", `data-nm-legend-ticks` = ""),
        htmltools::tags$p(
          class = "nm-sv-legend-note", `data-nm-legend-note` = "", hidden = ""
        )
      ),
      htmltools::tags$div(
        class = "nm-sv-readout",
        `data-nm-surface-readout` = "",
        `aria-hidden` = "true",
        hidden = ""
      ),
      htmltools::tags$p(
        id = paste0(safe_id, "-help"),
        class = "nm-sv-visually-hidden",
        paste(
          "Drag to rotate, scroll to zoom, double-click to reset.",
          "With the viewer focused: keys 1 to 9 choose a view, 0 resets,",
          "arrow keys rotate, plus and minus zoom, S saves a PNG."
        )
      ),
      htmltools::tags$p(
        class = "nm-sv-readout-live nm-sv-visually-hidden",
        `data-nm-surface-live` = "",
        `aria-live` = "polite"
      ),
      htmltools::tags$button(
        type = "button",
        class = "nm-sv-touch-gate",
        `data-nm-surface-touch-gate` = "",
        hidden = "",
        "Tap to explore in 3D"
      ),
      htmltools::tags$button(
        type = "button",
        class = "nm-sv-touch-done",
        `aria-label` = "Done exploring; return to page scrolling",
        `data-nm-surface-touch-done` = "",
        hidden = "",
        "Done"
      ),
      htmltools::tags$div(
        class = "nm-sv-status",
        `data-nm-surface-status` = "",
        role = "status",
        "Choose 3D surface above to load this view."
      )
    ),
    htmltools::tags$div(
      class = "nm-sv-toolbar",
      htmltools::tags$div(
        class = "nm-sv-views",
        `data-nm-surface-views` = "",
        role = "toolbar",
        `aria-label` = "Camera view"
      ),
      htmltools::tags$p(
        class = "nm-sv-hint",
        `aria-hidden` = "true",
        "Drag to rotate \u00b7 scroll to zoom"
      )
    )
    ),
    htmltools::tags$p(
      class = "nm-sv-foot",
      "Exploratory view. The static montage above is the figure of record. ",
      disclosure
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

.montage_surface_icon <- function(name) {
  paths <- switch(name,
    sliders = paste(
      "M3 5h7M14 5h3M3 10h2M9 10h8M3 15h9M16 15h1",
      "M12 3.5v3M7 8.5v3M14 13.5v3"
    ),
    reset = "M4 10a6 6 0 1 0 1.8-4.3M4 3.5v3.2h3.2",
    download = "M10 3.5v9M6.5 9 10 12.5 13.5 9M4 16h12",
    stop("Unknown surface icon: ", name, call. = FALSE)
  )
  htmltools::HTML(paste0(
    '<svg viewBox="0 0 20 20" aria-hidden="true" focusable="false">',
    '<path d="', paths, '" fill="none" stroke="currentColor" ',
    'stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/></svg>'
  ))
}

.montage_surface_control_tags <- function(controls, id = NULL) {
  if (!length(controls)) return(NULL)
  fields <- list()
  if ("palette" %in% controls) {
    fields <- c(fields, list(htmltools::tags$label(
      class = "nm-sv-field",
      htmltools::tags$span("Colour map"),
      htmltools::tags$select(
        `data-nm-surface-palette` = "",
        `aria-label` = "Surface colour map"
      )
    )))
  }
  pair <- function(legend, low_name, high_name, low_label, high_label) {
    htmltools::tags$fieldset(
      class = "nm-sv-field",
      htmltools::tags$legend(legend),
      htmltools::tags$div(
        class = "nm-sv-pair",
        .montage_surface_number_input(low_name, low_label),
        htmltools::tags$i(`aria-hidden` = "true", "to"),
        .montage_surface_number_input(high_name, high_label)
      )
    )
  }
  if ("range" %in% controls) {
    fields <- c(fields, list(pair(
      "Colour range", "range-low", "range-high",
      "Colour range low", "Colour range high"
    )))
  }
  if ("threshold" %in% controls) {
    fields <- c(fields, list(htmltools::tags$fieldset(
      class = "nm-sv-field nm-sv-threshold",
      `data-nm-surface-threshold-mode` = "symmetric",
      htmltools::tags$legend("Threshold"),
      htmltools::tags$label(
        class = "nm-sv-sym",
        htmltools::tags$span(`data-nm-surface-threshold-symbol` = "", "|value| above"),
        .montage_surface_number_input("threshold-abs", "Show values whose magnitude exceeds")
      ),
      htmltools::tags$div(
        class = "nm-sv-pair nm-sv-asym",
        .montage_surface_number_input("threshold-low", "Hide values from"),
        htmltools::tags$i(`aria-hidden` = "true", "to"),
        .montage_surface_number_input("threshold-high", "Hide values to")
      ),
      htmltools::tags$button(
        type = "button",
        class = "nm-sv-text-button nm-sv-mode-toggle",
        `data-nm-surface-threshold-toggle` = "",
        "Set each side separately"
      )
    )))
  }
  if ("opacity" %in% controls) {
    fields <- c(fields, list(htmltools::tags$label(
      class = "nm-sv-field",
      htmltools::tags$span("Overlay opacity"),
      htmltools::tags$span(
        class = "nm-sv-opacity",
        htmltools::tags$input(
          type = "range", min = "0", max = "1", step = "0.05",
          `data-nm-surface-opacity` = "",
          `aria-label` = "Overlay opacity"
        ),
        htmltools::tags$output(`data-nm-surface-opacity-value` = "")
      )
    )))
  }
  htmltools::tags$section(
    id = id,
    class = "nm-sv-display",
    `data-nm-surface-display-controls` = "",
    `aria-label` = "Surface display settings",
    tabindex = "-1",
    hidden = "",
    htmltools::tags$header(
      class = "nm-sv-display-head",
      htmltools::tags$h3("Display"),
      htmltools::tags$button(
        type = "button",
        class = "nm-sv-close",
        `data-nm-surface-display-close` = "",
        `aria-label` = "Close display settings",
        htmltools::HTML("&times;")
      )
    ),
    fields,
    htmltools::tags$footer(
      class = "nm-sv-display-foot",
      htmltools::tags$button(
        type = "button",
        class = "nm-sv-text-button nm-sv-restore",
        `data-nm-surface-reset-display` = "",
        "Restore report settings"
      )
    )
  )
}

.montage_surface_number_input <- function(name, label) {
  attrs <- list(
    type = "number", step = "any", inputmode = "decimal",
    `aria-label` = label
  )
  attrs[[paste0("data-nm-surface-", name)]] <- ""
  do.call(htmltools::tags$input, attrs)
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
  parcel_html <- ""
  if (!is.null(surface$parcel_assets)) {
    parcel_html <- paste0(
      paste(vapply(surface$parcel_assets$payloads, function(payload) {
        paste0(
          '<script type="application/octet-stream" id="',
          .montage_html_escape(payload$id), '" data-compression="gzip">',
          payload$base64, "</script>\n"
        )
      }, character(1)), collapse = ""),
      '<script type="application/json" id="nm-surface-parcel-table">',
      .montage_json_script_escape(as.character(jsonlite::toJSON(
        surface$parcel_assets$descriptor, auto_unbox = TRUE, null = "null"
      ))),
      "</script>\n"
    )
  }
  display_runtime <- .montage_read_inline_asset(
    .montage_surface_display_path()
  )
  bridge <- .montage_surface_bridge_assets()
  if (grepl("</script", tolower(display_runtime), fixed = TRUE)) {
    stop("Bundled surface display helper contains an unsafe script sequence.",
         call. = FALSE)
  }
  paste0(
    paste(manifest_html, collapse = ""),
    paste(payload_html, collapse = ""),
    parcel_html,
    "<script data-nm-surface-display>\n",
    display_runtime,
    "\n</script>\n",
    "<style data-nm-surface-bridge>\n",
    bridge$css,
    "\n</style>\n",
    "<script data-nm-surface-bridge>\n",
    bridge$js,
    "\n</script>\n"
  )
}

.montage_surface_bridge_assets <- function() {
  read <- function(file) {
    path <- system.file("htmlwidgets", "lib", "neuromosaic-surface", file,
                        package = "neuromosaic")
    if (!nzchar(path)) {
      path <- file.path("inst", "htmlwidgets", "lib", "neuromosaic-surface", file)
    }
    text <- .montage_read_inline_asset(path)
    if (grepl("</(script|style)", tolower(text))) {
      stop("Bundled surface bridge asset ", file,
           " contains an unsafe closing tag.", call. = FALSE)
    }
    text
  }
  list(css = read("bridge.css"), js = read("bridge.js"))
}
