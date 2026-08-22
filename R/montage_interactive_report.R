# Materialization and HTML emission for optional interactive volume reports.

.montage_neuroimjs_version <- "0.3.0"
.montage_neuroimjs_adapter_version <- "1.0.0"
.montage_volume_runtime_dir <- file.path(
  "htmlwidgets", "lib", "neuromosaic-volume"
)

.prepare_montage_interactive_report <- function(report_data,
                                                 interactive,
                                                 background,
                                                 output_file) {
  if (is.null(interactive)) return(NULL)
  if (!inherits(interactive, "montage_interactive")) {
    stop("'interactive' must be NULL or created by montage_interactive().",
         call. = FALSE)
  }
  if (is.null(background)) {
    stop("'bg' is required for an interactive volume report.",
         call. = FALSE)
  }
  manifest <- report_data$manifest
  missing_metadata <- vapply(as.character(manifest$map_id), function(map_id) {
    !inherits(report_data$panels[[map_id]][["volume"]],
              "montage_volume_display_metadata")
  }, logical(1))
  if (any(missing_metadata)) {
    stop(
      "Interactive volume reports require their static volume fallback; ",
      "render_volume must be TRUE for every map.",
      call. = FALSE
    )
  }

  output_dir <- dirname(output_file)
  report_stem <- tools::file_path_sans_ext(basename(output_file))
  companion_name <- paste0(report_stem, "_files")
  temporary_assets <- identical(interactive$assets, "embed")
  asset_dir <- if (temporary_assets) {
    tempfile("neuromosaic-embedded-assets-")
  } else {
    file.path(output_dir, companion_name, "interactive", "volumes")
  }
  if (temporary_assets) on.exit(unlink(asset_dir, recursive = TRUE), add = TRUE)
  ref_prefix <- if (temporary_assets) "" else file.path(
    companion_name, "interactive", "volumes"
  )
  support_masks <- lapply(as.character(manifest$map_id), function(map_id) {
    report_data$panels[[map_id]][["volume"]]$support_mask
  })
  bundle <- .montage_materialize_volume_assets(
    manifest = manifest,
    background = background,
    output_dir = asset_dir,
    compression = interactive$compression,
    support_masks = support_masks,
    ref_prefix = ref_prefix
  )

  payloads <- list()
  assets <- bundle$assets
  if (temporary_assets) {
    payloads <- vector("list", length(assets))
    for (i in seq_along(assets)) {
      asset_id <- assets[[i]]$asset_id
      path <- unname(bundle$files[[asset_id]])
      bytes <- readBin(path, what = "raw", n = file.info(path)$size)
      payload_id <- paste0("nm-volume-asset-", sub("^[^-]+-", "", asset_id))
      assets[[i]]$location <- list(kind = "embedded", ref = payload_id)
      payloads[[i]] <- list(
        id = payload_id,
        asset_id = asset_id,
        base64 = jsonlite::base64_enc(bytes)
      )
    }
    embedded_bytes <- sum(vapply(
      payloads, function(payload) nchar(payload$base64, type = "bytes"),
      numeric(1)
    ))
    budget <- round(interactive$max_embed_mb * 1024^2)
    if (embedded_bytes > budget) {
      compressed_bytes <- sum(vapply(
        assets, `[[`, numeric(1), "compressed_bytes"
      ))
      uncompressed_bytes <- sum(vapply(
        assets, `[[`, numeric(1), "uncompressed_bytes"
      ))
      stop(
        "Interactive volume assets require ",
        format(round(uncompressed_bytes / 1024^2, 2), nsmall = 2),
        " MiB raw, ",
        format(round(compressed_bytes / 1024^2, 2), nsmall = 2),
        " MiB gzip, and ",
        format(round(embedded_bytes / 1024^2, 2), nsmall = 2),
        " MiB base64, exceeding max_embed_mb = ", interactive$max_embed_mb,
        ". Use assets = 'bundle' or raise the explicit budget.",
        call. = FALSE
      )
    }
  } else {
    embedded_bytes <- 0
  }

  scene <- .montage_volume_scene_from_report(
    report_data = report_data,
    interactive = interactive,
    bundle = bundle,
    assets = assets
  )
  json <- .montage_volume_scene_json(scene)
  scene_id <- paste0("nm-volume-scene-", .montage_md5_text(json))
  bundle_files <- if (temporary_assets) {
    character()
  } else {
    .montage_write_interactive_bundle(
      root = dirname(asset_dir), scene_json = json
    )
  }
  summary <- list(
    asset_count = length(assets),
    compressed_bytes = sum(vapply(
      assets, `[[`, numeric(1), "compressed_bytes"
    )),
    uncompressed_bytes = sum(vapply(
      assets, `[[`, numeric(1), "uncompressed_bytes"
    )),
    embedded_bytes = embedded_bytes,
    packaging = interactive$assets,
    compression = interactive$compression
  )
  structure(
    list(
      scene = scene,
      scene_json = json,
      scene_id = scene_id,
      payloads = payloads,
      bundle_files = bundle_files,
      summary = summary,
      engine_version = .montage_neuroimjs_version,
      adapter_version = .montage_neuroimjs_adapter_version,
      runtime_sha256 =
        "f9623745f32e70c8960edc27724e6dc2f0281b5bfe4ce7d74eb9b009b8357bef"
    ),
    class = "montage_interactive_report"
  )
}

.montage_volume_runtime_path <- function(file = NULL) {
  path <- if (is.null(file)) {
    system.file(.montage_volume_runtime_dir, package = "neuromosaic")
  } else {
    system.file(.montage_volume_runtime_dir, file, package = "neuromosaic")
  }
  if (!nzchar(path)) {
    stop("Bundled interactive volume runtime not found.", call. = FALSE)
  }
  path
}

.montage_write_interactive_bundle <- function(root, scene_json) {
  runtime_dir <- file.path(root, "runtime")
  dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(runtime_dir)) {
    stop("Could not create interactive runtime directory: ", runtime_dir,
         call. = FALSE)
  }
  runtime_files <- c(
    paste0("neuroimjs-", .montage_neuroimjs_version, ".umd.js"),
    "adapter.js", "adapter.css", "display.js", "geometry.js", "runtime.json",
    "LICENSE-neuroimjs"
  )
  sources <- vapply(runtime_files, .montage_volume_runtime_path, character(1))
  destinations <- file.path(runtime_dir, runtime_files)
  copied <- file.copy(sources, destinations, overwrite = TRUE)
  if (!all(copied)) {
    stop(
      "Failed to copy interactive runtime bundle file(s): ",
      paste(runtime_files[!copied], collapse = ", "),
      call. = FALSE
    )
  }
  scene_path <- file.path(root, "scene.json")
  writeLines(scene_json, scene_path, useBytes = TRUE)
  if (!file.exists(scene_path)) {
    stop("Failed to write interactive VolumeScene bundle.", call. = FALSE)
  }
  normalizePath(c(scene_path, destinations), mustWork = TRUE)
}

.montage_volume_scene_from_report <- function(report_data,
                                               interactive,
                                               bundle,
                                               assets = bundle$assets) {
  manifest <- report_data$manifest
  panels <- report_data$panels
  background_id <- bundle$background$asset$asset_id
  analyses <- lapply(report_data$groups, function(group) {
    map_ids <- as.character(group$map_ids)
    maps <- lapply(map_ids, function(map_id) {
      row <- manifest[match(map_id, manifest$map_id), , drop = FALSE]
      metadata <- panels[[map_id]][["volume"]]
      .montage_volume_scene_map(
        row = row,
        metadata = metadata,
        asset_id = bundle$maps[[map_id]]$asset$asset_id
      )
    })
    primary <- panels[[group$primary_map_id]][["volume"]]
    initial <- if (!is.null(interactive$world_coord)) {
      interactive$world_coord
    } else if (identical(interactive$initial_position, "center")) {
      .montage_geometry_center(bundle$background$asset$geometry)
    } else {
      primary$initial_world_coord
    }
    list(
      analysis_id = as.character(group$analysis_id),
      primary_map_id = as.character(group$primary_map_id),
      initial_world_coord = as.numeric(initial),
      zlevel_bookmarks = as.numeric(primary$zlevel_bookmarks %||% numeric()),
      maps = maps
    )
  })

  .new_montage_volume_scene(
    interactive = interactive,
    background = list(asset_id = background_id),
    analyses = analyses,
    assets = assets,
    engine = list(
      name = "neuroimjs",
      version = .montage_neuroimjs_version,
      adapter_version = .montage_neuroimjs_adapter_version
    ),
    provenance = list(
      package = "neuromosaic",
      package_version = as.character(
        report_data$provenance$package_version %||% "unknown"
      ),
      report_mode = "montage"
    )
  )
}

.montage_volume_scene_map <- function(row, metadata, asset_id) {
  units <- metadata$units
  if (is.null(units) || is.na(units) || !nzchar(units)) units <- NULL
  selector_label <- metadata$selector_label
  if (is.null(selector_label) || is.na(selector_label) ||
      !nzchar(selector_label)) {
    selector_label <- NULL
  }
  list(
    map_id = as.character(row$map_id[[1L]]),
    quantity = as.character(row$quantity[[1L]]),
    label = as.character(row$label[[1L]]),
    units = units,
    asset_id = asset_id,
    display = list(
      mode = metadata$display_mode,
      scale = metadata$scale,
      center = metadata$center,
      limits = as.numeric(metadata$limits),
      threshold = if (identical(metadata$display_mode, "continuous")) {
        NULL
      } else {
        as.numeric(metadata$threshold)
      },
      tail = metadata$tail,
      palette = metadata$palette,
      alpha = as.numeric(metadata$alpha),
      alpha_mode = metadata$alpha_mode,
      support = if (metadata$support %in% c("analysis", "primary")) {
        metadata$support
      } else {
        "analysis"
      },
      units = units
    ),
    status = list(
      state = metadata$status,
      n_display_voxels = as.integer(metadata$n_display_voxels)
    ),
    selector_label = selector_label
  )
}

.montage_geometry_center <- function(geometry) {
  affine <- matrix(as.numeric(geometry$affine), nrow = 4L, ncol = 4L)
  voxel <- c((as.numeric(geometry$dimensions) - 1) / 2, 1)
  world <- as.numeric(affine %*% voxel)
  world[seq_len(3L)] / world[[4L]]
}

.montage_volume_scene_json <- function(scene) {
  json <- jsonlite::toJSON(
    unclass(scene),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  )
  .montage_json_script_escape(as.character(json))
}

.montage_json_script_escape <- function(x) {
  x <- gsub("&", "\\u0026", x, fixed = TRUE)
  x <- gsub("<", "\\u003c", x, fixed = TRUE)
  gsub(">", "\\u003e", x, fixed = TRUE)
}

.montage_interactive_document_html <- function(x, is_html = TRUE) {
  if (is.null(x) || !isTRUE(is_html)) return("")
  if (!inherits(x, "montage_interactive_report")) {
    stop("Interactive report data are malformed.", call. = FALSE)
  }
  runtime_dir <- .montage_volume_runtime_path()
  runtime <- .montage_read_inline_asset(file.path(
    runtime_dir, paste0("neuroimjs-", .montage_neuroimjs_version, ".umd.js")
  ))
  geometry <- .montage_read_inline_asset(file.path(runtime_dir, "geometry.js"))
  display <- .montage_read_inline_asset(file.path(runtime_dir, "display.js"))
  adapter <- .montage_read_inline_asset(file.path(runtime_dir, "adapter.js"))
  css <- .montage_read_inline_asset(file.path(runtime_dir, "adapter.css"))
  scripts <- c(runtime, geometry, display, adapter)
  if (any(grepl("</script", tolower(scripts), fixed = TRUE))) {
    stop("Bundled runtime contains an unsafe closing script sequence.",
         call. = FALSE)
  }
  payload_html <- vapply(x$payloads, function(payload) {
    paste0(
      '<script type="application/octet-stream" id="',
      .montage_html_escape(payload$id), '">', payload$base64, "</script>\n"
    )
  }, character(1))
  paste0(
    "<style data-nm-volume-runtime>\n", css, "\n</style>\n",
    "<script data-nm-volume-runtime>\n", runtime, "\n</script>\n",
    "<script data-nm-volume-geometry>\n", geometry, "\n</script>\n",
    "<script data-nm-volume-display>\n", display, "\n</script>\n",
    "<script data-nm-volume-adapter>\n", adapter, "\n</script>\n",
    '<script type="application/json" id="',
    .montage_html_escape(x$scene_id), '">', x$scene_json, "</script>\n",
    paste(payload_html, collapse = "")
  )
}

.montage_interactive_host_html <- function(x, analysis_id, is_html = TRUE) {
  if (is.null(x) || !isTRUE(is_html)) return("")
  ids <- vapply(x$scene$analyses, `[[`, character(1), "analysis_id")
  if (!analysis_id %in% ids) {
    stop("Interactive report has no analysis_id '", analysis_id, "'.",
         call. = FALSE)
  }
  safe_id <- paste0(
    "nm-volume-", substr(.montage_md5_text(as.character(analysis_id)), 1L, 12L)
  )
  analysis <- x$scene$analyses[[match(analysis_id, ids)]]
  map_ids <- vapply(analysis$maps, `[[`, character(1), "map_id")
  primary_map <- analysis$maps[[match(analysis$primary_map_id, map_ids)]]
  primary_map_label <- primary_map$selector_label
  if (is.null(primary_map_label) || !nzchar(primary_map_label)) {
    primary_map_label <- primary_map$label
  }
  summary <- x$summary
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
    "This report includes recoverable voxel data for interactive viewing (",
    summary$asset_count, " NIfTI assets; ",
    payload_size_text,
    " MiB ", if (identical(summary$packaging, "embed")) "embedded" else "bundled",
    "). Browser display changes are exploratory and do not alter the static report."
  )
  paste0(
    '<section class="nm-volume-interactive" data-nm-volume-host ',
    'data-nm-volume-analysis="', .montage_html_escape(analysis_id), '" ',
    'data-nm-volume-map="',
    .montage_html_escape(analysis$primary_map_id), '" ',
    'data-nm-volume-modified="false" ',
    'data-nm-volume-scene="', .montage_html_escape(x$scene_id), '">\n',
    '<button type="button" class="nm-volume-toggle" ',
    'data-nm-volume-toggle aria-expanded="false" aria-controls="', safe_id,
    '">Explore volume interactively</button>\n',
    '<p class="nm-volume-target">Interactive map: <strong ',
    'data-nm-volume-target>', .montage_html_escape(primary_map_label),
    '</strong></p>\n',
    '<p class="nm-volume-status" data-nm-volume-status role="status">',
    'The static montage is the report default.</p>\n',
    '<div class="nm-volume-shell" id="', safe_id,
    '" data-nm-volume-shell hidden>\n',
    '<div class="nm-volume-viewer" data-nm-volume-viewer ',
    'aria-label="Orthogonal volume viewer"></div>\n',
    '<aside class="nm-volume-sidebar" aria-label="Interactive map controls">\n',
    '<label class="nm-volume-map-field">Map</label>\n',
    '<select class="nm-volume-map-select" data-nm-volume-map ',
    'aria-label="Interactive map"></select>\n',
    '<p class="nm-volume-readout" data-nm-volume-readout ',
    'aria-live="polite"></p>\n',
    '<button type="button" class="nm-volume-reset-position" ',
    'data-nm-volume-reset-position>Reset to report position</button>\n',
    '<div class="nm-volume-controls" data-nm-volume-controls></div>\n',
    '</aside>\n</div>\n',
    '<p class="nm-volume-disclosure"><strong>Data disclosure.</strong> ',
    .montage_html_escape(disclosure), '</p>\n',
    '<noscript><p>JavaScript is disabled; use the static montage above.</p></noscript>\n',
    '</section>\n'
  )
}

#' Interactive Montage Report Hooks
#'
#' Stable emitters for custom R Markdown or Quarto templates that want the same
#' optional neuroimjs volume view as the bundled report templates. Emit the
#' document assets once, then emit one host after each static analysis group.
#'
#' @param interactive The `interactive` member of prepared montage report data,
#'   or `NULL` for a static-only report.
#' @param is_html Logical; interactive output is emitted only for HTML.
#'
#' @return A list with `emit_document()` and `emit_host(analysis_id)` functions.
#' @export
montage_interactive_report_hooks <- function(interactive = NULL,
                                             is_html = FALSE) {
  if (!is.null(interactive) &&
      !inherits(interactive, "montage_interactive_report")) {
    stop(
      "'interactive' must be NULL or prepared montage interactive data.",
      call. = FALSE
    )
  }
  list(
    emit_document = function() {
      cat(.montage_interactive_document_html(
        interactive, is_html = isTRUE(is_html)
      ), sep = "")
      invisible(NULL)
    },
    emit_host = function(analysis_id) {
      cat(.montage_interactive_host_html(
        interactive,
        analysis_id = as.character(analysis_id),
        is_html = isTRUE(is_html)
      ), sep = "")
      invisible(NULL)
    }
  )
}

.montage_read_inline_asset <- function(path) {
  if (!file.exists(path)) {
    stop("Missing bundled interactive runtime asset: ", basename(path),
         call. = FALSE)
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.montage_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
