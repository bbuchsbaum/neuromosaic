#' Render a Multi-Map Montage Report
#'
#' Renders a montage report from a validated render manifest. This contract is
#' intentionally separate from [render_cluster_report()]: HTML/PDF outputs use a
#' montage R Markdown template with `params$report_data`, while `.qmd` output
#' writes a Quarto source file plus a companion `_report-data.rds` sidecar.
#'
#' @param manifest A render manifest data frame, one row per statistical map.
#' @param output_file Path for the rendered output. Extension determines format
#'   (`.html`, `.pdf`, or `.qmd`).
#' @param template Path to a custom Rmd/Qmd template. `NULL` uses the bundled
#'   montage template matching `output_file`.
#' @param title Report title used by the bundled templates.
#' @param layout Optional character vector naming manifest columns used for
#'   nested report sections. Overrides `policy$layout` when supplied.
#' @param labeller Optional labeller passed to [apply_montage_labeller()].
#' @param policy A [montage_policy()] object.
#' @param bg Optional background `NeuroVol` or path. When supplied, volume
#'   montage PNGs are generated with [stat_montage()].
#' @param surfatlas Optional surface atlas. Required when `render_surface` is
#'   `TRUE`.
#' @param atlas Optional volumetric atlas. When supplied and `render_peaks` is
#'   `TRUE`, per-panel peak tables are generated with [montage_peak_table()].
#' @param panels Optional named list keyed by `map_id`. Each panel may contain
#'   `volume_image`, `surface_image`, `table`, or other renderer-specific data.
#' @param render_volume Logical; generate volume montage PNGs when `bg` is
#'   supplied?
#' @param render_surface Logical; generate surface montage PNGs when
#'   `surfatlas` is supplied?
#' @param render_peaks Logical; generate per-panel atlas peak tables when
#'   `atlas` is supplied?
#' @param image_dir Optional directory for generated montage PNGs.
#' @param cache_dir Optional directory for materialized derived maps.
#' @param materialize_recipes Logical; materialize recipe-backed manifest rows
#'   to disk before rendering?
#' @param overwrite_recipes Logical; recompute derived maps even when cached
#'   files exist?
#' @param cache_surface Logical; reuse existing surface PNGs keyed by map hash,
#'   threshold, and color cap.
#' @param image_width,image_height,image_res PNG device settings for generated
#'   volume and surface montage panels.
#' @param max_clusters Maximum number of atlas-annotated clusters per panel.
#' @param quiet Logical; suppress render progress messages? Default `TRUE`.
#' @param validate Logical; run [validate_manifest()] before rendering?
#' @param check_files Logical; passed to [validate_manifest()].
#' @param load_maps Logical; passed to [validate_manifest()] for map-level QC.
#' @param provenance Optional provenance list to include in the report bundle.
#'
#' @return The path to the rendered report (invisibly).
#' @export
render_montage_report <- function(manifest,
                                  output_file = "montage_report.html",
                                  template = NULL,
                                  title = "Montage Report",
                                  layout = NULL,
                                  labeller = NULL,
                                  policy = NULL,
                                  bg = NULL,
                                  surfatlas = NULL,
                                  atlas = NULL,
                                  panels = NULL,
                                  render_volume = !is.null(bg),
                                  render_surface = !is.null(surfatlas),
                                  render_peaks = !is.null(atlas),
                                  image_dir = NULL,
                                  cache_dir = NULL,
                                  materialize_recipes = TRUE,
                                  overwrite_recipes = FALSE,
                                  cache_surface = TRUE,
                                  image_width = 1400,
                                  image_height = 1000,
                                  image_res = 144,
                                  max_clusters = 20L,
                                  quiet = TRUE,
                                  validate = TRUE,
                                  check_files = TRUE,
                                  load_maps = FALSE,
                                  provenance = NULL) {
  ext <- tolower(tools::file_ext(output_file))
  if (!ext %in% c("html", "pdf", "qmd")) {
    stop(
      "Unsupported report extension '.", ext,
      "'. Use '.html', '.pdf', or '.qmd'.",
      call. = FALSE
    )
  }

  if (is.null(template)) {
    template_name <- if (ext == "qmd") "montage_report.qmd" else "montage_report.Rmd"
    template <- system.file("templates", template_name, package = "neuromosaic")
    if (!nzchar(template)) {
      stop("Bundled montage template not found. Is neuromosaic installed?",
           call. = FALSE)
    }
  }

  template_ext <- tolower(tools::file_ext(template))
  if (ext %in% c("html", "pdf") && identical(template_ext, "qmd")) {
    stop(
      "Qmd templates are only supported when `output_file` ends in '.qmd'.",
      call. = FALSE
    )
  }

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  output_file <- file.path(output_dir, basename(output_file))
  if (is.null(image_dir)) {
    image_dir <- file.path(
      output_dir,
      paste0(tools::file_path_sans_ext(basename(output_file)), "_files")
    )
  }

  report_data <- .prepare_montage_report_data(
    manifest = manifest,
    title = title,
    layout = layout,
    labeller = labeller,
    policy = policy,
    bg = bg,
    surfatlas = surfatlas,
    atlas = atlas,
    panels = panels,
    render_volume = render_volume,
    render_surface = render_surface,
    render_peaks = render_peaks,
    image_dir = image_dir,
    cache_dir = cache_dir,
    materialize_recipes = materialize_recipes,
    overwrite_recipes = overwrite_recipes,
    cache_surface = cache_surface,
    image_width = image_width,
    image_height = image_height,
    image_res = image_res,
    max_clusters = max_clusters,
    validate = validate,
    check_files = check_files,
    load_maps = load_maps,
    provenance = provenance
  )

  if (ext == "qmd") {
    return(invisible(.write_montage_report_qmd(
      report_data = report_data,
      output_file = output_file,
      template = template
    )))
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render reports.", call. = FALSE)
  }

  output_format <- .montage_rmarkdown_output_format(ext)
  withr::with_dir(output_dir, {
    rmarkdown::render(
      input = template,
      output_file = basename(output_file),
      output_dir = ".",
      output_format = output_format,
      params = list(report_data = report_data),
      envir = new.env(parent = globalenv()),
      quiet = quiet
    )
  })

  invisible(normalizePath(output_file, mustWork = FALSE))
}

.montage_rmarkdown_output_format <- function(ext) {
  if (identical(ext, "html")) {
    return(rmarkdown::html_document(
      toc = TRUE,
      toc_float = TRUE,
      theme = "flatly"
    ))
  }

  if (identical(ext, "pdf")) {
    return(rmarkdown::pdf_document(
      toc = TRUE,
      toc_depth = 3,
      number_sections = TRUE,
      latex_engine = "xelatex",
      fig_caption = TRUE
    ))
  }

  stop("Unsupported rmarkdown output extension: ", ext, call. = FALSE)
}

.prepare_montage_report_data <- function(manifest,
                                         title,
                                         layout,
                                         labeller,
                                         policy,
                                         bg,
                                         surfatlas,
                                         atlas,
                                         panels,
                                         render_volume,
                                         render_surface,
                                         render_peaks,
                                         image_dir,
                                         cache_dir,
                                         materialize_recipes,
                                         overwrite_recipes,
                                         cache_surface,
                                         image_width,
                                         image_height,
                                         image_res,
                                         max_clusters,
                                         validate,
                                         check_files,
                                         load_maps,
                                         provenance) {
  policy <- policy %||% montage_policy(layout = layout %||% character())
  if (!inherits(policy, "montage_policy")) {
    stop("'policy' must be created by montage_policy().", call. = FALSE)
  }

  if (isTRUE(materialize_recipes)) {
    manifest <- materialize_montage_recipes(
      manifest,
      cache_dir = cache_dir,
      overwrite = overwrite_recipes,
      validate = FALSE,
      check_files = check_files
    )
  } else {
    manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
    manifest <- .attach_montage_map_hashes(manifest)
  }

  if (!is.null(labeller)) {
    manifest <- apply_montage_labeller(
      manifest,
      labeller = labeller,
      check_files = check_files
    )
  } else if (isTRUE(validate)) {
    manifest <- validate_manifest(
      manifest,
      check_files = check_files,
      load_maps = load_maps
    )
  } else {
    manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  }

  layout <- layout %||% policy$layout %||% character()
  if (!is.character(layout)) {
    stop("'layout' must be a character vector of manifest column names.",
         call. = FALSE)
  }
  policy$layout <- layout
  manifest <- resolve_montage_policy(manifest, policy = policy)
  missing_layout <- setdiff(layout, names(manifest))
  if (length(missing_layout) > 0L) {
    stop(
      "'layout' column(s) not found in manifest: ",
      paste(missing_layout, collapse = ", "),
      call. = FALSE
    )
  }

  map_ids <- as.character(manifest$map_id)
  panels <- .normalize_montage_panels(panels, map_ids)
  qc <- .montage_qc_summary(manifest)
  panels <- .attach_montage_qc(panels, qc)
  if (isTRUE(render_peaks)) {
    panels <- .render_montage_peak_panels(
      manifest = manifest,
      atlas = atlas,
      panels = panels,
      max_clusters = max_clusters
    )
  }
  if (isTRUE(render_volume)) {
    panels <- .render_montage_volume_panels(
      manifest = manifest,
      bg = bg,
      panels = panels,
      image_dir = image_dir,
      width = image_width,
      height = image_height,
      res = image_res
    )
  }
  if (isTRUE(render_surface)) {
    panels <- .render_montage_surface_panels(
      manifest = manifest,
      surfatlas = surfatlas,
      panels = panels,
      image_dir = image_dir,
      cache_surface = cache_surface,
      width = image_width,
      height = image_height,
      res = image_res
    )
  }

  list(
    manifest = manifest,
    panels = panels,
    qc = qc,
    params = list(
      title = title,
      layout = layout,
      policy = policy,
      report_mode = "montage"
    ),
    provenance = provenance %||% .montage_report_provenance()
  )
}

.normalize_montage_panels <- function(panels, map_ids) {
  empty <- stats::setNames(vector("list", length(map_ids)), map_ids)
  empty <- lapply(empty, function(x) list())

  if (is.null(panels)) {
    return(empty)
  }
  if (!is.list(panels) || is.null(names(panels))) {
    stop("'panels' must be a named list keyed by manifest map_id.",
         call. = FALSE)
  }

  for (map_id in intersect(names(panels), map_ids)) {
    panel <- panels[[map_id]]
    if (is.null(panel)) {
      panel <- list()
    }
    if (!is.list(panel)) {
      stop(
        "Panel entry for map_id '", map_id, "' must be a list.",
        call. = FALSE
      )
    }
    empty[[map_id]] <- panel
  }

  empty
}

.render_montage_volume_panels <- function(manifest,
                                          bg,
                                          panels,
                                          image_dir,
                                          width,
                                          height,
                                          res) {
  if (is.null(bg)) {
    stop("'bg' is required when render_volume = TRUE.", call. = FALSE)
  }
  if (!dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
  }

  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })
  caps <- .montage_shared_caps(manifest, stat_maps)

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    image_path <- file.path(image_dir, paste0(.safe_file_stem(map_id), "_volume.png"))
    grDevices::png(
      filename = image_path,
      width = width,
      height = height,
      res = res
    )
    result <- tryCatch(
      stat_montage(
        bg = bg,
        stat = stat_maps[[i]],
        threshold = manifest$effective_threshold[[i]],
        tail = manifest$effective_tail[[i]],
        signed = manifest$signed[[i]],
        cap = caps[[manifest$cap_key[[i]]]],
        title = manifest$label[[i]],
        subtitle = .montage_panel_subtitle(manifest[i, , drop = FALSE]),
        draw = TRUE
      ),
      finally = grDevices::dev.off()
    )

    panels[[map_id]]$volume_image <- normalizePath(image_path, mustWork = FALSE)
    panels[[map_id]]$volume <- list(
      threshold = result$threshold,
      tail = result$tail,
      cap = result$cap,
      n_suprathreshold = result$n_suprathreshold,
      style = result$style
    )
  }

  panels
}

.render_montage_surface_panels <- function(manifest,
                                           surfatlas,
                                           panels,
                                           image_dir,
                                           cache_surface,
                                           width,
                                           height,
                                           res) {
  if (is.null(surfatlas)) {
    stop("'surfatlas' is required when render_surface = TRUE.", call. = FALSE)
  }
  if (!dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
  }
  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })
  caps <- .montage_shared_caps(manifest, stat_maps)

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    image_path <- .montage_cached_panel_image_path(
      image_dir = image_dir,
      map_id = map_id,
      kind = "surface",
      map_hash = manifest$map_hash[[i]] %||% NA_character_,
      threshold = manifest$effective_threshold[[i]],
      cap = caps[[manifest$cap_key[[i]]]]
    )

    if (isTRUE(cache_surface) && file.exists(image_path)) {
      panels[[map_id]]$surface_image <- normalizePath(image_path, mustWork = TRUE)
      panels[[map_id]]$surface <- list(
        threshold = manifest$effective_threshold[[i]],
        tail = manifest$effective_tail[[i]],
        cap = caps[[manifest$cap_key[[i]]]],
        n_suprathreshold = sum(.suprathreshold_mask(
          as.numeric(stat_maps[[i]]),
          threshold = manifest$effective_threshold[[i]],
          tail = manifest$effective_tail[[i]]
        ), na.rm = TRUE),
        surface_space = NA_character_,
        diagnostics = list(cache_hit = TRUE)
      )
      next
    }

    result <- surf_montage(
      stat = stat_maps[[i]],
      surfatlas = surfatlas,
      output_file = image_path,
      threshold = manifest$effective_threshold[[i]],
      tail = manifest$effective_tail[[i]],
      signed = manifest$signed[[i]],
      cap = caps[[manifest$cap_key[[i]]]],
      width = width,
      height = height,
      res = res,
      title = manifest$label[[i]],
      subtitle = .montage_panel_subtitle(manifest[i, , drop = FALSE])
    )
    panels[[map_id]]$surface_image <- result$image
    panels[[map_id]]$surface <- list(
      threshold = result$threshold,
      tail = result$tail,
      cap = result$cap,
      n_suprathreshold = result$n_suprathreshold,
      surface_space = result$surface_space,
      diagnostics = result$diagnostics
    )
  }

  panels
}

.montage_manifest_stat_source <- function(manifest, row) {
  if ("stat_map" %in% names(manifest) &&
      !.missing_column_values(manifest$stat_map)[[row]]) {
    col <- manifest$stat_map
    return(if (is.list(col)) col[[row]] else col[row])
  }
  if ("path" %in% names(manifest) &&
      !.missing_character(as.character(manifest$path))[[row]]) {
    return(neuroim2::read_vol(as.character(manifest$path[[row]])))
  }
  if ("recipe" %in% names(manifest) &&
      !.missing_column_values(manifest$recipe)[[row]]) {
    return(.montage_evaluate_recipe(
      manifest$recipe[[row]],
      manifest[row, , drop = FALSE]
    ))
  }
  stop(
    "Cannot render volume montage for map_id '", manifest$map_id[[row]],
    "': no path, recipe, or stat_map is available.",
    call. = FALSE
  )
}

.montage_shared_caps <- function(manifest, stat_maps) {
  keys <- unique(as.character(manifest$cap_key))
  caps <- stats::setNames(vector("list", length(keys)), keys)
  for (key in keys) {
    rows <- which(manifest$cap_key == key)
    signed <- any(manifest$signed[rows], na.rm = TRUE)
    values <- unlist(lapply(stat_maps[rows], as.numeric), use.names = FALSE)
    values <- values[is.finite(values)]
    caps[[key]] <- if (length(values) == 0L) {
      NA_real_
    } else if (isTRUE(signed)) {
      max(abs(values), na.rm = TRUE)
    } else {
      max(values, na.rm = TRUE)
    }
  }
  caps
}

.montage_panel_subtitle <- function(row) {
  fields <- intersect(c("contrast", "model", "variant", "level"), names(row))
  if (length(fields) == 0L) {
    return(NULL)
  }
  values <- vapply(fields, function(field) {
    value <- row[[field]][[1]]
    if (is.null(value) || length(value) == 0L || is.na(value)) "" else {
      paste0(field, ": ", value)
    }
  }, character(1))
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(NULL)
  }
  paste(values, collapse = " | ")
}

.safe_file_stem <- function(x) {
  stem <- gsub("[^A-Za-z0-9_.-]+", "_", x)
  if (!nzchar(stem)) {
    stem <- "panel"
  }
  stem
}

.montage_cached_panel_image_path <- function(image_dir,
                                             map_id,
                                             kind,
                                             map_hash,
                                             threshold,
                                             cap) {
  key <- if (!is.null(map_hash) && length(map_hash) == 1L &&
             !is.na(map_hash) && nzchar(map_hash)) {
    map_hash
  } else {
    map_id
  }
  cap_value <- if (length(cap) == 1L && is.finite(cap)) {
    signif(cap, 8)
  } else {
    "na"
  }
  threshold_value <- if (length(threshold) == 1L && is.finite(threshold)) {
    signif(threshold, 8)
  } else {
    "na"
  }
  file.path(
    image_dir,
    paste0(
      .safe_file_stem(map_id), "_", kind, "_",
      .safe_file_stem(paste(key, threshold_value, cap_value, sep = "_")),
      ".png"
    )
  )
}

.montage_report_provenance <- function() {
  list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package = "neuromosaic",
    package_version = as.character(utils::packageVersion("neuromosaic")),
    report_mode = "montage"
  )
}

.write_montage_report_qmd <- function(report_data, output_file, template) {
  lines <- readLines(template, warn = FALSE)
  sidecar <- paste0(
    tools::file_path_sans_ext(basename(output_file)),
    "_report-data.rds"
  )
  sidecar_path <- file.path(dirname(output_file), sidecar)

  saveRDS(report_data, sidecar_path)
  lines <- gsub("__REPORT_DATA_FILE__", sidecar, lines, fixed = TRUE)
  note_lines <- c(
    paste0(
      "<!-- NOTE: This .qmd expects the sidecar file '", sidecar,
      "' to stay in the same directory. -->"
    ),
    "<!-- Render from this directory unless you edit the embedded readRDS() path. -->",
    ""
  )
  yaml_delims <- which(trimws(lines) == "---")
  if (length(yaml_delims) >= 2L) {
    lines <- append(lines, note_lines, after = yaml_delims[2])
  } else {
    lines <- c(note_lines, lines)
  }
  writeLines(lines, output_file)

  invisible(normalizePath(output_file, mustWork = FALSE))
}
