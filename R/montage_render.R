#' Render a Multi-Map Montage Report
#'
#' Renders a montage report from a validated render manifest. This contract is
#' intentionally separate from [render_cluster_report()]: HTML/PDF outputs use a
#' montage R Markdown template with `params$report_data`, while `.qmd` output
#' writes a Quarto source file plus a companion `_report-data.rds` sidecar.
#'
#' @details
#' The minimal recipe is `build_manifest()` -> `render_montage_report(manifest,
#' output_file, surfatlas = )`. Surface panels are generated when a `surfatlas`
#' is supplied (`render_surface = !is.null(surfatlas)`); volume panels when a
#' `bg` is supplied (`render_volume = !is.null(bg)`). See
#' `vignette("montage-report", package = "neuromosaic")` for a worked example.
#'
#' **Manifest contract.** Every row describes one statistical map. The required
#' columns are `map_id` (unique stable key), `stat_kind` (one of `t`, `z`,
#' `beta`, `cope`), `signed` (a *logical*: does the statistic have positive and
#' negative semantics?), and `label` (the human-facing panel title). `df` is
#' additionally required for `stat_kind = "t"` rows whenever the threshold is
#' derived from a p-value rather than supplied directly. See
#' [montage_manifest_schema()] for the full column list and
#' [build_manifest()]/[validate_manifest()] for construction and checks.
#'
#' **Reserved passthrough arguments.** `volume_args` and `surface_args` forward
#' styling to [stat_montage()] and [surf_montage()], but renderer-managed
#' arguments are reserved and cannot be overridden: `volume_args` reserves
#' `bg`, `stat`, and `draw`; `surface_args` reserves `stat`, `surfatlas`,
#' `output_file`, `plot_fun`, and `projection`. Passing a reserved name is an
#' error.
#'
#' **Output format and portability.** The extension of `output_file` selects
#' the format. `.html` (the default) is the most portable: it has no external
#' toolchain dependency. `.pdf` renders through LaTeX and therefore needs a TeX
#' installation providing `latex_engine` (default `"xelatex"`, common on
#' workstations but often absent on minimal HPC nodes); use the `latex_engine`
#' argument to switch engines (e.g. `"pdflatex"`). `.qmd` does **not** render:
#' it writes the Quarto source plus a `_report-data.rds` sidecar and emits a
#' message telling you to run `quarto render` yourself.
#'
#' @param manifest A render manifest data frame, one row per statistical map.
#'   See Details for the required columns.
#' @param output_file Path for the rendered output. Extension determines format
#'   (`.html`, `.pdf`, or `.qmd`); see Details. `.html` is the portable default.
#' @param template Path to a custom Rmd/Qmd template. `NULL` uses the bundled
#'   montage template matching `output_file`.
#' @param latex_engine LaTeX engine used for `.pdf` output, passed to
#'   [rmarkdown::pdf_document()]. Defaults to `"xelatex"`. Set to `"pdflatex"`
#'   (or another installed engine) on systems without a XeTeX toolchain.
#'   Ignored for `.html` and `.qmd` output.
#' @param title Report title used by the bundled templates.
#' @param layout Optional character vector naming manifest columns used for
#'   nested report sections. Overrides `policy$layout` when supplied.
#' @param labeller Optional labeller passed to [apply_montage_labeller()].
#' @param intro Optional report-level preamble rendered once before the maps: a
#'   markdown character scalar, or a character vector collapsed with blank lines.
#'   Use it to frame the whole report (what the contrasts are, how to read them)
#'   beyond the individual panel labels.
#' @param section_notes Optional data frame of section-level narrative for
#'   layout-grouped reports. Alongside a `text` column (the markdown narrative),
#'   name a subset of the `layout` columns to identify the section: a row that
#'   fixes the first *k* layout columns (leaving deeper layout columns `NA`) has
#'   its text emitted under that section's heading. For example, with
#'   `layout = c("model", "contrast")`, a row `model = "m1", contrast = NA`
#'   annotates the whole `m1` section, while `model = "m1", contrast = "faces"`
#'   annotates the subsection. Every named column must be a layout column, each
#'   layout path must match at least one map, and paths must be unique (typos
#'   error out).
#' @param interludes Optional data frame of free-standing narrative placed
#'   between panels rather than inside a panel's `description`. Columns: `map_id`
#'   (an existing manifest map), `text` (markdown), and optional `position`
#'   (`"before"` or `"after"`, default `"before"`) selecting the side of that
#'   panel. Multiple rows for the same anchor render in row order.
#' @param policy A [montage_policy()] object.
#' @param bg Optional background `NeuroVol` or path. When supplied, volume
#'   montage PNGs are generated with [stat_montage()].
#' @param surfatlas Optional surface atlas. Required when `render_surface` is
#'   `TRUE`.
#' @param atlas Optional volumetric atlas. When supplied and `render_peaks` is
#'   `TRUE`, per-panel peak tables are generated with [montage_peak_table()].
#' @param panels Optional named list keyed by `map_id`. Each panel may contain
#'   `volume_image`, `surface_image`, `table`, or other renderer-specific data.
#' @param volume_args Optional named list of styling arguments forwarded to
#'   [stat_montage()] for every volume panel (e.g.
#'   `list(ov_alpha_mode = "ramp", cap = 8)`). Caller values win over the
#'   manifest/policy-derived defaults; `bg`, `stat`, and `draw` are managed by
#'   the renderer and cannot be overridden. Note that these are merged with
#'   [utils::modifyList()], so passing `cap = NULL` (or any `NULL`) *removes*
#'   the derived default rather than forcing it; omit the argument to keep the
#'   policy/robust default. Overriding analytical controls (`threshold`, `tail`,
#'   `signed`) here changes only the rendered panel, not the manifest-driven
#'   peak tables, so the two can diverge.
#' @param surface_args Optional named list of styling arguments forwarded to
#'   [surf_montage()] for every surface panel (e.g.
#'   `list(overlay_alpha = 0.9, fun = "mode")`). Caller values win over the
#'   defaults; `stat`, `surfatlas`, and `output_file` are managed by the
#'   renderer. The same `modifyList`/`NULL` and analytical-override caveats as
#'   `volume_args` apply.
#' @param render_volume Logical; generate volume montage PNGs when `bg` is
#'   supplied?
#' @param render_surface Logical; generate surface montage PNGs when
#'   `surfatlas` is supplied?
#' @param render_peaks Logical; generate per-panel atlas peak tables when
#'   `atlas` is supplied?
#' @param empty Action when a map has no suprathreshold voxels. The report-level
#'   default is `"warning"`: an empty contrast renders its base panel with no
#'   overlay and a warning, instead of aborting the whole report. Use `"error"`
#'   to restore the strict per-map behavior. (This differs from the
#'   [surf_montage()]/[stat_montage()] default of `"error"`, which is kept for
#'   direct callers.)
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
                                  latex_engine = "xelatex",
                                  title = "Montage Report",
                                  layout = NULL,
                                  labeller = NULL,
                                  intro = NULL,
                                  section_notes = NULL,
                                  interludes = NULL,
                                  policy = NULL,
                                  bg = NULL,
                                  surfatlas = NULL,
                                  atlas = NULL,
                                  panels = NULL,
                                  volume_args = list(),
                                  surface_args = list(),
                                  render_volume = !is.null(bg),
                                  render_surface = !is.null(surfatlas),
                                  render_peaks = !is.null(atlas),
                                  empty = c("warning", "error"),
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
  volume_args <- .validate_montage_passthrough(
    volume_args, stat_montage, c("bg", "stat", "draw"), "volume_args"
  )
  surface_args <- .validate_montage_passthrough(
    surface_args, surf_montage,
    # `plot_fun`/`projection` are advanced/testing hooks (executable code), not
    # styling; keep them out of the report-level passthrough surface.
    c("stat", "surfatlas", "output_file", "plot_fun", "projection"),
    "surface_args"
  )

  empty <- match.arg(empty)
  if (!is.character(latex_engine) || length(latex_engine) != 1L ||
      is.na(latex_engine) || !nzchar(latex_engine)) {
    stop("'latex_engine' must be a single non-empty string.", call. = FALSE)
  }

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

  # Absolutize the template so a relative `template=` still resolves after
  # withr::with_dir() changes the working directory below.
  template <- normalizePath(template, mustWork = TRUE)

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
    intro = intro,
    section_notes = section_notes,
    interludes = interludes,
    policy = policy,
    bg = bg,
    surfatlas = surfatlas,
    atlas = atlas,
    panels = panels,
    volume_args = volume_args,
    surface_args = surface_args,
    render_volume = render_volume,
    render_surface = render_surface,
    render_peaks = render_peaks,
    empty = empty,
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

  output_format <- .montage_rmarkdown_output_format(ext, latex_engine)
  withr::with_dir(output_dir, {
    rmarkdown::render(
      input = template,
      output_file = basename(output_file),
      output_dir = ".",
      # Write knit intermediates into the (writable, already-normalized) output
      # directory rather than next to the template, which lives in a possibly
      # read-only package/system library on HPC nodes and containers.
      intermediates_dir = output_dir,
      output_format = output_format,
      params = list(report_data = report_data),
      envir = new.env(parent = globalenv()),
      quiet = quiet
    )
  })

  invisible(normalizePath(output_file, mustWork = FALSE))
}

.montage_rmarkdown_output_format <- function(ext, latex_engine = "xelatex") {
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
      latex_engine = latex_engine,
      fig_caption = TRUE
    ))
  }

  stop("Unsupported rmarkdown output extension: ", ext, call. = FALSE)
}

.prepare_montage_report_data <- function(manifest,
                                         title,
                                         layout,
                                         labeller,
                                         intro = NULL,
                                         section_notes = NULL,
                                         interludes = NULL,
                                         policy,
                                         bg,
                                         surfatlas,
                                         atlas,
                                         panels,
                                         volume_args = list(),
                                         surface_args = list(),
                                         render_volume,
                                         render_surface,
                                         render_peaks,
                                         empty = "error",
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
      check_files = check_files,
      empty = empty
    )
  } else if (isTRUE(validate)) {
    manifest <- validate_manifest(
      manifest,
      check_files = check_files,
      load_maps = load_maps,
      empty = empty
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
  policy_stat_maps <- if (.montage_policy_uses_fdr(manifest, policy)) {
    lapply(seq_len(nrow(manifest)), function(i) {
      .montage_manifest_stat_source(manifest, i)
    })
  } else {
    NULL
  }
  manifest <- resolve_montage_policy(
    manifest,
    policy = policy,
    empty = empty,
    stat_maps = policy_stat_maps
  )
  missing_layout <- setdiff(layout, names(manifest))
  if (length(missing_layout) > 0L) {
    stop(
      "'layout' column(s) not found in manifest: ",
      paste(missing_layout, collapse = ", "),
      call. = FALSE
    )
  }

  map_ids <- as.character(manifest$map_id)
  narratives <- .validate_montage_narratives(
    intro = intro,
    section_notes = section_notes,
    interludes = interludes,
    layout = layout,
    manifest = manifest
  )
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
      res = image_res,
      policy = policy,
      volume_args = volume_args,
      empty = empty
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
      res = image_res,
      policy = policy,
      surface_args = surface_args,
      empty = empty
    )
  }

  list(
    manifest = manifest,
    panels = panels,
    qc = qc,
    intro = narratives$intro,
    section_notes = narratives$section_notes,
    interludes = narratives$interludes,
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

# Validate a styling passthrough list (volume_args/surface_args). Must be a
# named list whose names are forwardable formals of the target montage function
# and not one of the renderer-managed arguments. Returns the (possibly empty)
# list so the caller can assign it back.
.validate_montage_passthrough <- function(args, fn, managed, label) {
  if (is.null(args)) {
    return(list())
  }
  if (length(args) == 0L) {
    return(list())
  }
  if (!is.list(args) || is.null(names(args)) || any(!nzchar(names(args)))) {
    stop("'", label, "' must be a named list.", call. = FALSE)
  }
  if (anyDuplicated(names(args))) {
    stop("'", label, "' has duplicate argument names.", call. = FALSE)
  }
  allowed <- setdiff(names(formals(fn)), c("...", managed))
  unknown <- setdiff(names(args), allowed)
  if (length(unknown) > 0L) {
    stop(
      "'", label, "' contains argument(s) that cannot be forwarded: ",
      paste(unknown, collapse = ", "), ".\nAllowed: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  args
}

# Validate and normalize the report narrative inputs (intro / section_notes /
# interludes) against the resolved manifest and layout. Returns a list with the
# three normalized components (any of which may be NULL). Structural or
# referential errors (unknown layout columns, unknown map_ids, non-matching
# section paths) abort so authoring typos surface immediately rather than
# silently dropping narrative text.
.validate_montage_narratives <- function(intro, section_notes, interludes,
                                          layout, manifest) {
  list(
    intro = .validate_montage_intro(intro),
    section_notes = .validate_montage_section_notes(section_notes, layout,
                                                    manifest),
    interludes = .validate_montage_interludes(interludes, manifest)
  )
}

.validate_montage_intro <- function(intro) {
  if (is.null(intro)) {
    return(NULL)
  }
  if (!is.character(intro)) {
    stop("'intro' must be NULL or a character vector.", call. = FALSE)
  }
  if (anyNA(intro)) {
    stop("'intro' must not contain NA.", call. = FALSE)
  }
  intro <- paste(intro, collapse = "\n\n")
  if (!nzchar(trimws(intro))) {
    return(NULL)
  }
  intro
}

.validate_montage_section_notes <- function(section_notes, layout, manifest) {
  if (is.null(section_notes)) {
    return(NULL)
  }
  if (!is.data.frame(section_notes)) {
    stop("'section_notes' must be NULL or a data frame.", call. = FALSE)
  }
  section_notes <- as.data.frame(section_notes, stringsAsFactors = FALSE)
  if (nrow(section_notes) == 0L) {
    return(NULL)
  }
  if (!"text" %in% names(section_notes)) {
    stop("'section_notes' must contain a 'text' column.", call. = FALSE)
  }
  key_cols <- setdiff(names(section_notes), "text")
  unknown <- setdiff(key_cols, layout)
  if (length(unknown) > 0L) {
    stop(
      "'section_notes' column(s) are not layout columns: ",
      paste(unknown, collapse = ", "),
      ". Layout is: ",
      if (length(layout) == 0L) "(none)" else paste(layout, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (length(intersect(key_cols, layout)) == 0L) {
    stop(
      "'section_notes' must name at least one layout column ",
      "to identify the section.",
      call. = FALSE
    )
  }

  text <- as.character(section_notes$text)
  if (any(is.na(text) | !nzchar(trimws(text)))) {
    stop("'section_notes$text' must be non-empty for every row.",
         call. = FALSE)
  }

  # Normalize key cells to character and treat blanks (e.g. empty CSV cells for
  # deeper layout levels) as NA, so the same frame matches identically whether it
  # came from R or from a CSV read.
  for (col in key_cols) {
    values <- as.character(section_notes[[col]])
    values[is.na(values) | !nzchar(trimws(values))] <- NA_character_
    section_notes[[col]] <- values
  }

  ordered_keys <- intersect(layout, key_cols)
  paths <- character(nrow(section_notes))
  for (i in seq_len(nrow(section_notes))) {
    depth <- 0L
    for (col in ordered_keys) {
      value <- section_notes[[col]][[i]]
      specified <- !is.na(value) && nzchar(trimws(as.character(value)))
      if (specified) {
        if (depth != match(col, ordered_keys) - 1L) {
          stop(
            "'section_notes' row ", i, " skips a layout level before '", col,
            "'. Set the leading layout column(s) or leave deeper ones NA.",
            call. = FALSE
          )
        }
        depth <- match(col, ordered_keys)
      }
    }
    if (depth == 0L) {
      stop(
        "'section_notes' row ", i, " does not fix any layout value.",
        call. = FALSE
      )
    }
    used <- ordered_keys[seq_len(depth)]
    path_values <- vapply(used, function(col) {
      as.character(section_notes[[col]][[i]])
    }, character(1))
    if (!.montage_section_path_exists(manifest, used, path_values)) {
      stop(
        "'section_notes' row ", i, " does not match any map: ",
        paste(paste0(used, "=", path_values), collapse = ", "), ".",
        call. = FALSE
      )
    }
    paths[[i]] <- paste(used, path_values, sep = "=", collapse = "\r")
  }
  if (anyDuplicated(paths)) {
    stop("'section_notes' has duplicate section path(s).", call. = FALSE)
  }

  section_notes
}

# TRUE when at least one manifest row matches the layout path (columns `cols`
# fixed to `values`, in order).
.montage_section_path_exists <- function(manifest, cols, values) {
  if (length(cols) == 0L) {
    return(FALSE)
  }
  keep <- rep(TRUE, nrow(manifest))
  for (j in seq_along(cols)) {
    col <- cols[[j]]
    if (!col %in% names(manifest)) {
      return(FALSE)
    }
    keep <- keep & !is.na(manifest[[col]]) &
      as.character(manifest[[col]]) == values[[j]]
  }
  any(keep)
}

.validate_montage_interludes <- function(interludes, manifest) {
  if (is.null(interludes)) {
    return(NULL)
  }
  if (!is.data.frame(interludes)) {
    stop("'interludes' must be NULL or a data frame.", call. = FALSE)
  }
  interludes <- as.data.frame(interludes, stringsAsFactors = FALSE)
  if (nrow(interludes) == 0L) {
    return(NULL)
  }
  missing <- setdiff(c("map_id", "text"), names(interludes))
  if (length(missing) > 0L) {
    stop(
      "'interludes' must contain column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  text <- as.character(interludes$text)
  if (any(is.na(text) | !nzchar(trimws(text)))) {
    stop("'interludes$text' must be non-empty for every row.", call. = FALSE)
  }

  if ("position" %in% names(interludes)) {
    position <- trimws(tolower(as.character(interludes$position)))
    position[is.na(position) | !nzchar(position)] <- "before"
    bad <- !position %in% c("before", "after")
    if (any(bad)) {
      stop(
        "'interludes$position' must be 'before' or 'after'; bad value(s): ",
        paste(unique(as.character(interludes$position)[bad]), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    interludes$position <- position
  } else {
    interludes$position <- rep("before", nrow(interludes))
  }

  map_ids <- as.character(manifest$map_id)
  unknown <- setdiff(unique(as.character(interludes$map_id)), map_ids)
  if (length(unknown) > 0L) {
    stop(
      "'interludes$map_id' value(s) are not in the manifest: ",
      paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }

  interludes
}

.render_montage_volume_panels <- function(manifest,
                                          bg,
                                          panels,
                                          image_dir,
                                          width,
                                          height,
                                          res,
                                          policy = NULL,
                                          volume_args = list(),
                                          empty = "error") {
  if (is.null(bg)) {
    stop("'bg' is required when render_volume = TRUE.", call. = FALSE)
  }
  if (!dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
  }

  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })
  caps <- .montage_shared_caps(manifest, stat_maps, policy = policy)

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    image_path <- file.path(image_dir, paste0(.safe_file_stem(map_id), "_volume.png"))
    cap_value <- caps[[manifest$cap_key[[i]]]]
    base_args <- list(
      bg = bg,
      stat = stat_maps[[i]],
      threshold = manifest$effective_threshold[[i]],
      tail = manifest$effective_tail[[i]],
      signed = manifest$signed[[i]],
      # A non-positive group cap (an entirely empty/all-zero cap group) is not a
      # valid color magnitude; pass NULL so the montage picks its own benign
      # default instead of failing the cap > 0 check (defeats empty = "warning").
      cap = if (is.finite(cap_value) && cap_value > 0) cap_value else NULL,
      title = manifest$label[[i]],
      subtitle = .montage_panel_subtitle(manifest[i, , drop = FALSE]),
      draw = TRUE,
      empty = empty
    )
    call_args <- utils::modifyList(base_args, volume_args)
    grDevices::png(
      filename = image_path,
      width = width,
      height = height,
      res = res
    )
    result <- tryCatch(
      do.call(stat_montage, call_args),
      finally = grDevices::dev.off()
    )

    panels[[map_id]]$volume_image <- normalizePath(image_path, mustWork = FALSE)
    panels[[map_id]]$volume <- list(
      threshold = result$threshold,
      tail = result$tail,
      cap = result$cap,
      n_suprathreshold = result$n_suprathreshold,
      alpha_mode = result$alpha_mode,
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
                                           res,
                                           policy = NULL,
                                           surface_args = list(),
                                           empty = "error") {
  if (is.null(surfatlas)) {
    stop("'surfatlas' is required when render_surface = TRUE.", call. = FALSE)
  }
  if (!dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
  }
  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })
  caps <- .montage_shared_caps(manifest, stat_maps, policy = policy)

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    group_cap <- caps[[manifest$cap_key[[i]]]]
    # surface_args (caller wins) can override threshold/tail/signed/cap; merge
    # before computing the cache key so it reflects the values actually rendered.
    base_args <- utils::modifyList(
      list(
        stat = stat_maps[[i]],
        surfatlas = surfatlas,
        threshold = manifest$effective_threshold[[i]],
        tail = manifest$effective_tail[[i]],
        signed = manifest$signed[[i]],
        # See the volume renderer: a non-positive cap (empty cap group) is not a
        # valid magnitude; pass NULL so surf_montage uses its benign default.
        cap = if (is.finite(group_cap) && group_cap > 0) group_cap else NULL,
        width = width,
        height = height,
        res = res,
        title = manifest$label[[i]],
        subtitle = .montage_panel_subtitle(manifest[i, , drop = FALSE])
      ),
      surface_args
    )
    eff_threshold <- base_args$threshold
    eff_tail <- base_args$tail
    eff_cap <- base_args$cap
    style_key <- .montage_surface_style_key(base_args, surfatlas)
    image_path <- .montage_cached_panel_image_path(
      image_dir = image_dir,
      map_id = map_id,
      kind = "surface",
      map_hash = manifest$map_hash[[i]] %||% NA_character_,
      threshold = eff_threshold,
      cap = eff_cap,
      style_key = style_key
    )

    # Effective empty policy: a caller's surface_args$empty wins over the
    # report-level default. `empty` is excluded from the cache key (non-visual),
    # so the policy must be enforced explicitly on both branches below.
    eff_empty <- base_args$empty %||% empty

    if (isTRUE(cache_surface) && file.exists(image_path)) {
      n_supra <- sum(.suprathreshold_mask(
        as.numeric(stat_maps[[i]]),
        threshold = eff_threshold,
        tail = eff_tail
      ), na.rm = TRUE)
      if (n_supra == 0L) {
        # A panel cached under empty = "warning" must still abort under a later
        # empty = "error" request (the PNG carries no policy of its own).
        msg <- paste0(
          "No finite suprathreshold voxels for threshold ", eff_threshold,
          " and tail '", eff_tail, "'."
        )
        if (identical(eff_empty, "error")) {
          stop(msg, call. = FALSE)
        }
        warning(msg, call. = FALSE)
      }
      panels[[map_id]]$surface_image <- normalizePath(image_path, mustWork = TRUE)
      panels[[map_id]]$surface <- list(
        threshold = eff_threshold,
        tail = eff_tail,
        cap = eff_cap,
        n_suprathreshold = n_supra,
        surface_space = NA_character_,
        diagnostics = list(cache_hit = TRUE)
      )
      next
    }

    base_args$output_file <- image_path
    base_args$empty <- eff_empty
    result <- do.call(surf_montage, base_args)
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

.montage_shared_caps <- function(manifest, stat_maps, policy = NULL) {
  cap_override <- policy$cap
  cap_quantile <- policy$cap_quantile %||% 0.99
  cap_floor <- policy$cap_floor
  keys <- unique(as.character(manifest$cap_key))
  caps <- stats::setNames(vector("list", length(keys)), keys)
  for (key in keys) {
    rows <- which(manifest$cap_key == key)
    if (!is.null(cap_override)) {
      caps[[key]] <- cap_override
      next
    }
    caps[[key]] <- .montage_group_cap(
      manifest = manifest,
      stat_maps = stat_maps,
      rows = rows,
      cap_quantile = cap_quantile,
      cap_floor = cap_floor
    )
  }
  caps
}

# Robust per-group cap: a high quantile (default 0.99) of the pooled
# suprathreshold |stat| magnitudes. The raw maximum lets a single hot voxel set
# the scale, which then washes out the rest of a strongly significant map under
# proportional/soft alpha (see GitHub issue #5). Falls back to the map maximum
# when no voxels survive threshold so a cap is always available.
.montage_group_cap <- function(manifest, stat_maps, rows, cap_quantile,
                               cap_floor) {
  supra <- unlist(lapply(rows, function(r) {
    values <- as.numeric(stat_maps[[r]])
    mask <- .suprathreshold_mask(
      values,
      threshold = manifest$effective_threshold[[r]],
      tail = manifest$effective_tail[[r]]
    )
    abs(values[mask])
  }), use.names = FALSE)
  supra <- supra[is.finite(supra)]

  cap <- if (length(supra) > 0L) {
    as.numeric(stats::quantile(supra, probs = cap_quantile, names = FALSE,
                               type = 7))
  } else {
    values <- unlist(lapply(stat_maps[rows], as.numeric), use.names = FALSE)
    values <- abs(values[is.finite(values)])
    if (length(values) == 0L) NA_real_ else max(values)
  }

  if (!is.null(cap_floor) && is.finite(cap)) {
    cap <- max(cap, cap_floor)
  }
  cap
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
                                             cap,
                                             style_key = NULL) {
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
  # The cache stores rendered pixels, so the key must change when any
  # render-affecting argument changes, not just the map/threshold/cap.
  style_part <- if (!is.null(style_key) && length(style_key) == 1L &&
                    !is.na(style_key) && nzchar(style_key)) {
    style_key
  } else {
    "default"
  }
  file.path(
    image_dir,
    paste0(
      .safe_file_stem(map_id), "_", kind, "_",
      .safe_file_stem(paste(key, threshold_value, cap_value, style_part,
                            sep = "_")),
      ".png"
    )
  )
}

# Stable digest of the render-affecting surface call arguments (tail, signed,
# alpha, palette, sampling, views, hemis, device size, titles, ...) so the
# surface PNG cache key changes whenever any of them does. The statistic volume
# is covered by `map_hash`, and `surfatlas` by a cheap identity, so both are
# excluded from the (potentially large) hashed payload.
.montage_surface_style_key <- function(base_args, surfatlas) {
  payload <- base_args[setdiff(
    names(base_args), c("stat", "surfatlas", "output_file", "empty")
  )]
  payload$.surfatlas <- surfatlas$name %||% surfatlas$surface_space %||%
    NA_character_
  substr(rlang::hash(payload), 1, 16)
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

  # `.qmd` output writes source only; it does not render. Say so explicitly so
  # the empty-handed return is not mistaken for a silent failure (issue #10).
  message(
    "Wrote Quarto source to '", output_file, "'\n",
    "  and report data to '", sidecar_path, "'.\n",
    "This '.qmd' format does not render. To produce the report run:\n",
    "  quarto render ", shQuote(basename(output_file)), "  (from '",
    dirname(output_file), "')."
  )

  invisible(normalizePath(output_file, mustWork = FALSE))
}
