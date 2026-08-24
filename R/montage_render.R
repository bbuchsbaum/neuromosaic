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
#' **Manifest contract.** Every row describes one map variant. The required
#' columns are `map_id` (unique stable key), `quantity` (a built-in or custom
#' scientific quantity), and `label` (the human-facing panel title).
#' `analysis_id` groups associated variants and `role` identifies exactly one
#' primary map per group; both are derived for legacy one-map rows. Legacy
#' `stat_kind` values remain accepted. `df` is additionally required for t rows
#' whenever the threshold is
#' derived from a p-value rather than supplied directly. See
#' [montage_manifest_schema()] for the full column list and
#' [build_manifest()]/[validate_manifest()] for construction and checks. Surface
#' reports may use an in-memory `parcel_values` list-column instead of a
#' volumetric `path`/`stat_map`; those rows render directly through
#' [surf_montage(vals = )] and do not support volume panels or peak tables.
#'
#' **Reserved passthrough arguments.** `volume_args` and `surface_args` forward
#' styling to [stat_montage()] and [surf_montage()], but renderer-managed
#' arguments are reserved and cannot be overridden: `volume_args` reserves
#' `bg`, `stat`, and `draw`; `surface_args` reserves `stat`, `surfatlas`,
#' `vals`, `output_file`, `plot_fun`, and `projection`. Passing a reserved name
#' is an error.
#'
#' **Output format and portability.** The extension of `output_file` selects
#' the format. `.html` (the default) is the most portable: it has no external
#' toolchain dependency. `.pdf` renders through LaTeX and therefore needs a TeX
#' installation providing `latex_engine` (default `"xelatex"`, common on
#' workstations but often absent on minimal HPC nodes); use the `latex_engine`
#' argument to switch engines (e.g. `"pdflatex"`). `.qmd` does **not** render:
#' it writes the Quarto source plus a `_report-data.rds` sidecar and emits a
#' message telling you to run `quarto render` yourself. The QMD sidecar omits
#' source-map paths, executable recipes, and already-materialized map payloads;
#' its static images and interactive bundle files use companion-relative paths
#' so the QMD, sidecar, and `<report>_files/` directory can move together.
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
#' @param profiles Optional list of [montage_map_profile()] objects used to
#'   resolve custom quantities and display overrides without global state.
#' @param map_selector HTML selector style: `"auto"` uses segmented tabs for
#'   two to four variants and a select control for larger groups; `"tabs"` and
#'   `"select"` force a style; `"none"` expands every map. Non-HTML outputs
#'   always expand all variants in primary-first order.
#' @param interactive Optional [montage_interactive()] configuration for a lazy
#'   orthogonal volume viewer in HTML output. The viewer uses the same resolved
#'   maps, geometry, display limits, thresholds, palettes, and opacity defaults
#'   as the static panels; reader changes are exploratory and reset exactly per
#'   map. HTML can bundle relative compressed NIfTI assets or embed them for
#'   direct `file://` viewing. QMD preserves the scene and assets for a later
#'   HTML render. PDF remains static and does not include browser runtime code.
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
#'   defaults; `stat`, `vals`, `surfatlas`, and `output_file` are managed by the
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
#' @param surface_scene Optional `neurosurf::SurfaceScene` rendered once as the
#'   report's shared lazy interactive viewer. Static per-panel surface figures
#'   remain in the report as print and failure fallbacks.
#' @param surface Optional [montage_surface()] configuration. Unlike the legacy
#'   `surface_scene` sidecar, this builds one multi-layer `SurfaceScene` per
#'   analysis from the resolved manifest and associates every layer with its
#'   `map_id`. Static surface panels are required and remain authoritative.
#'   Surface typed arrays can be compressed into the HTML for direct `file://`
#'   viewing or written once as relative content-addressed bundle assets.
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
                                  profiles = NULL,
                                  map_selector = c("auto", "tabs", "select",
                                                   "none"),
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
                                  provenance = NULL,
                                  surface_scene = NULL,
                                  interactive = NULL,
                                  surface = NULL) {
  volume_args <- .validate_montage_passthrough(
    volume_args, stat_montage, c("bg", "stat", "draw"), "volume_args"
  )
  surface_args <- .validate_montage_passthrough(
    surface_args, surf_montage,
    # `plot_fun`/`projection` are advanced/testing hooks (executable code), not
    # styling; keep them out of the report-level passthrough surface.
    c("stat", "vals", "surfatlas", "output_file", "plot_fun", "projection"),
    "surface_args"
  )

  empty <- match.arg(empty)
  map_selector <- match.arg(map_selector)
  if (!is.null(interactive) &&
      !inherits(interactive, "montage_interactive")) {
    stop("'interactive' must be NULL or created by montage_interactive().",
         call. = FALSE)
  }
  if (!is.null(surface) && !inherits(surface, "montage_surface")) {
    stop("'surface' must be NULL or created by montage_surface().",
         call. = FALSE)
  }
  if (!is.null(surface) && !is.null(surface_scene)) {
    stop(
      "Supply either 'surface = montage_surface(...)' or the legacy ",
      "'surface_scene', not both.", call. = FALSE
    )
  }
  if (!is.null(interactive) && !isTRUE(render_volume)) {
    stop(
      "Interactive volume reports require render_volume = TRUE so the static ",
      "fallback remains authoritative.",
      call. = FALSE
    )
  }
  if (!is.null(interactive) &&
      identical(volume_args$on_mismatch, "restamp")) {
    stop(
      "Interactive volume reports do not support ",
      "volume_args$on_mismatch = 'restamp'; align the inputs first.",
      call. = FALSE
    )
  }
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
    profiles = profiles,
    map_selector = map_selector,
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
    provenance = provenance,
    surface_scene = surface_scene,
    surface = if (ext %in% c("html", "qmd")) surface else NULL
  )
  report_data$params$surface_requested <- !is.null(surface)
  report_data$params$interactive_requested <- !is.null(interactive)
  # Keep an exact, named `surface = NULL` entry when no grouped interactive
  # surface was requested. Assigning NULL with `$<-` removes the entry, after
  # which `$surface` partially matches the legacy `surface_scene` field.
  report_data["surface"] <- list(.package_montage_surface_report(
    report_data[["surface"]], output_file = output_file
  ))
  report_data$interactive <- if (!is.null(interactive) &&
                                 ext %in% c("html", "qmd")) {
    .prepare_montage_interactive_report(
      report_data = report_data,
      interactive = interactive,
      background = bg,
      output_file = output_file
    )
  } else {
    NULL
  }
  if (!is.null(report_data$interactive)) {
    interactive_summary <- report_data$interactive$summary
    report_data$provenance$interactive_engine <- paste0(
      "neuroimjs ", report_data$interactive$engine_version,
      " / adapter ", report_data$interactive$adapter_version
    )
    report_data$provenance$interactive_packaging <- paste0(
      interactive_summary$packaging, " / ", interactive_summary$compression
    )
    report_data$provenance$interactive_asset_count <-
      interactive_summary$asset_count
    report_data$provenance$interactive_scene_version <-
      report_data$interactive$scene$schema_version
    report_data$provenance$interactive_runtime_sha256 <-
      report_data$interactive$runtime_sha256
    report_data$provenance$interactive_compressed_bytes <-
      interactive_summary$compressed_bytes
    report_data$provenance$interactive_uncompressed_bytes <-
      interactive_summary$uncompressed_bytes
    report_data$provenance$interactive_transformations <- paste(unique(unlist(
      lapply(report_data$interactive$scene$assets, `[[`, "transformations")
    )), collapse = ", ")
    report_data$provenance$interactive_asset_hashes <- paste(vapply(
      report_data$interactive$scene$assets,
      function(asset) paste0(asset$asset_id, "=", asset$hash),
      character(1)
    ), collapse = "; ")
  }
  if (!is.null(report_data$surface)) {
    report_data$provenance$interactive_surface_engine <-
      "neurosurf SurfaceScene / surfview"
    report_data$provenance$interactive_surface_scene_count <-
      length(report_data$surface$scenes)
    report_data$provenance$interactive_surface_layer_count <- sum(vapply(
      report_data$surface$scenes,
      function(group) group$layer_count,
      integer(1)
    ))
    report_data$provenance$interactive_surface_projection <-
      report_data$surface$projection
    surface_summary <- report_data$surface$summary
    report_data$provenance$interactive_surface_packaging <- paste0(
      surface_summary$packaging, " / ", surface_summary$compression
    )
    report_data$provenance$interactive_surface_asset_count <-
      surface_summary$asset_count
    report_data$provenance$interactive_surface_compressed_bytes <-
      surface_summary$compressed_bytes
    report_data$provenance$interactive_surface_uncompressed_bytes <-
      surface_summary$uncompressed_bytes
    report_data$provenance$interactive_surface_runtime <- paste0(
      "surfview ", report_data$surface$runtime$version,
      " / neurosurf ", report_data$surface$runtime$neurosurf_version,
      " / adapter ", report_data$surface$runtime$adapter_version
    )
    report_data$provenance$interactive_surface_runtime_sha256 <-
      report_data$surface$runtime$sha256
    report_data$provenance$interactive_surface_adapter_sha256 <-
      report_data$surface$runtime$adapter_sha256
    report_data$provenance$interactive_surface_transformations <- paste(
      unique(unlist(lapply(
        report_data$surface$assets, `[[`, "transformations"
      ))),
      collapse = ", "
    )
    report_data$provenance$interactive_surface_asset_hashes <- paste(vapply(
      report_data$surface$assets,
      function(asset) paste0(asset$asset_id, "=", asset$hash),
      character(1)
    ), collapse = "; ")
  }

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

  has_interactive_bundle <- !is.null(report_data$interactive) &&
    identical(report_data$interactive$summary$packaging, "bundle")
  has_surface_bundle <- !is.null(report_data$surface) &&
    identical(report_data$surface$summary$packaging, "bundle")
  output_format <- .montage_rmarkdown_output_format(
    ext,
    latex_engine,
    self_contained = !(has_interactive_bundle || has_surface_bundle)
  )
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

.montage_rmarkdown_output_format <- function(ext,
                                             latex_engine = "xelatex",
                                             self_contained = TRUE) {
  if (identical(ext, "html")) {
    return(rmarkdown::html_document(
      toc = TRUE,
      toc_float = TRUE,
      theme = "flatly",
      self_contained = isTRUE(self_contained),
      mathjax = NULL
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
                                         profiles = NULL,
                                         map_selector = "auto",
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
                                         provenance,
                                         surface_scene = NULL,
                                         surface = NULL) {
  if (!is.null(surface_scene) && !methods::is(surface_scene, "SurfaceScene")) {
    stop("'surface_scene' must be NULL or a neurosurf::SurfaceScene.",
         call. = FALSE)
  }
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
  profile_values <- if (isTRUE(render_volume) || isTRUE(render_surface) ||
                        !is.null(surface) ||
                        isTRUE(render_peaks) ||
                        .montage_policy_uses_fdr(manifest, policy)) {
    lapply(seq_len(nrow(manifest)), function(i) {
      .montage_manifest_stat_values(manifest, i)
    })
  } else {
    NULL
  }
  if (!is.null(profile_values)) {
    # Resolve geometry before masks, profiles, static images, or interactive
    # assets can hide a mismatch. The interactive MVP never restamps or
    # resamples a map, even if a direct stat_montage() caller opts into that
    # legacy behavior.
    if (isTRUE(render_volume) &&
        !identical(volume_args$on_mismatch, "restamp")) {
      .montage_validate_report_volume_geometry(
        manifest, background = bg, stat_maps = profile_values
      )
    }
    .validate_montage_group_sources(manifest, profile_values)
    profile_values <- .montage_profile_values_with_analysis_masks(
      manifest, profile_values
    )
  }
  manifest <- resolve_montage_profiles(
    manifest,
    profiles = profiles,
    map_values = profile_values
  )
  policy_stat_maps <- if (.montage_policy_uses_fdr(manifest, policy)) {
    profile_values
  } else {
    NULL
  }
  manifest <- resolve_montage_policy(
    manifest,
    policy = policy,
    empty = empty,
    stat_maps = policy_stat_maps
  )
  manifest <- .apply_montage_shared_profile_limits(manifest, policy)
  missing_layout <- setdiff(layout, names(manifest))
  if (length(missing_layout) > 0L) {
    stop(
      "'layout' column(s) not found in manifest: ",
      paste(missing_layout, collapse = ", "),
      call. = FALSE
    )
  }
  .validate_montage_group_layout(manifest, layout)

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

  groups <- .montage_analysis_groups(manifest)
  groups <- .attach_montage_group_views(groups, panels)
  surface_report <- .prepare_montage_surface_report(
    manifest = manifest,
    panels = panels,
    surfatlas = surfatlas,
    surface = surface,
    surface_args = surface_args
  )

  list(
    manifest = manifest,
    groups = groups,
    panels = panels,
    qc = qc,
    intro = narratives$intro,
    section_notes = narratives$section_notes,
    interludes = narratives$interludes,
    surface_scene = surface_scene,
    surface = surface_report,
    params = list(
      title = title,
      layout = layout,
      policy = policy,
      map_selector = map_selector,
      report_mode = "montage"
    ),
    provenance = provenance %||% .montage_report_provenance()
  )
}

.apply_montage_shared_profile_limits <- function(manifest, policy) {
  if (length(policy$cap_within) == 0L) return(manifest)
  for (key in unique(manifest$cap_key)) {
    rows <- which(
      manifest$cap_key == key & manifest$effective_display_mode == "continuous"
    )
    if (length(rows) < 2L) next
    if (!is.null(policy$cap)) {
      if (identical(manifest$effective_scale[[rows[[1L]]]], "diverging")) {
        center <- manifest$effective_center[[rows[[1L]]]]
        shared <- c(center - policy$cap, center + policy$cap)
      } else {
        shared <- c(min(manifest$effective_lower[rows]), policy$cap)
      }
    } else {
      shared <- c(
        min(manifest$effective_lower[rows]),
        max(manifest$effective_upper[rows])
      )
    }
    if ("effective_domain_lower" %in% names(manifest)) {
      shared[[1L]] <- max(
        shared[[1L]], max(manifest$effective_domain_lower[rows])
      )
    }
    if ("effective_domain_upper" %in% names(manifest)) {
      shared[[2L]] <- min(
        shared[[2L]], min(manifest$effective_domain_upper[rows])
      )
    }
    if (!all(is.finite(shared)) || shared[[1L]] >= shared[[2L]]) {
      stop(
        "Shared display limits are incompatible for cap group '", key, "'.",
        call. = FALSE
      )
    }
    manifest$effective_lower[rows] <- shared[[1L]]
    manifest$effective_upper[rows] <- shared[[2L]]
  }
  manifest
}

.validate_montage_group_layout <- function(manifest, layout) {
  if (length(layout) == 0L) return(invisible(TRUE))
  for (id in unique(manifest$analysis_id)) {
    rows <- manifest$analysis_id == id
    for (field in layout) {
      values <- unique(as.character(manifest[[field]][rows]))
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) > 1L) {
        stop(
          "Layout column '", field, "' must be invariant within analysis_id '",
          id, "'.",
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
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
  if (!identical(volume_args$on_mismatch, "restamp")) {
    .montage_validate_report_volume_geometry(manifest, bg, stat_maps)
  }
  support_masks <- .montage_render_support_masks(manifest, stat_maps)
  caps <- .montage_shared_caps(
    manifest, stat_maps, policy = policy, support_masks = support_masks
  )
  group_zlevels <- .montage_group_zlevels(
    manifest,
    stat_maps,
    support_masks,
    along = volume_args$along %||% 3L
  )

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    image_path <- file.path(image_dir, paste0(.safe_file_stem(map_id), "_volume.png"))
    cap_value <- caps[[manifest$cap_key[[i]]]]
    continuous <- identical(
      manifest$effective_display_mode[[i]], "continuous"
    )
    limits <- c(
      manifest$effective_lower[[i]], manifest$effective_upper[[i]]
    )
    base_args <- list(
      bg = bg,
      stat = stat_maps[[i]],
      threshold = manifest$effective_threshold[[i]],
      tail = manifest$effective_tail[[i]],
      signed = manifest$effective_signed[[i]],
      # A non-positive group cap (an entirely empty/all-zero cap group) is not a
      # valid color magnitude; pass NULL so the montage picks its own benign
      # default instead of failing the cap > 0 check (defeats empty = "warning").
      cap = if (!continuous && is.finite(cap_value) && cap_value > 0) {
        cap_value
      } else {
        NULL
      },
      limits = if (continuous) limits else NULL,
      support_mask = support_masks[[i]],
      zlevels = group_zlevels[[as.character(manifest$analysis_id[[i]])]],
      ov_cmap = .montage_volume_palette(
        manifest$effective_palette_family[[i]],
        manifest$effective_scale[[i]]
      ),
      ov_alpha_mode = manifest$effective_alpha_mode[[i]],
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
    panels[[map_id]]$volume <- .montage_volume_display_metadata(
      spec = result$display_spec,
      row = manifest[i, , drop = FALSE],
      support = support_masks[[i]]
    )
    panels[[map_id]]$volume$style <- result$style
  }

  .montage_share_group_volume_views(manifest, panels)
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
  surface_sources <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_surface_source_args(manifest, i)
  })
  stat_values <- lapply(surface_sources, function(source) {
    source$vals %||% source$stat
  })
  support_masks <- .montage_render_support_masks(manifest, stat_values)
  caps <- .montage_shared_caps(
    manifest, stat_values, policy = policy, support_masks = support_masks
  )

  for (i in seq_len(nrow(manifest))) {
    map_id <- as.character(manifest$map_id[[i]])
    group_cap <- caps[[manifest$cap_key[[i]]]]
    continuous <- identical(
      manifest$effective_display_mode[[i]], "continuous"
    )
    limits <- c(
      manifest$effective_lower[[i]], manifest$effective_upper[[i]]
    )
    source_args <- surface_sources[[i]]
    # surface_args (caller wins) can override threshold/tail/signed/cap; merge
    # before computing the cache key so it reflects the values actually rendered.
    base_args <- utils::modifyList(
      c(
        source_args,
        list(
          surfatlas = surfatlas,
          threshold = manifest$effective_threshold[[i]],
          tail = manifest$effective_tail[[i]],
          signed = manifest$effective_signed[[i]],
          # See the volume renderer: a non-positive cap (empty cap group) is not
          # a valid magnitude; pass NULL so surf_montage uses its benign default.
          cap = if (!continuous && is.finite(group_cap) && group_cap > 0) {
            group_cap
          } else {
            NULL
          },
          limits = if (continuous) limits else NULL,
          support_mask = support_masks[[i]],
          overlay_palette = .montage_surface_palette(
            manifest$effective_palette_family[[i]],
            manifest$effective_scale[[i]]
          ),
          width = width,
          height = height,
          res = res,
          title = manifest$label[[i]],
          subtitle = .montage_panel_subtitle(manifest[i, , drop = FALSE])
        )
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
      n_supra <- sum(.montage_display_mask(
        as.numeric(stat_values[[i]]),
        display_mode = manifest$effective_display_mode[[i]],
        threshold = eff_threshold,
        tail = eff_tail,
        support_mask = base_args$support_mask
      ), na.rm = TRUE)
      if (n_supra == 0L) {
        # A panel cached under empty = "warning" must still abort under a later
        # empty = "error" request (the PNG carries no policy of its own).
        msg <- if (continuous) {
          paste0(
            "No finite display ",
            if (!is.null(source_args$vals)) "parcels" else "voxels",
            " for the continuous overlay."
          )
        } else {
          paste0(
            "No finite suprathreshold ",
            if (!is.null(source_args$vals)) "parcels" else "voxels",
            " for threshold ", eff_threshold,
            " and tail '", eff_tail, "'."
          )
        }
        if (identical(eff_empty, "error")) {
          stop(msg, call. = FALSE)
        }
        warning(msg, call. = FALSE)
      }
      panels[[map_id]]$surface_image <- normalizePath(image_path, mustWork = TRUE)
      panels[[map_id]]$surface <- list(
        threshold = eff_threshold,
        display_mode = manifest$effective_display_mode[[i]],
        tail = eff_tail,
        signed = base_args$signed,
        cap = eff_cap,
        limits = .montage_cached_surface_limits(
          limits = base_args$limits,
          cap = eff_cap,
          signed = base_args$signed,
          tail = eff_tail,
          fallback = c(
            manifest$effective_lower[[i]], manifest$effective_upper[[i]]
          )
        ),
        palette = base_args$overlay_palette,
        alpha = if (!is.null(source_args$vals)) {
          1
        } else {
          base_args$overlay_alpha %||% 0.85
        },
        support = manifest$effective_support[[i]],
        n_suprathreshold = n_supra,
        surface_space = NA_character_,
        views = base_args$views %||% c("lateral", "medial"),
        hemis = base_args$hemis %||% c("left", "right"),
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
      display_mode = result$display_mode,
      tail = result$tail,
      signed = base_args$signed,
      cap = result$cap,
      limits = result$limits,
      palette = base_args$overlay_palette,
      alpha = if (!is.null(source_args$vals)) {
        1
      } else {
        base_args$overlay_alpha %||% 0.85
      },
      support = manifest$effective_support[[i]],
      n_suprathreshold = result$n_suprathreshold,
      surface_space = result$surface_space,
      views = result$views,
      hemis = result$hemis,
      diagnostics = result$diagnostics
    )
  }

  panels
}

.montage_cached_surface_limits <- function(limits,
                                           cap,
                                           signed,
                                           tail,
                                           fallback) {
  if (!is.null(limits) && length(limits) == 2L && all(is.finite(limits))) {
    return(as.numeric(limits))
  }
  if (!is.null(cap) && length(cap) == 1L && is.finite(cap) && cap > 0) {
    if (isTRUE(signed)) return(c(-cap, cap))
    if (identical(tail, "negative")) return(c(-cap, 0))
    return(c(0, cap))
  }
  as.numeric(fallback)
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
  if ("parcel_values" %in% names(manifest) &&
      !.missing_column_values(manifest$parcel_values)[[row]]) {
    stop(
      "Cannot render a volumetric montage for map_id '", manifest$map_id[[row]],
      "' from 'parcel_values'. Provide 'path', 'recipe', or 'stat_map', ",
      "or set render_volume/render_peaks to FALSE.",
      call. = FALSE
    )
  }
  stop(
    "Cannot render volume montage for map_id '", manifest$map_id[[row]],
    "': no path, recipe, or stat_map is available.",
    call. = FALSE
  )
}

.montage_manifest_stat_values <- function(manifest, row) {
  if ("parcel_values" %in% names(manifest) &&
      !.missing_column_values(manifest$parcel_values)[[row]]) {
    return(.manifest_row_parcel_values(manifest, row))
  }
  .montage_manifest_stat_source(manifest, row)
}

.montage_manifest_surface_source_args <- function(manifest, row) {
  if ("parcel_values" %in% names(manifest) &&
      !.missing_column_values(manifest$parcel_values)[[row]]) {
    return(list(vals = .manifest_row_parcel_values(manifest, row)))
  }
  list(stat = .montage_manifest_stat_source(manifest, row))
}

.montage_render_support_masks <- function(manifest, sources) {
  groups <- .montage_analysis_groups(manifest)
  lapply(seq_len(nrow(manifest)), function(i) {
    values <- as.numeric(sources[[i]])
    group <- groups[[as.character(manifest$analysis_id[[i]])]]
    primary_row <- match(group$primary_map_id, manifest$map_id)
    requested <- manifest$effective_support[[i]]
    if (identical(requested, "primary")) {
      primary_values <- as.numeric(sources[[primary_row]])
      if (length(primary_values) != length(values)) {
        stop(
          "Primary support cannot be shared across different map sizes in ",
          "analysis_id '", manifest$analysis_id[[i]], "'.",
          call. = FALSE
        )
      }
      primary_analysis_mask <- if (.montage_row_has_mask(
        manifest, primary_row
      )) {
        .montage_manifest_mask_values(
          manifest, primary_row, reference = sources[[primary_row]]
        )
      } else {
        NULL
      }
      primary_support <- .montage_display_mask(
        primary_values,
        display_mode = manifest$effective_display_mode[[primary_row]],
        threshold = manifest$effective_threshold[[primary_row]],
        tail = manifest$effective_tail[[primary_row]],
        support_mask = primary_analysis_mask
      )
      target_mask_row <- if (.montage_row_has_mask(manifest, i)) {
        i
      } else {
        primary_row
      }
      if (.montage_row_has_mask(manifest, target_mask_row)) {
        target_mask <- .montage_manifest_mask_values(
          manifest, target_mask_row, reference = sources[[i]]
        )
        primary_support <- primary_support &
          .normalize_montage_support_mask(target_mask, length(values))
      }
      return(primary_support)
    }

    mask_row <- if (.montage_row_has_mask(manifest, i)) i else primary_row
    if (!.montage_row_has_mask(manifest, mask_row)) return(NULL)
    mask <- .montage_manifest_mask_values(
      manifest, mask_row, reference = sources[[i]]
    )
    if (length(mask) != length(values)) {
      stop(
        "Analysis mask size does not match map_id '", manifest$map_id[[i]],
        "'.",
        call. = FALSE
      )
    }
    .normalize_montage_support_mask(mask, length(values))
  })
}

.montage_profile_values_with_analysis_masks <- function(manifest, sources) {
  groups <- .montage_analysis_groups(manifest)
  lapply(seq_len(nrow(manifest)), function(i) {
    group <- groups[[as.character(manifest$analysis_id[[i]])]]
    primary_row <- match(group$primary_map_id, manifest$map_id)
    mask_row <- if (.montage_row_has_mask(manifest, i)) i else primary_row
    if (!.montage_row_has_mask(manifest, mask_row)) return(sources[[i]])

    values <- as.numeric(sources[[i]])
    mask <- .montage_manifest_mask_values(
      manifest, mask_row, reference = sources[[i]]
    )
    if (length(mask) != length(values)) {
      stop(
        "Analysis mask size does not match map_id '", manifest$map_id[[i]],
        "'.",
        call. = FALSE
      )
    }
    values[!.normalize_montage_support_mask(mask, length(values))] <- NA_real_
    values
  })
}

.montage_row_has_mask <- function(manifest, row) {
  "mask" %in% names(manifest) &&
    !.missing_column_values(manifest$mask)[[row]]
}

.montage_manifest_mask_values <- function(manifest, row, reference = NULL) {
  mask <- .montage_manifest_mask_source(manifest, row)
  if (methods::is(mask, "NeuroVol") &&
      methods::is(reference, "NeuroVol") &&
      !.same_neuro_space(neuroim2::space(mask), neuroim2::space(reference))) {
    stop(
      "Analysis mask geometry does not match map_id '",
      manifest$map_id[[row]], "'.",
      call. = FALSE
    )
  }
  as.numeric(mask)
}

.montage_display_mask <- function(values,
                                  display_mode,
                                  threshold,
                                  tail,
                                  support_mask = NULL) {
  mask <- if (identical(display_mode, "thresholded")) {
    .suprathreshold_mask(values, threshold = threshold, tail = tail)
  } else {
    is.finite(values)
  }
  if (!is.null(support_mask)) {
    mask <- mask & .normalize_montage_support_mask(
      support_mask, length(values)
    )
  }
  mask
}

.montage_group_zlevels <- function(manifest, stat_maps, support_masks,
                                   along = 3L) {
  groups <- .montage_analysis_groups(manifest)
  stats::setNames(lapply(groups, function(group) {
    row <- match(group$primary_map_id, manifest$map_id)
    stat <- stat_maps[[row]]
    if (!methods::is(stat, "NeuroVol")) return(NULL)
    mask <- .montage_display_mask(
      as.numeric(stat),
      display_mode = manifest$effective_display_mode[[row]],
      threshold = manifest$effective_threshold[[row]],
      tail = manifest$effective_tail[[row]],
      support_mask = support_masks[[row]]
    )
    .stat_montage_zlevels(array(mask, dim = dim(stat)), along = along)
  }), names(groups))
}

.montage_volume_palette <- function(palette_family, scale) {
  if (identical(palette_family, "diverging")) return("blue-red")
  if (identical(palette_family, "sequential")) return("inferno")
  palette_family %||% if (identical(scale, "diverging")) {
    "blue-red"
  } else {
    "inferno"
  }
}

.montage_surface_palette <- function(palette_family, scale) {
  if (identical(palette_family, "diverging")) return("vik")
  if (identical(palette_family, "sequential")) return("inferno")
  palette_family %||% if (identical(scale, "diverging")) "vik" else "inferno"
}

.attach_montage_group_views <- function(groups, panels) {
  lapply(groups, function(group) {
    primary <- panels[[group$primary_map_id]] %||% list()
    # Use exact list indexing: `$volume` partially matches the public
    # `volume_image` field when no rendered-volume metadata is present.
    volume <- primary[["volume"]]
    surface <- primary[["surface"]]
    group$view <- list(
      zlevels = if (is.list(volume)) volume[["zlevels"]] else NULL,
      zlevel_bookmarks = if (is.list(volume)) {
        volume[["zlevel_bookmarks"]]
      } else {
        NULL
      },
      initial_world_coord = if (is.list(volume)) {
        volume[["initial_world_coord"]]
      } else {
        NULL
      },
      surface_views = if (is.list(surface)) surface[["views"]] else NULL,
      surface_hemis = if (is.list(surface)) surface[["hemis"]] else NULL
    )
    group
  })
}

.montage_shared_caps <- function(manifest, stat_maps, policy = NULL,
                                 support_masks = NULL) {
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
      cap_floor = cap_floor,
      support_masks = support_masks
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
                               cap_floor, support_masks = NULL) {
  supra <- unlist(lapply(rows, function(r) {
    values <- as.numeric(stat_maps[[r]])
    mask <- .montage_display_mask(
      values = values,
      display_mode = manifest$effective_display_mode[[r]],
      threshold = manifest$effective_threshold[[r]],
      tail = manifest$effective_tail[[r]],
      support_mask = if (is.null(support_masks)) NULL else support_masks[[r]]
    )
    abs(values[mask])
  }), use.names = FALSE)
  supra <- supra[is.finite(supra)]

  cap <- if (length(supra) > 0L) {
    as.numeric(stats::quantile(supra, probs = cap_quantile, names = FALSE,
                               type = 7))
  } else {
    values <- unlist(lapply(rows, function(r) {
      values <- as.numeric(stat_maps[[r]])
      if (!is.null(support_masks) && !is.null(support_masks[[r]])) {
        values <- values[.normalize_montage_support_mask(
          support_masks[[r]], length(values)
        )]
      }
      values
    }), use.names = FALSE)
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
# surface PNG cache key changes whenever any of them does. The statistic source
# is covered by `map_hash`, and `surfatlas` by a cheap identity, so both are
# excluded from the (potentially large) hashed payload.
.montage_surface_style_key <- function(base_args, surfatlas) {
  payload <- base_args[setdiff(
    names(base_args), c("stat", "vals", "surfatlas", "output_file", "empty")
  )]
  payload$.surfatlas <- list(
    name = surfatlas$name %||% NA_character_,
    surface_space = surfatlas$surface_space %||% NA_character_,
    surf_type = surfatlas$surf_type %||% NA_character_,
    cortex_mask_source = surfatlas$cortex_mask_source %||% NA_character_,
    cortex_mask_hash = if (!is.null(surfatlas$cortex_mask)) {
      rlang::hash(surfatlas$cortex_mask)
    } else {
      NA_character_
    },
    anatomy_metric_source = surfatlas$anatomy_metric_source %||% NA_character_,
    anatomy_metric_surface = surfatlas$anatomy_metric_surface %||% NA_character_,
    anatomy_metric_hash = if (!is.null(surfatlas$anatomy_metric)) {
      rlang::hash(surfatlas$anatomy_metric)
    } else {
      NA_character_
    }
  )
  # Bump when an implicit rendering/projection default changes. Explicit
  # surface_args are already in payload; this version prevents a new release
  # from reusing pixels produced under an older default contract.
  payload$.surface_contract_version <- 2L
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

  report_data <- .montage_qmd_portable_report_data(
    report_data, output_file
  )
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

.montage_qmd_portable_report_data <- function(report_data, output_file) {
  output_dir <- normalizePath(dirname(output_file), mustWork = TRUE,
                              winslash = "/")
  companion <- paste0(
    tools::file_path_sans_ext(basename(output_file)), "_files"
  )

  # Rendering has already materialized every static panel and interactive
  # asset. Source maps, recipes, masks, and parcel vectors are neither needed
  # by the QMD template nor safe/portable to retain in its render sidecar.
  source_fields <- intersect(
    c("path", "recipe", "stat_map", "space", "template", "mask",
      "parcel_values"),
    names(report_data$manifest)
  )
  if (length(source_fields) > 0L) {
    report_data$manifest[source_fields] <- NULL
  }

  image_fields <- c(
    "volume_image", "volume_image_path",
    "surface_image", "surface_image_path"
  )
  report_data$panels <- lapply(report_data$panels, function(panel) {
    for (field in intersect(image_fields, names(panel))) {
      value <- panel[[field]]
      if (is.character(value) && length(value) == 1L &&
          !is.na(value) && nzchar(value)) {
        panel[[field]] <- .montage_qmd_portable_image(
          value, output_dir = output_dir, companion = companion
        )
      }
    }
    panel
  })

  if (!is.null(report_data$interactive) &&
      length(report_data$interactive$bundle_files) > 0L) {
    report_data$interactive$bundle_files <- unname(vapply(
      report_data$interactive$bundle_files,
      .montage_qmd_relative_path,
      character(1),
      output_dir = output_dir,
      label = "interactive bundle asset"
    ))
  }
  report_data
}

.montage_qmd_portable_image <- function(path, output_dir, companion) {
  source <- .montage_qmd_existing_path(path, output_dir, "panel image")
  relative <- .montage_path_below(source, output_dir)
  if (!is.null(relative)) return(relative)

  image_dir <- file.path(output_dir, companion, "qmd-images")
  if (!dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE)
  }
  digest <- substr(unname(tools::md5sum(source)), 1L, 12L)
  filename <- paste0(digest, "-", .safe_file_stem(basename(source)))
  destination <- file.path(image_dir, filename)
  if (!file.exists(destination) && !file.copy(source, destination)) {
    stop("Could not copy QMD panel image '", source, "'.", call. = FALSE)
  }
  file.path(companion, "qmd-images", filename)
}

.montage_qmd_relative_path <- function(path, output_dir, label) {
  source <- .montage_qmd_existing_path(path, output_dir, label)
  relative <- .montage_path_below(source, output_dir)
  if (is.null(relative)) {
    stop("QMD ", label, " is outside the report directory: ", source,
         call. = FALSE)
  }
  relative
}

.montage_qmd_existing_path <- function(path, output_dir, label) {
  candidates <- c(path, file.path(output_dir, path))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    stop("QMD ", label, " does not exist: ", path, call. = FALSE)
  }
  normalizePath(existing[[1L]], mustWork = TRUE, winslash = "/")
}

.montage_path_below <- function(path, directory) {
  prefix <- paste0(sub("/+$", "", directory), "/")
  if (!startsWith(path, prefix)) return(NULL)
  gsub("\\\\", "/", substring(path, nchar(prefix) + 1L))
}
