# Packaging and progressive enhancement for map-set comparisons.

.prepare_montage_space_report <- function(manifest, space, background,
                                           output_file, is_html = TRUE) {
  if (is.null(space)) return(NULL)
  if (!inherits(space, "montage_space")) {
    stop("'space' must be created by montage_space().", call. = FALSE)
  }
  resolved <- .montage_resolve_map_sets(manifest, space, background,
                                       load_maps = TRUE)
  sets <- resolved$sets
  groups <- .montage_analysis_groups(resolved$manifest)
  # Capability checks are separate from the renderer-neutral membership API.
  for (set in sets) {
    prefix <- paste0("Space set '", set$id, "': ")
    if (!identical(set$member_kind, "subject")) {
      stop(prefix, "this runtime supports subject sets; generic map labels ",
           "require a newer upstream Space interface.", call. = FALSE)
    }
    if (any(set$members$channel == "z")) {
      stop(prefix, "z statistics are not supported by the Space 0.6 interface.",
           call. = FALSE)
    }
    comparisons <- unique(set$members[c("model", "contrast")])
    if (nrow(comparisons) != 1L) {
      stop(prefix, "the first integration supports one model/contrast per set; ",
           "create separate sets for these comparisons.", call. = FALSE)
    }
  }
  root_ref <- paste0(tools::file_path_sans_ext(basename(output_file)),
                     "_files/space")
  root <- file.path(dirname(output_file), root_ref)
  files <- character()
  emit_asset <- function(volume, kind = "overlay") {
    asset <- .montage_materialize_volume_asset(
      volume, kind = kind, output_dir = file.path(root, "volumes"),
      ref_prefix = paste0(root_ref, "/volumes"))
    files <<- c(files, asset$path)
    asset$asset
  }
  scenes <- lapply(sets, function(set) {
    rows <- set$members
    member_ids <- unique(rows$member_id)
    channels <- Reduce(intersect, lapply(member_ids, function(id) {
      rows$channel[rows$member_id == id & rows$channel != "standard_error"]
    }))
    common <- intersect(c("t", "z", "estimate"), channels)
    statistic <- common[[1L]]
    if (identical(set$id, space$initial_set) && !is.null(space$statistic)) {
      if (!space$statistic %in% common) {
        stop("Space set '", set$id, "': requested statistic is not common to ",
             "all members.", call. = FALSE)
      }
      statistic <- space$statistic
    }
    bindings <- vector("list", nrow(rows))
    members <- lapply(seq_along(member_ids), function(j) {
      indices <- which(rows$member_id == member_ids[[j]])
      first <- indices[[1L]]
      member <- list(id = paste0("member-", j), subject = member_ids[[j]],
                     model = rows$model[[first]], contrast = rows$contrast[[first]])
      for (i in indices) {
        channel <- rows$channel[[i]]
        key <- switch(channel, estimate = "beta", standard_error = "se", channel)
        if (is_html) member[[key]] <- emit_asset(rows$volume[[i]])
        bindings[[i]] <<- list(map_id = rows$map_id[[i]],
          analysis_id = rows$analysis_id[[i]], member_id = member$id,
          subject = member$subject, label = rows$label[[i]],
          model = member$model, contrast = member$contrast, channel = channel,
          report_anchor = if (is_html && length(groups[[rows$analysis_id[[i]]]]$map_ids) > 1L) {
            .montage_report_dom_id("nm-variant", rows$analysis_id[[i]], rows$map_id[[i]])
          } else .montage_report_map_anchor(rows$analysis_id[[i]], rows$map_id[[i]]),
          provenance = rows$provenance[[i]])
      }
      member
    })
    affine <- matrix(set$geometry$affine, 4L, 4L)
    scene <- list(id = set$id, label = set$label, member_kind = set$member_kind,
      grid = list(dimensions = unname(set$geometry$dimensions),
                  affine = lapply(seq_len(4L), function(i) unname(affine[i, ]))),
      members = unname(members), bindings = bindings, common_channels = unname(common),
      payload_bytes = set$payload_bytes,
      comparison_mask = set$mask_provenance,
      initial_state = list(mode = space$view, model = rows$model[[1L]],
        contrast = rows$contrast[[1L]],
        statistic = if (statistic == "estimate") "beta" else statistic,
        worldCoord = if (identical(set$id, space$initial_set)) space$world_coord else NULL,
        threshold = 0))
    if (is_html) {
      scene$template <- emit_asset(set$template_volume, "background")
      scene$mask <- emit_asset(set$mask_volume, "mask")
      if (!is.null(set$atlas)) {
        scene$atlas <- list(name = set$atlas$name,
          labels = emit_asset(set$atlas_volume), regions = set$atlas$regions)
      }
    }
    if (!is.null(set$covariates)) {
      scene$covariates <- lapply(seq_len(nrow(set$covariates)), function(i) {
        list(subject = set$covariates$member_id[[i]], values = as.list(
          set$covariates[i, setdiff(names(set$covariates), "member_id"), drop = FALSE]))
      })
    }
    scene
  })
  runtime <- NULL
  if (is_html) {
    source <- system.file("htmlwidgets", "lib", "neuromosaic-space", "runtime",
                          package = "neuromosaic")
    if (!nzchar(source)) stop("Space runtime is not installed.", call. = FALSE)
    runtime <- jsonlite::fromJSON(file.path(source, "runtime.json"),
                                  simplifyVector = FALSE)
    relative <- vapply(runtime$files, `[[`, character(1), "path")
    if (any(grepl("(^/|(^|/)\\.\\.(/|$))", relative))) {
      stop("Invalid Space runtime file manifest.", call. = FALSE)
    }
    for (entry in runtime$files) {
      path <- file.path(source, entry$path)
      if (!file.exists(path) || !identical(
        digest::digest(file = path, algo = "sha256", serialize = FALSE), entry$sha256
      )) stop("Space runtime checksum mismatch: ", entry$path, call. = FALSE)
    }
    relative <- c(relative, "runtime.json")
    for (path in relative) {
      dest <- file.path(root, "runtime", path)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(file.path(source, path), dest, overwrite = TRUE)) {
        stop("Could not package Space runtime: ", path, call. = FALSE)
      }
      files <- c(files, normalizePath(dest, winslash = "/"))
    }
  }
  scene <- list(schema = "org.neuromosaic.map-set-scene", schema_version = 1L,
    sets = unname(scenes), initial_set = space$initial_set,
    runtime_script = paste0(root_ref, "/runtime/space-host.js"),
    runtime_base = paste0(root_ref, "/runtime/frame/"),
    provenance = list(runtime = runtime,
      adapter = list(version = 1L, files = lapply(c("montage-space.js", "montage-space.css"), function(name) {
        list(file = name, sha256 = digest::digest(file = system.file("report", name,
          package = "neuromosaic"), algo = "sha256", serialize = FALSE))
      })), comparison = "signed Pearson; 1 - r",
      support = "explicit mask; common finite voxels; finite zero retained",
      conversion = "Float32 at Space boundary; finite overflow rejected",
      display = "raw values before report display thresholds",
      interpretation = "descriptive; distances and clusters are not statistical inference"))
  structure(list(scene = scene, height = space$height,
    bundle_files = unique(files)), class = "montage_space_report")
}

#' Map-set Comparison Hooks for Report Templates
#'
#' Custom templates call `emit_document()` once and `emit_host()` once, after
#' their overview and before their per-map sections. Non-HTML output receives
#' a static member list. Source volumes and recipes are never stored in these
#' prepared hooks.
#' @param space Prepared report Space data, or `NULL`.
#' @param is_html Whether the template is emitting HTML.
#' @return A list of `emit_document()` and `emit_host()` functions.
#' @md
#' @export
montage_space_report_hooks <- function(space = NULL, is_html = FALSE) {
  if (!is.null(space) && !inherits(space, "montage_space_report")) {
    stop("'space' must be prepared montage Space data.", call. = FALSE)
  }
  list(emit_document = function() {
    if (is.null(space) || !isTRUE(is_html)) return(invisible(NULL))
    asset <- function(name) .montage_read_inline_asset(system.file(
      "report", name, package = "neuromosaic"))
    cat("<style>\n", asset("montage-space.css"), "\n</style>\n",
        '<script type="application/json" id="nm-space-scene">',
        .montage_volume_scene_json(space$scene), "</script>\n",
        "<script>\n", asset("montage-space.js"), "\n</script>\n", sep = "")
    invisible(NULL)
  }, emit_host = function() {
    if (is.null(space)) return(invisible(NULL))
    esc <- .montage_html_escape
    sets <- space$scene$sets
    cat("\n# Explore map sets\n\n")
    if (isTRUE(is_html)) {
      options <- vapply(sets, function(set) paste0('<option value="', esc(set$id),
        '"', if (set$id == space$scene$initial_set) ' selected' else '', '>',
        esc(set$label), "</option>"), character(1))
      height <- if (is.null(space$height)) "" else paste0(' style="height:',
        esc(as.character(space$height)), if (is.numeric(space$height)) 'px' else '', '"')
      cat('<section class="nm-space" data-nm-space-host aria-label="Map-set comparison">',
        '<div class="nm-space-controls"><label for="nm-space-set">Map set</label>',
        '<select id="nm-space-set" data-nm-space-set>', paste(options, collapse = ""),
        '</select><button type="button" data-nm-space-open aria-expanded="false" ',
        'aria-controls="nm-space-stage">Open comparison</button>',
        '<button type="button" data-nm-space-close hidden>Close comparison</button></div>',
        '<p data-nm-space-summary></p>',
        '<p class="nm-space-policy">Compare raw maps within the explicit mask. ',
        'Distances describe spatial similarity; they are not tests of significance. ',
        'Display controls here do not change the report figures.</p>',
        '<p data-nm-space-status role="status" aria-live="polite"></p>',
        '<div class="nm-space-selection" hidden><span data-nm-space-selection></span> ',
        '<button type="button" data-nm-space-show disabled>Show in report</button></div>',
        '<div id="nm-space-stage" data-nm-space-stage hidden', height, '></div>',
        '<noscript><p>JavaScript is required to open comparisons. ',
        'The member list and report maps remain available below.</p></noscript>', sep = "")
    }
    cat(if (isTRUE(is_html)) '<details class="nm-space-members" open><summary>Included maps</summary>\n\n' else "")
    for (set in sets) {
      cat(if (isTRUE(is_html)) paste0("<p><strong>", esc(set$label), "</strong></p><ul>") else paste0("**", set$label, "**\n\n"))
      for (binding in set$bindings) {
        label <- paste(binding$subject, binding$channel, binding$map_id, sep = " / ")
        cat(if (isTRUE(is_html)) paste0('<li><a href="#', esc(binding$report_anchor),
          '"', if (startsWith(binding$report_anchor, "nm-variant")) paste0(
            ' data-nm-goto-group="', esc(binding$analysis_id),
            '" data-nm-goto-map="', esc(binding$map_id), '"'), '>', esc(label), '</a></li>') else
          paste0("- [", label, "](#", binding$report_anchor, ")\n"))
      }
      cat(if (isTRUE(is_html)) "</ul>" else "\n")
    }
    if (isTRUE(is_html)) cat("</details></section>\n\n")
    invisible(NULL)
  })
}
