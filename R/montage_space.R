# Space configuration and map-set resolution.

#' Configure a Space comparison view
#'
#' Pass this object as `space` to [render_montage_report()]. Space opens lazily
#' from the report overview and links a selected member to its exact report
#' map. Its display controls are exploratory and leave static report results
#' unchanged. The HTML and its companion `_files` folder must be served from
#' the same HTTP(S) origin. PDF output retains a member list.
#'
#' Each set uses raw map values within the row's analysis mask (or its primary
#' map's mask), before display thresholding. Maps, template, explicit set mask,
#' and optional atlas must share a three-dimensional positive-diagonal RAS+
#' grid. No implicit alignment or resampling is performed. Values are stored
#' losslessly in the bundle and converted to Float32 by Space, with overflow
#' checks. Distances are descriptive spatial correlations, not significance
#' tests. Only one interactive viewer is active while Space is open.
#'
#' @param sets One map set or a list of map sets.
#' @param view Initial Space view.
#' @param initial_set Optional set id.
#' @param statistic Optional initial t, z, or estimate channel.
#' @param world_coord Optional xyz coordinate.
#' @param height Optional positive height in CSS pixels.
#' @param max_data_mb Maximum uncompressed typed-array input per set in MiB.
#'   This is a payload guard, not a bound on browser memory.
#' @return A montage_space value object.
#' @md
#' @export
montage_space <- function(sets, view = c("space", "montage", "facet", "ortho"),
                          initial_set = NULL, statistic = NULL, world_coord = NULL,
                          height = NULL, max_data_mb = 256) {
  sets <- .montage_space_sets(sets)
  ids <- vapply(sets, function(x) x$id, character(1))
  view <- match.arg(view)
  if (!is.null(initial_set)) {
    initial_set <- .montage_map_set_character(initial_set, "initial_set")
    if (!initial_set %in% ids) stop("'initial_set' must name one supplied set.", call. = FALSE)
  }
  if (!is.null(statistic)) {
    statistic <- .montage_map_set_character(statistic, "statistic")
    if (!statistic %in% c("t", "z", "estimate")) {
      stop("'statistic' must be NULL, 't', 'z', or 'estimate'.", call. = FALSE)
    }
  }
  if (!is.null(world_coord) && (!is.numeric(world_coord) || length(world_coord) != 3L ||
      anyNA(world_coord) || any(!is.finite(world_coord)))) {
    stop("'world_coord' must be NULL or three finite numeric values.", call. = FALSE)
  }
  if (!is.null(height) && (!is.numeric(height) || length(height) != 1L ||
      !is.finite(height) || height <= 0)) stop("'height' must be a positive number.", call. = FALSE)
  if (!is.numeric(max_data_mb) || length(max_data_mb) != 1L ||
      !is.finite(max_data_mb) || max_data_mb <= 0) {
    stop("'max_data_mb' must be a positive finite number.", call. = FALSE)
  }
  structure(list(sets = sets, view = view, initial_set = initial_set %||% ids[[1]],
                 statistic = statistic,
                 world_coord = if (is.null(world_coord)) NULL else as.numeric(world_coord),
                 height = height, max_data_mb = as.numeric(max_data_mb)),
            class = "montage_space")
}

.montage_space_sets <- function(sets) {
  if (inherits(sets, "montage_map_set")) sets <- list(sets)
  if (!is.list(sets) || !length(sets) || is.data.frame(sets) ||
      !all(vapply(sets, inherits, logical(1), what = "montage_map_set"))) {
    stop("'sets' must be one montage_map_set or a non-empty list of them.", call. = FALSE)
  }
  ids <- vapply(sets, function(x) x$id, character(1))
  if (anyDuplicated(ids)) stop("Map-set ids must be unique.", call. = FALSE)
  unname(sets)
}

#' Validate and resolve Space map sets
#'
#' @param manifest Render manifest.
#' @param sets Space or map-set configuration.
#' @param bg Optional report background.
#' @param load_maps Whether to load raw map data.
#' @return A list with normalized `sets`, `manifest`, `space`, and `checks`.
#'   With `load_maps = TRUE`, sets include loaded volumes and geometry; this
#'   can require substantial memory. Recipes must already be materialized.
#' @md
#' @export
validate_montage_map_sets <- function(manifest, sets, bg = NULL, load_maps = FALSE) {
  .montage_resolve_map_sets(manifest, sets, bg, load_maps)
}

.montage_resolve_map_sets <- function(manifest, sets, bg = NULL, load_maps = FALSE) {
  manifest <- validate_manifest(manifest, load_maps = FALSE, check_files = FALSE,
                                check_overlays = FALSE)
  space <- if (inherits(sets, "montage_space")) sets else montage_space(sets)
  resolved <- lapply(space$sets, .montage_resolve_one_map_set, manifest = manifest,
                     bg = bg, load_maps = load_maps)
  if (isTRUE(load_maps)) for (set in resolved) {
    if (set$payload_bytes > space$max_data_mb * 1024^2) {
      stop("Space set '", set$id, "': uncompressed input exceeds max_data_mb (",
           round(set$payload_bytes / 1024^2, 2), " MiB).", call. = FALSE)
    }
  }
  names(resolved) <- vapply(resolved, function(x) x$id, character(1))
  initial <- resolved[[space$initial_set]]
  if (!is.null(space$statistic) && !space$statistic %in% initial$common_channels[[1]]) {
    stop("Space statistic '", space$statistic, "' is not common to every member of initial set '",
         initial$id, "'.", call. = FALSE)
  }
  if (!is.null(space$world_coord) && isTRUE(load_maps) &&
      !.montage_world_coord_in_geometry(space$world_coord, initial$geometry)) {
    stop("'world_coord' is outside the initial map-set grid.", call. = FALSE)
  }
  list(sets = resolved, manifest = manifest, space = space,
       checks = list(references = TRUE, semantics = TRUE, maps_loaded = isTRUE(load_maps),
                     payload_limit = isTRUE(load_maps)))
}

.montage_resolve_one_map_set <- function(set, manifest, bg, load_maps) {
  rows <- match(set$members$map_id, manifest$map_id)
  if (anyNA(rows)) stop("Map set '", set$id, "' references unknown map_id: ",
                        paste(set$members$map_id[is.na(rows)], collapse = ", "), ".", call. = FALSE)
  members <- set$members
  members$analysis_id <- as.character(manifest$analysis_id[rows])
  members$quantity <- as.character(manifest$quantity[rows])
  members$distribution <- as.character(manifest$distribution[rows])
  members$units <- if ("units" %in% names(manifest)) trimws(as.character(manifest$units[rows])) else NA_character_
  members$units[is.na(members$units) | !nzchar(members$units)] <- NA_character_
  members$channel <- mapply(.montage_map_set_channel, members$quantity, members$distribution,
                            members$map_id, MoreArgs = list(set_id = set$id), USE.NAMES = FALSE)
  tuple <- paste(members$member_id, members$model, members$contrast, members$channel, sep = "\r")
  if (anyDuplicated(tuple)) stop("Map set '", set$id,
                                 "' repeats (member_id, model, contrast, channel).", call. = FALSE)
  members <- .montage_map_set_labels(members, manifest, rows, set$id, set$member_kind)
  .montage_map_set_semantics(members, set$id)
  out <- list(id = set$id, label = set$label, member_kind = set$member_kind,
              members = members, mask = set$mask, template = set$template,
              atlas = set$atlas, covariates = set$covariates,
              common_channels = .montage_map_set_common_channels(members, set$id))
  if (isTRUE(load_maps)) out <- .montage_map_set_load(out, manifest, rows, bg)
  out
}

.montage_map_set_labels <- function(members, manifest, rows, set_id, member_kind) {
  for (id in unique(members$member_id)) {
    ix <- which(members$member_id == id)
    supplied <- unique(members$label[ix][!is.na(members$label[ix])])
    if (length(supplied) > 1L) stop("Map set '", set_id,
                                    "' gives inconsistent labels for member_id '", id, "'.", call. = FALSE)
    members$label[ix] <- if (length(supplied)) supplied else if (identical(member_kind, "subject")) {
      id
    } else if (length(ix) == 1L) {
      as.character(manifest$label[[rows[ix]]])
    } else id
  }
  members
}

.montage_map_set_semantics <- function(members, set_id) {
  se_rows <- which(members$channel == "standard_error")
  for (i in se_rows) {
    same <- members$member_id == members$member_id[[i]] &
      members$model == members$model[[i]] & members$contrast == members$contrast[[i]] &
      members$channel == "estimate"
    if (!any(same)) stop("Map set '", set_id, "' standard_error map_id '",
                         members$map_id[[i]], "' lacks an estimate in its tuple.", call. = FALSE)
    eu <- members$units[which(same)[[1]]]; su <- members$units[[i]]
    if (!is.na(eu) && !is.na(su) && !identical(eu, su)) {
      stop("Map set '", set_id, "' standard_error map_id '", members$map_id[[i]],
           "' has units incompatible with its estimate.", call. = FALSE)
    }
  }
  groups <- unique(paste(members$model, members$contrast, members$channel, sep = "\r"))
  for (key in groups) {
    ix <- which(paste(members$model, members$contrast, members$channel, sep = "\r") == key)
    known <- unique(members$units[ix][!is.na(members$units[ix])])
    if (length(known) > 1L) {
      stop("Map set '", set_id,
           "' has incompatible declared units within a model/contrast/channel comparison.",
           call. = FALSE)
    }
  }
}

.montage_map_set_common_channels <- function(members, set_id) {
  group <- paste(members$model, members$contrast, sep = "\r")
  lapply(unique(group), function(key) {
    ix <- which(group == key)
    per_member <- split(members$channel[ix], members$member_id[ix])
    common <- Reduce(intersect, lapply(per_member, intersect, c("t", "z", "estimate")))
    common <- c("t", "z", "estimate")[c("t", "z", "estimate") %in% common]
    if (!length(common)) stop("Map set '", set_id,
                              "' model/contrast group has no channel common to every member.", call. = FALSE)
    common
  })
}

.montage_map_set_analysis_mask_row <- function(manifest, row) {
  if (.montage_row_has_mask(manifest, row)) return(row)
  group <- .montage_analysis_groups(manifest)[[as.character(manifest$analysis_id[[row]])]]
  primary <- match(group$primary_map_id, manifest$map_id)
  if (.montage_row_has_mask(manifest, primary)) primary else NULL
}

.montage_map_set_load <- function(out, manifest, rows, bg) {
  volumes <- lapply(rows, function(row) .montage_map_set_source(manifest, row))
  geometry <- .montage_map_set_ras_geometry(volumes[[1]], "first map", out$id)
  for (i in seq_along(volumes)) {
    .montage_map_set_same_geometry(geometry,
      .montage_map_set_ras_geometry(volumes[[i]], paste0("map_id '", out$members$map_id[[i]], "'"), out$id),
      paste0("map_id '", out$members$map_id[[i]], "'"), out$id)
  }
  mask <- .montage_map_set_read_volume(out$mask, "mask", out$id)
  .montage_map_set_same_geometry(geometry, .montage_map_set_ras_geometry(mask, "mask", out$id), "mask", out$id)
  mask_data <- as.numeric(mask)
  if (any(!is.finite(mask_data)) || any(!mask_data %in% c(0, 1)) || !any(mask_data > .5)) {
    stop("Map set '", out$id, "' mask must be finite, binary, and nonempty above 0.5.", call. = FALSE)
  }
  template <- out$template %||% bg
  if (is.null(template)) stop("Map set '", out$id, "' needs template or report bg when maps are loaded.", call. = FALSE)
  template <- .montage_map_set_read_volume(template, "template", out$id)
  .montage_map_set_same_geometry(geometry, .montage_map_set_ras_geometry(template, "template", out$id), "template", out$id)
  template_data <- as.numeric(template)
  if (any(!is.finite(template_data))) stop("Map set '", out$id, "' template must be finite.", call. = FALSE)
  atlas <- NULL
  if (!is.null(out$atlas)) {
    atlas <- .montage_map_set_read_volume(out$atlas$labels, "atlas labels", out$id)
    .montage_map_set_same_geometry(geometry,
      .montage_map_set_ras_geometry(atlas, "atlas", out$id), "atlas", out$id)
    atlas_values <- as.numeric(atlas)
    if (any(!is.finite(atlas_values)) || any(atlas_values < 0) ||
        any(atlas_values > .Machine$integer.max) ||
        any(atlas_values != floor(atlas_values))) {
      stop("Map set '", out$id, "' atlas labels must be non-negative int32 values.",
           call. = FALSE)
    }
    observed <- unique(atlas_values[atlas_values != 0])
    region_ids <- as.numeric(out$atlas$regions$id)
    if (any(!observed %in% region_ids)) {
      stop("Map set '", out$id, "' atlas regions do not cover nonzero label ids.",
           call. = FALSE)
    }
  }
  source_provenance <- vector("list", length(volumes))
  data <- lapply(seq_along(volumes), function(i) {
    values <- as.numeric(volumes[[i]])
    source_provenance[[i]] <<- list(
      source = .montage_map_set_content_hash(values, geometry), analysis_mask = NULL)
    if (any(is.infinite(values))) {
      stop("Map set '", out$id, "' map_id '", out$members$map_id[[i]],
           "' contains infinite values.", call. = FALSE)
    }
    mask_row <- .montage_map_set_analysis_mask_row(manifest, rows[[i]])
    if (!is.null(mask_row)) {
      analysis_mask <- .montage_manifest_mask_source(manifest, mask_row)
      if (methods::is(analysis_mask, "NeuroVol")) {
        .montage_map_set_same_geometry(geometry,
          .montage_map_set_ras_geometry(analysis_mask, "analysis mask", out$id), "analysis mask", out$id)
      }
      support <- .normalize_montage_support_mask(analysis_mask, length(values))
      source_provenance[[i]]$analysis_mask <<- list(
        map_id = as.character(manifest$map_id[[mask_row]]),
        inherited = mask_row != rows[[i]],
        support = .montage_map_set_content_hash(as.numeric(support), geometry))
      values[!support] <- NA_real_
    }
    if (out$members$channel[[i]] == "standard_error" && any(values[is.finite(values)] < 0)) {
      stop("Map set '", out$id, "' standard_error map_id '", out$members$map_id[[i]],
           "' has negative finite values.", call. = FALSE)
    }
    .montage_map_set_float32(values, paste0("map_id '", out$members$map_id[[i]], "'"))
  })
  out$members$volume <- lapply(seq_along(volumes), function(i) neuroim2::NeuroVol(
    array(data[[i]], dim = dim(volumes[[i]])), space = neuroim2::space(volumes[[i]])))
  out$members$data <- data
  out$members$geometry <- rep(list(geometry), nrow(out$members))
  out$members$provenance <- source_provenance
  out$mask_provenance <- .montage_map_set_content_hash(mask_data, geometry)
  out$mask_volume <- mask; out$mask_data <- .montage_map_set_float32(mask_data, "mask")
  out$template_volume <- template; out$template_data <- .montage_map_set_float32(template_data, "template")
  out$atlas_volume <- atlas
  out$atlas_data <- if (is.null(atlas)) NULL else as.integer(as.numeric(atlas))
  out$geometry <- geometry
  out$payload_bytes <- 4 * (sum(vapply(data, length, integer(1))) + length(template_data) +
    if (is.null(atlas)) 0L else length(atlas)) + length(mask_data)
  out
}

# Hash the loaded scientific input before support masking or Float32 conversion.
# No source paths or participant metadata are retained in report provenance.
.montage_map_set_content_hash <- function(values, geometry) {
  list(algorithm = "sha256", scope = "loaded values and grid; R serialization v2",
       hash = digest::digest(list(dimensions = as.integer(geometry$dimensions),
         affine = as.numeric(geometry$affine), values = as.numeric(values)),
         algo = "sha256", serialize = TRUE, serializeVersion = 2))
}
