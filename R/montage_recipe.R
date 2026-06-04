#' Materialize Derived Montage Map Recipes
#'
#' Converts recipe-backed render-manifest rows into path-backed rows by
#' evaluating each recipe and writing the resulting `NeuroVol` to disk. Existing
#' cache files are reused by default, making derived maps testable and stable
#' across report renders.
#'
#' Recipe entries may be functions, structured lists, paths, or `NeuroVol`
#' objects. Function recipes are called with `row = <one-row manifest>` when
#' their formals include `row`, otherwise with no arguments. Structured recipes
#' are lists with a `fun` (or `function`) member and optional `args` and `key`
#' members.
#'
#' @param manifest A render manifest data frame.
#' @param cache_dir Directory for materialized recipe maps. Defaults to a
#'   session temp directory.
#' @param overwrite Logical; recompute recipe rows even when the cached file
#'   already exists?
#' @param validate Logical; run [validate_manifest()] after materialization?
#' @param check_files Logical; passed to [validate_manifest()].
#'
#' @return A render manifest with recipe rows converted to file-backed `path`
#'   rows and a `map_hash` column populated when possible.
#' @export
materialize_montage_recipes <- function(manifest,
                                        cache_dir = NULL,
                                        overwrite = FALSE,
                                        validate = TRUE,
                                        check_files = TRUE) {
  if (!is.data.frame(manifest)) {
    stop("'manifest' must be a data frame.", call. = FALSE)
  }
  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  if (nrow(manifest) == 0L) {
    stop("'manifest' must contain at least one row.", call. = FALSE)
  }

  cache_dir <- cache_dir %||%
    file.path(tempdir(), "neuromosaic-montage-derived-maps")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!"path" %in% names(manifest)) {
    manifest$path <- NA_character_
  }
  if (!"recipe_materialized" %in% names(manifest)) {
    manifest$recipe_materialized <- FALSE
  }

  recipe_missing <- if ("recipe" %in% names(manifest)) {
    .missing_column_values(manifest$recipe)
  } else {
    rep(TRUE, nrow(manifest))
  }

  for (i in which(!recipe_missing)) {
    row <- manifest[i, , drop = FALSE]
    cache_path <- .montage_recipe_cache_path(row, manifest$recipe[[i]], cache_dir)
    existing_path <- if (!.missing_character(as.character(manifest$path))[[i]]) {
      as.character(manifest$path[[i]])
    } else {
      cache_path
    }

    if (!isTRUE(overwrite) && file.exists(existing_path)) {
      manifest$path[[i]] <- normalizePath(existing_path, mustWork = TRUE)
      manifest$recipe_materialized[[i]] <- TRUE
      next
    }

    value <- .montage_evaluate_recipe(manifest$recipe[[i]], row)
    path <- .montage_recipe_value_to_path(value, cache_path)
    manifest$path[[i]] <- path
    manifest$recipe_materialized[[i]] <- TRUE
  }

  manifest <- .attach_montage_map_hashes(manifest)

  if (isTRUE(validate)) {
    manifest <- validate_manifest(manifest, check_files = check_files)
  }
  manifest
}

.montage_recipe_cache_path <- function(row, recipe, cache_dir) {
  key <- NULL
  if ("recipe_key" %in% names(row) &&
      !.missing_character(as.character(row$recipe_key))[[1]]) {
    key <- as.character(row$recipe_key[[1]])
  } else if (is.list(recipe) && !is.null(recipe$key)) {
    key <- as.character(recipe$key[[1]])
  }
  map_id <- if ("map_id" %in% names(row) &&
                !.missing_character(as.character(row$map_id))[[1]]) {
    as.character(row$map_id[[1]])
  } else {
    "map"
  }
  stem <- .safe_file_stem(paste(c(map_id, key), collapse = "_"))
  file.path(cache_dir, paste0(stem, ".nii.gz"))
}

.montage_evaluate_recipe <- function(recipe, row) {
  if (methods::is(recipe, "NeuroVol") ||
      (is.character(recipe) && length(recipe) == 1L)) {
    return(recipe)
  }

  if (is.function(recipe)) {
    return(.montage_call_recipe_fun(recipe, row = row, args = list()))
  }

  if (is.list(recipe)) {
    fun <- recipe$fun %||% recipe$`function` %||% recipe$recipe
    if (is.null(fun) || !is.function(fun)) {
      stop(
        "Structured montage recipes must include a function in 'fun' or 'function'.",
        call. = FALSE
      )
    }
    args <- recipe$args %||% list()
    if (!is.list(args)) {
      stop("Structured montage recipe 'args' must be a list.", call. = FALSE)
    }
    return(.montage_call_recipe_fun(fun, row = row, args = args))
  }

  stop(
    "Montage recipe entries must be functions, structured lists, paths, or NeuroVol objects.",
    call. = FALSE
  )
}

.montage_call_recipe_fun <- function(fun, row, args) {
  formals <- names(formals(fun))
  if ("row" %in% formals && !"row" %in% names(args)) {
    args$row <- row
  }
  do.call(fun, args)
}

.montage_recipe_value_to_path <- function(value, cache_path) {
  if (is.character(value) && length(value) == 1L) {
    if (!file.exists(value)) {
      stop("Recipe returned a path that does not exist: ", value, call. = FALSE)
    }
    return(normalizePath(value, mustWork = TRUE))
  }

  if (is.list(value) && !methods::is(value, "NeuroVol")) {
    if (!is.null(value$path)) {
      return(.montage_recipe_value_to_path(value$path, cache_path))
    }
    if (!is.null(value$stat_map)) {
      value <- value$stat_map
    } else if (!is.null(value$map)) {
      value <- value$map
    }
  }

  if (!methods::is(value, "NeuroVol")) {
    stop("Montage recipes must return a NeuroVol, a path, or a list containing one.",
         call. = FALSE)
  }

  if (!dir.exists(dirname(cache_path))) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  }
  neuroim2::write_vol(value, cache_path)
  normalizePath(cache_path, mustWork = TRUE)
}

.attach_montage_map_hashes <- function(manifest) {
  if (!"map_hash" %in% names(manifest)) {
    manifest$map_hash <- NA_character_
  }

  for (i in seq_len(nrow(manifest))) {
    if ("path" %in% names(manifest) &&
        !.missing_character(as.character(manifest$path))[[i]] &&
        file.exists(as.character(manifest$path[[i]]))) {
      manifest$map_hash[[i]] <- unname(tools::md5sum(as.character(manifest$path[[i]])))
      next
    }

    if ("stat_map" %in% names(manifest) &&
        !.missing_column_values(manifest$stat_map)[[i]]) {
      col <- manifest$stat_map
      stat_map <- if (is.list(col)) col[[i]] else col[i]
      if (methods::is(stat_map, "NeuroVol")) {
        manifest$map_hash[[i]] <- .montage_neurovol_hash(stat_map)
      }
    }
  }

  manifest
}

.montage_neurovol_hash <- function(x) {
  tmp <- tempfile("neuromosaic-map-hash-", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(
    list(
      space = .neuro_space_signature(neuroim2::space(x)),
      values = as.numeric(x)
    ),
    tmp
  )
  unname(tools::md5sum(tmp))
}
