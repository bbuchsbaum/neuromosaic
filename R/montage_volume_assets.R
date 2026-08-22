# Canonical, content-addressed NIfTI assets for browser volume reports.

.montage_materialize_volume_asset <- function(source,
                                               kind = c("background", "overlay",
                                                        "mask"),
                                               output_dir,
                                               compression = c("gzip", "none"),
                                               support_mask = NULL,
                                               ref_prefix = "") {
  kind <- match.arg(kind)
  compression <- match.arg(compression)
  volume <- .montage_asset_volume(source)
  if (length(dim(volume)) != 3L) {
    stop("Interactive assets must be three-dimensional volumes.", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("'output_dir' must be a non-empty path.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) {
    stop("Could not create interactive asset directory: ", output_dir,
         call. = FALSE)
  }

  values <- as.numeric(volume)
  transformations <- "canonical_nifti_header"
  if (identical(kind, "mask")) {
    values <- as.numeric(.normalize_montage_support_mask(values, length(values)))
    data_type <- "UBYTE"
    datatype <- "uint8"
    transformations <- c(transformations, "logical_mask_uint8")
  } else {
    if (!is.null(support_mask)) {
      if (methods::is(support_mask, "NeuroVol") &&
          !.same_neuro_space(
            neuroim2::space(volume), neuroim2::space(support_mask)
          )) {
        stop("Support-mask geometry does not match its volume asset.",
             call. = FALSE)
      }
      support <- .normalize_montage_support_mask(
        support_mask, length(values)
      )
      values[!support] <- NA_real_
      transformations <- c(transformations, "outside_support_to_nan")
    }
    # R NeuroVol values are doubles. Store them as FLOAT64 so compression is
    # lossless under the declared source precision and never silently quantizes
    # or clips display data.
    data_type <- "DOUBLE"
    datatype <- "float64"
    transformations <- c(transformations, "storage_float64_lossless")
  }

  canonical <- neuroim2::NeuroVol(
    array(values, dim = dim(volume)),
    space = neuroim2::space(volume)
  )
  uncompressed <- tempfile(
    "neuromosaic-volume-", tmpdir = output_dir, fileext = ".nii"
  )
  on.exit(unlink(uncompressed), add = TRUE)
  neuroim2::write_vol(
    canonical, uncompressed, data_type = data_type
  )
  if (!file.exists(uncompressed)) {
    stop("Failed to write canonical NIfTI asset.", call. = FALSE)
  }

  content_hash <- .montage_md5_file(uncompressed)
  asset_id <- paste0(kind, "-", content_hash)
  suffix <- if (identical(compression, "gzip")) ".nii.gz" else ".nii"
  file_name <- paste0(asset_id, suffix)
  final_path <- file.path(output_dir, file_name)
  staged_path <- tempfile(
    paste0(".", asset_id, "-"), tmpdir = output_dir, fileext = suffix
  )
  on.exit(unlink(staged_path), add = TRUE)

  if (identical(compression, "gzip")) {
    .montage_gzip_file(uncompressed, staged_path)
  } else if (!file.copy(uncompressed, staged_path, overwrite = FALSE)) {
    stop("Failed to stage uncompressed NIfTI asset.", call. = FALSE)
  }
  if (!file.exists(staged_path)) {
    stop("Failed to stage interactive volume asset.", call. = FALSE)
  }

  if (file.exists(final_path)) {
    existing_hash <- .montage_asset_content_hash(final_path, compression)
    if (!identical(existing_hash, content_hash)) {
      stop(
        "Refusing to overwrite an existing interactive asset whose content ",
        "does not match its content-addressed name: ", final_path,
        call. = FALSE
      )
    }
    unlink(staged_path)
  } else if (!file.rename(staged_path, final_path)) {
    stop("Failed to atomically install interactive volume asset.",
         call. = FALSE)
  }

  uncompressed_bytes <- as.numeric(file.info(uncompressed)$size)
  compressed_bytes <- as.numeric(file.info(final_path)$size)
  location_ref <- if (nzchar(ref_prefix)) {
    file.path(ref_prefix, file_name)
  } else {
    file_name
  }
  asset <- list(
    asset_id = asset_id,
    kind = kind,
    location = list(kind = "relative", ref = location_ref),
    encoding = "nifti",
    datatype = datatype,
    compression = compression,
    hash_algorithm = "md5",
    hash_scope = "uncompressed",
    hash = content_hash,
    compressed_bytes = compressed_bytes,
    uncompressed_bytes = uncompressed_bytes,
    geometry = .montage_volume_geometry(canonical),
    transformations = transformations
  )
  structure(
    list(asset = asset, path = normalizePath(final_path, mustWork = TRUE)),
    class = "montage_materialized_volume_asset"
  )
}

.montage_materialize_volume_assets <- function(manifest,
                                                background,
                                                output_dir,
                                                compression = c("gzip", "none"),
                                                support_masks = NULL,
                                                ref_prefix = "") {
  compression <- match.arg(compression)
  stat_maps <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_manifest_stat_source(manifest, i)
  })
  .montage_validate_report_volume_geometry(manifest, background, stat_maps)
  if (is.null(support_masks)) {
    support_masks <- .montage_render_support_masks(manifest, stat_maps)
  }
  if (!is.list(support_masks) || length(support_masks) != nrow(manifest)) {
    stop("'support_masks' must have one entry per manifest row.",
         call. = FALSE)
  }

  background_asset <- .montage_materialize_volume_asset(
    background,
    kind = "background",
    output_dir = output_dir,
    compression = compression,
    ref_prefix = ref_prefix
  )
  map_assets <- stats::setNames(lapply(seq_len(nrow(manifest)), function(i) {
    .montage_materialize_volume_asset(
      stat_maps[[i]],
      kind = "overlay",
      output_dir = output_dir,
      compression = compression,
      support_mask = support_masks[[i]],
      ref_prefix = ref_prefix
    )
  }), as.character(manifest$map_id))
  materialized <- c(list(background_asset), unname(map_assets))
  asset_ids <- vapply(materialized, function(x) x$asset$asset_id, character(1))
  keep <- !duplicated(asset_ids)
  unique_ids <- asset_ids[keep]

  structure(
    list(
      background = background_asset,
      maps = map_assets,
      assets = lapply(materialized[keep], `[[`, "asset"),
      files = stats::setNames(
        vapply(materialized[keep], `[[`, character(1), "path"),
        unique_ids
      )
    ),
    class = "montage_volume_asset_bundle"
  )
}

.montage_asset_volume <- function(source) {
  if (is.character(source) && length(source) == 1L && !is.na(source)) {
    source <- neuroim2::read_vol(source)
  }
  if (!methods::is(source, "NeuroVol")) {
    stop("A volume asset source must be a NeuroVol or readable NIfTI path.",
         call. = FALSE)
  }
  source
}

.montage_md5_file <- function(path) {
  hash <- unname(tools::md5sum(path))
  if (length(hash) != 1L || is.na(hash) || !nzchar(hash)) {
    stop("Failed to hash interactive volume asset.", call. = FALSE)
  }
  as.character(hash)
}

.montage_gzip_file <- function(source, target) {
  input <- file(source, open = "rb")
  output <- gzfile(target, open = "wb", compression = 9)
  on.exit(try(close(input), silent = TRUE), add = TRUE)
  on.exit(try(close(output), silent = TRUE), add = TRUE)
  repeat {
    chunk <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (!length(chunk)) break
    writeBin(chunk, output)
  }
  close(input)
  close(output)
  invisible(target)
}

.montage_asset_content_hash <- function(path, compression) {
  if (identical(compression, "none")) return(.montage_md5_file(path))
  expanded <- tempfile("neuromosaic-expanded-", fileext = ".nii")
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
  .montage_md5_file(expanded)
}
