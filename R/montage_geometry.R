# Fail-closed geometry contracts for portable volume scenes.

# A combined absolute/relative tolerance in world-coordinate units. This is
# intentionally tighter than NIfTI header display precision while tolerating
# harmless floating-point serialization noise. It never authorizes restamping,
# interpolation, or resampling.
.montage_geometry_tolerance <- 1e-6

.montage_volume_geometry <- function(x,
                                     tolerance = .montage_geometry_tolerance) {
  space <- if (methods::is(x, "NeuroVol")) neuroim2::space(x) else x
  if (!methods::is(space, "NeuroSpace")) {
    stop("Geometry requires a neuroim2 NeuroVol or NeuroSpace.", call. = FALSE)
  }
  dims <- as.integer(dim(space))[seq_len(3L)]
  affine <- as.numeric(neuroim2::trans(space))
  geometry <- list(
    dimensions = dims,
    spacing = abs(as.numeric(neuroim2::spacing(space))[seq_len(3L)]),
    origin = as.numeric(neuroim2::origin(space))[seq_len(3L)],
    orientation = .montage_space_orientation(space),
    affine = affine
  )
  geometry$fingerprint <- .montage_geometry_fingerprint(
    geometry, tolerance = tolerance
  )
  geometry
}

.montage_space_orientation <- function(space) {
  codes <- tryCatch(
    neuroim2::orientation_to_axcodes(
      neuroim2::affine_to_orientation(neuroim2::trans(space))
    ),
    error = function(e) NULL
  )
  if (!is.null(codes) && length(codes) >= 3L) {
    return(paste(as.character(codes[seq_len(3L)]), collapse = ""))
  }
  axes <- neuroim2::axes(space)
  paste(vapply(c("i", "j", "k"), function(slot) {
    if (!slot %in% methods::slotNames(axes)) return("unknown")
    as.character(methods::slot(methods::slot(axes, slot), "axis"))
  }, character(1)), collapse = "|")
}

.montage_geometry_fingerprint <- function(geometry,
                                          tolerance =
                                            .montage_geometry_tolerance) {
  required <- c("dimensions", "spacing", "origin", "orientation", "affine")
  missing <- setdiff(required, names(geometry))
  if (length(missing)) {
    stop(
      "Cannot fingerprint geometry missing: ", paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance <= 0) {
    stop("Geometry tolerance must be a positive finite number.", call. = FALSE)
  }
  canonical <- paste(
    "geometry-v1",
    paste(as.integer(geometry$dimensions), collapse = ","),
    paste(sprintf("%.12g", as.numeric(geometry$spacing)), collapse = ","),
    paste(sprintf("%.12g", as.numeric(geometry$origin)), collapse = ","),
    paste(as.character(geometry$orientation), collapse = ","),
    paste(sprintf("%.12g", as.numeric(geometry$affine)), collapse = ","),
    sprintf("tol=%.12g", tolerance),
    sep = "|"
  )
  paste0("md5:", .montage_md5_text(canonical))
}

.montage_md5_text <- function(x) {
  path <- tempfile("neuromosaic-md5-")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(x)), con)
  close(con)
  # Remove the now-closed connection cleanup before returning.
  on.exit(NULL, add = FALSE)
  hash <- unname(tools::md5sum(path))
  unlink(path)
  as.character(hash)
}

.montage_geometry_mismatches <- function(reference,
                                         candidate,
                                         tolerance =
                                           .montage_geometry_tolerance) {
  mismatches <- character()
  if (!identical(as.integer(reference$dimensions),
                 as.integer(candidate$dimensions))) {
    mismatches <- c(mismatches, "dimensions")
  }
  for (field in c("spacing", "origin", "affine")) {
    if (!.montage_geometry_numeric_equal(
      reference[[field]], candidate[[field]], tolerance
    )) {
      mismatches <- c(mismatches, field)
    }
  }
  if (!identical(as.character(reference$orientation),
                 as.character(candidate$orientation))) {
    mismatches <- c(mismatches, "orientation")
  }
  unique(mismatches)
}

.montage_geometry_numeric_equal <- function(x, y, tolerance) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y) || anyNA(x) || anyNA(y) ||
      any(!is.finite(x)) || any(!is.finite(y))) {
    return(FALSE)
  }
  scale <- pmax(1, abs(x), abs(y))
  all(abs(x - y) <= tolerance * scale)
}

.montage_assert_same_geometry <- function(reference,
                                          candidate,
                                          analysis_id,
                                          map_id,
                                          reference_label = "background",
                                          tolerance =
                                            .montage_geometry_tolerance) {
  reference_geometry <- .montage_volume_geometry(reference, tolerance)
  candidate_geometry <- .montage_volume_geometry(candidate, tolerance)
  mismatch <- .montage_geometry_mismatches(
    reference_geometry, candidate_geometry, tolerance
  )
  if (length(mismatch)) {
    stop(
      "Geometry mismatch for analysis_id '", analysis_id,
      "', map_id '", map_id, "' against ", reference_label, ": ",
      paste(mismatch, collapse = ", "), " (tolerance ",
      format(tolerance, scientific = TRUE), "). No restamping, interpolation, ",
      "or resampling is performed for interactive volume reports.",
      call. = FALSE
    )
  }
  invisible(candidate_geometry)
}

.montage_validate_report_volume_geometry <- function(manifest,
                                                      background,
                                                      stat_maps,
                                                      tolerance =
                                                        .montage_geometry_tolerance) {
  background <- if (is.character(background) && length(background) == 1L) {
    neuroim2::read_vol(background)
  } else {
    background
  }
  if (!methods::is(background, "NeuroVol")) {
    stop("Volume report background must be a NeuroVol or readable path.",
         call. = FALSE)
  }
  groups <- .montage_analysis_groups(manifest)
  for (i in seq_len(nrow(manifest))) {
    analysis_id <- as.character(manifest$analysis_id[[i]])
    map_id <- as.character(manifest$map_id[[i]])
    candidate <- stat_maps[[i]]
    if (!methods::is(candidate, "NeuroVol")) {
      stop(
        "Interactive volume geometry requires a NeuroVol for analysis_id '",
        analysis_id, "', map_id '", map_id, "'.",
        call. = FALSE
      )
    }
    .montage_assert_same_geometry(
      background, candidate, analysis_id, map_id,
      reference_label = "background", tolerance = tolerance
    )

    group <- groups[[analysis_id]]
    primary_row <- match(group$primary_map_id, manifest$map_id)
    if (!identical(i, primary_row)) {
      .montage_assert_same_geometry(
        stat_maps[[primary_row]], candidate, analysis_id, map_id,
        reference_label = paste0("primary map '", group$primary_map_id, "'"),
        tolerance = tolerance
      )
    }

    mask_row <- if (.montage_row_has_mask(manifest, i)) i else primary_row
    if (.montage_row_has_mask(manifest, mask_row)) {
      mask <- .montage_manifest_mask_source(manifest, mask_row)
      if (methods::is(mask, "NeuroVol")) {
        .montage_assert_same_geometry(
          candidate, mask, analysis_id, map_id,
          reference_label = paste0(
            "analysis mask from map '", manifest$map_id[[mask_row]], "'"
          ),
          tolerance = tolerance
        )
      } else if (length(mask) != length(candidate)) {
        stop(
          "Analysis mask size mismatch for analysis_id '", analysis_id,
          "', map_id '", map_id, "'.",
          call. = FALSE
        )
      }
    }
  }
  invisible(.montage_volume_geometry(background, tolerance))
}

.montage_manifest_mask_source <- function(manifest, row) {
  mask <- if (is.list(manifest$mask) && !is.data.frame(manifest$mask)) {
    manifest$mask[[row]]
  } else {
    manifest$mask[[row]]
  }
  if (is.character(mask) && length(mask) == 1L) {
    mask <- neuroim2::read_vol(mask)
  }
  mask
}

.montage_world_coord_in_geometry <- function(coord,
                                             geometry,
                                             tolerance =
                                               .montage_geometry_tolerance) {
  coord <- as.numeric(coord)
  if (length(coord) != 3L || anyNA(coord) || any(!is.finite(coord))) {
    return(FALSE)
  }
  affine <- matrix(as.numeric(geometry$affine), nrow = 4L, ncol = 4L)
  voxel <- tryCatch(
    solve(affine, c(coord, 1)),
    error = function(e) rep(NA_real_, 4L)
  )
  if (anyNA(voxel) || any(!is.finite(voxel)) || abs(voxel[[4L]]) <= tolerance) {
    return(FALSE)
  }
  voxel <- voxel[seq_len(3L)] / voxel[[4L]]
  dims <- as.numeric(geometry$dimensions)
  all(voxel >= -0.5 - tolerance & voxel <= dims - 0.5 + tolerance)
}
