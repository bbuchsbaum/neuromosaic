# Vendored utilities from neuroatlas
#
# Small, self-contained helpers that neuromosaic needs but that are
# internal implementation details of neuroatlas.  Vendored here to avoid
# promoting them to public API in the parent package.

#' Extract the volume slot from an atlas object
#'
#' @param atlas An atlas object (S3 list with \code{$atlas} or \code{$data}).
#' @return A \code{NeuroVol} or \code{ClusteredNeuroVol}.
#' @keywords internal
#' @noRd
.get_atlas_volume <- function(atlas) {
  vol <- NULL

  if (!is.null(atlas$atlas) && methods::is(atlas$atlas, "NeuroVol")) {
    vol <- atlas$atlas
  } else if (!is.null(atlas$atlas) && methods::is(atlas$atlas, "ClusteredNeuroVol")) {
    vol <- atlas$atlas
  } else if (!is.null(atlas$data) && methods::is(atlas$data, "NeuroVol")) {
    vol <- atlas$data
  } else if (!is.null(atlas$data) && methods::is(atlas$data, "ClusteredNeuroVol")) {
    vol <- atlas$data
  }

  if (is.null(vol)) {
    stop("Could not determine atlas volume from object")
  }
  vol
}

#' Project 3D vertices to 2D for a given view
#'
#' @param verts N x 3 matrix of vertex coordinates.
#' @param view Character: one of \code{"lateral"}, \code{"medial"},
#'   \code{"dorsal"}, \code{"ventral"}.
#' @param hemi Character: \code{"left"} or \code{"right"}.
#' @return A list with \code{xy} (N x 2 matrix) and \code{view_dir}
#'   (length-3 numeric).
#' @keywords internal
#' @noRd
.project_view <- function(verts, view, hemi) {
  if (view == "lateral" && hemi == "left") {
    view_dir <- c(-1, 0, 0)
    xy <- cbind(verts[, 2], verts[, 3])
  } else if (view == "medial" && hemi == "left") {
    view_dir <- c(1, 0, 0)
    xy <- cbind(-verts[, 2], verts[, 3])
  } else if (view == "lateral" && hemi == "right") {
    view_dir <- c(1, 0, 0)
    xy <- cbind(-verts[, 2], verts[, 3])
  } else if (view == "medial" && hemi == "right") {
    view_dir <- c(-1, 0, 0)
    xy <- cbind(verts[, 2], verts[, 3])
  } else if (view == "dorsal") {
    view_dir <- c(0, 0, 1)
    xy <- if (hemi == "left") {
      cbind(verts[, 1], verts[, 2])
    } else {
      cbind(-verts[, 1], verts[, 2])
    }
  } else if (view == "ventral") {
    view_dir <- c(0, 0, -1)
    xy <- if (hemi == "left") {
      cbind(verts[, 1], -verts[, 2])
    } else {
      cbind(-verts[, 1], -verts[, 2])
    }
  } else {
    stop("Unknown view: ", view)
  }

  list(xy = xy, view_dir = view_dir)
}

#' Encode a plot_brain data ID string
#'
#' Produces a \code{"panel::parcel_id::shape_id"} string used as the
#' interactive data-id in ggiraph brain plots.
#'
#' @param panel Character panel label.
#' @param parcel_id Integer parcel ID.
#' @param shape_id Integer shape/polygon ID.
#' @return A single character string.
#' @keywords internal
#' @noRd
.encode_plot_brain_data_id <- function(panel, parcel_id, shape_id) {
  paste(
    as.character(panel),
    as.integer(parcel_id),
    as.integer(shape_id),
    sep = "::"
  )
}
