#' Prepare Background and Statistic Volumes for Overlay Rendering
#'
#' Loads and reconciles a background/statistic volume pair before montage
#' rendering. By default, mismatched grids are a hard error. `on_mismatch =
#' "restamp"` is intentionally conservative: it only re-stamps the statistic
#' volume into the background space when voxel array dimensions already match.
#'
#' @param bg Background `NeuroVol` or file path.
#' @param stat Statistic `NeuroVol` or file path.
#' @param on_mismatch How to handle a grid mismatch. `"error"` fails loudly;
#'   `"restamp"` copies the statistic array into the background space only when
#'   dimensions match.
#' @param tolerance Numeric tolerance used when comparing space metadata.
#'
#' @return A list with `background`, `stat`, `reconciled`, and `action`.
#' @export
prepare_overlay <- function(bg,
                            stat,
                            on_mismatch = c("error", "restamp"),
                            tolerance = sqrt(.Machine$double.eps)) {
  on_mismatch <- match.arg(on_mismatch)
  bg <- .load_overlay_neurovol(bg, "bg")
  stat <- .load_overlay_neurovol(stat, "stat")

  bg_space <- neuroim2::space(bg)
  stat_space <- neuroim2::space(stat)
  if (.same_neuro_space(bg_space, stat_space, tolerance = tolerance)) {
    return(list(
      background = bg,
      stat = stat,
      reconciled = TRUE,
      action = "already_aligned"
    ))
  }

  if (identical(on_mismatch, "error")) {
    stop(
      "Background and statistic volumes have different grids.\n",
      "Background: ", .overlay_space_summary(bg_space), "\n",
      "Statistic: ", .overlay_space_summary(stat_space),
      call. = FALSE
    )
  }

  bg_dim <- dim(bg_space)
  stat_dim <- dim(stat_space)
  if (!identical(bg_dim, stat_dim)) {
    stop(
      "Cannot restamp statistic volume because dimensions differ. ",
      "Background dim: ", paste(bg_dim, collapse = "x"),
      "; statistic dim: ", paste(stat_dim, collapse = "x"), ".",
      call. = FALSE
    )
  }

  list(
    background = bg,
    stat = neuroim2::NeuroVol(as.array(stat), space = bg_space),
    reconciled = TRUE,
    action = "restamped"
  )
}

.load_overlay_neurovol <- function(x, label) {
  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      stop("Overlay ", label, " path not found: ", x, call. = FALSE)
    }
    x <- neuroim2::read_vol(x)
  }
  if (!methods::is(x, "NeuroVol")) {
    stop("Overlay ", label, " must be a NeuroVol or path.", call. = FALSE)
  }
  x
}

.overlay_space_summary <- function(space) {
  paste0(
    "dim=", paste(dim(space), collapse = "x"),
    "; spacing=", paste(signif(neuroim2::spacing(space), 6), collapse = ","),
    "; origin=", paste(signif(neuroim2::origin(space), 6), collapse = ",")
  )
}
