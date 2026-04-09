#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats aggregate setNames qnorm sd
#' @importFrom utils globalVariables write.csv
#' @importFrom methods is slotNames
#' @importFrom rlang .data f_lhs f_rhs %||%
#' @importFrom withr with_dir
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".", ".data", ".sample_index", "cluster_id", "parcel_id", "label",
  "n_voxels", "peak_stat", "sign", "hemi", "network",
  "mean_stat", "voxel_idx", "grid_x", "grid_y", "grid_z",
  "atlas_id", "overlap_frac", "mean_value", "se_value",
  "ci_lower", "ci_upper", "frac", "value", "Sign",
  "data_id", "tooltip", "tooltip_", "x", "y"
))
