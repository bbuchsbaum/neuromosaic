.normalize_path_or_null <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }
  normalizePath(path, mustWork = FALSE)
}

.formula_labels <- function(formulas) {
  if (length(formulas) == 0L) {
    return(list())
  }

  nm <- names(formulas)
  if (is.null(nm)) {
    nm <- paste0("formula_", seq_along(formulas))
  }

  stats::setNames(
    as.list(vapply(formulas, function(f) {
      paste(deparse(f), collapse = " ")
    }, character(1))),
    nm
  )
}

.build_cluster_report_provenance <- function(stat_map_path = NULL,
                                             stat_map,
                                             data_source,
                                             atlas,
                                             design,
                                             formulas,
                                             output_file,
                                             template,
                                             report_mode,
                                             extra = NULL) {
  auto <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    package = "neuromosaic",
    package_version = as.character(utils::packageVersion("neuromosaic")),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    report_mode = report_mode,
    inputs = list(
      stat_map_path = .normalize_path_or_null(stat_map_path),
      stat_map_class = class(stat_map),
      data_source_class = if (is.null(data_source)) NULL else class(data_source),
      atlas_name = atlas$name %||% NULL,
      atlas_class = class(atlas),
      design_columns = if (is.null(design)) NULL else names(design)
    ),
    formulas = .formula_labels(formulas),
    outputs = list(
      output_file = .normalize_path_or_null(output_file),
      template = .normalize_path_or_null(template)
    )
  )

  if (is.null(extra)) {
    return(auto)
  }

  utils::modifyList(auto, extra, keep.null = TRUE)
}

.write_provenance_yaml <- function(provenance, path) {
  yaml::write_yaml(provenance, file = path)
  invisible(path)
}
