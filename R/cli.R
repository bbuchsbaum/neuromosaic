#' Command-Line Interface for neuromosaic
#'
#' Drive report generation, CSV extraction, interactive exploration, and
#' `nftab` manifest workflows from the shell.
#'
#' @param args Character vector of CLI arguments. Defaults to
#'   `commandArgs(trailingOnly = TRUE)`.
#' @param execute Logical. When `TRUE` (default), execute the requested
#'   command. When `FALSE`, return a prepared command specification without
#'   side effects. This is primarily useful for testing.
#'
#' @return Invisibly, the execution result or prepared command specification.
#' @export
cli_main <- function(args = commandArgs(trailingOnly = TRUE),
                     execute = TRUE) {
  invocation <- .cli_parse_invocation(args)

  if (isTRUE(invocation$help)) {
    cat(.cli_help_text(invocation$command, invocation$subcommand))
    return(invisible(invocation))
  }

  spec <- .cli_prepare_command(invocation)
  if (!isTRUE(execute)) {
    return(invisible(spec))
  }

  invisible(.cli_execute_command(spec))
}

.cli_prepare_command <- function(invocation) {
  command <- invocation$command
  subcommand <- invocation$subcommand
  opts <- invocation$options

  switch(
    command,
    report = .cli_prepare_report(opts),
    extract = .cli_prepare_extract(opts),
    explore = .cli_prepare_explore(opts),
    manifest = switch(
      subcommand,
      create = .cli_prepare_manifest_create(opts),
      validate = .cli_prepare_manifest_validate(opts),
      show = .cli_prepare_manifest_show(opts),
      .cli_abort(
        "Unknown manifest subcommand. Use 'manifest create', 'manifest validate', or 'manifest show'."
      )
    ),
    .cli_abort(
      paste0("Unknown command '", command, "'. Use '--help' to see available commands.")
    )
  )
}

.cli_execute_command <- function(spec) {
  switch(
    spec$type,
    report = {
      result <- do.call(cluster_report, spec$args)
      print(result)
      result
    },
    extract = {
      result <- do.call(cluster_report, spec$report_args)
      paths <- do.call(export_csv, c(list(x = result), spec$export_args))
      list(result = result, paths = paths)
    },
    explore = {
      if (!requireNamespace("shiny", quietly = TRUE)) {
        .cli_abort("Package 'shiny' is required for the 'explore' command.")
      }
      app <- do.call(cluster_explorer, spec$args)
      shiny::runApp(app)
      app
    },
    manifest_create = {
      neurotabs::nf_write(
        x = spec$dataset,
        path = spec$out_dir,
        manifest_name = spec$manifest_name
      )
      cat("Wrote nftab dataset to ", normalizePath(spec$out_dir), "\n", sep = "")
      .cli_print_dataset_summary(spec$dataset)
      invisible(spec$out_dir)
    },
    manifest_validate = {
      ds <- neurotabs::nf_read(spec$manifest_path, validate_schema = TRUE)
      cat("Manifest is valid: ", spec$manifest_path, "\n", sep = "")
      .cli_print_dataset_summary(ds)
      ds
    },
    manifest_show = {
      ds <- neurotabs::nf_read(spec$manifest_path, validate_schema = FALSE)
      .cli_print_dataset_summary(ds)
      ds
    },
    .cli_abort(paste0("Unsupported CLI spec type '", spec$type, "'."))
  )
}

.cli_prepare_report <- function(opts) {
  stat_map <- .cli_load_stat_map(.cli_require_scalar(opts, "stat_map"))
  atlas <- .cli_load_atlas(.cli_opt_scalar(opts, "atlas", "schaefer:200:7"))
  dataset <- NULL
  source <- NULL

  if (.cli_has_dataset_inputs(opts)) {
    dataset <- .cli_resolve_dataset(opts, require_feature = TRUE)
    source <- .cli_materialize_feature_source(
      ds = dataset$ds,
      feature = dataset$feature,
      stat_map = stat_map,
      strategy = .cli_opt_scalar(opts, "strategy", "collect")
    )
  }

  formulas <- .cli_parse_formulas(
    opts$formula,
    default = if (is.null(source)) list() else list(value ~ time)
  )
  if (!is.null(dataset)) {
    formulas <- .cli_normalize_report_formulas(formulas, dataset$feature)
  }

  args <- c(
    list(
      stat_map = stat_map,
      data_source = if (is.null(source)) NULL else source$data_source,
      atlas = atlas,
      formulas = formulas,
      design = if (is.null(source)) NULL else source$design,
      threshold = .cli_opt_numeric(opts, "threshold", 3.5),
      min_cluster_size = .cli_opt_integer(opts, "min_cluster_size", 10L),
      connectivity = .cli_opt_scalar(opts, "connectivity", "18-connect"),
      tail = .cli_opt_scalar(opts, "tail", "two_sided"),
      max_clusters = .cli_opt_integer(opts, "max_clusters", 20L),
      output_file = .cli_opt_scalar(opts, "out", "neuromosaic-report.html"),
      template = .cli_opt_scalar(opts, "template", NULL),
      table_style = .cli_opt_scalar(opts, "table_style", "gt"),
      brain_slices = .cli_opt_flag(opts, "brain_slices", TRUE),
      quiet = .cli_opt_flag(opts, "quiet", TRUE)
    ),
    if (is.null(source)) list() else source$extra_args
  )

  list(
    type = "report",
    args = args,
    dataset = dataset,
    mode = if (is.null(source)) "stat_only" else "dataset"
  )
}

.cli_prepare_extract <- function(opts) {
  stat_map <- .cli_load_stat_map(.cli_require_scalar(opts, "stat_map"))
  atlas <- .cli_load_atlas(.cli_opt_scalar(opts, "atlas", "schaefer:200:7"))
  dataset <- NULL
  source <- NULL

  if (.cli_has_dataset_inputs(opts)) {
    dataset <- .cli_resolve_dataset(opts, require_feature = TRUE)
    source <- .cli_materialize_feature_source(
      ds = dataset$ds,
      feature = dataset$feature,
      stat_map = stat_map,
      strategy = .cli_opt_scalar(opts, "strategy", "collect")
    )
  }

  formulas <- .cli_parse_formulas(
    opts$formula,
    default = if (is.null(source)) list() else list(value ~ time)
  )
  if (!is.null(dataset)) {
    formulas <- .cli_normalize_report_formulas(formulas, dataset$feature)
  }

  report_args <- c(
    list(
      stat_map = stat_map,
      data_source = if (is.null(source)) NULL else source$data_source,
      atlas = atlas,
      formulas = formulas,
      design = if (is.null(source)) NULL else source$design,
      threshold = .cli_opt_numeric(opts, "threshold", 3.5),
      min_cluster_size = .cli_opt_integer(opts, "min_cluster_size", 10L),
      connectivity = .cli_opt_scalar(opts, "connectivity", "18-connect"),
      tail = .cli_opt_scalar(opts, "tail", "two_sided"),
      max_clusters = .cli_opt_integer(opts, "max_clusters", 20L),
      output_file = NULL,
      table_style = .cli_opt_scalar(opts, "table_style", "gt"),
      brain_slices = .cli_opt_flag(opts, "brain_slices", TRUE),
      quiet = .cli_opt_flag(opts, "quiet", TRUE)
    ),
    if (is.null(source)) list() else source$extra_args
  )

  list(
    type = "extract",
    report_args = report_args,
    export_args = list(
      dir = .cli_opt_scalar(opts, "dir", "."),
      prefix = .cli_opt_scalar(opts, "prefix", "neuromosaic")
    ),
    dataset = dataset,
    mode = if (is.null(source)) "stat_only" else "dataset"
  )
}

.cli_prepare_explore <- function(opts) {
  dataset <- .cli_resolve_dataset(opts, require_feature = TRUE)
  stat_map <- .cli_load_stat_map(.cli_require_scalar(opts, "stat_map"))
  atlas <- .cli_load_atlas(.cli_opt_scalar(opts, "atlas", "schaefer:200:7"))
  surfatlas <- .cli_load_surfatlas(.cli_opt_scalar(opts, "surfatlas", NULL))
  source <- .cli_materialize_feature_source(
    ds = dataset$ds,
    feature = dataset$feature,
    stat_map = stat_map,
    strategy = .cli_opt_scalar(opts, "strategy", "lazy")
  )

  plot_formula <- .cli_opt_scalar(opts, "plot_formula", NULL)
  plot_plugins <- NULL
  default_plot_plugin <- "auto"
  if (!is.null(plot_formula) && nzchar(plot_formula)) {
    plot_plugins <- list(
      formula = formula_plot_plugin(
        formula = plot_formula,
        id = "formula",
        label = paste0("Formula: ", plot_formula)
      )
    )
    default_plot_plugin <- "formula"
  }

  list(
    type = "explore",
    args = c(
      list(
        data_source = source$data_source,
        atlas = atlas,
        stat_map = stat_map,
        surfatlas = surfatlas,
        sample_table = source$sample_table,
        design = source$design,
        threshold = .cli_opt_numeric(opts, "threshold", 3),
        min_cluster_size = .cli_opt_integer(opts, "min_cluster_size", 20L),
        connectivity = .cli_opt_scalar(opts, "connectivity", "26-connect"),
        tail = .cli_opt_scalar(opts, "tail", "two_sided"),
        selection_engine = .cli_opt_scalar(opts, "selection_engine", "cluster"),
        plot_plugins = plot_plugins,
        default_plot_plugin = default_plot_plugin
      ),
      source$extra_args
    ),
    dataset = dataset,
    plot_formula = plot_formula
  )
}

.cli_prepare_manifest_create <- function(opts) {
  .cli_require_namespace("neurotabs", "manifest creation")

  out_dir <- .cli_require_scalar(opts, "out")
  manifest_name <- .cli_opt_scalar(opts, "manifest_name", "nftab.yaml")
  dataset <- .cli_build_adhoc_dataset(opts)

  list(
    type = "manifest_create",
    dataset = dataset$ds,
    out_dir = out_dir,
    manifest_name = manifest_name
  )
}

.cli_prepare_manifest_validate <- function(opts) {
  .cli_require_namespace("neurotabs", "manifest validation")
  list(
    type = "manifest_validate",
    manifest_path = .cli_resolve_manifest_path(.cli_require_scalar(opts, "manifest"))
  )
}

.cli_prepare_manifest_show <- function(opts) {
  .cli_require_namespace("neurotabs", "manifest inspection")
  list(
    type = "manifest_show",
    manifest_path = .cli_resolve_manifest_path(.cli_require_scalar(opts, "manifest"))
  )
}

.cli_resolve_dataset <- function(opts, require_feature = TRUE) {
  .cli_require_namespace("neurotabs", "dataset-backed CLI commands")

  feature <- .cli_opt_scalar(opts, "feature", NULL)
  if (isTRUE(require_feature) && (is.null(feature) || !nzchar(feature))) {
    .cli_abort("A '--feature' value is required for this command.")
  }

  manifest <- .cli_opt_scalar(opts, "manifest", NULL)
  design <- .cli_opt_scalar(opts, "design", NULL)

  if (!is.null(manifest) && !is.null(design)) {
    .cli_abort("Use either '--manifest' or '--design', not both.")
  }

  if (!is.null(manifest)) {
    ds <- neurotabs::nf_read(.cli_resolve_manifest_path(manifest),
                             validate_schema = TRUE)
    if (!feature %in% neurotabs::nf_feature_names(ds)) {
      .cli_abort(
        paste0(
          "Feature '", feature, "' was not found in the manifest dataset. ",
          "Available features: ",
          paste(neurotabs::nf_feature_names(ds), collapse = ", "),
          "."
        )
      )
    }
    return(list(ds = ds, feature = feature, source = "manifest"))
  }

  if (!is.null(design)) {
    dataset <- .cli_build_adhoc_dataset(opts)
    if (!feature %in% neurotabs::nf_feature_names(dataset$ds)) {
      .cli_abort(
        paste0(
          "Feature '", feature, "' was not found after ad hoc dataset creation."
        )
      )
    }
    return(list(ds = dataset$ds, feature = feature, source = "adhoc"))
  }

  .cli_abort("Provide either '--manifest' or '--design' for dataset-backed commands.")
}

.cli_has_dataset_inputs <- function(opts) {
  !is.null(.cli_opt_scalar(opts, "manifest", NULL)) ||
    !is.null(.cli_opt_scalar(opts, "design", NULL))
}

.cli_build_adhoc_dataset <- function(opts) {
  .cli_require_namespace("neurotabs", "ad hoc dataset creation")

  feature <- .cli_require_scalar(opts, "feature")
  design_path <- .cli_require_scalar(opts, "design")
  observations <- .cli_read_table(design_path)
  root <- .cli_opt_scalar(
    opts,
    "root",
    dirname(normalizePath(design_path, mustWork = TRUE))
  )
  axes <- .cli_parse_axes(.cli_opt_scalar(opts, "axes", NULL))
  dataset_id <- .cli_opt_scalar(
    opts,
    "dataset_id",
    tools::file_path_sans_ext(basename(design_path))
  )
  backend <- .cli_opt_scalar(opts, "backend", NULL)
  space <- .cli_opt_scalar(opts, "space", NULL)
  row_id <- .cli_opt_scalar(opts, "row_id", "row_id")

  path_template <- .cli_opt_scalar(opts, "path_template", NULL)
  locator_col_opt <- .cli_opt_scalar(opts, "locator_col", NULL)
  shared_4d <- .cli_opt_scalar(opts, "shared_4d", NULL)

  has_template <- !is.null(path_template) && nzchar(path_template)
  has_locator_col <- !is.null(locator_col_opt) && nzchar(locator_col_opt)
  has_shared <- !is.null(shared_4d) && nzchar(shared_4d)

  if (sum(c(has_template, has_locator_col, has_shared)) != 1L) {
    .cli_abort(
      "Provide exactly one of '--path-template', '--locator-col', or '--shared-4d'."
    )
  }

  nf_args <- list(
    observations = observations,
    feature = feature,
    row_id = row_id,
    axes = axes,
    backend = backend,
    space = space,
    dataset_id = dataset_id,
    root = root
  )

  if (has_template) {
    path_col <- if (has_locator_col) locator_col_opt else {
      paste0(tolower(feature), "_path")
    }
    observations[[path_col]] <- .cli_apply_path_template(path_template,
                                                          observations)
    nf_args$observations <- observations
    nf_args$locator_col <- path_col
  } else if (has_locator_col) {
    if (!locator_col_opt %in% names(observations)) {
      .cli_abort(
        paste0("Locator column '", locator_col_opt, "' was not found in the design table.")
      )
    }
    nf_args$locator_col <- locator_col_opt
  } else {
    nf_args$locator <- shared_4d
  }

  list(
    ds = do.call(neurotabs::nf_from_table, nf_args),
    observations = observations
  )
}

.cli_materialize_feature_source <- function(ds,
                                            feature,
                                            stat_map,
                                            strategy = c("lazy", "collect")) {
  .cli_require_namespace("neurotabs", "feature-backed dataset access")

  strategy <- match.arg(strategy)
  design <- neurotabs::nf_design(ds)
  sample_table <- design

  if (identical(strategy, "collect")) {
    list(
      data_source = .nf_collect_to_neurovec(ds, feature, stat_map),
      sample_table = sample_table,
      design = design,
      extra_args = list()
    )
  } else {
    list(
      data_source = ds,
      sample_table = sample_table,
      design = design,
      extra_args = list(series_fun = .nf_make_series_fun(feature))
    )
  }
}

.cli_load_stat_map <- function(path) {
  if (!file.exists(path)) {
    .cli_abort(paste0("Statistic map not found: ", path))
  }
  neuroim2::read_vol(path)
}

.cli_load_atlas <- function(spec) {
  if (is.null(spec) || !nzchar(spec)) {
    return(neuroatlas::get_schaefer_atlas(200, 7))
  }
  if (file.exists(spec)) {
    return(.cli_read_validated_rds(spec, class_name = "atlas", label = "atlas"))
  }

  parsed <- .cli_parse_atlas_spec(spec)
  kind <- parsed$kind

  switch(
    kind,
    schaefer = {
      parcels <- parsed$parcels %||% "200"
      networks <- parsed$networks %||% "7"
      neuroatlas::get_schaefer_atlas(parcels = parcels, networks = networks)
    },
    glasser = neuroatlas::get_glasser_atlas(),
    aseg = neuroatlas::get_aseg_atlas(),
    subcortical = {
      if (is.null(parsed$name) || !nzchar(parsed$name)) {
        .cli_abort("Use 'subcortical:<name>' for subcortical atlas specs.")
      }
      neuroatlas::get_subcortical_atlas(parsed$name)
    },
    .cli_abort(
      paste0(
        "Unsupported atlas spec '", spec,
        "'. Use an .rds path or a built-in spec like 'Schaefer400', ",
        "'Schaefer400x17', 'Glasser', 'ASEG', or 'subcortical:cit168'."
      )
    )
  )
}

.cli_load_surfatlas <- function(spec) {
  if (is.null(spec) || !nzchar(spec)) {
    return(NULL)
  }
  if (file.exists(spec)) {
    return(.cli_read_validated_rds(
      spec,
      class_name = "surfatlas",
      label = "surfatlas"
    ))
  }

  parsed <- .cli_parse_atlas_spec(spec)
  kind <- parsed$kind

  switch(
    kind,
    schaefer = {
      parcels <- parsed$parcels %||% "200"
      networks <- parsed$networks %||% "7"
      neuroatlas::get_schaefer_surfatlas(parcels = parcels, networks = networks)
    },
    glasser = neuroatlas::glasser_surf(),
    .cli_abort(
      paste0(
        "Unsupported surfatlas spec '", spec,
        "'. Use an .rds path or a built-in spec like 'Schaefer400', ",
        "'Schaefer400x17', or 'Glasser'."
      )
    )
  )
}

.cli_parse_atlas_spec <- function(spec) {
  raw <- trimws(as.character(spec))
  norm <- tolower(gsub("[ _]", "", raw))

  if (grepl("^schaefer:[0-9]+:[0-9]+$", norm)) {
    parts <- strsplit(norm, ":", fixed = TRUE)[[1]]
    return(list(kind = "schaefer", parcels = parts[2], networks = parts[3]))
  }

  if (grepl("^schaefer:[0-9]+$", norm)) {
    parts <- strsplit(norm, ":", fixed = TRUE)[[1]]
    return(list(kind = "schaefer", parcels = parts[2], networks = "7"))
  }

  if (grepl("^schaefer[0-9]+([x-][0-9]+)?$", norm)) {
    body <- sub("^schaefer", "", norm)
    parts <- strsplit(body, "[x-]")[[1]]
    return(list(
      kind = "schaefer",
      parcels = parts[1],
      networks = if (length(parts) >= 2L) parts[2] else "7"
    ))
  }

  if (norm %in% c("glasser", "glasser360")) {
    return(list(kind = "glasser"))
  }

  if (norm %in% c("aseg", "freesurferaseg")) {
    return(list(kind = "aseg"))
  }

  if (startsWith(norm, "subcortical:")) {
    return(list(kind = "subcortical", name = substring(raw, 13L)))
  }

  list(kind = norm)
}

.cli_parse_formulas <- function(values, default = NULL) {
  if (is.null(values)) {
    return(default)
  }

  vals <- as.character(values)
  forms <- vector("list", length(vals))
  names(forms) <- character(length(vals))

  for (i in seq_along(vals)) {
    raw <- vals[[i]]
    split_pos <- regexpr("::", raw, fixed = TRUE)[1]
    tilde_pos <- regexpr("~", raw, fixed = TRUE)[1]

    if (split_pos > 0L && tilde_pos > split_pos) {
      fname <- trimws(substr(raw, 1L, split_pos - 1L))
      formula_text <- substr(raw, split_pos + 2L, nchar(raw))
    } else {
      fname <- NULL
      formula_text <- raw
    }

    f <- .cli_safe_as_formula(formula_text)
    forms[[i]] <- f
    names(forms)[i] <- if (!is.null(fname) && nzchar(fname)) {
      fname
    } else {
      paste(deparse(f), collapse = " ")
    }
  }

  forms
}

.cli_normalize_report_formulas <- function(formulas, feature_name) {
  if (length(formulas) == 0L || is.null(feature_name) || !nzchar(feature_name)) {
    return(formulas)
  }

  out <- formulas
  for (i in seq_along(out)) {
    lhs <- rlang::f_lhs(out[[i]])
    if (is.null(lhs)) {
      next
    }

    lhs_text <- paste(deparse(lhs), collapse = " ")
    if (identical(lhs_text, "value")) {
      next
    }
    if (!identical(lhs_text, feature_name)) {
      warning(
        paste0(
          "Report formula '", paste(deparse(out[[i]]), collapse = " "),
          "' has left-hand side '", lhs_text,
          "'. Leaving it unchanged; it may fail later unless the data ",
          "already contains that column."
        ),
        call. = FALSE
      )
      next
    }

    rhs <- rlang::f_rhs(out[[i]])
    rhs_text <- paste(deparse(rhs), collapse = " ")
    out[[i]] <- stats::as.formula(
      paste("value ~", rhs_text),
      env = environment(out[[i]])
    )
  }

  names(out) <- names(formulas)
  out
}

.cli_read_validated_rds <- function(path, class_name, label) {
  obj <- tryCatch(
    readRDS(path),
    error = function(e) {
      .cli_abort(
        paste0(
          "Failed to read ", label, " from '", path,
          "': ", conditionMessage(e)
        )
      )
    }
  )

  if (!inherits(obj, class_name)) {
    .cli_abort(
      paste0(
        "File '", path, "' did not contain a valid ", label,
        " object (expected class '", class_name, "')."
      )
    )
  }

  obj
}

.cli_safe_as_formula <- function(text, env = parent.frame()) {
  formula_text <- trimws(as.character(text))
  if (!nzchar(formula_text)) {
    .cli_abort("Formula input cannot be empty.")
  }

  parsed <- tryCatch(
    parse(text = formula_text, keep.source = FALSE),
    error = function(e) {
      .cli_abort(
        paste0(
          "Invalid formula '", formula_text,
          "': ", conditionMessage(e)
        )
      )
    }
  )

  if (length(parsed) != 1L) {
    .cli_abort(
      paste0(
        "Formula '", formula_text,
        "' must contain exactly one expression."
      )
    )
  }

  expr <- parsed[[1]]
  if (!is.call(expr) || !identical(as.character(expr[[1]]), "~")) {
    .cli_abort(
      paste0("Expected a formula expression, got '", formula_text, "'.")
    )
  }

  bad_call <- .cli_find_dangerous_call(expr)
  if (!is.null(bad_call)) {
    .cli_abort(
      paste0(
        "Unsafe formula rejected: contains disallowed call '",
        bad_call, "'."
      )
    )
  }

  stats::as.formula(formula_text, env = env)
}

.cli_find_dangerous_call <- function(expr,
                                     denylist = c(
                                       "system", "system2", "shell",
                                       "eval", "evalq", "source", "parse"
                                     )) {
  if (!is.call(expr)) {
    return(NULL)
  }

  head <- expr[[1]]
  if (is.symbol(head)) {
    head_name <- as.character(head)
    if (head_name %in% denylist) {
      return(head_name)
    }

    if (head_name %in% c("::", ":::") && length(expr) >= 3L) {
      target <- expr[[3]]
      if (is.symbol(target)) {
        target_name <- as.character(target)
        if (target_name %in% denylist) {
          pkg_name <- paste(deparse(expr[[2]]), collapse = " ")
          return(paste0(pkg_name, head_name, target_name))
        }
      }
    }
  }
  if (is.call(head)) {
    bad_call <- .cli_find_dangerous_call(head, denylist = denylist)
    if (!is.null(bad_call)) {
      return(bad_call)
    }
  }

  if (length(expr) >= 2L) {
    for (i in 2:length(expr)) {
      bad_call <- .cli_find_dangerous_call(expr[[i]], denylist = denylist)
      if (!is.null(bad_call)) {
        return(bad_call)
      }
    }
  }

  NULL
}

.cli_parse_invocation <- function(args) {
  if (length(args) == 0L) {
    return(list(command = NULL, subcommand = NULL, options = list(), help = TRUE))
  }

  first <- args[[1]]
  if (first %in% c("-h", "--help", "help")) {
    command <- if (length(args) >= 2L) args[[2]] else NULL
    subcommand <- if (length(args) >= 3L) args[[3]] else NULL
    return(list(command = command, subcommand = subcommand,
                options = list(), help = TRUE))
  }

  command <- first
  subcommand <- NULL
  rest <- args[-1]

  if (identical(command, "manifest")) {
    if (length(rest) == 0L) {
      return(list(command = command, subcommand = NULL,
                  options = list(), help = TRUE))
    }
    subcommand <- rest[[1]]
    rest <- rest[-1]
  }

  options <- .cli_parse_options(rest)
  help <- isTRUE(.cli_opt_flag(options, "help", FALSE))

  list(
    command = command,
    subcommand = subcommand,
    options = options,
    help = help
  )
}

.cli_parse_options <- function(args) {
  opts <- list()
  positional <- character()

  i <- 1L
  n <- length(args)
  while (i <= n) {
    token <- args[[i]]
    if (!startsWith(token, "--")) {
      positional <- c(positional, token)
      i <- i + 1L
      next
    }

    token <- substring(token, 3L)
    key <- token
    value <- TRUE

    if (grepl("=", token, fixed = TRUE)) {
      split <- strsplit(token, "=", fixed = TRUE)[[1]]
      key <- split[1]
      value <- paste(split[-1], collapse = "=")
    } else if (startsWith(token, "no-")) {
      key <- substring(token, 4L)
      value <- FALSE
    } else if (i < n && !startsWith(args[[i + 1L]], "--")) {
      value <- args[[i + 1L]]
      i <- i + 1L
    }

    key <- gsub("-", "_", key, fixed = TRUE)
    if (is.null(opts[[key]])) {
      opts[[key]] <- value
    } else {
      opts[[key]] <- c(opts[[key]], value)
    }

    i <- i + 1L
  }

  opts$.args <- positional
  opts
}

.cli_read_table <- function(path) {
  if (!file.exists(path)) {
    .cli_abort(paste0("File not found: ", path))
  }

  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (ext %in% c("tsv", "txt")) {
    return(utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  .cli_abort("Design files must be CSV or TSV.")
}

.cli_resolve_manifest_path <- function(path) {
  if (dir.exists(path)) {
    candidates <- file.path(path, c("nftab.yaml", "nftab.yml", "nftab.json"))
    hits <- candidates[file.exists(candidates)]
    if (length(hits) == 0L) {
      .cli_abort(
        paste0("No nftab manifest found in directory: ", normalizePath(path))
      )
    }
    return(normalizePath(hits[[1]], mustWork = TRUE))
  }

  if (!file.exists(path)) {
    .cli_abort(paste0("Manifest not found: ", path))
  }

  normalizePath(path, mustWork = TRUE)
}

.cli_apply_path_template <- function(template, data) {
  placeholders <- gregexpr("\\{[^}]+\\}", template, perl = TRUE)
  matches <- regmatches(template, placeholders)[[1]]

  if (length(matches) == 0L) {
    return(rep(template, nrow(data)))
  }

  vars <- unique(substring(matches, 2L, nchar(matches) - 1L))
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0L) {
    .cli_abort(
      paste0(
        "Path template references missing design columns: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }

  vapply(seq_len(nrow(data)), function(i) {
    path <- template
    for (var in vars) {
      path <- gsub(
        pattern = paste0("\\{", var, "\\}"),
        replacement = as.character(data[[var]][i]),
        x = path
      )
    }
    path
  }, character(1))
}

.cli_parse_axes <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(NULL)
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

.cli_print_dataset_summary <- function(ds) {
  manifest <- ds$manifest
  features <- if (requireNamespace("neurotabs", quietly = TRUE)) {
    neurotabs::nf_feature_names(ds)
  } else {
    names(manifest$features)
  }
  axes <- if (requireNamespace("neurotabs", quietly = TRUE)) {
    neurotabs::nf_axes(ds)
  } else {
    manifest$observation_axes
  }

  cat("<nftab>", manifest$dataset_id, "\n")
  cat("Rows: ", nrow(ds$observations), "\n", sep = "")
  cat("Axes: ", if (length(axes) > 0L) paste(axes, collapse = ", ") else "<none>", "\n", sep = "")
  cat("Features: ", paste(features, collapse = ", "), "\n", sep = "")
}

.cli_opt_scalar <- function(opts, name, default = NULL) {
  value <- opts[[name]]
  if (is.null(value)) {
    return(default)
  }
  as.character(value[[length(value)]])
}

.cli_require_scalar <- function(opts, name) {
  value <- .cli_opt_scalar(opts, name, NULL)
  if (is.null(value) || !nzchar(value)) {
    .cli_abort(paste0("Missing required option '--", gsub("_", "-", name), "'."))
  }
  value
}

.cli_opt_flag <- function(opts, name, default = FALSE) {
  value <- opts[[name]]
  if (is.null(value)) {
    return(default)
  }
  .cli_as_flag(value[[length(value)]], default = default)
}

.cli_as_flag <- function(x, default = FALSE) {
  if (is.logical(x)) {
    return(isTRUE(x))
  }
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  value <- tolower(as.character(x[[1]]))
  if (value %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  default
}

.cli_opt_numeric <- function(opts, name, default) {
  value <- .cli_opt_scalar(opts, name, NULL)
  if (is.null(value)) {
    return(default)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.finite(numeric_value)) {
    .cli_abort(paste0("Option '--", gsub("_", "-", name), "' must be numeric."))
  }
  numeric_value
}

.cli_opt_integer <- function(opts, name, default) {
  value <- .cli_opt_scalar(opts, name, NULL)
  if (is.null(value)) {
    return(default)
  }
  integer_value <- suppressWarnings(as.integer(value))
  if (is.na(integer_value)) {
    .cli_abort(paste0("Option '--", gsub("_", "-", name), "' must be an integer."))
  }
  integer_value
}

.cli_require_namespace <- function(pkg, context) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .cli_abort(
      paste0("Package '", pkg, "' is required for ", context, ".")
    )
  }
}

.cli_abort <- function(...) {
  stop(..., call. = FALSE)
}

.cli_help_text <- function(command = NULL, subcommand = NULL) {
  atlas_help <- paste0(
    "Atlas specs:\n",
    "  Schaefer100, Schaefer200, Schaefer300, Schaefer400, Schaefer500,\n",
    "  Schaefer600, Schaefer800, Schaefer1000    (defaults to 7 networks)\n",
    "  Schaefer400x17, Schaefer400-17, or schaefer:400:17\n",
    "  Glasser / Glasser360\n",
    "  ASEG\n",
    "  subcortical:cit168, subcortical:hcp_thalamus,\n",
    "  subcortical:mdtb10, subcortical:hcp_hippamyg\n",
    "  Any .rds file containing an atlas object\n\n"
  )

  header <- paste0(
    "neuromosaic CLI\n",
    "Version ", utils::packageVersion("neuromosaic"), "\n\n"
  )

  top_level <- paste0(
    "Usage:\n",
    "  neuromosaic report   [options]\n",
    "  neuromosaic extract  [options]\n",
    "  neuromosaic explore  [options]\n",
    "  neuromosaic manifest create   [options]\n",
    "  neuromosaic manifest validate --manifest <path>\n",
    "  neuromosaic manifest show     --manifest <path>\n\n",
    "Dataset inputs:\n",
    "  --manifest <file-or-dir>       Existing nftab manifest\n",
    "  --design <csv-or-tsv>          Ad hoc design table\n",
    "  --feature <name>               Dataset feature name (dataset-backed modes)\n",
    "  --path-template <template>     One file per row, e.g. sub-{subject}/maps/AUC.nii.gz\n",
    "  --locator-col <column>         Existing path column in the design table\n",
    "  --shared-4d <file>             Shared 4D NIfTI, one volume per row\n\n",
    "Common options:\n",
    "  --stat-map <file>              Cluster-defining statistic map\n",
    "  --atlas <spec-or-rds>          Atlas spec, default schaefer:200:7\n",
    "  --strategy <lazy|collect>      Dataset access strategy, default lazy\n",
    "  --threshold <num>\n",
    "  --min-cluster-size <int>\n",
    "  --tail <two_sided|positive|negative>\n",
    "  --connectivity <26-connect|18-connect|6-connect>\n",
    "  --help                         Show this help text\n"
  )

  report_help <- paste0(
    "Usage: neuromosaic report [dataset options] --stat-map <file> [options]\n\n",
    "If you omit --manifest/--design, neuromosaic generates a stat-map-only table report.\n\n",
    "Options:\n",
    "  --feature <name>               Required for dataset-backed reports\n",
    "  --formula <expr>               Repeatable. Optional 'name::formula' labels supported.\n",
    "  --out <file>                   Output path (.html, .pdf, or .qmd), default neuromosaic-report.html\n",
    "  --template <file>              Optional custom Rmd/Qmd template\n",
    "  --table-style <gt|flextable|kable>\n",
    "  --brain-slices / --no-brain-slices\n",
    "  --quiet / --no-quiet\n\n",
    atlas_help
  )

  extract_help <- paste0(
    "Usage: neuromosaic extract [dataset options] --stat-map <file> [options]\n\n",
    "If you omit --manifest/--design, neuromosaic exports cluster/parcellation tables from the stat map alone.\n\n",
    "Options:\n",
    "  --feature <name>               Required for dataset-backed extracts\n",
    "  --formula <expr>               Optional report-style plot formula(s)\n",
    "  --dir <path>                   Output directory, default .\n",
    "  --prefix <name>                CSV prefix, default neuromosaic\n\n",
    atlas_help
  )

  explore_help <- paste0(
    "Usage: neuromosaic explore [dataset options] --feature <name> --stat-map <file> [options]\n\n",
    "Options:\n",
    "  --plot-formula <expr>          Launch with a formula-driven plot plugin\n",
    "  --surfatlas <spec-or-rds>      Optional custom surface atlas\n",
    "  --selection-engine <cluster|parcel|sphere|custom>\n\n",
    atlas_help
  )

  manifest_create_help <- paste0(
    "Usage: neuromosaic manifest create --design <csv-or-tsv> --feature <name> [locator options] --out <dir> [options]\n\n",
    "Locator options:\n",
    "  --path-template <template>\n",
    "  --locator-col <column>\n",
    "  --shared-4d <file>\n\n",
    "Options:\n",
    "  --axes <col1,col2>\n",
    "  --space <name>\n",
    "  --dataset-id <id>\n",
    "  --root <dir>\n",
    "  --manifest-name <file>         Default nftab.yaml\n"
  )

  manifest_validate_help <- "Usage: neuromosaic manifest validate --manifest <file-or-dir>\n"
  manifest_show_help <- "Usage: neuromosaic manifest show --manifest <file-or-dir>\n"

  if (is.null(command)) {
    return(paste0(header, top_level))
  }

  if (identical(command, "report")) {
    return(paste0(header, report_help))
  }
  if (identical(command, "extract")) {
    return(paste0(header, extract_help))
  }
  if (identical(command, "explore")) {
    return(paste0(header, explore_help))
  }
  if (identical(command, "manifest")) {
    if (identical(subcommand, "create")) {
      return(paste0(header, manifest_create_help))
    }
    if (identical(subcommand, "validate")) {
      return(paste0(header, manifest_validate_help))
    }
    if (identical(subcommand, "show")) {
      return(paste0(header, manifest_show_help))
    }
    return(paste0(
      header,
      "Usage: neuromosaic manifest <create|validate|show> [options]\n"
    ))
  }

  paste0(header, top_level)
}
