#' Infer Design Variable Type
#'
#' Classify a vector as \code{"continuous"} or \code{"categorical"} for plot
#' defaults.
#'
#' @param x A vector.
#' @return A character scalar.
#' @export
infer_design_var_type <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return("continuous")
  }
  if (is.numeric(x) || is.integer(x) || is.double(x)) {
    return("continuous")
  }
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return("categorical")
  }
  "categorical"
}

.resolve_plugin_param_choices <- function(def, context = list()) {
  choices <- def$choices
  if (is.function(choices)) {
    choices <- choices(context)
  }
  if (is.null(choices)) character(0) else choices
}

.as_analysis_plugin <- function(x, fallback_id = "plugin") {
  if (is.null(x)) return(NULL)

  if (is.function(x)) {
    return(list(
      id = fallback_id,
      label = fallback_id,
      run = x,
      param_defs = list()
    ))
  }

  if (is.list(x)) {
    if (!is.function(x$run)) {
      stop("Analysis plugin must define a callable '$run' function.",
           call. = FALSE)
    }
    id <- if (!is.null(x$id) && nzchar(x$id)) as.character(x$id) else fallback_id
    label <- if (!is.null(x$label) && nzchar(x$label)) {
      as.character(x$label)
    } else {
      id
    }
    defs <- x$param_defs
    if (is.null(defs)) defs <- list()
    return(list(
      id = id,
      label = label,
      run = x$run,
      param_defs = defs
    ))
  }

  stop("Analysis plugins must be functions or lists with fields id/label/run.",
       call. = FALSE)
}

.as_plot_plugin <- function(x, fallback_id = "plot_plugin") {
  if (is.null(x)) return(NULL)

  if (is.function(x)) {
    return(list(
      id = fallback_id,
      label = fallback_id,
      render = x,
      param_defs = list()
    ))
  }

  if (is.list(x)) {
    if (!is.function(x$render)) {
      stop("Plot plugin must define a callable '$render' function.",
           call. = FALSE)
    }
    id <- if (!is.null(x$id) && nzchar(x$id)) as.character(x$id) else fallback_id
    label <- if (!is.null(x$label) && nzchar(x$label)) {
      as.character(x$label)
    } else {
      id
    }
    defs <- x$param_defs
    if (is.null(defs)) defs <- list()
    return(list(
      id = id,
      label = label,
      render = x$render,
      param_defs = defs
    ))
  }

  stop("Plot plugins must be functions or lists with fields id/label/render.",
       call. = FALSE)
}

# -- Built-in analysis plugins -------------------------------------------------

.builtin_group_mean_plugin <- function() {
  list(
    id = "group_mean",
    label = "Group Mean \u00b1 SE",
    run = function(ts_data, design, params = list(), context = list()) {
      gvar <- params$group_var
      if (is.null(gvar) || !nzchar(gvar) || !gvar %in% names(ts_data)) {
        avail <- setdiff(names(ts_data),
                         c("signal", "cluster_id", ".sample_index", "sign"))
        return(list(
          data = ts_data, design = design,
          diagnostics = list(
            status = "info",
            reason = paste0(
              "Enter a design column name to group by. Available: ",
              paste(avail, collapse = ", ")
            )
          ),
          meta = list()
        ))
      }

      grp <- dplyr::group_by(ts_data,
                              dplyr::across(dplyr::all_of(c("cluster_id", gvar))))
      summ <- dplyr::summarise(
        grp,
        signal = mean(.data$signal, na.rm = TRUE),
        se = stats::sd(.data$signal, na.rm = TRUE) / sqrt(dplyr::n()),
        n = dplyr::n(),
        .groups = "drop"
      )
      summ$.sample_index <- seq_len(nrow(summ))

      list(data = summ, design = design, diagnostics = NULL, meta = list())
    },
    param_defs = list(
      list(
        name = "group_var",
        label = "Group by (column name)",
        type = "text",
        default = "",
        help = "Name of a design column (e.g. condition, group)"
      )
    )
  )
}

.normalize_analysis_plugins <- function(analysis_plugins = NULL,
                                        default_plugin = "none") {
  none_plugin <- list(
    id = "none",
    label = "None (raw signal)",
    run = function(ts_data, design, params = list(), context = list()) {
      list(data = ts_data, design = design, diagnostics = NULL, meta = list())
    },
    param_defs = list()
  )

  plugs <- list(none = none_plugin, group_mean = .builtin_group_mean_plugin())
  if (!is.null(analysis_plugins)) {
    if (!is.list(analysis_plugins)) {
      analysis_plugins <- list(analysis_plugins)
    }
    nm <- names(analysis_plugins)
    for (i in seq_along(analysis_plugins)) {
      fallback_id <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else {
        paste0("plugin", i)
      }
      p <- .as_analysis_plugin(analysis_plugins[[i]], fallback_id = fallback_id)
      plugs[[p$id]] <- p
    }
  }

  if (!default_plugin %in% names(plugs)) {
    default_plugin <- "none"
  }

  plugs <- c(plugs[setdiff(names(plugs), default_plugin)],
             plugs[default_plugin])
  plugs[c(default_plugin, setdiff(names(plugs), default_plugin))]
}

.builtin_auto_plot_plugin <- function() {
  list(
    id = "auto",
    label = "Auto",
    render = function(data, params = list(), context = list(),
                      interactive = FALSE) {
      .build_design_plot(
        data = data,
        x_var = context$x_var %||% ".sample_index",
        collapse_vars = context$collapse_vars %||% character(0),
        interactive = interactive
      )
    },
    param_defs = list()
  )
}

.builtin_group_overlay_plot_plugin <- function() {
  list(
    id = "group_overlay",
    label = "Grouped Overlay",
    render = function(data, params = list(), context = list(),
                      interactive = FALSE) {
      group_var <- params$group_var %||% ""
      base_plot <- .build_design_plot(
        data = data,
        x_var = context$x_var %||% ".sample_index",
        collapse_vars = context$collapse_vars %||% character(0),
        group_var = if (nzchar(group_var)) group_var else NULL,
        interactive = interactive
      )

      if (!nzchar(group_var)) {
        return(list(
          plot = base_plot,
          diagnostics = list(
            status = "info",
            reason = "Select a grouping variable to overlay group-specific colors and fits."
          )
        ))
      }

      list(plot = base_plot, diagnostics = NULL)
    },
    param_defs = list(
      list(
        name = "group_var",
        label = "Grouping variable",
        type = "select",
        default = "",
        choices = function(context) {
          cols <- context$categorical_columns %||% character(0)
          c("None" = "", stats::setNames(cols, cols))
        },
        help = paste(
          "Categorical column used to color points and overlay group-specific",
          "summaries or fits."
        )
      )
    )
  )
}

#' Create a Formula-Driven Plot Plugin for cluster_explorer()
#'
#' Wrap a plot formula into a cluster explorer plot plugin. The formula controls
#' layout only: the final right-hand-side term becomes the x-axis, the last
#' remaining term maps to colour, and an earlier remaining term maps to facet
#' panels when present. The left-hand side is ignored and may be used as a
#' symbolic label such as \code{AUC ~ measure + group}.
#'
#' @param formula A formula object or single formula string.
#' @param id Plugin identifier. Default \code{"formula"}.
#' @param label Optional display label. Defaults to the formula text.
#'
#' @return A plot plugin definition suitable for \code{plot_plugins}.
#' @export
formula_plot_plugin <- function(formula,
                                id = "formula",
                                label = NULL) {
  if (is.character(formula)) {
    if (length(formula) != 1L || !nzchar(formula)) {
      stop("'formula' must be a non-empty formula string.", call. = FALSE)
    }
    formula <- stats::as.formula(formula)
  }
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula or a single formula string.",
         call. = FALSE)
  }

  formula_text <- paste(deparse(formula), collapse = " ")
  if (is.null(label) || !nzchar(label)) {
    label <- formula_text
  }

  list(
    id = as.character(id),
    label = as.character(label),
    render = function(data, params = list(), context = list(),
                      interactive = FALSE) {
      parsed <- .resolve_formula_plot_mapping(formula, data)
      if (is.null(parsed$x_var) || !nzchar(parsed$x_var)) {
        reason <- paste0(
          "Formula does not reference any columns available in the extracted signal table."
        )
        return(list(
          plot = .empty_plot("Formula variables not available",
                             subtitle = reason),
          diagnostics = list(
            status = "error",
            reason = reason,
            formula = formula_text
          ),
          meta = list(formula = formula_text, failed = TRUE)
        ))
      }
      required <- unique(stats::na.omit(c(
        parsed$x_var,
        parsed$color_var,
        parsed$facet_var
      )))
      missing <- setdiff(required, names(data))
      if (length(missing) > 0) {
        reason <- paste0(
          "Formula references unavailable columns: ",
          paste(missing, collapse = ", "),
          "."
        )
        return(list(
          plot = .empty_plot("Formula variables not available",
                             subtitle = reason),
          diagnostics = list(
            status = "error",
            reason = reason,
            formula = formula_text
          ),
          meta = list(formula = formula_text, failed = TRUE)
        ))
      }

      list(
        plot = .build_design_plot(
          data = data,
          x_var = parsed$x_var,
          group_var = parsed$color_var,
          facet_var = parsed$facet_var,
          interactive = interactive
        ),
        diagnostics = NULL,
        meta = list(formula = formula_text)
      )
    },
    param_defs = list()
  )
}

.resolve_formula_plot_mapping <- function(formula, data) {
  rhs_terms <- all.vars(rlang::f_rhs(formula))
  rhs_terms <- rhs_terms[rhs_terms %in% names(data)]
  if (length(rhs_terms) == 0L) {
    return(list(x_var = NULL, color_var = NULL, facet_var = NULL))
  }

  if ("time" %in% rhs_terms) {
    x_var <- "time"
  } else {
    continuous_terms <- rhs_terms[
      vapply(data[rhs_terms], infer_design_var_type, character(1)) ==
        "continuous"
    ]
    if (length(continuous_terms) > 0L) {
      x_var <- continuous_terms[[length(continuous_terms)]]
    } else {
      x_var <- rhs_terms[[length(rhs_terms)]]
    }
  }

  non_x <- setdiff(rhs_terms, x_var)
  if (length(non_x) == 0L) {
    return(list(x_var = x_var, color_var = NULL, facet_var = NULL))
  }

  categorical_terms <- non_x[
    vapply(data[non_x], infer_design_var_type, character(1)) ==
      "categorical"
  ]
  if (length(categorical_terms) > 0L) {
    color_var <- categorical_terms[[length(categorical_terms)]]
    facet_candidates <- setdiff(categorical_terms, color_var)
    facet_var <- if (length(facet_candidates) > 0L) facet_candidates[[1L]] else NULL
  } else {
    color_var <- NULL
    facet_var <- NULL
  }

  list(x_var = x_var, color_var = color_var, facet_var = facet_var)
}

.normalize_plot_plugins <- function(plot_plugins = NULL,
                                    default_plugin = "auto") {
  plugs <- list(
    auto = .builtin_auto_plot_plugin(),
    group_overlay = .builtin_group_overlay_plot_plugin()
  )
  if (!is.null(plot_plugins)) {
    if (!is.list(plot_plugins)) {
      plot_plugins <- list(plot_plugins)
    }
    nm <- names(plot_plugins)
    for (i in seq_along(plot_plugins)) {
      fallback_id <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else {
        paste0("plot_plugin", i)
      }
      p <- .as_plot_plugin(plot_plugins[[i]], fallback_id = fallback_id)
      plugs[[p$id]] <- p
    }
  }

  if (!default_plugin %in% names(plugs)) {
    default_plugin <- "auto"
  }

  plugs <- c(plugs[setdiff(names(plugs), default_plugin)],
             plugs[default_plugin])
  plugs[c(default_plugin, setdiff(names(plugs), default_plugin))]
}

.plugin_param_ui <- function(plugin,
                             input_prefix,
                             empty_text = "No plugin parameters.",
                             context = list()) {
  val_or <- function(x, y) if (is.null(x)) y else x

  defs <- plugin$param_defs
  if (length(defs) == 0) {
    return(shiny::tags$div(class = "ce-help", empty_text))
  }

  controls <- lapply(defs, function(def) {
    type <- if (!is.null(def$type)) as.character(def$type) else "numeric"
    name <- as.character(def$name)
    label <- if (!is.null(def$label)) as.character(def$label) else name
    if (!is.null(def$help) && nzchar(as.character(def$help))) {
      label <- .ce_label_with_help(label, as.character(def$help))
    }
    input_id <- paste0(input_prefix, name)
    value <- def$default
    choices <- .resolve_plugin_param_choices(def, context = context)
    multiple <- isTRUE(def$multiple)

    switch(
      type,
      numeric = shiny::numericInput(
        input_id, label, value = as.numeric(val_or(value, 0)),
        min = if (!is.null(def$min)) as.numeric(def$min) else NA_real_,
        max = if (!is.null(def$max)) as.numeric(def$max) else NA_real_,
        step = if (!is.null(def$step)) as.numeric(def$step) else NA_real_
      ),
      integer = shiny::numericInput(
        input_id, label, value = as.integer(val_or(value, 0L)),
        min = if (!is.null(def$min)) as.integer(def$min) else NA_integer_,
        max = if (!is.null(def$max)) as.integer(def$max) else NA_integer_,
        step = if (!is.null(def$step)) as.integer(def$step) else 1L
      ),
      logical = shiny::checkboxInput(
        input_id, label, value = isTRUE(value)
      ),
      text = shiny::textInput(
        input_id, label, value = as.character(val_or(value, ""))
      ),
      select = shiny::selectInput(
        input_id, label,
        choices = choices,
        selected = value,
        multiple = multiple
      ),
      selectize = shiny::selectizeInput(
        input_id, label,
        choices = choices,
        selected = value,
        multiple = multiple
      ),
      shiny::textInput(
        input_id, label, value = as.character(val_or(value, ""))
      )
    )
  })

  shiny::tagList(controls)
}

.analysis_plugin_param_ui <- function(plugin, context = list()) {
  .plugin_param_ui(
    plugin = plugin,
    input_prefix = "analysis_param_",
    empty_text = "No analysis parameters.",
    context = context
  )
}

.plot_plugin_param_ui <- function(plugin, context = list()) {
  .plugin_param_ui(
    plugin = plugin,
    input_prefix = "plot_param_",
    empty_text = "No plot parameters.",
    context = context
  )
}

.collect_plugin_params <- function(input, plugin, input_prefix) {
  defs <- plugin$param_defs
  if (length(defs) == 0) return(list())

  params <- lapply(defs, function(def) {
    input_id <- paste0(input_prefix, as.character(def$name))
    val <- input[[input_id]]
    if (is.null(val) && !is.null(def$default)) {
      val <- def$default
    }
    val
  })
  names(params) <- vapply(defs, function(def) as.character(def$name), character(1))
  params
}

.collect_analysis_params <- function(input, plugin) {
  .collect_plugin_params(
    input = input,
    plugin = plugin,
    input_prefix = "analysis_param_"
  )
}

.collect_plot_params <- function(input, plugin) {
  .collect_plugin_params(
    input = input,
    plugin = plugin,
    input_prefix = "plot_param_"
  )
}

.run_analysis_plugin <- function(plugin,
                                 ts_data,
                                 design,
                                 params = list(),
                                 context = list()) {
  if (nrow(ts_data) == 0 || is.null(plugin) || identical(plugin$id, "none")) {
    return(list(data = ts_data, design = design, diagnostics = NULL, meta = list()))
  }

  raw <- tryCatch(
    plugin$run(ts_data = ts_data, design = design, params = params,
               context = context),
    error = function(e) {
      list(
        data = ts_data,
        design = design,
        diagnostics = list(
          status = "error",
          reason = conditionMessage(e),
          plugin = plugin$id
        ),
        meta = list(plugin_id = plugin$id, failed = TRUE)
      )
    }
  )

  if (is.data.frame(raw)) {
    raw <- list(data = tibble::as_tibble(raw), design = design,
                diagnostics = NULL, meta = list(plugin_id = plugin$id))
  }

  if (!is.list(raw)) {
    return(list(
      data = ts_data,
      design = design,
      diagnostics = list(
        status = "error",
        reason = "Plugin returned unsupported output type."
      ),
      meta = list(plugin_id = plugin$id, failed = TRUE)
    ))
  }

  if (is.null(raw$data) || !is.data.frame(raw$data)) {
    raw$data <- ts_data
  } else {
    raw$data <- tibble::as_tibble(raw$data)
  }

  required_cols <- c("signal", "cluster_id")
  missing_cols <- setdiff(required_cols, names(raw$data))
  if (length(missing_cols) > 0) {
    raw$data <- ts_data
    raw$diagnostics <- list(
      status = "error",
      reason = paste0(
        "Plugin '", plugin$id, "' output missing required columns: ",
        paste(missing_cols, collapse = ", "),
        ". Falling back to raw signal."
      ),
      plugin = plugin$id
    )
    raw$meta <- list(plugin_id = plugin$id, failed = TRUE)
  }

  if (is.null(raw$design) || !is.data.frame(raw$design)) {
    raw$design <- design
  } else {
    raw$design <- tibble::as_tibble(raw$design)
  }
  if (is.null(raw$meta)) raw$meta <- list()
  raw$meta$plugin_id <- plugin$id
  raw
}

.run_plot_plugin <- function(plugin,
                             data,
                             params = list(),
                             context = list(),
                             interactive = FALSE) {
  if (nrow(data) == 0 || is.null(plugin)) {
    p <- .empty_plot("No data available")
    if (isTRUE(interactive)) p <- ggiraph::girafe(ggobj = p)
    return(list(plot = p, diagnostics = NULL, meta = list()))
  }

  raw <- tryCatch(
    plugin$render(
      data = data,
      params = params,
      context = context,
      interactive = interactive
    ),
    error = function(e) {
      list(
        plot = .empty_plot("Plot rendering failed", subtitle = conditionMessage(e)),
        diagnostics = list(
          status = "error",
          reason = conditionMessage(e),
          plugin = plugin$id
        ),
        meta = list(plugin_id = plugin$id, failed = TRUE)
      )
    }
  )

  if (inherits(raw, "ggplot") || inherits(raw, "gg") || inherits(raw, "girafe")) {
    raw <- list(plot = raw, diagnostics = NULL, meta = list(plugin_id = plugin$id))
  }

  if (!is.list(raw)) {
    raw <- list(
      plot = .empty_plot("Plot plugin returned unsupported output."),
      diagnostics = list(
        status = "error",
        reason = "Plot plugin returned unsupported output type.",
        plugin = plugin$id
      ),
      meta = list(plugin_id = plugin$id, failed = TRUE)
    )
  }

  if (is.null(raw$plot)) {
    raw$plot <- .empty_plot("No plot returned")
  }
  if (isTRUE(interactive) &&
      !inherits(raw$plot, "girafe") &&
      (inherits(raw$plot, "ggplot") || inherits(raw$plot, "gg"))) {
    raw$plot <- ggiraph::girafe(ggobj = raw$plot)
  }
  if (is.null(raw$meta)) raw$meta <- list()
  raw$meta$plugin_id <- plugin$id
  raw
}
