#' Build Explorer Data for a Montage Report
#'
#' Converts montage report data into the row-indexed structure used by
#' [montage_explorer()]. The same render manifest remains the source of truth:
#' each row maps to panel images, a peak/cluster table, QC metadata, and optional
#' signal data.
#'
#' @param report_data A report data list produced internally by
#'   [render_montage_report()] or [montage_explorer()].
#' @param manifest Optional manifest override when `report_data` is not
#'   supplied.
#' @param panels Optional named panel list keyed by `map_id`.
#' @param qc Optional QC data frame keyed by `map_id`.
#' @param design Optional design table exposed to signal-plot code.
#' @param signals Optional long data frame of design-linked signal values. When
#'   it contains a `map_id` column, the Shiny explorer filters it to the selected
#'   map row.
#'
#' @return A list with `manifest`, `panel_index`, `panels`, `cluster_tables`,
#'   `qc`, `design`, and `signals`.
#' @export
montage_explorer_data <- function(report_data = NULL,
                                  manifest = NULL,
                                  panels = NULL,
                                  qc = NULL,
                                  design = NULL,
                                  signals = NULL) {
  if (!is.null(report_data)) {
    manifest <- manifest %||% report_data$manifest
    panels <- panels %||% report_data$panels
    qc <- qc %||% report_data$qc
  }
  if (is.null(manifest) || !is.data.frame(manifest)) {
    stop("Provide a montage report data list or a manifest data frame.",
         call. = FALSE)
  }

  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  if (!"map_id" %in% names(manifest)) {
    stop("Montage explorer manifests must contain 'map_id'.", call. = FALSE)
  }
  map_ids <- as.character(manifest$map_id)
  panels <- .normalize_montage_panels(panels, map_ids)

  if (is.null(qc)) {
    qc <- .montage_qc_summary(manifest)
  }
  qc <- as.data.frame(qc, stringsAsFactors = FALSE)

  panel_index <- .montage_explorer_panel_index(manifest, panels, qc)
  cluster_tables <- stats::setNames(vector("list", length(map_ids)), map_ids)
  for (map_id in map_ids) {
    panel <- panels[[map_id]] %||% list()
    cluster_tables[[map_id]] <- panel$peak_table %||% panel$table %||% data.frame()
  }

  list(
    manifest = manifest,
    panel_index = panel_index,
    panels = panels,
    cluster_tables = cluster_tables,
    qc = qc,
    design = design,
    signals = signals
  )
}

#' Launch a Shiny Explorer for a Montage Manifest
#'
#' Builds a lightweight row picker over the montage report contract. Selecting a
#' manifest row shows the already-rendered volume/surface montage images, the
#' per-panel cluster/peak table, QC metadata, and optional design-linked signal
#' values.
#'
#' @param manifest A render manifest data frame. Ignored when `report_data` is
#'   supplied.
#' @param report_data Optional precomputed report data list.
#' @param title Explorer title.
#' @param layout,labeller,policy,bg,surfatlas,atlas,panels,render_volume,render_surface,render_peaks,image_dir,cache_dir,materialize_recipes,overwrite_recipes,cache_surface,image_width,image_height,image_res,max_clusters,validate,check_files,load_maps
#'   Arguments forwarded to the montage report-data preparation path when
#'   `report_data` is not supplied.
#' @param design Optional design table for signal plot context.
#' @param signals Optional long signal data frame. If a `map_id` column is
#'   present, it is filtered to the selected row.
#'
#' @return A `shiny.appobj`.
#' @export
montage_explorer <- function(manifest = NULL,
                             report_data = NULL,
                             title = "Montage Explorer",
                             layout = NULL,
                             labeller = NULL,
                             policy = NULL,
                             bg = NULL,
                             surfatlas = NULL,
                             atlas = NULL,
                             panels = NULL,
                             render_volume = !is.null(bg),
                             render_surface = !is.null(surfatlas),
                             render_peaks = !is.null(atlas),
                             image_dir = NULL,
                             cache_dir = NULL,
                             materialize_recipes = TRUE,
                             overwrite_recipes = FALSE,
                             cache_surface = TRUE,
                             image_width = 1400,
                             image_height = 1000,
                             image_res = 144,
                             max_clusters = 20L,
                             validate = TRUE,
                             check_files = TRUE,
                             load_maps = FALSE,
                             design = NULL,
                             signals = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for montage_explorer().", call. = FALSE)
  }

  if (is.null(report_data)) {
    if (is.null(manifest)) {
      stop("Provide 'manifest' or 'report_data'.", call. = FALSE)
    }
    report_data <- .prepare_montage_report_data(
      manifest = manifest,
      title = title,
      layout = layout,
      labeller = labeller,
      policy = policy,
      bg = bg,
      surfatlas = surfatlas,
      atlas = atlas,
      panels = panels,
      render_volume = render_volume,
      render_surface = render_surface,
      render_peaks = render_peaks,
      image_dir = image_dir %||%
        file.path(tempdir(), "neuromosaic-montage-explorer-images"),
      cache_dir = cache_dir,
      materialize_recipes = materialize_recipes,
      overwrite_recipes = overwrite_recipes,
      cache_surface = cache_surface,
      image_width = image_width,
      image_height = image_height,
      image_res = image_res,
      max_clusters = max_clusters,
      validate = validate,
      check_files = check_files,
      load_maps = load_maps,
      provenance = .montage_report_provenance()
    )
  }

  explorer <- montage_explorer_data(
    report_data = report_data,
    design = design,
    signals = signals
  )
  title <- report_data$params$title %||% title

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      paste(
        ".nm-montage-image{max-width:100%;height:auto;margin:0 0 16px 0;}",
        ".nm-montage-empty{color:#666;font-style:italic;}",
        ".nm-montage-meta table{width:100%;}"
      )
    ))),
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "map_id",
          "Map",
          choices = stats::setNames(
            explorer$panel_index$map_id,
            explorer$panel_index$label
          )
        ),
        shiny::div(class = "nm-montage-meta", shiny::tableOutput("metadata")),
        shiny::div(class = "nm-montage-meta", shiny::tableOutput("qc"))
      ),
      shiny::mainPanel(
        shiny::uiOutput("images"),
        shiny::tableOutput("cluster_table"),
        shiny::plotOutput("signal_plot", height = 260)
      )
    )
  )

  server <- function(input, output, session) {
    selected_map <- shiny::reactive({
      value <- input$map_id
      if (is.null(value) || !nzchar(value)) {
        explorer$panel_index$map_id[[1]]
      } else {
        value
      }
    })

    selected_row <- shiny::reactive({
      explorer$manifest[match(selected_map(), explorer$manifest$map_id), ,
                        drop = FALSE]
    })

    selected_panel <- shiny::reactive({
      explorer$panels[[selected_map()]] %||% list()
    })

    output$metadata <- shiny::renderTable({
      .montage_explorer_metadata(selected_row())
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$qc <- shiny::renderTable({
      panel <- selected_panel()
      qc <- panel$qc %||%
        explorer$qc[match(selected_map(), explorer$qc$map_id), , drop = FALSE]
      .montage_explorer_qc_table(qc)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$images <- shiny::renderUI({
      panel <- selected_panel()
      image_paths <- c(panel$volume_image, panel$surface_image)
      image_paths <- image_paths[nzchar(as.character(image_paths)) &
                                   file.exists(as.character(image_paths))]
      if (length(image_paths) == 0L) {
        return(shiny::tags$p(class = "nm-montage-empty", "No montage images."))
      }
      shiny::tagList(lapply(image_paths, function(path) {
        shiny::tags$img(
          class = "nm-montage-image",
          src = knitr::image_uri(path),
          alt = basename(path)
        )
      }))
    })

    output$cluster_table <- shiny::renderTable({
      tbl <- explorer$cluster_tables[[selected_map()]]
      if (!is.data.frame(tbl) || nrow(tbl) == 0L) {
        return(data.frame(Message = "No cluster table."))
      }
      tbl
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$signal_plot <- shiny::renderPlot({
      .plot_montage_explorer_signal(explorer$signals, selected_map())
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

.montage_explorer_panel_index <- function(manifest, panels, qc) {
  map_ids <- as.character(manifest$map_id)
  out <- data.frame(
    map_id = map_ids,
    label = if ("label" %in% names(manifest)) {
      as.character(manifest$label)
    } else {
      map_ids
    },
    volume_image = NA_character_,
    surface_image = NA_character_,
    n_clusters = NA_integer_,
    qc_status = NA_character_,
    effective_n = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(map_ids)) {
    panel <- panels[[map_ids[[i]]]] %||% list()
    out$volume_image[[i]] <- panel$volume_image %||% NA_character_
    out$surface_image[[i]] <- panel$surface_image %||% NA_character_
    tbl <- panel$peak_table %||% panel$table
    out$n_clusters[[i]] <- if (is.data.frame(tbl)) nrow(tbl) else NA_integer_
  }

  if (is.data.frame(qc) && "map_id" %in% names(qc)) {
    idx <- match(out$map_id, qc$map_id)
    if ("qc_status" %in% names(qc)) {
      out$qc_status <- qc$qc_status[idx]
    }
    if ("effective_n" %in% names(qc)) {
      out$effective_n <- qc$effective_n[idx]
    }
  }

  out
}

.montage_explorer_metadata <- function(row) {
  fields <- intersect(
    c("map_id", "label", "contrast", "model", "variant", "stat_kind",
      "units", "effective_threshold", "effective_tail", "cap_key", "path"),
    names(row)
  )
  values <- vapply(fields, function(field) {
    value <- row[[field]][[1]]
    if (is.null(value) || length(value) == 0L || is.na(value)) "" else {
      as.character(value)
    }
  }, character(1))
  keep <- nzchar(values)
  data.frame(Field = fields[keep], Value = values[keep], check.names = FALSE)
}

.montage_explorer_qc_table <- function(qc) {
  if (!is.data.frame(qc) || nrow(qc) == 0L) {
    return(data.frame(Message = "No QC metadata."))
  }
  fields <- intersect(
    c("effective_n", "source_n", "dropped_n", "dropped_subjects", "qc_status"),
    names(qc)
  )
  values <- vapply(fields, function(field) {
    value <- qc[[field]][[1]]
    if (is.null(value) || length(value) == 0L || is.na(value)) "" else {
      as.character(value)
    }
  }, character(1))
  keep <- nzchar(values)
  if (!any(keep)) {
    return(data.frame(Message = "No QC metadata."))
  }
  data.frame(Field = fields[keep], Value = values[keep], check.names = FALSE)
}

.plot_montage_explorer_signal <- function(signals, map_id) {
  if (!is.data.frame(signals) || nrow(signals) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No signal data")
    return(invisible(NULL))
  }

  dat <- as.data.frame(signals, stringsAsFactors = FALSE)
  if ("map_id" %in% names(dat)) {
    dat <- dat[as.character(dat$map_id) == map_id, , drop = FALSE]
  }
  if (nrow(dat) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No signal data for selected map")
    return(invisible(NULL))
  }

  numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
  if (length(numeric_cols) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No numeric signal column")
    return(invisible(NULL))
  }
  y_col <- intersect(c("signal", "value", "mean_signal"), numeric_cols)
  y_col <- if (length(y_col) > 0L) y_col[[1]] else numeric_cols[[1]]
  x_col <- intersect(c("time", "trial", ".sample_index"), names(dat))
  if (length(x_col) == 0L) {
    dat$.sample_index <- seq_len(nrow(dat))
    x_col <- ".sample_index"
  } else {
    x_col <- x_col[[1]]
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = x_col, y = y_col)
  print(p)
  invisible(p)
}
