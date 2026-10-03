#' Montage Report Template Formatters
#'
#' Shared table, metadata, image, and QC emitters used by the bundled montage
#' R Markdown and Quarto templates. Keeping these helpers in package code avoids
#' template drift and makes report formatting testable without rendering through
#' Pandoc.
#'
#' @param manifest Optional render manifest used by the layout-heading emitter.
#' @param layout Optional character vector of layout columns.
#' @param is_html Logical; emit images as base64 HTML tags when `TRUE`, otherwise
#'   emit markdown image links.
#' @param intro Optional report-level preamble. A markdown character scalar (or
#'   vector collapsed with blank lines) emitted once before the maps by
#'   `emit_intro()`.
#' @param section_notes Optional data frame of section-level narrative keyed by
#'   `layout` column values plus a `text` column. A row that sets the first *k*
#'   layout columns (leaving deeper ones `NA`) has its text emitted under that
#'   section's heading. See [render_montage_report()] for the contract.
#' @param interludes Optional data frame of free-standing inter-map narrative
#'   with columns `map_id`, `text`, and optional `position` (`"before"` or
#'   `"after"`), emitted around the named panel by `emit_interludes()`.
#' @param groups Optional analysis-group metadata produced by the report engine.
#' @param map_selector HTML selector style; see [render_montage_report()].
#' @param panels Optional per-map panel data produced by the report engine.
#' @param figure_assets Optional list with `dir` (filesystem directory) and
#'   `href` (relative URL prefix). When supplied, HTML panel images are copied
#'   into `dir` under content-addressed names and referenced by relative URL
#'   instead of being inlined as data URIs. [render_montage_report()] sets this
#'   for bundle-mode reports, whose HTML already travels with a companion
#'   directory.
#'
#' @return A named list of formatter and emitter functions.
#' @export
montage_report_formatters <- function(manifest = NULL,
                                      layout = character(),
                                      is_html = FALSE,
                                      intro = NULL,
                                      section_notes = NULL,
                                      interludes = NULL,
                                      groups = NULL,
                                      map_selector = "auto",
                                      panels = NULL,
                                      figure_assets = NULL) {
  layout_state <- new.env(parent = emptyenv())
  # Section-note change detection for the HTML path is tracked separately from
  # `layout_state` (which drives markdown/PDF headings) so the two never share
  # state; only one branch runs per render, but keeping them distinct avoids any
  # cross-talk if a custom template calls both emitters.
  section_state <- new.env(parent = emptyenv())
  page_state <- new.env(parent = emptyenv())
  layout <- layout %||% character()
  map_selector <- match.arg(
    map_selector,
    c("auto", "tabs", "select", "none")
  )
  if (is.null(manifest)) {
    manifest <- data.frame()
  }

  list(
    fmt_val = .montage_report_fmt_val,
    tail_label = .montage_report_tail_label,
    status_label = .montage_report_status_label,
    qc_has_values = .montage_report_qc_has_values,
    format_peak_table = .montage_report_format_peak_table,
    emit_report_styles = function() {
      .montage_report_emit_styles(is_html = isTRUE(is_html))
    },
    emit_intro = function() {
      .montage_report_emit_narrative_block(
        intro, "nm-intro", is_html = isTRUE(is_html)
      )
    },
    emit_report_header = function(title, date = NULL, qc = NULL, labelling = NULL) {
      qc_line <- .montage_report_qc_line(qc)
      label_line <- .montage_report_label_line(labelling)
      if (isTRUE(is_html)) {
        page_state$qc_in_header <- !is.null(qc_line)
        # Shared spec parts (N included: the QC line or the facts line says it).
        page_state$spec_stated <- .montage_report_shared_spec(manifest, n_stated = FALSE)
        cat(.montage_report_header_html(
          title = title, date = date, manifest = manifest, layout = layout,
          groups = groups, qc_line = qc_line, label_line = label_line
        ))
        return(invisible(NULL))
      }
      .montage_report_emit_report_header(
        title = title, date = date, manifest = manifest, layout = layout,
        groups = groups, is_html = FALSE
      )
      if (!is.null(label_line)) cat(label_line, "\n\n", sep = "")
      invisible(NULL)
    },
    emit_results_overview = function(heading = "Results at a glance") {
      .montage_report_emit_results_overview(
        groups = groups, manifest = manifest, panels = panels,
        layout = layout, is_html = isTRUE(is_html),
        figure_assets = figure_assets, heading = heading
      )
    },
    emit_map_start = function(i, label, analysis_id) {
      .montage_report_emit_map_start(
        i = i, label = label, analysis_id = as.character(analysis_id),
        manifest = manifest, layout = layout, layout_state = layout_state,
        section_notes = section_notes, section_state = section_state,
        is_html = isTRUE(is_html)
      )
    },
    emit_map_end = function() {
      if (isTRUE(is_html)) cat("</article>\n\n")
      invisible(NULL)
    },
    emit_view_slot = function(analysis_id, label = "") {
      .montage_report_emit_view_slot(analysis_id, label, is_html = isTRUE(is_html))
    },
    emit_methods = function(report_data) {
      .montage_report_emit_methods(report_data, is_html = isTRUE(is_html))
    },
    emit_provenance = function(prov) {
      .montage_report_emit_provenance(prov, is_html = isTRUE(is_html))
    },
    emit_summary_matrix = function() {
      .montage_report_emit_summary_matrix(
        groups = groups, manifest = manifest, panels = panels,
        layout = layout, is_html = isTRUE(is_html)
      )
    },
    emit_maps_controls = function() {
      .montage_report_emit_maps_controls(
        groups = groups, manifest = manifest, map_selector = map_selector,
        is_html = isTRUE(is_html)
      )
    },
    emit_report_overview = function() {
      .montage_report_emit_report_overview(
        manifest = manifest,
        layout = layout,
        is_html = isTRUE(is_html)
      )
    },
    emit_panel_image = function(path, alt, empty = FALSE, caption = NULL,
                                dark = NULL, narrow = NULL, narrow_dark = NULL,
                                full_caption = NULL, download_name = NULL,
                                title = NULL, kicker = NULL) {
      .montage_report_emit_panel_image(path, alt, is_html = isTRUE(is_html),
                                       empty = empty,
                                       figure_assets = figure_assets,
                                       caption = caption, dark = dark,
                                       narrow = narrow, narrow_dark = narrow_dark,
                                       full_caption = full_caption,
                                       download_name = download_name,
                                       title = title, kicker = kicker)
    },
    emit_map_figures = function(row, panel, empty = FALSE) {
      .montage_report_emit_map_figures(
        row, panel, empty = empty, is_html = isTRUE(is_html),
        figure_assets = figure_assets, layout = layout
      )
    },
    figure_caption = .montage_report_figure_caption,
    emit_table = function(tbl, row = NULL) {
      .montage_report_emit_table(tbl, is_html = isTRUE(is_html), row = row)
    },
    emit_metadata = function(row) {
      .montage_report_emit_metadata(
        row, is_html = isTRUE(is_html),
        omit = if (isTRUE(is_html)) page_state$spec_stated %||% character() else character()
      )
    },
    emit_qc_summary = function(qc_tbl) {
      .montage_report_emit_qc_summary(
        qc_tbl, is_html = isTRUE(is_html),
        stated = isTRUE(page_state$qc_in_header)
      )
    },
    emit_panel_qc = function(qc_row) {
      .montage_report_emit_panel_qc(qc_row, is_html = isTRUE(is_html))
    },
    emit_interludes = function(map_id, position) {
      .montage_report_emit_interludes(
        interludes, map_id, position, is_html = isTRUE(is_html)
      )
    },
    emit_panel_heading = function(i, label) {
      .montage_report_emit_panel_heading(
        i = i,
        label = label,
        manifest = manifest,
        layout = layout,
        layout_state = layout_state,
        is_html = isTRUE(is_html),
        section_notes = section_notes,
        section_state = section_state
      )
    },
    emit_group_start = function(group, primary_i, rows) {
      .montage_report_emit_group_start(
        group = group,
        primary_i = primary_i,
        rows = rows,
        manifest = manifest,
        layout = layout,
        layout_state = layout_state,
        section_notes = section_notes,
        section_state = section_state,
        map_selector = map_selector,
        is_html = isTRUE(is_html),
        panels = panels
      )
    },
    emit_group_end = function() {
      if (isTRUE(is_html)) cat("</section>\n\n")
      invisible(NULL)
    },
    emit_variant_start = function(row, group, position) {
      .montage_report_emit_variant_start(
        row = row,
        group = group,
        position = position,
        map_selector = map_selector,
        is_html = isTRUE(is_html)
      )
    },
    emit_variant_end = function() {
      if (isTRUE(is_html)) cat("</article>\n")
      invisible(NULL)
    },
    emit_layout_headings = function(i) {
      .montage_report_emit_layout_headings(
        i = i,
        manifest = manifest,
        layout = layout,
        layout_state = layout_state,
        section_notes = section_notes
      )
    }
  )
}

# Emit an author-provided markdown narrative block. `text` may be a character
# vector (collapsed with blank lines). In HTML we wrap the block in a pandoc
# fenced div so the class is available for styling *and* the inner markdown is
# still parsed; in PDF/markdown we emit the raw markdown (an unknown fenced-div
# class would otherwise be dropped by pandoc's LaTeX writer). Mirrors the
# convention used for per-panel `description` text: author markdown, emitted
# verbatim, never HTML-escaped.
.montage_report_emit_narrative_block <- function(text, class, is_html) {
  if (is.null(text) || length(text) == 0L) {
    return(invisible(NULL))
  }
  text <- paste(text[!is.na(text)], collapse = "\n\n")
  if (!nzchar(trimws(text))) {
    return(invisible(NULL))
  }
  if (isTRUE(is_html)) {
    cat("\n::: {.", class, "}\n\n", text, "\n\n:::\n\n", sep = "")
  } else {
    cat("\n", text, "\n\n", sep = "")
  }
  invisible(NULL)
}

# Look up the section-level narrative attached to the section node identified by
# `path_values` (a named character vector for layout[seq_len(depth)]). A note row
# matches when it fixes exactly those leading layout columns to those values and
# leaves every deeper layout column NA. Returns the text or NULL.
.montage_report_section_note <- function(section_notes, layout, path_values) {
  if (is.null(section_notes) || !is.data.frame(section_notes) ||
      nrow(section_notes) == 0L || !"text" %in% names(section_notes) ||
      length(path_values) == 0L) {
    return(NULL)
  }
  cols <- names(path_values)
  keep <- rep(TRUE, nrow(section_notes))
  for (col in cols) {
    if (!col %in% names(section_notes)) {
      return(NULL)
    }
    keep <- keep & !is.na(section_notes[[col]]) &
      as.character(section_notes[[col]]) == path_values[[col]]
  }
  for (col in setdiff(layout, cols)) {
    if (col %in% names(section_notes)) {
      keep <- keep & is.na(section_notes[[col]])
    }
  }
  hit <- which(keep)
  if (length(hit) == 0L) {
    return(NULL)
  }
  text <- section_notes$text[[hit[[1]]]]
  if (is.na(text) || !nzchar(trimws(as.character(text)))) {
    return(NULL)
  }
  as.character(text)
}

# Emit HTML section-level narrative when a layout section is newly entered.
# HTML reports carry no per-section headings (each panel shows a breadcrumb), so
# section change detection lives here rather than in the heading emitter.
.montage_report_emit_html_section_notes <- function(i,
                                                    manifest,
                                                    layout,
                                                    section_notes,
                                                    section_state) {
  if (is.null(section_notes) || length(layout) == 0L ||
      !is.data.frame(manifest) || nrow(manifest) == 0L) {
    return(invisible(NULL))
  }
  changed <- FALSE
  path <- character(0)
  for (field in layout) {
    if (!field %in% names(manifest)) {
      next
    }
    value <- as.character(manifest[[field]][[i]])
    if (is.na(value) || !nzchar(value)) {
      next
    }
    path[[field]] <- value
    previous <- section_state[[field]]
    if (changed || is.null(previous) || !identical(previous, value)) {
      changed <- TRUE
      note <- .montage_report_section_note(section_notes, layout, path)
      if (!is.null(note)) {
        .montage_report_emit_narrative_block(note, "nm-section-note",
                                             is_html = TRUE)
      }
    }
    section_state[[field]] <- value
  }
  invisible(NULL)
}

# Emit any free-standing inter-map narrative anchored to `map_id` at the given
# `position` ("before" or "after" the panel). Multiple blocks for the same
# anchor render in row order.
.montage_report_emit_interludes <- function(interludes, map_id, position,
                                            is_html) {
  if (is.null(interludes) || !is.data.frame(interludes) ||
      nrow(interludes) == 0L || !all(c("map_id", "text") %in% names(interludes))) {
    return(invisible(NULL))
  }
  pos <- if ("position" %in% names(interludes)) {
    as.character(interludes$position)
  } else {
    rep("before", nrow(interludes))
  }
  hit <- which(as.character(interludes$map_id) == map_id & pos == position)
  for (h in hit) {
    .montage_report_emit_narrative_block(
      interludes$text[[h]], "nm-interlude", is_html = is_html
    )
  }
  invisible(NULL)
}

.montage_report_fmt_val <- function(v) {
  if (length(v) != 1L || is.na(v)) {
    return("")
  }
  if (is.numeric(v)) {
    if (isTRUE(v == round(v))) {
      return(format(v, trim = TRUE))
    }
    return(format(round(v, 2), trim = TRUE))
  }
  trimws(as.character(v))
}

.montage_report_tail_label <- function(tail) {
  switch(
    as.character(tail),
    two_sided = "two-sided",
    positive = "positive (> 0)",
    negative = "negative (< 0)",
    as.character(tail)
  )
}

.montage_report_status_label <- function(status) {
  values <- c(
    ok = "OK",
    dropped_subjects = "Dropped subjects",
    not_reported = "-"
  )
  out <- values[as.character(status)]
  ifelse(is.na(out), as.character(status), unname(out))
}

.montage_report_status_class <- function(status) {
  values <- c(
    ok = "ok",
    dropped_subjects = "warning",
    not_reported = "muted"
  )
  out <- values[as.character(status)]
  ifelse(is.na(out), "warning", unname(out))
}

.montage_report_html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

.montage_report_missing_scalar <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))
}

.montage_report_scalar <- function(row, field) {
  if (!field %in% names(row)) {
    return(NA)
  }
  value <- row[[field]][[1]]
  if (.montage_report_missing_scalar(value)) {
    return(NA)
  }
  value
}

.montage_report_pick <- function(row, effective, raw) {
  value <- .montage_report_scalar(row, effective)
  if (length(value) == 1L && is.na(value)) {
    .montage_report_scalar(row, raw)
  } else {
    value
  }
}

.montage_report_layout_values <- function(row, layout) {
  if (length(layout) == 0L || !is.data.frame(row)) {
    return(character(0))
  }
  values <- vapply(layout, function(field) {
    if (!field %in% names(row)) {
      return(NA_character_)
    }
    value <- row[[field]][[1]]
    if (.montage_report_missing_scalar(value)) {
      return(NA_character_)
    }
    as.character(value)
  }, character(1))
  values[!is.na(values) & nzchar(values)]
}

.montage_report_emit_styles <- function(is_html) {
  if (!isTRUE(is_html)) {
    return(invisible(NULL))
  }
  read_asset <- function(name) {
    path <- system.file("report", name, package = "neuromosaic")
    if (!nzchar(path)) {
      stop("Missing report asset inst/report/", name, call. = FALSE)
    }
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  cat(
    "<style>\n", read_asset("montage-report.css"), "\n</style>\n",
    "<script>\n", read_asset("montage-report.js"), "\n</script>\n\n",
    sep = ""
  )
  invisible(NULL)
}

.montage_report_overview_html <- function(analysis_count, map_count, layout,
                                          layout_value, shared = NULL) {
  item <- function(value, label, value_first = TRUE) {
    value <- paste0("<span class=\"nm-overview-value\">",
                    .montage_report_html_escape(value), "</span>")
    label <- paste0("<span class=\"nm-overview-label\">", label, "</span>")
    paste0("<span class=\"nm-overview-card\">",
           if (value_first) paste0(value, label) else paste0(label, value),
           "</span>")
  }
  # "3 models x 3 thresholds" when every analysis offers the same variants.
  if (length(layout) == 1L && length(shared) > 1L && analysis_count > 1L) {
    return(paste0(
      "<div class=\"nm-report-overview\">",
      item(analysis_count, paste0(.montage_report_plural(layout), " \u00d7")),
      item(length(shared), "variants"),
      "</div>"
    ))
  }
  paste0(
    "<div class=\"nm-report-overview\">",
    item(analysis_count, if (analysis_count == 1L) "analysis" else "analyses"),
    item(map_count, if (map_count == 1L) "map" else "maps"),
    item(layout_value, if (length(layout) > 0L) "grouped by" else "layout",
         value_first = FALSE),
    "</div>"
  )
}

.montage_report_emit_report_header <- function(title, date, manifest, layout,
                                               is_html, groups = NULL) {
  if (!isTRUE(is_html)) {
    cat("# ", title, "\n\n", sep = "")
    return(invisible(NULL))
  }
  map_count <- if (is.data.frame(manifest)) nrow(manifest) else 0L
  analysis_count <- if (is.data.frame(manifest) &&
                        "analysis_id" %in% names(manifest)) {
    length(unique(manifest$analysis_id))
  } else {
    map_count
  }
  layout_value <- if (length(layout) > 0L) {
    paste(layout, collapse = " / ")
  } else {
    "flat"
  }
  cat(
    "<header class=\"nm-report-header\">",
    "<div class=\"nm-eyebrow\">Montage report",
    if (!is.null(date) && nzchar(date)) {
      paste0(" &middot; ", .montage_report_html_escape(date))
    } else {
      ""
    },
    "</div>",
    "<p class=\"nm-report-title\" role=\"heading\" aria-level=\"1\">",
    .montage_report_html_escape(title),
    "</p>",
    .montage_report_overview_html(analysis_count, map_count, layout,
                                  layout_value,
                                  shared = .montage_report_shared_labels(
                                    groups, manifest
                                  )),
    "</header>\n\n",
    sep = ""
  )
  invisible(NULL)
}

.montage_report_emit_report_overview <- function(manifest, layout, is_html) {
  map_count <- if (is.data.frame(manifest)) nrow(manifest) else 0L
  analysis_count <- if (is.data.frame(manifest) &&
                        "analysis_id" %in% names(manifest)) {
    length(unique(manifest$analysis_id))
  } else {
    map_count
  }
  layout_value <- if (length(layout) > 0L) {
    paste(layout, collapse = " / ")
  } else {
    "flat"
  }

  if (isTRUE(is_html)) {
    cat(.montage_report_overview_html(analysis_count, map_count, layout,
                                      layout_value), "\n\n", sep = "")
  } else {
    cat(
      "**Analyses:** ", analysis_count, "  \n",
      "**Maps:** ", map_count, "  \n",
      "**Layout:** ", layout_value, "\n\n",
      sep = ""
    )
  }
  invisible(NULL)
}

.montage_report_drop_empty_cols <- function(tbl) {
  if (!is.data.frame(tbl) || ncol(tbl) == 0L) {
    return(tbl)
  }
  keep <- vapply(tbl, function(col) {
    if (is.character(col) || is.factor(col)) {
      return(any(!is.na(col) & nzchar(trimws(as.character(col)))))
    }
    any(!is.na(col))
  }, logical(1))
  tbl[, keep, drop = FALSE]
}

.montage_report_format_peak_table <- function(tbl, value_label = "Value") {
  if (!is.data.frame(tbl) || nrow(tbl) == 0L) {
    return(tbl)
  }
  if (inherits(tbl, "montage_parcel_table")) {
    out <- data.frame(
      Parcel = tbl$parcel,
      Value = round(tbl$value, 2),
      Network = tbl$network,
      Hemi = .montage_report_hemi_short(tbl$hemisphere),
      ID = tbl$parcel_id,
      check.names = FALSE
    )
    names(out)[names(out) == "Value"] <- value_label
    return(.montage_report_drop_empty_cols(out))
  }
  required <- c(
    "cluster_id", "sign", "n_voxels", "peak_mni_x", "peak_mni_y",
    "peak_mni_z", "max_stat", "atlas_label", "hemisphere", "network"
  )
  if (!all(required %in% names(tbl))) {
    return(.montage_report_drop_empty_cols(tbl))
  }

  cells <- .montage_report_region_cells(tbl)
  region <- cells$region
  hemisphere <- cells$hemisphere
  near <- cells$near
  off_atlas <- cells$off_atlas
  derived <- cells$derived
  has_networks <- any(!is.na(tbl$network) & nzchar(as.character(tbl$network)))
  peak_digits <- .montage_report_tie_digits(tbl$max_stat)
  peak_label <- if (identical(value_label, "Value")) {
    "Peak Stat"
  } else {
    paste("Peak", value_label)
  }

  out <- data.frame(
    Cluster = tbl$cluster_id,
    Sign = tbl$sign,
    `N Voxels` = tbl$n_voxels,
    `X (MNI)` = round(tbl$peak_mni_x, 1),
    `Y (MNI)` = round(tbl$peak_mni_y, 1),
    `Z (MNI)` = round(tbl$peak_mni_z, 1),
    `Peak Stat` = round(tbl$max_stat, peak_digits),
    Region = region,
    Hemisphere = hemisphere,
    # The region label already carries the network ("Default · Temp 3").
    Network = if (has_networks) NA_character_ else tbl$network,
    check.names = FALSE
  )
  names(out)[names(out) == "Peak Stat"] <- peak_label
  # Positive clusters first, then negative, each in cluster-number order, so
  # the IDs read in sequence (the selection itself is by |peak|).
  sign_rank <- match(as.character(tbl$sign), c("positive", "negative"))
  sign_rank[is.na(sign_rank)] <- 3L
  id_num <- suppressWarnings(as.numeric(gsub("\\D+", "", tbl$cluster_id)))
  id_num[is.na(id_num)] <- Inf
  ord <- order(sign_rank, id_num, -abs(tbl$max_stat))
  out <- out[ord, , drop = FALSE]
  off_atlas <- off_atlas[ord]
  derived <- derived[ord]
  near <- near[ord]
  rownames(out) <- NULL
  out <- .montage_report_drop_empty_cols(out)
  # Hand the HTML renderer the cells that need a marker style, and each row's
  # peak coordinate (rows open the interactive slices there).
  attr(out, "nm_cell_class") <- list(
    Region = ifelse(off_atlas, "nm-off-atlas", ifelse(near, "nm-near-atlas", "")),
    Hemisphere = ifelse(derived, "nm-derived", "")
  )
  if (any(peak_digits[ord] > 2L)) {
    attr(out, "nm_cell_digits") <- stats::setNames(list(peak_digits[ord]), peak_label)
  }
  attr(out, "nm_row_peak") <- cbind(tbl$peak_mni_x, tbl$peak_mni_y,
                                    tbl$peak_mni_z)[ord, , drop = FALSE]
  out
}

# Region and hemisphere cells as every view shows them (table, CSV, glance):
# "outside atlas" when nothing labels the peak, with the hemisphere then
# derived from the peak's x coordinate (flagged `derived`).
# Decimal places per peak so that shown peaks never look tied when their
# unrounded values differ: 2, raised (up to 4) for rows equal at 2 dp.
.montage_report_tie_digits <- function(values, base = 2L, max_digits = 4L) {
  values <- as.numeric(values)
  digits <- rep(base, length(values))
  for (d in seq.int(base, max_digits - 1L)) {
    r <- round(values, d)
    tied <- is.finite(r) & (duplicated(r) | duplicated(r, fromLast = TRUE)) & digits == d
    distinct <- tied & !(duplicated(values) | duplicated(values, fromLast = TRUE))
    if (!any(distinct)) break
    digits[distinct] <- d + 1L
  }
  digits
}

.montage_report_region_cells <- function(tbl) {
  n <- nrow(tbl)
  hemisphere <- if ("hemisphere" %in% names(tbl)) as.character(tbl$hemisphere) else
    rep(NA_character_, n)
  x <- if ("peak_mni_x" %in% names(tbl)) suppressWarnings(as.numeric(tbl$peak_mni_x)) else
    rep(NA_real_, n)
  region <- .montage_report_region_text(tbl)
  near <- attr(region, "near") %||% rep(FALSE, n)
  region <- as.character(region)
  off_atlas <- is.na(region) | !nzchar(trimws(region))
  derived <- off_atlas & (is.na(hemisphere) | !nzchar(trimws(hemisphere))) &
    is.finite(x)
  hemisphere[derived] <- .montage_hemisphere_from_x(x[derived])
  region[off_atlas] <- "outside atlas"
  list(region = region, hemisphere = hemisphere, near = near,
       off_atlas = off_atlas, derived = derived)
}

# Region text for each peak row: "Default · Temp 3" at the peak voxel;
# "≈ Control · PFCl 4 (2 mm)" when the label is the nearest region within the
# search radius; NA when nothing is near. attr "near" flags the approximate
# rows.
.montage_report_region_text <- function(tbl) {
  col <- function(name, na) if (name %in% names(tbl)) tbl[[name]] else rep(na, nrow(tbl))
  region <- .montage_report_region_label(col("atlas_label", NA_character_),
                                         col("network", NA_character_))
  source <- as.character(col("label_source", NA_character_))
  dist <- as.numeric(col("label_distance_mm", NA_real_))
  near <- !is.na(source) & source == "nearest" & !is.na(region) & nzchar(region)
  region[near] <- paste0("\u2248 ", region[near], " (",
                         as.character(round(dist[near], 1)), " mm)")
  attr(region, "near") <- near
  region
}

# Atlas region with its network: "Temp_3" + "Default" -> "Default · Temp 3";
# "Vis_11" + "Vis" -> "Visual · 11". Labels without a network are kept as-is.
.montage_report_region_label <- function(label, network) {
  label <- as.character(label)
  network <- as.character(network)
  if (length(network) != length(label)) network <- rep(NA_character_, length(label))
  out <- label
  has <- !is.na(label) & nzchar(label) & !is.na(network) & nzchar(network)
  if (!any(has)) return(out)
  area <- label[has]
  prefix <- paste0(network[has], "_")
  strip <- startsWith(area, prefix)
  area[strip] <- substring(area[strip], nchar(prefix[strip]) + 1L)
  area <- gsub("_", " ", area, fixed = TRUE)
  out[has] <- paste0(.montage_report_network_name(network[has]), " \u00b7 ", area)
  out
}

.montage_report_network_name <- function(network) {
  names7 <- c(
    Vis = "Visual", SomMot = "Somatomotor", DorsAttn = "Dorsal attention",
    SalVentAttn = "Salience / ventral attention", VentAttn = "Ventral attention",
    Limbic = "Limbic", Cont = "Control", Default = "Default",
    TempPar = "Temporoparietal"
  )
  network <- as.character(network)
  out <- unname(names7[network])
  ifelse(is.na(out), network, out)
}

.montage_report_emit_panel_image <- function(path, alt, is_html, empty = FALSE,
                                             figure_assets = NULL,
                                             caption = NULL,
                                             dark = NULL, narrow = NULL,
                                             narrow_dark = NULL,
                                             full_caption = NULL,
                                             download_name = NULL,
                                             title = NULL, kicker = NULL,
                                             kind = NULL) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }
  if (!file.exists(path)) {
    cat("*Image not found:* `", path, "`\n\n", sep = "")
    return(invisible(NULL))
  }
  if (!isTRUE(is_html)) {
    cat("![", alt, "](", path, ")\n\n", sep = "")
    if (!is.null(full_caption) && nzchar(full_caption)) cat("*", full_caption, "*\n\n", sep = "")
    return(invisible(NULL))
  }
  usable <- function(x) {
    !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x) && file.exists(x)
  }
  esc <- .montage_report_html_escape
  src <- .montage_report_image_src(path, figure_assets)
  dims_attr <- function(p) {
    d <- .montage_png_dimensions(p)
    if (is.null(d)) "" else paste0(" width=\"", d[[1]], "\" height=\"", d[[2]], "\"")
  }
  source_tag <- function(p, media, theme = NULL) {
    if (!usable(p)) return("")
    paste0("<source media=\"", media, "\" data-nm-media=\"", media, "\"",
           if (!is.null(theme)) paste0(" data-nm-theme=\"", theme, "\""),
           " srcset=\"", .montage_report_image_src(p, figure_assets), "\"",
           dims_attr(p), " />")
  }
  narrow_q <- "(max-width: 600px)"
  # Screen only: paper always gets the light figure of record.
  dark_q <- "screen and (prefers-color-scheme: dark)"
  sources <- paste0(
    source_tag(narrow_dark, paste0(dark_q, " and ", narrow_q), "dark"),
    source_tag(narrow, narrow_q, "light"),
    source_tag(dark, dark_q, "dark")
  )
  has_dark <- usable(dark)
  # Companion-file figures load when they near the viewport, then in idle
  # time, and all of them before printing (montage-report.js): `data-nm-src`
  # / `data-nm-srcset` hold the URLs and an explicit aspect ratio reserves the
  # box so nothing below moves when a figure arrives. Inline (data URI)
  # figures are already in the page and load directly.
  defer <- !startsWith(src, "data:")
  # The reserved box must follow whichever source the browser picks: the
  # narrow (phone) variant has its own shape, so each ratio is a custom
  # property that the stylesheet applies under the same media query as the
  # <source> (an inline aspect-ratio would squash the narrow figure).
  dims <- .montage_png_dimensions(path)
  ndims <- if (usable(narrow)) .montage_png_dimensions(narrow)
  ratio <- if (!is.null(dims)) {
    paste0(" style=\"--nm-ar: ", dims[[1]], " / ", dims[[2]],
           if (!is.null(ndims)) paste0("; --nm-ar-narrow: ", ndims[[1]], " / ", ndims[[2]]),
           "\"")
  } else {
    ""
  }
  if (defer) sources <- gsub(" srcset=\"", " data-nm-srcset=\"", sources, fixed = TRUE)
  img <- if (defer) {
    paste0("<img data-nm-src=\"", src, "\" alt=\"", esc(alt), "\"", dims_attr(path), ratio,
           " class=\"nm-defer\" decoding=\"async\" />")
  } else {
    paste0("<img src=\"", src, "\" alt=\"", esc(alt), "\"", dims_attr(path),
           " loading=\"lazy\" decoding=\"async\" />")
  }
  picture <- if (nzchar(sources)) paste0("<picture>", sources, img, "</picture>") else img
  if (defer) {
    # Without the page script the deferred figure is hidden and this one shown.
    picture <- paste0(picture, "<noscript><img src=\"", src, "\" alt=\"", esc(alt), "\"",
                      dims_attr(path), " /></noscript>")
  }
  show_caption <- !isTRUE(empty)
  short <- if (show_caption && !is.null(caption) && length(caption) == 1L && nzchar(caption)) {
    paste0("<span class=\"nm-fig-cap\">", esc(caption), "</span>")
  } else {
    ""
  }
  actions <- if (show_caption) {
    paste0(
      "<span class=\"nm-fig-actions\">",
      # An inline (data URI) figure is not repeated in the link: the page
      # script points the link at the image instead.
      # "Download PNG" composes the title and full caption onto the light
      # figure in the browser; "image only" is the figure file itself.
      if (startsWith(src, "data:")) {
        paste0("<a class=\"nm-fig-action nm-fig-download\" data-nm-download data-nm-titled download=\"",
               esc(download_name %||% basename(path)), "\">Download PNG</a>",
               "<a class=\"nm-fig-action nm-fig-plain nm-fig-download\" data-nm-download download=\"",
               esc(download_name %||% basename(path)), "\">image only</a>")
      } else {
        paste0("<a class=\"nm-fig-action\" data-nm-titled href=\"", src, "\" download=\"",
               esc(download_name %||% basename(path)), "\">Download PNG</a>",
               "<a class=\"nm-fig-action nm-fig-plain\" href=\"", src, "\" download=\"",
               esc(sub("\\.png$", "_untitled.png", download_name %||% basename(path))),
               "\">image only</a>")
      },
      if (!is.null(full_caption) && nzchar(full_caption)) {
        "<button type=\"button\" class=\"nm-fig-action nm-fig-copy\" data-nm-copy-caption>Copy caption</button>"
      },
      "</span>"
    )
  } else {
    ""
  }
  full <- if (show_caption && !is.null(full_caption) && nzchar(full_caption)) {
    paste0("<span class=\"nm-fig-full\" data-nm-caption-text>", esc(full_caption), "</span>")
  } else {
    ""
  }
  cat(
    "<figure class=\"nm-figure", if (!is.null(kind)) paste0(" nm-figure-", kind),
    if (isTRUE(empty)) " nm-figure-empty",
    if (has_dark) " nm-figure-dark-ready", "\"",
    if (!is.null(title) && nzchar(title)) paste0(" data-nm-title=\"", esc(title), "\""),
    if (!is.null(kicker) && nzchar(kicker)) paste0(" data-nm-kicker=\"", esc(kicker), "\""),
    ">", picture,
    if (nzchar(paste0(short, actions, full))) {
      paste0("<figcaption>", short, actions, full, "</figcaption>")
    },
    "</figure>\n\n",
    sep = ""
  )
  invisible(NULL)
}

# A panel's volume renderer metadata (cap, signed, slice labels), or NULL.
# Exact lookup: `panel$volume` partially matches `volume_image` (a path) in
# precomputed panels that carry no renderer metadata.
.montage_report_volume_meta <- function(panel) {
  meta <- if (is.list(panel)) panel[["volume"]]
  if (is.list(meta)) meta else NULL
}

# Both static figures of one map, each with a short caption, a download link,
# a full copyable caption, and dark / narrow variants when the renderer made
# them (panel$*_image_dark, *_image_narrow, *_image_narrow_dark).
.montage_report_emit_map_figures <- function(row, panel, empty = FALSE, is_html,
                                             figure_assets = NULL,
                                             layout = character()) {
  if (!is.list(panel)) panel <- list()
  label <- as.character(row$label[[1L]])
  analysis_label <- if ("analysis_label" %in% names(row)) as.character(row$analysis_label[[1L]]) else ""
  # The line above the title on a downloaded figure: the map's section
  # (e.g. "Working memory (N-back)"), else its analysis when that differs.
  section <- if (length(layout)) .montage_report_layout_values(row, layout) else character()
  section <- section[!is.na(section) & section != label]
  kicker <- if (length(section)) paste(section, collapse = " / ") else
    if (nzchar(analysis_label) && !identical(analysis_label, label)) analysis_label
  stem <- .montage_report_file_stem(label)
  slices <- .montage_report_slice_text(.montage_report_volume_meta(panel)$slice_labels)
  vol_alt <- paste0(label, ": slice montage",
                    if (nzchar(slices)) paste0(" at ", slices), ", ",
                    "positive effects in warm colours, negative in cool")
  shared <- .montage_report_slice_share(panel$slice_scope, label)
  .montage_report_emit_panel_image(
    panel$volume_image %||% panel$volume_image_path, vol_alt,
    is_html = is_html, empty = empty, figure_assets = figure_assets,
    caption = paste0(if (nzchar(slices)) "Axial slices" else "Volume slices",
                     " \u00b7 left hemisphere on the left",
                     if (!is.null(shared)) paste0(" \u00b7 ", shared$short)),
    dark = panel$volume_image_dark, narrow = panel$volume_image_narrow,
    narrow_dark = panel$volume_image_narrow_dark,
    full_caption = .montage_report_figure_caption(row, panel, "volume"),
    download_name = paste0(stem, "_volume.png"),
    title = label, kicker = kicker, kind = "volume"
  )
  .montage_report_emit_panel_image(
    panel$surface_image %||% panel$surface_image_path,
    paste0(label, ": cortical surface, lateral and medial views of both hemispheres"),
    is_html = is_html, empty = empty, figure_assets = figure_assets,
    caption = paste("Cortical surface \u00b7 values averaged over cortical depth,",
                    "so peaks are lower than in the volume \u00b7",
                    "subcortical and cerebellar effects not shown"),
    dark = panel$surface_image_dark, narrow = panel$surface_image_narrow,
    narrow_dark = panel$surface_image_narrow_dark,
    full_caption = .montage_report_figure_caption(row, panel, "surface"),
    download_name = paste0(stem, "_surface.png"),
    title = label, kicker = kicker, kind = "surface"
  )
  invisible(NULL)
}

# Whether this map's slices are shared with others (panel$slice_scope from
# the renderer): list(short = caption fragment, full = caption sentence,
# n = maps sharing) or NULL when the map chose its own.
.montage_report_slice_share <- function(scope, label = NULL) {
  if (!is.list(scope)) return(NULL)
  ids <- unique(as.character(scope$map_ids %||% character()))
  kind <- as.character(scope$scope %||% "map")
  if (length(ids) < 2L || !kind %in% c("section", "analysis", "report")) return(NULL)
  n <- length(ids)
  where <- as.character(scope$section %||% NA)
  where <- if (kind == "report") "the report" else if (!is.na(where) && nzchar(where)) where else
    if (kind == "analysis") "this analysis" else "this section"
  list(
    short = if (kind == "report") paste0("same slices in all ", n, " maps") else
      paste0("same slices for all ", n, " maps in ", where),
    full = paste0("The same slices are shown for all ", n, " maps in ", where, "."),
    n = n, where = where, kind = kind
  )
}

# "z = −30, −22, … , +62 mm" from the figure's slice labels ("z = −30").
.montage_report_slice_text <- function(labels) {
  labels <- as.character(labels %||% character(0))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) return("")
  axis <- sub("^\\s*([xyz])\\s*=.*$", "\\1", labels[[1L]])
  if (!axis %in% c("x", "y", "z")) return("")
  values <- trimws(sub("^\\s*[xyz]\\s*=\\s*", "", labels))
  values <- sub("\\s*mm$", "", values)
  paste0(axis, " = ", paste(values, collapse = ", "), " mm")
}

# A self-contained caption for a figure exported to a slide or paper: what was
# tested, threshold, extent, N, colour scale and (volume) slice positions.
.montage_report_figure_caption <- function(row, panel = list(), kind = c("volume", "surface")) {
  kind <- match.arg(kind)
  if (!is.list(panel)) panel <- list()
  fmt <- .montage_report_fmt_val
  label <- as.character(row$label[[1L]])
  group <- if ("analysis_label" %in% names(row)) as.character(row$analysis_label[[1L]]) else NA
  title <- if (!is.na(group) && nzchar(group) && !identical(group, label)) {
    paste0(label, " (", group, ").")
  } else {
    paste0(label, ".")
  }
  parts <- .montage_report_metadata_parts(row)
  stat <- if ("stat" %in% names(parts)) parts[["stat"]]
  threshold <- .montage_report_pick(row, "effective_threshold", "threshold")
  tail <- .montage_report_pick(row, "effective_tail", "tail")
  sym <- tolower(.montage_report_value_label(row))
  sym <- if (identical(sym, "value")) "value" else sym
  thr <- if (!(length(threshold) == 1L && is.na(threshold))) {
    paste0(
      switch(as.character(tail),
             positive = paste0(sym, " \u2265 ", fmt(threshold)),
             negative = paste0(sym, " \u2264 \u2212", fmt(threshold)),
             paste0("|", sym, "| \u2265 ", fmt(threshold), " (two-sided)"))
    )
  }
  k <- .montage_report_pick(row, "effective_min_cluster_size", "min_cluster_size")
  conn <- .montage_report_pick(row, "effective_connectivity", "connectivity")
  extent <- if (!(length(k) == 1L && is.na(k)) && !.montage_report_is_parcel_row(row)) {
    paste0("clusters of at least ", fmt(k), " voxels",
           if (!(length(conn) == 1L && is.na(conn))) paste0(" (", sub("-connect$", "-connectivity", conn), ")"))
  }
  n <- .montage_report_scalar(row, "n")
  meta <- panel[[kind]]
  if (!is.list(meta)) meta <- panel[["volume"]]
  if (!is.list(meta)) meta <- list()
  cap <- suppressWarnings(as.numeric(meta$cap %||% NA))
  signed <- isTRUE(meta$signed %||% TRUE)
  scale <- if (length(cap) == 1L && is.finite(cap) && cap > 0) {
    paste0("Colour scale saturates at ", if (signed) "\u00b1" else "", fmt(round(cap, 1)),
           "; warm = positive, cool = negative.")
  }
  first <- paste0(
    paste(c(if (!is.null(stat)) paste0(stat, " map"),
            if (!(length(n) == 1L && is.na(n))) paste0("N = ", fmt(n))),
          collapse = ", "),
    if (!is.null(thr)) paste0(", thresholded at ", thr),
    if (!is.null(extent)) paste0(", ", extent),
    "."
  )
  where <- if (kind == "volume") {
    slices <- .montage_report_slice_text(meta$slice_labels)
    shared <- .montage_report_slice_share(panel$slice_scope)
    paste0(if (nzchar(slices)) paste0("Axial slices at ", slices, " (MNI)") else "Volume slices",
           "; left hemisphere on the left.",
           if (!is.null(shared)) paste0(" ", shared$full))
  } else {
    paste("Cortical surface, lateral and medial views; values are averaged",
          "over cortical depth, so peaks are lower than in the volume;",
          "subcortical and cerebellar effects are not shown.")
  }
  paste(c(title, if (first != ".") first, scale, where), collapse = " ")
}

# A relative companion-file URL when `figure_assets` names a directory,
# otherwise an inline data URI.
.montage_report_image_src <- function(path, figure_assets = NULL) {
  if (is.null(figure_assets)) {
    return(knitr::image_uri(path))
  }
  dir.create(figure_assets$dir, recursive = TRUE, showWarnings = FALSE)
  name <- paste0(
    substr(digest::digest(file = path, algo = "sha256"), 1L, 16L), ".",
    tolower(tools::file_ext(path))
  )
  target <- file.path(figure_assets$dir, name)
  if (!file.exists(target) && !file.copy(path, target)) {
    stop("Could not copy report figure to ", target, call. = FALSE)
  }
  paste0(figure_assets$href, "/", utils::URLencode(name))
}

# Intrinsic PNG size from the IHDR chunk, so the browser reserves the figure's
# box before the image loads. NULL for anything that is not a PNG.
.montage_png_dimensions <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  head <- readBin(con, "raw", 24L)
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(head) < 24L || !identical(head[1:8], signature)) {
    return(NULL)
  }
  be32 <- function(bytes) sum(as.numeric(bytes) * 256^(3:0))
  c(be32(head[17:20]), be32(head[21:24]))
}

.montage_report_emit_table <- function(tbl, is_html, row = NULL) {
  if (is.null(tbl)) {
    return(invisible(NULL))
  }
  if (is.data.frame(tbl) && nrow(tbl) == 0L) {
    unit <- if (.montage_report_is_parcel_row(row) ||
                inherits(tbl, "montage_parcel_table")) "parcels" else "clusters"
    msg <- paste0("No ", unit, " survive this threshold.")
    if (isTRUE(is_html)) {
      cat("<p class=\"nm-empty\">", msg, "</p>\n\n", sep = "")
    } else {
      cat("*", msg, "*\n\n", sep = "")
    }
    return(invisible(NULL))
  }
  cat("\n")
  if (is.data.frame(tbl)) {
    formatted <- .montage_report_format_peak_table(
      tbl, value_label = .montage_report_value_label(row)
    )
    n_total <- attr(tbl, "n_total")
    parcel <- inherits(tbl, "montage_parcel_table")
    if (isTRUE(is_html)) {
      caption <- if (parcel) {
        n_all <- n_total %||% nrow(tbl)
        paste0(n_all, if (n_all == 1L) " surviving parcel" else " surviving parcels",
               ", strongest first")
      } else {
        .montage_report_cluster_caption(tbl)
      }
      # The P/N cluster ID and the sign-coloured peak already carry the sign.
      if (!parcel && "Sign" %in% names(formatted) && "Cluster" %in% names(formatted)) {
        keep_attrs <- attributes(formatted)[c("nm_cell_class", "nm_row_peak", "nm_cell_digits")]
        keep_attrs <- keep_attrs[!is.na(names(keep_attrs))]
        formatted <- formatted[, names(formatted) != "Sign", drop = FALSE]
        attributes(formatted)[names(keep_attrs)] <- keep_attrs
      }
      cat(.montage_report_html_table(
        formatted,
        class = paste("nm-table nm-peak-table",
                      if (parcel) "nm-parcel-table" else "nm-cluster-table"),
        caption = caption
      ), "\n", sep = "")
      if (parcel && !is.null(n_total) && n_total > nrow(tbl)) {
        cat("<p class=\"nm-table-note\">Strongest ", nrow(tbl), " of ",
            n_total, " surviving parcels.</p>\n", sep = "")
      }
      if (!parcel) cat(.montage_report_table_foot(tbl, row), "\n", sep = "")
    } else {
      note <- if (!parcel) .montage_report_cluster_caption(tbl, html = FALSE)
      if (!is.null(note)) cat("*", note, "*\n\n", sep = "")
      cat(knitr::kable(formatted, format = "pipe", row.names = FALSE),
          sep = "\n")
    }
  } else {
    print(tbl)
  }
  cat("\n\n")
  invisible(NULL)
}

# "Showing 12 of 22 clusters · 8 of 12 positive, 4 of 10 negative" from the
# totals the peak-table builder attaches; NULL for tables without them.
.montage_report_cluster_caption <- function(tbl, html = TRUE) {
  n_total <- attr(tbl, "n_total")
  if (is.null(n_total) || !is.data.frame(tbl)) return(NULL)
  shown <- nrow(tbl)
  by_sign <- attr(tbl, "n_by_sign")
  unit <- if (n_total == 1L) "cluster" else "clusters"
  sign_ok <- length(by_sign) == 2L && sum(by_sign) == n_total &&
    "sign" %in% names(tbl)
  shown_sign <- if (sign_ok) {
    c(positive = sum(as.character(tbl$sign) == "positive"),
      negative = sum(as.character(tbl$sign) == "negative"))
  }
  int <- .montage_report_int
  if (shown < n_total) {
    head <- paste0("Showing ", shown, " of ", int(n_total), " ", unit)
    split <- if (sign_ok) {
      paste0(shown_sign[["positive"]], " of ", int(by_sign[["positive"]]), " positive, ",
             shown_sign[["negative"]], " of ", int(by_sign[["negative"]]), " negative")
    }
    tail <- "strongest by |peak|, each sign kept; numbered by |peak| within sign"
  } else {
    head <- if (n_total == 1L) "1 cluster" else paste0("All ", int(n_total), " clusters")
    split <- if (sign_ok) {
      paste0(by_sign[["positive"]], " positive, ", by_sign[["negative"]], " negative")
    }
    tail <- NULL
  }
  paste0(paste(c(head, split, tail), collapse = " \u00b7 "), ".")
}

# Under a cluster table: what "≈" means (only when used) and a CSV of the
# complete, untruncated cluster list.
.montage_report_table_foot <- function(tbl, row = NULL) {
  all <- attr(tbl, "all_clusters")
  if (!is.data.frame(all) || nrow(all) == 0L) all <- tbl
  near <- any(attr(.montage_report_region_text(all), "near"))
  label <- if (is.data.frame(row) && "label" %in% names(row)) {
    as.character(row$label[[1L]])
  } else if ("map_id" %in% names(all)) {
    as.character(all$map_id[[1L]])
  } else {
    "map"
  }
  csv <- .montage_report_cluster_csv(all, row)
  n <- nrow(all)
  link <- if (!is.null(csv)) {
    paste0("<a class=\"nm-csv\" download=\"",
           .montage_report_html_escape(paste0(.montage_report_file_stem(label), "_clusters.csv")),
           "\" href=\"data:text/csv;charset=utf-8,", utils::URLencode(csv, reserved = TRUE),
           "\">Download ", if (n == 1L) "the cluster" else paste0("all ", .montage_report_int(n), " clusters"),
           " (CSV)</a>")
  }
  cere <- attr(tbl, "cerebellum_source") %||% attr(all, "cerebellum_source")
  keys <- c(
    if (near) "\u2248 nearest atlas region to a peak outside it, with the distance.",
    if (!is.null(cere)) paste0("Cerebellum labels: ", cere, ".")
  )
  note <- if (length(keys)) {
    paste0("<span class=\"nm-table-key\">",
           paste(.montage_report_html_escape(keys), collapse = " "), "</span>")
  }
  if (is.null(link) && is.null(note)) return("")
  paste0("<p class=\"nm-table-foot\">", note, link, "</p>")
}

# The full cluster list as CSV text (RFC 4180 quoting); NULL when the table
# lacks the peak columns.
.montage_report_cluster_csv <- function(all, row = NULL) {
  need <- c("cluster_id", "sign", "n_voxels", "peak_mni_x", "peak_mni_y",
            "peak_mni_z", "max_stat")
  if (!all(need %in% names(all))) return(NULL)
  # Same region and hemisphere text as the table: "outside atlas" with the
  # hemisphere from the x coordinate; the plain label (no "≈", the source and
  # distance have their own columns).
  cells <- .montage_report_region_cells(all)
  region <- .montage_report_region_label(
    if ("atlas_label" %in% names(all)) all$atlas_label else NA_character_,
    if ("network" %in% names(all)) all$network else NA_character_
  )
  region[cells$off_atlas] <- "outside atlas"
  value <- .montage_report_value_label(row)
  ord <- order(match(as.character(all$sign), c("positive", "negative")),
               suppressWarnings(as.numeric(gsub("\\D+", "", all$cluster_id))))
  pick <- function(name) if (name %in% names(all)) all[[name]] else rep(NA, nrow(all))
  label_source <- as.character(pick("label_source"))
  label_source[is.na(label_source) | !nzchar(label_source)] <- "none"
  df <- data.frame(
    map = if (is.data.frame(row) && "label" %in% names(row)) as.character(row$label[[1L]]) else
      as.character(pick("map_id")),
    cluster = all$cluster_id, sign = all$sign, n_voxels = all$n_voxels,
    x_mm = all$peak_mni_x, y_mm = all$peak_mni_y, z_mm = all$peak_mni_z,
    peak = round(all$max_stat, 4), region = region,
    hemisphere = cells$hemisphere,
    label_source = label_source,
    label_distance_mm = pick("label_distance_mm"),
    stringsAsFactors = FALSE
  )[ord, , drop = FALSE]
  names(df)[names(df) == "peak"] <- if (identical(value, "Value")) "peak" else paste0("peak_", value)
  quote <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    ifelse(grepl("[\",\n]", x), paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\""), x)
  }
  lines <- c(paste(names(df), collapse = ","),
             apply(vapply(df, quote, character(nrow(df))), 1L, paste, collapse = ","))
  paste0(paste(lines, collapse = "\r\n"), "\r\n")
}

.montage_report_file_stem <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub(">", " gt ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) "map" else x
}

.montage_report_int <- function(x) {
  formatC(as.numeric(x), format = "d", big.mark = ",")
}

# Minimal accessible HTML table: numeric columns are right-aligned via a
# class rather than by column position. Every cell carries its column name
# (`data-label`) and a column class so narrow screens can restack rows;
# `attr(df, "nm_cell_class")` may add per-cell marker classes.
.montage_report_html_table <- function(df, class = "nm-table", caption = NULL) {
  num <- vapply(df, is.numeric, logical(1))
  esc <- .montage_report_html_escape
  cell_class <- attr(df, "nm_cell_class") %||% list()
  col_key <- paste0("nm-col-", gsub("[^a-z0-9]+", "-", tolower(names(df))))
  col_key <- sub("-$", "", col_key)
  # One decimal count per column so values align on the point.
  digits <- vapply(df, function(col) {
    if (!is.numeric(col)) return(0L)
    col <- col[is.finite(col)]
    if (!length(col) || all(col == round(col))) return(0L)
    if (all(col * 10 == round(col * 10))) 1L else 2L
  }, integer(1))
  is_peak <- grepl("^Peak ", names(df)) & num
  is_sign <- names(df) == "Sign"
  cell_digits <- attr(df, "nm_cell_digits") %||% list()
  cell <- function(value, k, r) {
    is_num <- num[[k]]
    per_cell <- cell_digits[[names(df)[[k]]]]
    dk <- if (length(per_cell) >= r) max(digits[[k]], per_cell[[r]]) else digits[[k]]
    text <- if (is.na(value)) "" else if (is_num) {
      formatC(value, format = "f", digits = dk, big.mark = ",")
    } else {
      as.character(value)
    }
    if (is_num) text <- sub("^-", "\u2212", text)
    classes <- c(if (is_num) "num", col_key[[k]])
    extra <- cell_class[[names(df)[[k]]]]
    if (length(extra) >= r && nzchar(extra[[r]])) classes <- c(classes, extra[[r]])
    inner <- esc(text)
    if (is_peak[[k]] && !is.na(value)) {
      classes <- c(classes, if (value < 0) "nm-neg" else "nm-pos")
    }
    if (is_sign[[k]] && !is.na(value)) {
      sign <- tolower(as.character(value))
      glyph <- if (sign == "positive") "+" else if (sign == "negative") "\u2212" else NULL
      if (!is.null(glyph)) {
        inner <- paste0("<span class=\"nm-sign nm-sign-",
                        if (sign == "positive") "pos" else "neg",
                        "\" aria-hidden=\"true\">", glyph,
                        "</span><span class=\"nm-sign-text\">", esc(text), "</span>")
      }
    }
    if (identical(names(df)[[k]], "Cluster") && !is.na(value)) {
      sign_word <- if (grepl("^P[0-9]", value)) " (positive)" else if (grepl("^N[0-9]", value)) " (negative)"
      if (!is.null(sign_word)) {
        inner <- paste0(inner, "<span class=\"nm-sr-only\">", sign_word, "</span>")
      }
    }
    if (identical(extra[r], "nm-near-atlas")) {
      inner <- paste0("<span title=\"Nearest atlas region: the peak voxel itself is unlabelled\">",
                      inner, "</span>")
    }
    if (identical(extra[r], "nm-derived")) {
      inner <- paste0("<span title=\"Outside the atlas: hemisphere from the sign of x\">",
                      inner, "</span>")
    }
    paste0("<td class=\"", paste(classes, collapse = " "), "\" data-label=\"",
           esc(names(df)[[k]]), "\">", inner, "</td>")
  }
  head <- paste0(
    "<thead><tr>",
    paste0("<th scope=\"col\"", ifelse(num, " class=\"num\"", ""), ">",
           esc(names(df)), "</th>", collapse = ""),
    "</tr></thead>"
  )
  peaks <- attr(df, "nm_row_peak")
  row_open <- function(r) {
    if (is.null(peaks) || nrow(peaks) < r || any(!is.finite(peaks[r, ]))) return("<tr>")
    paste0("<tr data-nm-peak=\"", paste(format(peaks[r, ], trim = TRUE), collapse = ","), "\">")
  }
  body <- vapply(seq_len(nrow(df)), function(r) {
    paste0(row_open(r), paste0(vapply(seq_along(df), function(k) {
      cell(df[[k]][[r]], k, r)
    }, ""), collapse = ""), "</tr>")
  }, "")
  paste0("<div class=\"nm-table-wrap\"><table class=\"", class, "\">",
         if (!is.null(caption)) paste0("<caption>", esc(caption), "</caption>"),
         head, "<tbody>", paste(body, collapse = ""), "</tbody></table></div>")
}

.montage_report_metadata_parts <- function(row) {
  stat <- .montage_report_scalar(row, "stat_kind")
  quantity <- .montage_report_scalar(row, "quantity")
  distribution <- .montage_report_scalar(row, "distribution")
  profile_label <- .montage_report_scalar(row, "effective_profile_label")
  display_mode <- .montage_report_scalar(row, "effective_display_mode")
  lower <- .montage_report_scalar(row, "effective_lower")
  upper <- .montage_report_scalar(row, "effective_upper")
  units <- .montage_report_pick(row, "effective_units", "units")
  threshold <- .montage_report_pick(row, "effective_threshold", "threshold")
  tail <- .montage_report_pick(row, "effective_tail", "tail")
  connectivity <- .montage_report_pick(
    row,
    "effective_connectivity",
    "connectivity"
  )
  min_cluster_size <- .montage_report_pick(
    row,
    "effective_min_cluster_size",
    "min_cluster_size"
  )
  n <- .montage_report_scalar(row, "n")

  parts <- character(0)
  if (!(length(quantity) == 1L && is.na(quantity))) {
    if (identical(as.character(quantity), "test_statistic") &&
        !(length(distribution) == 1L && is.na(distribution))) {
      parts <- c(parts, stat = paste0(toupper(distribution), "-statistic"))
    } else if (!(length(profile_label) == 1L && is.na(profile_label))) {
      parts <- c(parts, stat = as.character(profile_label))
    } else {
      parts <- c(parts, stat = .profile_title(as.character(quantity)))
    }
  } else if (!(length(stat) == 1L && is.na(stat))) {
    stat <- as.character(stat)
    parts <- c(parts, stat = paste0(
      toupper(substr(stat, 1L, 1L)),
      substring(stat, 2L),
      "-statistic"
    ))
  }
  if (!(length(threshold) == 1L && is.na(threshold))) {
    value <- paste0("threshold ", .montage_report_fmt_val(threshold))
    if (!(length(tail) == 1L && is.na(tail))) {
      value <- paste0(
        value,
        " (",
        .montage_report_tail_label(tail),
        ")"
      )
    }
    parts <- c(parts, threshold = value)
  } else if (identical(as.character(display_mode), "continuous") &&
             !(length(lower) == 1L && is.na(lower)) &&
             !(length(upper) == 1L && is.na(upper))) {
    parts <- c(parts, threshold = paste0(
      "continuous range ", .montage_report_fmt_val(lower), " to ",
      .montage_report_fmt_val(upper)
    ))
  }
  if (!(length(threshold) == 1L && is.na(threshold)) &&
      !(length(connectivity) == 1L && is.na(connectivity)) &&
      !.montage_report_is_parcel_row(row)) {
    value <- as.character(connectivity)
    if (!(length(min_cluster_size) == 1L && is.na(min_cluster_size))) {
      value <- paste0(
        value,
        ", min ",
        .montage_report_fmt_val(min_cluster_size),
        " vox"
      )
    }
    parts <- c(parts, extent = value)
  }
  if (!(length(n) == 1L && is.na(n))) {
    parts <- c(parts, n = paste0("N = ", .montage_report_fmt_val(n)))
  }
  if (!(length(units) == 1L && is.na(units)) &&
      nzchar(as.character(units))) {
    parts <- c(parts, units = paste0("units: ", as.character(units)))
  }
  parts
}

# The map's spec line. `omit` holds parts already stated once for the whole
# report (header facts line), so each map shows only what differs.
.montage_report_emit_metadata <- function(row, is_html, omit = character()) {
  parts <- .montage_report_metadata_parts(row)
  if (length(omit)) parts <- parts[!parts %in% omit]
  if (length(parts) == 0L) {
    return(invisible(NULL))
  }

  if (isTRUE(is_html)) {
    cat(
      "<p class=\"nm-panel-meta\">",
      paste0("<span>", .montage_report_html_escape(parts), "</span>",
             # An entity, not a literal middle dot: cat() in a C locale
             # would print "<U+00B7>".
             collapse = "<span class=\"nm-sep\" aria-hidden=\"true\"> &middot; </span>"),
      "</p>\n\n",
      sep = ""
    )
  } else {
    cat("*", paste(parts, collapse = " | "), "*\n\n", sep = "")
  }
  invisible(NULL)
}

.montage_report_qc_has_values <- function(qc_tbl) {
  is.data.frame(qc_tbl) && nrow(qc_tbl) > 0L &&
    "qc_status" %in% names(qc_tbl) &&
    any(qc_tbl$qc_status != "not_reported", na.rm = TRUE)
}

.montage_report_emit_qc_summary <- function(qc_tbl, is_html, stated = FALSE) {
  if (!.montage_report_qc_has_values(qc_tbl)) {
    return(invisible(NULL))
  }
  # Uniform N and all-OK status: one line instead of a table of repeats.
  line <- .montage_report_qc_line(qc_tbl)
  if (!is.null(line)) {
    if (isTRUE(stated)) return(invisible(NULL))
    if (isTRUE(is_html)) {
      cat("<p class=\"nm-qc-line\">", .montage_report_html_escape(line),
          "</p>\n\n", sep = "")
    } else {
      cat("*", line, "*\n\n", sep = "")
    }
    return(invisible(NULL))
  }
  map_label <- if ("label" %in% names(qc_tbl)) {
    qc_tbl$label
  } else {
    qc_tbl$map_id
  }
  display <- data.frame(Map = map_label, check.names = FALSE)
  if ("effective_n" %in% names(qc_tbl)) {
    display[["N"]] <- qc_tbl$effective_n
  }
  if ("source_n" %in% names(qc_tbl) && any(!is.na(qc_tbl$source_n))) {
    display[["Input N"]] <- qc_tbl$source_n
  }
  if ("dropped_n" %in% names(qc_tbl) &&
      any(!is.na(qc_tbl$dropped_n) & qc_tbl$dropped_n > 0)) {
    display[["Dropped"]] <- qc_tbl$dropped_n
  }
  display[["Status"]] <- .montage_report_status_label(qc_tbl$qc_status)

  if (isTRUE(is_html)) {
    display_html <- display
    for (field in setdiff(names(display_html), "Status")) {
      display_html[[field]] <- .montage_report_html_escape(display_html[[field]])
    }
    status_class <- .montage_report_status_class(qc_tbl$qc_status)
    display_html[["Status"]] <- paste0(
      "<span class=\"nm-status nm-status-",
      status_class,
      "\">",
      .montage_report_html_escape(display[["Status"]]),
      "</span>"
    )
    # The wrapper scrolls wide QC tables inside themselves on narrow screens
    # instead of widening the page (as the other report tables do).
    cat("\n\n<section class=\"nm-qc-section\">\n<h2 class=\"nm-qc-title\">Effective N / QC</h2>\n",
        "<div class=\"nm-table-wrap\">\n", sep = "")
    cat(
      knitr::kable(
        display_html,
        format = "html",
        escape = FALSE,
        row.names = FALSE,
        table.attr = "class=\"nm-table nm-qc-table\""
      ),
      sep = "\n"
    )
    cat("\n</div>\n</section>\n\n")
  } else {
    cat("\n\n## Effective N / QC\n\n")
    cat(knitr::kable(display, format = "pipe", row.names = FALSE), sep = "\n")
    cat("\n\n")
  }
  invisible(NULL)
}

.montage_report_emit_panel_qc <- function(qc_row, is_html) {
  if (!is.data.frame(qc_row) || nrow(qc_row) == 0L) {
    return(invisible(NULL))
  }
  status <- if ("qc_status" %in% names(qc_row)) {
    as.character(qc_row$qc_status[[1]])
  } else {
    NA_character_
  }
  dropped_n <- if ("dropped_n" %in% names(qc_row)) {
    qc_row$dropped_n[[1]]
  } else {
    NA_real_
  }
  dropped <- if ("dropped_subjects" %in% names(qc_row)) {
    as.character(qc_row$dropped_subjects[[1]])
  } else {
    ""
  }

  noteworthy <- (!is.na(status) && !status %in% c("ok", "not_reported")) ||
    (!is.na(dropped_n) && dropped_n > 0) ||
    (length(dropped) > 0L && !is.na(dropped) && nzchar(dropped))
  if (!noteworthy) {
    return(invisible(NULL))
  }

  msg <- if (!is.na(dropped_n) && dropped_n > 0) {
    paste0(dropped_n, " subject(s) dropped")
  } else {
    "QC flagged"
  }
  if (length(dropped) > 0L && !is.na(dropped) && nzchar(dropped)) {
    msg <- paste0(msg, " (", dropped, ")")
  }
  if (isTRUE(is_html)) {
    cat(
      "<div class=\"nm-caution\"><strong>Caution</strong>",
      .montage_report_html_escape(msg),
      "</div>\n\n",
      sep = ""
    )
  } else {
    cat("> **Caution - ", msg, "**\n\n", sep = "")
  }
  invisible(NULL)
}

.montage_report_emit_group_start <- function(group,
                                             primary_i,
                                             rows,
                                             manifest,
                                             layout,
                                             layout_state,
                                             section_notes,
                                             section_state,
                                             map_selector,
                                             is_html,
                                             panels = NULL) {
  if (!isTRUE(is_html)) return(invisible(NULL))
  group_label <- group$analysis_label %||% group$analysis_id
  cat(
    "<section class=\"nm-analysis-group\" data-nm-map-group=\"",
    .montage_report_html_escape(group$analysis_id),
    "\" aria-label=\"",
    .montage_report_html_escape(group_label),
    "\">\n<div class=\"nm-group-head\">\n",
    sep = ""
  )
  .montage_report_emit_panel_heading(
    i = primary_i,
    label = group_label,
    manifest = manifest,
    layout = layout,
    layout_state = layout_state,
    is_html = TRUE,
    section_notes = section_notes,
    section_state = section_state
  )

  style <- .montage_report_selector_style(map_selector, length(rows))
  if (identical(style, "none")) {
    cat("</div>\n")
    return(invisible(NULL))
  }
  labels <- vapply(rows, function(i) {
    label <- .montage_report_scalar(manifest[i, , drop = FALSE],
                                    "selector_label")
    if (length(label) == 1L && is.na(label)) {
      label <- manifest$label[[i]]
    }
    as.character(label)
  }, character(1))
  map_ids <- as.character(manifest$map_id[rows])
  variant_ids <- vapply(map_ids, function(id) {
    .montage_report_dom_id("nm-variant", group$analysis_id, id)
  }, character(1))
  primary <- map_ids == group$primary_map_id

  if (identical(style, "tabs")) {
    cat(
      "<div class=\"nm-map-selector nm-map-tabs\" role=\"tablist\" ",
      "aria-label=\"Map variant\">\n",
      sep = ""
    )
    for (j in seq_along(rows)) {
      tab_id <- .montage_report_dom_id(
        "nm-tab", group$analysis_id, map_ids[[j]]
      )
      count <- .montage_report_map_count(
        manifest[rows[[j]], , drop = FALSE], panels[[map_ids[[j]]]]
      )
      count_html <- if (is.na(count$n)) "" else paste0(
        " <span class=\"nm-tab-count\" aria-label=\"", count$n, " ",
        count$unit, "\">", count$n, "</span>"
      )
      cat(
        "<button type=\"button\" class=\"nm-map-tab\" role=\"tab\" id=\"",
        tab_id,
        "\" aria-controls=\"", variant_ids[[j]],
        "\" aria-selected=\"", if (primary[[j]]) "true" else "false",
        "\" tabindex=\"", if (primary[[j]]) "0" else "-1",
        "\" data-nm-label=\"", .montage_report_html_escape(labels[[j]]),
        "\"", if (identical(count$n, 0L)) " data-nm-empty=\"true\"" else "",
        ">",
        .montage_report_html_escape(labels[[j]]), count_html,
        "</button>\n",
        sep = ""
      )
    }
    cat("</div>\n</div>\n")
  } else {
    select_id <- .montage_report_dom_id(
      "nm-select", group$analysis_id, "selector"
    )
    cat(
      "<div class=\"nm-map-selector\"><label class=\"nm-sr-only\" for=\"",
      select_id,
      "\">Map variant</label><select class=\"nm-map-select\" id=\"",
      select_id,
      "\" data-nm-map-select aria-label=\"Map variant\">\n",
      sep = ""
    )
    for (j in seq_along(rows)) {
      cat(
        "<option value=\"", variant_ids[[j]], "\"",
        if (primary[[j]]) " selected" else "",
        ">", .montage_report_html_escape(labels[[j]]), "</option>\n",
        sep = ""
      )
    }
    cat("</select></div>\n</div>\n")
  }
  invisible(NULL)
}

.montage_report_emit_variant_start <- function(row,
                                               group,
                                               position,
                                               map_selector,
                                               is_html) {
  if (!isTRUE(is_html)) return(invisible(NULL))
  map_id <- as.character(row$map_id[[1L]])
  label <- as.character(row$label[[1L]])
  variant_id <- .montage_report_dom_id(
    "nm-variant", group$analysis_id, map_id
  )
  style <- .montage_report_selector_style(
    map_selector, length(group$map_ids)
  )
  tab_id <- .montage_report_dom_id("nm-tab", group$analysis_id, map_id)
  cat(
    "<article class=\"nm-map-variant\" data-nm-map-variant=\"\" ",
    "data-nm-primary=\"",
    if (identical(map_id, group$primary_map_id)) "true" else "false",
    "\" data-nm-map-id=\"", .montage_report_html_escape(map_id),
    "\" id=\"", variant_id, "\"",
    if (identical(style, "tabs")) {
      paste0(" role=\"tabpanel\" aria-labelledby=\"", tab_id, "\"")
    } else {
      ""
    },
    ">\n<h3 class=\"nm-variant-title\">",
    .montage_report_html_escape(label),
    "</h3>\n",
    sep = ""
  )
  invisible(NULL)
}

.montage_report_selector_style <- function(map_selector, n) {
  if (n <= 1L || identical(map_selector, "none")) return("none")
  if (!identical(map_selector, "auto")) return(map_selector)
  if (n <= 4L) "tabs" else "select"
}

.montage_report_dom_id <- function(prefix, analysis_id, map_id) {
  stem <- .safe_file_stem(paste(prefix, analysis_id, map_id, sep = "-"))
  paste0(stem, "-", substr(rlang::hash(c(analysis_id, map_id)), 1L, 8L))
}

.montage_report_emit_panel_heading <- function(i,
                                               label,
                                               manifest,
                                               layout,
                                               layout_state,
                                               is_html,
                                               section_notes = NULL,
                                               section_state = NULL) {
  if (isTRUE(is_html)) {
    if (!is.null(section_state)) {
      .montage_report_emit_html_section_notes(
        i = i,
        manifest = manifest,
        layout = layout,
        section_notes = section_notes,
        section_state = section_state
      )
    }
    row <- manifest[i, , drop = FALSE]
    layout_values <- .montage_report_layout_values(row, layout)
    # A layout path that only repeats the heading adds noise.
    if (identical(paste(layout_values, collapse = " / "), as.character(label))) {
      layout_values <- character(0)
    }
    cat("\n\n<div class=\"nm-panel-heading\">\n", sep = "")
    if (length(layout_values) > 0L) {
      cat(
        "<div class=\"nm-layout-path\">",
        .montage_report_html_escape(paste(layout_values, collapse = " / ")),
        "</div>\n",
        sep = ""
      )
    }
    cat(
      "<h3 class=\"nm-panel-title\">",
      .montage_report_html_escape(label),
      "</h3>\n</div>\n\n",
      sep = ""
    )
    return(invisible(NULL))
  }

  .montage_report_emit_layout_headings(
    i = i,
    manifest = manifest,
    layout = layout,
    layout_state = layout_state,
    section_notes = section_notes
  )
  panel_hashes <- paste(rep("#", min(length(layout) + 2L, 6L)), collapse = "")
  cat("\n\n", panel_hashes, " ", label, "\n\n", sep = "")
  invisible(NULL)
}

.montage_report_emit_layout_headings <- function(i,
                                                 manifest,
                                                 layout,
                                                 layout_state,
                                                 section_notes = NULL) {
  if (length(layout) == 0L || !is.data.frame(manifest) || nrow(manifest) == 0L) {
    return(invisible(NULL))
  }
  changed <- FALSE
  path <- character(0)
  for (depth in seq_along(layout)) {
    field <- layout[[depth]]
    if (!field %in% names(manifest)) {
      next
    }
    value <- as.character(manifest[[field]][[i]])
    if (is.na(value) || !nzchar(value)) {
      next
    }
    path[[field]] <- value
    previous <- layout_state[[field]]
    if (changed || is.null(previous) || !identical(previous, value)) {
      changed <- TRUE
      hashes <- paste(rep("#", min(depth + 1L, 6L)), collapse = "")
      cat("\n\n", hashes, " ", value, "\n\n", sep = "")
      note <- .montage_report_section_note(section_notes, layout, path)
      if (!is.null(note)) {
        .montage_report_emit_narrative_block(note, "nm-section-note",
                                             is_html = FALSE)
      }
    }
    layout_state[[field]] <- value
  }
  invisible(NULL)
}

.montage_report_plural <- function(word) {
  word <- as.character(word)
  if (grepl("is$", word)) sub("is$", "es", word)
  else if (grepl("(s|x|ch|sh)$", word)) paste0(word, "es")
  else if (grepl("[^aeiou]y$", word)) sub("y$", "ies", word)
  else paste0(word, "s")
}

.montage_report_is_parcel_row <- function(row) {
  is.data.frame(row) && nrow(row) >= 1L && "parcel_values" %in% names(row) &&
    !is.null(row$parcel_values[[1L]]) &&
    length(row$parcel_values[[1L]]) > 0L
}

# Suprathreshold extent of one map: rendered surface parcels when available,
# otherwise parcel values or the annotated cluster table.
.montage_report_map_count <- function(row, panel) {
  if (!is.list(panel)) panel <- list()
  # Exact lookups: `$` would partially match `surface_image` / `table_*`.
  surface <- panel[["surface"]]
  if (!is.list(surface)) surface <- list()
  table <- panel[["table"]]
  parcel <- .montage_report_is_parcel_row(row)
  unit <- if (parcel) "parcels" else "clusters"
  n <- NA_integer_
  max_abs <- NA_real_
  if (parcel) {
    vals <- as.numeric(row$parcel_values[[1L]])
    thr <- suppressWarnings(as.numeric(
      .montage_report_pick(row, "effective_threshold", "threshold")
    ))
    keep <- is.finite(vals)
    if (length(thr) == 1L && is.finite(thr)) keep <- keep & abs(vals) >= thr
    n <- as.integer(surface[["n_suprathreshold"]] %||% sum(keep))
    if (any(keep)) max_abs <- max(abs(vals[keep]))
  } else if (is.data.frame(table)) {
    n <- as.integer(attr(table, "n_total") %||% nrow(table))
    if (n > 0L && "max_stat" %in% names(table)) {
      max_abs <- max(abs(table$max_stat), na.rm = TRUE)
    }
  }
  list(n = n, unit = unit, max_abs = max_abs)
}

# Variant labels shared, in order, by every analysis group (NULL otherwise).
.montage_report_shared_labels <- function(groups, manifest) {
  if (length(groups) < 2L || !is.data.frame(manifest) ||
      nrow(manifest) == 0L) {
    return(NULL)
  }
  label_of <- function(i) {
    label <- .montage_report_scalar(manifest[i, , drop = FALSE],
                                    "selector_label")
    if (length(label) == 1L && is.na(label)) label <- manifest$label[[i]]
    as.character(label)
  }
  sets <- lapply(groups, function(g) {
    vapply(match(g$map_ids, manifest$map_id), label_of, "")
  })
  if (length(sets[[1L]]) < 2L ||
      !all(vapply(sets, identical, logical(1), sets[[1L]]))) {
    return(NULL)
  }
  sets[[1L]]
}

.montage_report_emit_summary_matrix <- function(groups, manifest, panels,
                                                layout, is_html) {
  labels <- .montage_report_shared_labels(groups, manifest)
  if (is.null(labels)) return(invisible(NULL))
  esc <- .montage_report_html_escape
  row_head <- if (length(layout) == 1L) {
    tools::toTitleCase(as.character(layout))
  } else {
    "Analysis"
  }
  cells <- lapply(groups, function(g) {
    rows <- match(g$map_ids, manifest$map_id)
    lapply(rows, function(i) {
      row <- manifest[i, , drop = FALSE]
      list(count = .montage_report_map_count(
        row, panels[[as.character(row$map_id[[1L]])]]
      ), map_id = as.character(row$map_id[[1L]]))
    })
  })
  unit <- cells[[1L]][[1L]]$count$unit
  if (!isTRUE(is_html)) {
    tbl <- data.frame(
      vapply(groups, function(g) as.character(g$analysis_label %||% g$analysis_id), ""),
      check.names = FALSE
    )
    names(tbl) <- row_head
    for (k in seq_along(labels)) {
      tbl[[labels[[k]]]] <- vapply(cells, function(r) {
        n <- r[[k]]$count$n
        if (is.na(n)) "" else as.character(n)
      }, "")
    }
    cat(knitr::kable(tbl, format = "pipe"), sep = "\n")
    cat("\n\n")
    return(invisible(NULL))
  }
  head <- paste0(
    "<thead><tr><th scope=\"col\">", esc(row_head), "</th>",
    paste0("<th scope=\"col\" class=\"num\">", esc(labels), "</th>",
           collapse = ""),
    "</tr></thead>"
  )
  body <- vapply(seq_along(groups), function(r) {
    g <- groups[[r]]
    tds <- vapply(cells[[r]], function(cell) {
      n <- cell$count$n
      max_abs <- cell$count$max_abs
      inner <- if (is.na(n)) {
        "<span class=\"nm-cell-empty\">&ndash;</span>"
      } else if (n == 0L) {
        "<span class=\"nm-cell-empty\">0</span>"
      } else {
        paste0("<span class=\"nm-cell-n\">", n, "</span>",
               if (is.finite(max_abs)) paste0(
                 "<span class=\"nm-cell-max\">max ", sprintf("%.2f", max_abs),
                 "</span>") else "")
      }
      paste0(
        "<td class=\"num\"><a class=\"nm-cell\" href=\"#",
        .montage_report_dom_id("nm-variant", g$analysis_id, cell$map_id),
        "\" data-nm-goto-group=\"",
        esc(g$analysis_id), "\" data-nm-goto-map=\"", esc(cell$map_id), "\">",
        inner, "</a></td>"
      )
    }, "")
    paste0("<tr><th scope=\"row\">",
           .montage_report_split_label(g$analysis_label %||% g$analysis_id),
           "</th>", paste(tds, collapse = ""), "</tr>")
  }, "")
  cat(
    "<div class=\"nm-summary\">",
    "<p class=\"nm-summary-note\">Surviving ", unit,
    " per map (largest |statistic| beneath). Select a cell to open that map.</p>",
    "<div class=\"nm-table-wrap\"><table class=\"nm-table nm-summary-table\">",
    head, "<tbody>", paste(body, collapse = ""), "</tbody></table></div></div>\n\n",
    sep = ""
  )
  invisible(NULL)
}

# One control that switches every analysis to the same variant.
.montage_report_emit_maps_controls <- function(groups, manifest, map_selector,
                                               is_html) {
  if (!isTRUE(is_html) || identical(map_selector, "none")) {
    return(invisible(NULL))
  }
  labels <- .montage_report_shared_labels(groups, manifest)
  if (is.null(labels)) return(invisible(NULL))
  esc <- .montage_report_html_escape
  cat(
    "<div class=\"nm-maps-bar\" data-nm-global-tabs>",
    "<span class=\"nm-maps-bar-label\" id=\"nm-global-label\">All sections",
    "</span>",
    "<div class=\"nm-map-tabs nm-global-tabs\" role=\"group\" ",
    "aria-labelledby=\"nm-global-label\">",
    paste0("<button type=\"button\" class=\"nm-map-tab\" aria-pressed=\"false\" ",
           "data-nm-label=\"", esc(labels), "\">", esc(labels), "</button>",
           collapse = ""),
    "</div></div>\n\n",
    sep = ""
  )
  invisible(NULL)
}

# "VGG low (block1_conv2, DCT6)" -> name plus a muted detail line.
.montage_report_split_label <- function(label) {
  label <- as.character(label)
  m <- regmatches(label, regexec("^(.*?)\\s*\\((.+)\\)$", label))[[1L]]
  if (length(m) != 3L || !nzchar(m[[2L]])) {
    return(.montage_report_html_escape(label))
  }
  paste0(.montage_report_html_escape(m[[2L]]),
         "<span class=\"nm-row-detail\">",
         .montage_report_html_escape(m[[3L]]), "</span>")
}

.montage_report_value_label <- function(row) {
  if (!is.data.frame(row) || nrow(row) == 0L) return("Value")
  dist <- .montage_report_scalar(row, "distribution")
  quantity <- .montage_report_scalar(row, "quantity")
  if (identical(as.character(quantity), "test_statistic") &&
      !(length(dist) == 1L && is.na(dist))) {
    return(as.character(dist))
  }
  "Value"
}

.montage_report_hemi_short <- function(hemi) {
  h <- tolower(as.character(hemi))
  out <- ifelse(h %in% c("left", "lh", "l"), "L",
                ifelse(h %in% c("right", "rh", "r"), "R", as.character(hemi)))
  out
}

# ---- Report page: header facts, results overview, map articles, provenance --

# QC is "uniform" when every map reports the same effective N, nothing was
# dropped, and every status is OK; one line then says everything the table
# would. Returns the shared N (numeric) or NULL.
.montage_report_qc_uniform_n <- function(qc_tbl) {
  if (!.montage_report_qc_has_values(qc_tbl)) return(NULL)
  n <- if ("effective_n" %in% names(qc_tbl)) qc_tbl$effective_n else NA
  if (any(is.na(n)) || length(unique(n)) != 1L) return(NULL)
  if (!all(qc_tbl$qc_status == "ok")) return(NULL)
  if ("dropped_n" %in% names(qc_tbl) &&
      any(!is.na(qc_tbl$dropped_n) & qc_tbl$dropped_n > 0)) return(NULL)
  if ("source_n" %in% names(qc_tbl) &&
      any(!is.na(qc_tbl$source_n) & qc_tbl$source_n != n)) return(NULL)
  n[[1L]]
}

.montage_report_qc_line <- function(qc_tbl) {
  n <- .montage_report_qc_uniform_n(qc_tbl)
  if (is.null(n)) return(NULL)
  maps <- nrow(qc_tbl)
  paste0("N = ", .montage_report_fmt_val(n),
         if (maps > 1L) paste0(" for all ", maps, " maps") else "",
         " \u00b7 QC: ok (as supplied)")
}

# Spec-line parts every map shares (stated once in the header facts line);
# per-map lines then show only what differs. N is left to the QC line when
# `n_stated` (it says "N = 486 for all 8 maps").
.montage_report_shared_spec <- function(manifest, n_stated = TRUE) {
  if (!is.data.frame(manifest) || nrow(manifest) < 2L) return(NULL)
  specs <- lapply(seq_len(nrow(manifest)), function(i) {
    .montage_report_metadata_parts(manifest[i, , drop = FALSE])
  })
  first <- specs[[1L]]
  if (!length(first)) return(NULL)
  shared <- first[vapply(seq_along(first), function(j) {
    all(vapply(specs, function(sp) {
      nm <- names(first)[[j]]
      nm %in% names(sp) && identical(sp[[nm]], first[[j]])
    }, logical(1)))
  }, logical(1))]
  if (isTRUE(n_stated)) shared <- shared[names(shared) != "n"]
  if (!length(shared)) return(NULL)
  unname(shared)
}

.montage_report_header_html <- function(title, date, manifest, layout, groups,
                                        qc_line = NULL, label_line = NULL) {
  esc <- .montage_report_html_escape
  map_count <- if (is.data.frame(manifest)) nrow(manifest) else 0L
  analysis_count <- if (is.data.frame(manifest) &&
                        "analysis_id" %in% names(manifest)) {
    length(unique(manifest$analysis_id))
  } else {
    map_count
  }
  shared <- .montage_report_shared_labels(groups, manifest)
  overview <- if (length(layout) == 1L && length(shared) > 1L &&
                  analysis_count > 1L) {
    .montage_report_overview_html(analysis_count, map_count, layout,
                                  paste(layout, collapse = " / "),
                                  shared = shared)
  } else {
    item <- function(value, label) {
      paste0("<span class=\"nm-overview-card\"><span class=\"nm-overview-value\">",
             esc(value), "</span><span class=\"nm-overview-label\">", label,
             "</span></span>")
    }
    paste0(
      "<div class=\"nm-report-overview\">",
      if (analysis_count != map_count || map_count == 1L) {
        item(analysis_count, if (analysis_count == 1L) "analysis" else "analyses")
      },
      item(map_count, if (map_count == 1L) "map" else "maps"),
      "</div>"
    )
  }
  # The QC line's parts ("N = 486 for all 8 maps", "QC: ok (as supplied)") wrap
  # as separate facts so a narrow screen never overflows on one long run.
  qc_parts <- if (!is.null(qc_line)) strsplit(qc_line, " \u00b7 ", fixed = TRUE)[[1L]] else character()
  facts <- c(qc_parts, .montage_report_shared_spec(manifest, n_stated = !is.null(qc_line)))
  paste0(
    "<a class=\"nm-skip\" href=\"#nm-main\">Skip to content</a>",
    "<header class=\"nm-report-header\">",
    "<div class=\"nm-eyebrow\">Montage report",
    if (!is.null(date) && nzchar(date)) paste0(" &middot; ", esc(date)) else "",
    "</div>",
    "<h1 class=\"nm-report-title\">", esc(title), "</h1>",
    "<div class=\"nm-report-meta\">",
    overview,
    if (length(facts)) {
      paste0("<p class=\"nm-report-facts\">",
             paste0("<span", ifelse(seq_along(facts) <= length(qc_parts), " class=\"nm-fact-qc\"", ""),
                    ">", esc(facts), "</span>",
                    collapse = "<span class=\"nm-sep\" aria-hidden=\"true\"> &middot; </span>"),
             "</p>")
    },
    "</div>",
    if (!is.null(label_line)) {
      # The last clause and the Methods link wrap together, never the link alone.
      parts <- strsplit(label_line, "; ", fixed = TRUE)[[1L]]
      tail <- parts[[length(parts)]]
      head <- if (length(parts) > 1L) paste0(paste(parts[-length(parts)], collapse = "; "), "; ") else ""
      paste0("<p class=\"nm-report-labels\">", esc(head),
             "<span class=\"nm-nowrap\">", esc(tail),
             " <a href=\"#nm-methods\">Methods</a></span></p>")
    },
    "</header>\n\n"
  )
}

# DOM id of one map's article; also the target of overview links.
.montage_report_map_anchor <- function(analysis_id, map_id) {
  .montage_report_dom_id("nm-map", analysis_id, map_id)
}

.montage_report_emit_map_start <- function(i, label, analysis_id, manifest,
                                           layout, layout_state, section_notes,
                                           section_state, is_html) {
  if (!isTRUE(is_html)) {
    .montage_report_emit_panel_heading(
      i = i, label = label, manifest = manifest, layout = layout,
      layout_state = layout_state, is_html = FALSE,
      section_notes = section_notes, section_state = section_state
    )
    return(invisible(NULL))
  }
  .montage_report_emit_html_section_notes(
    i = i, manifest = manifest, layout = layout,
    section_notes = section_notes, section_state = section_state
  )
  map_id <- as.character(manifest$map_id[[i]])
  anchor <- .montage_report_map_anchor(analysis_id, map_id)
  cat("\n\n<article class=\"nm-map\" id=\"", anchor, "\" aria-labelledby=\"",
      anchor, "-title\" data-nm-map-article=\"",
      .montage_report_html_escape(analysis_id),
      # Every file saved from this map (figures, viewer exports) is named from
      # one slug, e.g. "story-gt-math_volume.png" / "story-gt-math_slices.png".
      "\" data-nm-file-stem=\"", .montage_report_file_stem(label), "\">\n", sep = "")
  out <- utils::capture.output(.montage_report_emit_panel_heading(
    i = i, label = label, manifest = manifest, layout = layout,
    layout_state = layout_state, is_html = TRUE
  ))
  out <- sub("<h3 class=\"nm-panel-title\">",
             paste0("<h3 class=\"nm-panel-title\" id=\"", anchor, "-title\">"),
             out, fixed = TRUE)
  cat(out, sep = "\n")
  cat("\n")
  invisible(NULL)
}

# Where the Static / Slices / 3D view router belongs: directly under the
# map's spec line. The surface bridge creates the router; the report script
# moves it here (see montage-report.js).
.montage_report_emit_view_slot <- function(analysis_id, label, is_html) {
  if (!isTRUE(is_html)) return(invisible(NULL))
  cat("<div class=\"nm-view-slot\" data-nm-view-slot=\"",
      .montage_report_html_escape(analysis_id), "\" data-nm-view-label=\"",
      .montage_report_html_escape(label), "\"></div>\n\n", sep = "")
  invisible(NULL)
}

# ---- Results at a glance --------------------------------------------------

# One record per map with the numbers the overview shows.
.montage_report_map_result <- function(row, panel) {
  if (!is.list(panel)) panel <- list()
  table <- panel[["table"]]
  if (!is.data.frame(table) || inherits(table, "montage_parcel_table") ||
      !all(c("max_stat", "sign") %in% names(table))) {
    return(NULL)
  }
  n_total <- attr(table, "n_total") %||% nrow(table)
  voxels <- attr(table, "n_voxels_total")
  if (is.null(voxels) && "n_voxels" %in% names(table) && n_total == nrow(table)) {
    voxels <- sum(table$n_voxels, na.rm = TRUE)
  }
  peak <- function(s) {
    hit <- which(as.character(table$sign) == s)
    if (!length(hit)) return(NULL)
    j <- hit[which.max(abs(table$max_stat[hit]))]
    cells <- .montage_report_region_cells(table[j, , drop = FALSE])
    list(value = table$max_stat[j], region = cells$region,
         off = cells$off_atlas, hemi = cells$hemisphere, id = as.character(table$cluster_id[j]),
         xyz = c(table$peak_mni_x[j], table$peak_mni_y[j], table$peak_mni_z[j]))
  }
  list(n_total = n_total, voxels = voxels, by_sign = attr(table, "n_by_sign"),
       pos = peak("positive"), neg = peak("negative"))
}

.montage_report_signed <- function(v, digits = 1L) {
  text <- formatC(abs(v), format = "f", digits = digits)
  paste0(if (v < 0) "\u2212" else "+", text)
}

.montage_report_emit_results_overview <- function(groups, manifest, panels,
                                                  layout, is_html,
                                                  figure_assets = NULL,
                                                  heading = "Results at a glance") {
  if (!is.data.frame(manifest) || nrow(manifest) < 2L) return(invisible(FALSE))
  # Analyses with shared variants already get the variant matrix.
  if (!is.null(.montage_report_shared_labels(groups, manifest))) {
    cat("# ", heading, "\n\n", sep = "")
    .montage_report_emit_summary_matrix(groups, manifest, panels, layout, is_html)
    return(invisible(TRUE))
  }
  if (is.null(groups)) {
    groups <- lapply(seq_len(nrow(manifest)), function(i) {
      id <- if ("analysis_id" %in% names(manifest)) manifest$analysis_id[[i]] else manifest$map_id[[i]]
      list(analysis_id = as.character(id), map_ids = as.character(manifest$map_id[[i]]))
    })
  }
  entries <- list()
  for (g in groups) {
    rows <- match(g$map_ids, manifest$map_id)
    for (i in rows[!is.na(rows)]) {
      row <- manifest[i, , drop = FALSE]
      map_id <- as.character(row$map_id[[1L]])
      result <- .montage_report_map_result(row, panels[[map_id]])
      if (is.null(result)) next
      entries[[length(entries) + 1L]] <- list(
        group = g, row = row, map_id = map_id, result = result,
        panel = if (is.list(panels[[map_id]])) panels[[map_id]] else list()
      )
    }
  }
  if (length(entries) < 2L) return(invisible(FALSE))
  cat("# ", heading, "\n\n", sep = "")
  if (!isTRUE(is_html)) {
    tbl <- data.frame(
      Map = vapply(entries, function(e) as.character(e$row$label[[1L]]), ""),
      Clusters = vapply(entries, function(e) as.integer(e$result$n_total), 0L),
      Voxels = vapply(entries, function(e) as.numeric(e$result$voxels %||% NA), 0),
      `Peak +` = vapply(entries, function(e) {
        p <- e$result$pos
        if (is.null(p)) "" else paste0(.montage_report_signed(p$value, 2L), " ", p$region)
      }, ""),
      `Peak -` = vapply(entries, function(e) {
        p <- e$result$neg
        if (is.null(p)) "" else paste0(.montage_report_signed(p$value, 2L), " ", p$region)
      }, ""),
      check.names = FALSE
    )
    cat(knitr::kable(tbl, format = "pipe", row.names = FALSE), sep = "\n")
    cat("\n\n")
    return(invisible(TRUE))
  }
  esc <- .montage_report_html_escape
  peak_max <- max(unlist(lapply(entries, function(e) {
    c(abs(e$result$pos$value %||% 0), abs(e$result$neg$value %||% 0))
  })), 1e-9)
  hemi_short <- function(h) {
    h <- tolower(as.character(h %||% NA))
    if (is.na(h)) "" else switch(h, left = " (L)", right = " (R)",
                                 midline = " (midline)", "")
  }
  peak_html <- function(p, cls) {
    if (is.null(p)) {
      return(paste0("<span class=\"nm-atlas-peak nm-atlas-none\">no ",
                    if (cls == "pos") "positive" else "negative",
                    " clusters</span>"))
    }
    paste0(
      "<span class=\"nm-atlas-peak\"><span class=\"nm-atlas-z nm-", cls, "\">",
      .montage_report_signed(p$value), "</span><span class=\"nm-atlas-region",
      if (isTRUE(p$off)) " nm-off-atlas" else "", "\" title=\"",
      esc(paste0(p$region, hemi_short(p$hemi))), "\">",
      # The hemisphere never wraps onto a line of its own.
      # ("pCunPCC 2 (R)" stays together: a parcel number keeps its name.)
      sub("^(.*?)(\\S+(?: [0-9]+)?)$", "\\1<span class=\"nm-nowrap\">\\2",
          esc(p$region), perl = TRUE),
      esc(hemi_short(p$hemi)), "</span></span></span>"
    )
  }
  value_sym <- tolower(.montage_report_value_label(entries[[1L]]$row))
  value_sym <- if (identical(value_sym, "value")) "" else paste0(" ", value_sym)
  thumb_alt <- paste0("maximum-|", trimws(value_sym), "| projection")
  if (identical(trimws(value_sym), "")) thumb_alt <- "maximum-magnitude projection"
  cards <- vapply(entries, function(e) {
    g <- e$group
    row <- e$row
    label <- as.character(row$label[[1L]])
    multi <- length(g$map_ids) > 1L
    href <- if (multi) {
      .montage_report_dom_id("nm-variant", g$analysis_id, e$map_id)
    } else {
      .montage_report_map_anchor(g$analysis_id, e$map_id)
    }
    section <- .montage_report_layout_values(row, layout)
    section <- section[section != label]
    thumb_path <- e$panel[["summary_image"]]
    if ((is.null(thumb_path) || !nzchar(thumb_path %||% "")) && !is.null(figure_assets)) {
      thumb_path <- e$panel[["surface_image"]] %||% e$panel[["volume_image"]]
    }
    thumb <- if (!is.null(thumb_path) && length(thumb_path) == 1L &&
                 !is.na(thumb_path) && file.exists(thumb_path)) {
      dims <- .montage_png_dimensions(thumb_path)
      thumb_dark <- e$panel[["summary_image_dark"]]
      dark_src <- if (!is.null(thumb_dark) && length(thumb_dark) == 1L && !is.na(thumb_dark) &&
                      file.exists(thumb_dark)) {
        q <- "screen and (prefers-color-scheme: dark)"
        paste0("<source media=\"", q, "\" data-nm-media=\"", q,
               "\" data-nm-theme=\"dark\" srcset=\"",
               .montage_report_image_src(thumb_dark, figure_assets), "\" />")
      }
      img <- paste0("<img src=\"", .montage_report_image_src(thumb_path, figure_assets),
                    "\" alt=\"", esc(paste0("Glass brain of ", label, ": ", thumb_alt)), "\"",
                    if (!is.null(dims)) paste0(" width=\"", dims[[1]], "\" height=\"", dims[[2]], "\""),
                    " loading=\"lazy\" decoding=\"async\" />")
      paste0("<span class=\"nm-atlas-thumb", if (!is.null(dark_src)) " nm-thumb-dark-ready", "\">",
             if (!is.null(dark_src)) paste0("<picture>", dark_src, img, "</picture>") else img,
             "</span>")
    } else {
      ""
    }
    r <- e$result
    pos <- if (is.null(r$pos)) 0 else abs(r$pos$value) / peak_max
    neg <- if (is.null(r$neg)) 0 else abs(r$neg$value) / peak_max
    stats <- c(
      if (!is.null(r$voxels) && is.finite(r$voxels)) {
        paste0("<span><b>", .montage_report_int(r$voxels), "</b> voxels</span>")
      },
      paste0("<span><b>", r$n_total, "</b> ",
             if (identical(as.integer(r$n_total), 1L)) "cluster" else "clusters",
             if (length(r$by_sign) == 2L && r$n_total > 0L) {
               paste0(" <span class=\"nm-atlas-split\">(", r$by_sign[["positive"]],
                      "+ / ", r$by_sign[["negative"]], "\u2212)</span>")
             } else "",
             "</span>")
    )
    paste0(
      "<li class=\"nm-atlas-card", if (nzchar(thumb)) "" else " nm-atlas-nothumb",
      "\"><a class=\"nm-atlas-link\" href=\"#", href, "\"",
      if (multi) paste0(" data-nm-goto-group=\"", esc(g$analysis_id),
                        "\" data-nm-goto-map=\"", esc(e$map_id), "\"") else "",
      ">", thumb,
      "<span class=\"nm-atlas-body\">",
      if (length(section)) paste0("<span class=\"nm-atlas-section\">",
                                  gsub("(\\([^()]*\\))", "<span class=\"nm-nowrap\">\\1</span>",
                                       esc(paste(section, collapse = " / "))),
                                  "</span>"),
      "<span class=\"nm-atlas-title\">", esc(label), "</span>",
      "<span class=\"nm-atlas-stats\">", paste(stats, collapse = ""), "</span>",
      "<span class=\"nm-atlas-bar\" aria-hidden=\"true\" style=\"--nm-bar-neg:",
      sprintf("%.3f", neg), ";--nm-bar-pos:", sprintf("%.3f", pos),
      "\"><span class=\"nm-atlas-bar-neg\"></span><span class=\"nm-atlas-bar-pos\"></span></span>",
      peak_html(r$pos, "pos"), peak_html(r$neg, "neg"),
      "</span></a></li>"
    )
  }, "")
  has_thumbs <- any(vapply(entries, function(e) !is.null(e$panel[["summary_image"]]), logical(1)))
  sym_bars <- if (nzchar(trimws(value_sym))) paste0("|", trimws(value_sym), "|") else "magnitude"
  cat(
    "<div class=\"nm-atlas\">",
    # One line above the grid (what a card shows and how to use it); the
    # reading rules for the glass brains sit under it, as a figure legend.
    "<p class=\"nm-atlas-note\">Extent and strongest positive and negative peak of each map; ",
    "<span class=\"nm-atlas-key\"><span class=\"nm-atlas-key-bar\" aria-hidden=\"true\"></span>",
    "bars show |peak", esc(value_sym), "| on one 0\u2013", esc(formatC(peak_max, format = "f", digits = 1)),
    " scale.</span> Select a map to open it.</p>",
    "<ol class=\"nm-atlas-grid\">", paste(cards, collapse = ""), "</ol>",
    if (has_thumbs) {
      # The renderer states what its thumbnails encode; use its words.
      semantics <- Filter(function(x) is.character(x) && length(x) == 1L && nzchar(x),
                          lapply(entries, function(e) e$panel[["summary_semantics"]]))
      text <- if (length(semantics)) semantics[[1L]] else paste0(
        "Glass brains: the strongest ", sym_bars,
        " along each line of sight (an opposite-sign effect behind it is hidden); ",
        "left hemisphere on the left, sagittal view anterior on the left.")
      paste0("<p class=\"nm-atlas-legend\">", esc(text), "</p>")
    },
    "</div>\n\n",
    sep = ""
  )
  invisible(TRUE)
}

# ---- Provenance -------------------------------------------------------------

.montage_report_prov_key <- function(key) {
  key <- as.character(key)
  key <- sub("^interactive_surface_", "", key)
  key <- sub("^interactive_", "", key)
  words <- strsplit(key, "_", fixed = TRUE)[[1L]]
  words[words == "sha256"] <- "SHA-256"
  words[words == "id"] <- "ID"
  out <- paste(words, collapse = " ")
  paste0(toupper(substr(out, 1L, 1L)), substring(out, 2L))
}

.montage_report_prov_group <- function(key) {
  if (startsWith(key, "interactive_surface_")) "Interactive surface"
  else if (startsWith(key, "interactive_")) "Interactive volume"
  else "Report"
}

.montage_report_bytes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) return(NULL)
  units <- c("bytes", "KiB", "MiB", "GiB")
  k <- if (x < 1024) 1L else min(floor(log(x, 1024)) + 1L, 4L)
  shown <- if (k == 1L) .montage_report_int(x) else sprintf("%.1f", x / 1024^(k - 1L))
  paste0(shown, " ", units[[k]])
}

# Value cell: hex digests shown truncated (the full value stays in the text,
# so copying a hash copies all of it), lists split one item per line.
.montage_report_prov_value_html <- function(key, value) {
  esc <- .montage_report_html_escape
  hash_html <- function(x) {
    x <- trimws(x)
    m <- regmatches(x, regexec("^(.*?=)?([0-9a-fA-F]{24,})$", x))[[1L]]
    if (length(m) == 3L) {
      paste0(if (nzchar(m[[2L]])) paste0("<span class=\"nm-prov-name\">",
                                         esc(sub("=$", "", m[[2L]])), "</span> "),
             "<code class=\"nm-hash\" title=\"", esc(m[[3L]]), "\">",
             esc(m[[3L]]), "</code>")
    } else {
      esc(x)
    }
  }
  if (grepl("_bytes$", key)) {
    pretty <- .montage_report_bytes(value)
    if (!is.null(pretty)) return(esc(pretty))
  }
  items <- strsplit(value, ";\\s*")[[1L]]
  if (length(items) > 1L) {
    return(paste0("<ul class=\"nm-prov-items\">",
                  paste0("<li>", vapply(items, hash_html, ""), "</li>", collapse = ""),
                  "</ul>"))
  }
  hash_html(value)
}

.montage_report_emit_provenance <- function(prov, is_html) {
  if (is.null(prov)) {
    cat("*No provenance metadata available.*\n")
    return(invisible(NULL))
  }
  fields <- vapply(names(prov), function(field) {
    value <- prov[[field]]
    if (is.null(value) || length(value) != 1L || is.na(value)) "" else as.character(value)
  }, character(1))
  fields <- fields[nzchar(fields)]
  if (length(fields) == 0L) {
    cat("*No scalar provenance metadata available.*\n")
    return(invisible(NULL))
  }
  keys <- names(fields)
  if (!isTRUE(is_html)) {
    tbl <- data.frame(
      Field = vapply(keys, function(k) {
        g <- .montage_report_prov_group(k)
        paste0(if (g != "Report") paste0(g, ": ") else "", .montage_report_prov_key(k))
      }, ""),
      Value = unname(fields),
      check.names = FALSE
    )
    cat(knitr::kable(tbl, format = "pipe", row.names = FALSE), sep = "\n")
    cat("\n\n")
    return(invisible(NULL))
  }
  esc <- .montage_report_html_escape
  summary_bits <- c(
    if (!is.na(fields["package"]) && !is.na(fields["package_version"])) {
      paste(fields[["package"]], fields[["package_version"]])
    },
    if (!is.na(fields["generated_at"])) paste("generated", fields[["generated_at"]]),
    paste(length(fields), "fields")
  )
  groups <- vapply(keys, .montage_report_prov_group, "")
  body <- vapply(unique(groups), function(g) {
    ks <- keys[groups == g]
    paste0(
      "<div class=\"nm-prov-group\"><h3 class=\"nm-prov-head\">", esc(g), "</h3><dl class=\"nm-prov-list\">",
      paste0("<div class=\"nm-prov-row\"><dt>", esc(vapply(ks, .montage_report_prov_key, "")),
             "</dt><dd>", vapply(ks, function(k) .montage_report_prov_value_html(k, fields[[k]]), ""),
             "</dd></div>", collapse = ""),
      "</dl></div>"
    )
  }, "")
  cat(
    "<details class=\"nm-details nm-provenance\"><summary>Show provenance",
    "<span class=\"nm-details-meta\">", esc(paste(summary_bits, collapse = " \u00b7 ")),
    "</span></summary><div class=\"nm-prov-body\">", paste(body, collapse = ""),
    "</div></details>\n\n", sep = ""
  )
  invisible(NULL)
}

# ---- Label sources and Methods ---------------------------------------------

.montage_report_and <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[[length(x)]])
}

.montage_report_count_word <- function(n) {
  words <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight",
             "Nine", "Ten", "Eleven", "Twelve")
  if (is.numeric(n) && length(n) == 1L && n >= 1 && n <= 12 && n == round(n)) {
    words[[n]]
  } else {
    as.character(n)
  }
}

# One line, stated in the header: where every peak label came from.
.montage_report_label_line <- function(labelling) {
  if (is.null(labelling) || !length(labelling$atlases)) return(NULL)
  bits <- paste0("Peak labels: ", paste(labelling$atlases, collapse = ", "))
  if (!is.null(labelling$cerebellum)) {
    bits <- paste0(bits, "; cerebellum: ", labelling$cerebellum$short)
  }
  if (is.finite(labelling$radius %||% NA) && labelling$radius > 0) {
    bits <- paste0(bits, "; \u2248 nearest region \u2264 ",
                   .montage_report_fmt_val(labelling$radius), " mm")
  }
  paste0(bits, ".")
}

.montage_report_cite <- function(cits) {
  short <- vapply(cits, function(c) as.character(c$short %||% ""), "")
  short <- short[nzchar(short)]
  if (!length(short)) return("")
  paste0(" (", paste(short, collapse = "; "), ")")
}

# The fMRIPrep-style boilerplate: a copyable paragraph built from the
# manifest, the policy, the rendered panels, the label sources and the
# software versions. Returns list(text, references) or NULL.
.montage_report_methods <- function(rd) {
  manifest <- rd$manifest
  if (!is.data.frame(manifest) || nrow(manifest) == 0L) return(NULL)
  panels <- rd$panels %||% list()
  policy <- rd$params$policy
  labelling <- rd$labelling
  prov <- rd$provenance %||% list()
  fmt <- .montage_report_fmt_val
  n_maps <- nrow(manifest)
  first <- manifest[1L, , drop = FALSE]
  parts1 <- .montage_report_metadata_parts(first)
  stat_name <- if ("stat" %in% names(parts1)) parts1[["stat"]] else NULL
  sym <- tolower(.montage_report_value_label(first))
  if (identical(sym, "value")) sym <- "value"
  same <- function(field) {
    v <- if (field %in% names(manifest)) manifest[[field]] else NULL
    if (is.null(v) || is.list(v)) return(NULL)
    u <- unique(v)
    if (length(u) == 1L && !is.na(u)) u else NULL
  }
  stat_all <- unique(vapply(seq_len(n_maps), function(i) {
    p <- .montage_report_metadata_parts(manifest[i, , drop = FALSE])
    if ("stat" %in% names(p)) p[["stat"]] else NA_character_
  }, ""))
  level <- same("level")
  n <- same("n")
  sentences <- character(0)
  refs <- list()

  # Inputs.
  what <- paste0(
    .montage_report_count_word(n_maps),
    if (!is.null(level)) paste0(" ", level, "-level") else "",
    if (length(stat_all) == 1L && !is.na(stat_all)) {
      # "Z-statistic" reads "z-statistic" mid-sentence.
      paste0(" ", sub("^([A-Z])-statistic", "\\L\\1-statistic", stat_all, perl = TRUE))
    } else {
      " statistical"
    },
    if (n_maps == 1L) " map" else " maps"
  )
  sentences <- c(sentences, paste0(
    what,
    if (!is.null(n)) paste0(" (N = ", fmt(n), if (n_maps > 1L) " each" else "", ")") else "",
    " were displayed and tabulated with neuromosaic",
    if (!is.null(prov$package_version)) paste0(" ", prov$package_version) else "",
    if (!is.null(prov$package_url) && !is.na(prov$package_url)) {
      paste0(" (", prov$package_url, ")")
    } else {
      ""
    },
    "."
  ))
  refs <- c(refs, list(list(
    short = paste0("neuromosaic", if (!is.null(prov$package_version)) paste0(" ", prov$package_version) else ""),
    title = "neuromosaic: rich reports and interactive exploration for neuroimaging statistical maps (R package)",
    journal = NA_character_, doi = NA_character_,
    url = if (!is.null(prov$package_url) && !is.na(prov$package_url)) prov$package_url else NA_character_,
    note = if (!is.null(prov$package_source) && !is.na(prov$package_source)) prov$package_source else NA_character_
  )))

  # Threshold and extent.
  thr <- same("effective_threshold")
  tail <- same("effective_tail")
  k <- same("effective_min_cluster_size")
  conn <- same("effective_connectivity")
  fixed <- "threshold" %in% names(manifest) && all(!is.na(manifest$threshold))
  q <- same("effective_q")
  p <- if (!is.null(policy)) policy$p else NULL
  if (!is.null(thr) && !is.null(tail)) {
    rule <- switch(as.character(tail),
                   positive = paste0(sym, " \u2265 ", fmt(thr), " (positive tail)"),
                   negative = paste0(sym, " \u2264 \u2212", fmt(thr), " (negative tail)"),
                   paste0("|", sym, "| \u2265 ", fmt(thr), " (two-sided)"))
    how <- if (fixed) {
      "a fixed voxelwise threshold of "
    } else if (!is.null(q)) {
      paste0("Benjamini\u2013Hochberg FDR q = ", format(signif(q, 3), scientific = FALSE, trim = TRUE), " per map, i.e. ")
    } else if (is.numeric(p) && length(p) == 1L) {
      paste0("voxelwise p < ", format(signif(p, 3), scientific = FALSE, trim = TRUE),
             " (uncorrected), i.e. ")
    } else {
      ""
    }
    s <- paste0("Maps were thresholded at ", how, rule)
    if (!is.null(k) && !is.null(conn)) {
      s <- paste0(s, ", and clusters smaller than ", fmt(k), " voxels (",
                  sub("-connect$", "-connectivity", conn), ") were removed")
    }
    sentences <- c(sentences, paste0(s, "; no further correction was applied in the report."))
  } else {
    sentences <- c(sentences, "Thresholds differ by map and are stated in each map's specification line.")
  }

  # Colour cap.
  caps <- unlist(lapply(panels, function(pn) {
    vm <- .montage_report_volume_meta(pn)
    v <- if (!is.null(vm)) suppressWarnings(as.numeric(vm$cap)) else NA
    if (length(v) == 1L && is.finite(v)) v else NULL
  }))
  signed <- any(vapply(panels, function(pn) {
    isTRUE((.montage_report_volume_meta(pn) %||% list())$signed %||% TRUE)
  }, logical(1)))
  if (length(caps)) {
    pm <- if (signed) "\u00b1" else ""
    uniq <- unique(round(caps, 1))
    if (!is.null(policy) && !is.null(policy$cap)) {
      sentences <- c(sentences, paste0("Colour scales saturate at a fixed ", pm, fmt(policy$cap), "."))
    } else {
      qtile <- policy$cap_quantile %||% 0.99
      pct <- paste0(fmt(round(100 * qtile, 1)), "th percentile")
      pooled <- length(policy$cap_within %||% character()) > 0L ||
        identical(policy$cap_scope %||% "quantity", "quantity")
      sentences <- c(sentences, paste0(
        "Colour scales saturate at ",
        if (length(uniq) == 1L) paste0(pm, fmt(uniq)) else
          paste0(pm, fmt(min(uniq)), "\u2013", fmt(max(uniq))),
        ", the ", pct, " of suprathreshold |", sym, "|",
        if (pooled && length(uniq) == 1L && n_maps > 1L) " pooled across all maps of the same statistic" else
          if (pooled) " pooled within each group of comparable maps" else " of each map",
        if (!is.null(policy$cap_floor)) paste0(" (at least ", fmt(policy$cap_floor), ")") else "",
        "; warm colours are positive and cool colours negative."
      ))
    }
  }

  # Slices.
  slice_sets <- lapply(panels, function(pn) {
    l <- .montage_report_volume_meta(pn)$slice_labels
    as.character(l %||% character())
  })
  slice_sets <- Filter(length, slice_sets)
  if (length(slice_sets)) {
    n_slices <- max(lengths(slice_sets))
    axis <- sub("^\\s*([xyz]).*$", "\\1", slice_sets[[1L]][[1L]])
    plane <- switch(axis, x = "sagittal", y = "coronal", "axial")
    scopes <- lapply(panels, function(pn) .montage_report_slice_share(if (is.list(pn)) pn$slice_scope))
    shared <- any(vapply(scopes, Negate(is.null), logical(1)))
    layout <- rd$params$layout %||% character()
    key <- if (length(layout) && layout[[1L]] %in% names(manifest)) {
      as.character(manifest[[layout[[1L]]]])
    } else if ("analysis_id" %in% names(manifest)) {
      as.character(manifest$analysis_id)
    } else {
      NULL
    }
    if (!shared && !is.null(key)) {
      ids <- as.character(manifest$map_id)
      for (g in unique(key)) {
        members <- intersect(ids[key == g], names(slice_sets))
        if (length(members) > 1L &&
            length(unique(lapply(slice_sets[members], paste, collapse = "|"))) == 1L) {
          shared <- TRUE
        }
      }
    }
    ul <- rd$underlay
    underlay <- if (is.list(ul) && !is.null(ul$template) && !is.na(ul$template)) {
      sp <- ul$spacing
      paste0(" on the ", ul$template, " brain template",
             if (length(sp) == 3L && all(is.finite(sp))) {
               if (length(unique(round(sp, 2))) == 1L) paste0(" (", fmt(round(sp[[1L]], 2)), " mm voxels)") else
                 paste0(" (", paste(fmt(round(sp, 2)), collapse = " \u00d7 "), " mm voxels)")
             } else "")
    } else {
      ""
    }
    sentences <- c(sentences, paste0(
      "Volume figures show up to ", n_slices, " ", plane, " slices", underlay,
      ", spaced in equal whole-voxel steps and centred within the 5th\u201395th percentile range of the",
      " displayed (suprathreshold, extent-filtered) voxels' ", plane, " positions",
      if (shared) ", with one slice set shared by the maps of a section (chosen from their combined extent) so that related maps are shown at the same positions" else "",
      "; coordinates are MNI millimetres and the left hemisphere is on the left."
    ))
  }
  iu <- rd$interactive$summary$underlay
  if (is.list(iu) && identical(iu$source, "underlay") && length(iu$voxel_mm)) {
    sentences <- c(sentences, paste0(
      "The interactive slice viewer draws the maps over ",
      if (!is.null(iu$template) && !is.na(iu$template)) paste0("the ", iu$template, " template") else
        "a separate underlay",
      " at ", .montage_voxel_size_text(iu$voxel_mm),
      ", each map at its own voxel positions without resampling."
    ))
  }
  if (any(vapply(panels, function(pn) is.list(pn) && !is.null(pn$surface_image), logical(1)))) {
    sm <- rd$surface_method
    thr_s <- same("effective_threshold")
    if (is.list(sm) && identical(sm$sampling, "thickness") && length(sm$depth)) {
      mesh <- paste0(
        if (!is.null(sm$space) && !is.na(sm$space)) paste0("the ", sm$space, " ") else "the ",
        "cortical surface",
        if (is.finite(sm$vertices %||% NA)) paste0(" (", .montage_report_int(sm$vertices), " vertices per hemisphere)") else ""
      )
      depth <- paste0(length(sm$depth), " depths from ", fmt(round(100 * min(sm$depth))), "% to ",
                      fmt(round(100 * max(sm$depth))), "% of cortical thickness")
      # surf_montage projects the unthresholded map (only voxels of clusters
      # below the extent removed) and thresholds the depth averages.
      k_s <- suppressWarnings(as.integer(k %||% NA))
      removed <- if (length(k_s) == 1L && !is.na(k_s) && k_s > 1L) {
        paste0(" (voxels of clusters smaller than ", k_s, " voxels removed)")
      } else {
        ""
      }
      sentences <- c(sentences, paste0(
        "Surface figures sample the unthresholded ", sym, " map", removed, " on ", mesh,
        " at ", depth, " (", if (identical(sm$interpolation, "linear")) "trilinear interpolation" else
          paste0(sm$interpolation, " interpolation"), "), average them per vertex",
        if (!is.null(thr_s)) paste0(", and show vertices whose depth-averaged value reaches |", sym, "| \u2265 ", fmt(thr_s)) else "",
        ", drawn on the ", sm$surface %||% "inflated", " surface",
        if (isTRUE(sm$medial_wall)) " with the medial wall masked" else "",
        ". The voxel extent rule applies before projection, not to surface patches; ",
        "surface peaks are lower than volume peaks, and subcortical and cerebellar effects ",
        "appear only in the volume figures."
      ))
    } else {
      sentences <- c(sentences, paste(
        "Surface figures show the volume values projected to the cortical surface and",
        "averaged across cortical depth, so surface peaks are lower than volume peaks;",
        "subcortical and cerebellar effects appear only in the volume figures."
      ))
    }
  }
  has_glance <- nrow(manifest) >= 2L && any(vapply(panels, function(pn) {
    is.list(pn) && !is.null(pn$summary_image)
  }, logical(1)))
  if (has_glance) {
    sentences <- c(sentences, paste0(
      "The glance thumbnails are maximum-|", sym, "| projections (glass brain) of each map's displayed voxels ",
      "in sagittal, coronal and axial views, on one colour scale for all maps; ",
      "an opposite-sign effect behind a stronger one is hidden."
    ))
  }

  # Labels.
  if (!is.null(labelling) && length(labelling$atlases)) {
    s <- paste0(
      "Clusters were numbered within sign by peak strength (P1 is the strongest positive cluster, ",
      "N1 the strongest negative; peaks equal at the printed precision are ordered by their unrounded values), and each peak was labelled with the ",
      .montage_report_and(labelling$atlases),
      if (length(labelling$atlases) > 1L) " atlases" else " atlas",
      .montage_report_cite(labelling$citations)
    )
    refs <- c(refs, labelling$citations)
    if (is.finite(labelling$radius %||% NA) && labelling$radius > 0) {
      s <- paste0(s, "; a peak outside every region took the nearest labelled region within ",
                  fmt(labelling$radius), " mm (marked \u2248, with the distance)")
    }
    s <- paste0(s, "; the hemisphere follows the peak's x coordinate (midline when |x| < ",
                fmt(.montage_midline_mm), " mm)")
    if (!is.null(labelling$cerebellum)) {
      cer <- labelling$cerebellum
      s <- paste0(s, "; cerebellar peaks were labelled from the ",
                  if (isTRUE(cer$hcp)) "HCP grayordinate subcortical segmentation" else cer$source,
                  if (isTRUE(cer$hcp)) paste0(" (", cer$citations[[1L]]$short, ") obtained from TemplateFlow (",
                                              cer$citations[[2L]]$short, ")") else "")
      refs <- c(refs, cer$citations)
    }
    sentences <- c(sentences, paste0(s, "."))
  }

  # Software.
  sw <- prov$software
  if (!is.null(sw) && !is.na(sw) && nzchar(sw)) {
    sentences <- c(sentences, paste0(
      "Software: ", gsub("; ", ", ", sw, fixed = TRUE),
      if (!is.null(prov$package_source) && !is.na(prov$package_source)) {
        paste0(" (neuromosaic ", prov$package_source, ")")
      } else {
        ""
      },
      if (!is.null(prov$r_version)) paste0("; R ", prov$r_version) else "", "."
    ))
  }
  refs <- refs[!duplicated(vapply(refs, function(r) as.character(r$short %||% ""), ""))]
  list(text = paste(sentences, collapse = " "), references = refs)
}

.montage_report_reference_text <- function(r) {
  ok <- function(x) !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  paste0(r$short, ". ",
         if (ok(r$title)) paste0(r$title, ". ") else "",
         if (ok(r$journal)) paste0(r$journal, ". ") else "",
         if (ok(r$note)) paste0(r$note, ". ") else "",
         if (ok(r$doi)) paste0("doi:", r$doi) else if (ok(r$url)) r$url else "")
}

.montage_report_emit_methods <- function(rd, is_html) {
  m <- .montage_report_methods(rd)
  if (is.null(m)) return(invisible(FALSE))
  refs <- vapply(m$references, .montage_report_reference_text, "")
  if (!isTRUE(is_html)) {
    cat(m$text, "\n\n", sep = "")
    if (length(refs)) cat(paste0("- ", refs, collapse = "\n"), "\n\n", sep = "")
    return(invisible(TRUE))
  }
  esc <- .montage_report_html_escape
  ref_html <- if (length(refs)) {
    paste0("<ol class=\"nm-methods-refs\">", paste0(vapply(m$references, function(r) {
      text <- esc(.montage_report_reference_text(r))
      doi <- as.character(r$doi %||% NA)
      url <- as.character(r$url %||% NA)
      if (!is.na(doi) && nzchar(doi)) {
        text <- sub(paste0("doi:", esc(doi)),
                    paste0("<a href=\"https://doi.org/", esc(doi), "\">doi:", esc(doi), "</a>"),
                    text, fixed = TRUE)
      } else if (!is.na(url) && nzchar(url)) {
        text <- sub(esc(url), paste0("<a href=\"", esc(url), "\">", esc(url), "</a>"),
                    text, fixed = TRUE)
      }
      paste0("<li>", text, "</li>")
    }, ""), collapse = ""), "</ol>")
  } else {
    ""
  }
  cat(
    "<div class=\"nm-methods\" id=\"nm-methods\">",
    "<div class=\"nm-methods-head\"><span class=\"nm-methods-note\">Generated from the report's settings; ",
    "edit freely.</span><button type=\"button\" class=\"nm-fig-action nm-methods-copy\" ",
    "data-nm-copy-target=\"nm-methods-copy\">Copy methods</button></div>",
    "<div data-nm-copy-text id=\"nm-methods-copy\"><p class=\"nm-methods-text\">", esc(m$text), "</p>",
    ref_html, "</div></div>\n\n",
    sep = ""
  )
  invisible(TRUE)
}
