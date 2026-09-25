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
                                      panels = NULL) {
  layout_state <- new.env(parent = emptyenv())
  # Section-note change detection for the HTML path is tracked separately from
  # `layout_state` (which drives markdown/PDF headings) so the two never share
  # state; only one branch runs per render, but keeping them distinct avoids any
  # cross-talk if a custom template calls both emitters.
  section_state <- new.env(parent = emptyenv())
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
    emit_report_header = function(title, date = NULL) {
      .montage_report_emit_report_header(
        title = title, date = date, manifest = manifest, layout = layout,
        groups = groups, is_html = isTRUE(is_html)
      )
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
    emit_panel_image = function(path, alt, empty = FALSE) {
      .montage_report_emit_panel_image(path, alt, is_html = isTRUE(is_html),
                                       empty = empty)
    },
    emit_table = function(tbl, row = NULL) {
      .montage_report_emit_table(tbl, is_html = isTRUE(is_html), row = row)
    },
    emit_metadata = function(row) {
      .montage_report_emit_metadata(row, is_html = isTRUE(is_html))
    },
    emit_qc_summary = function(qc_tbl) {
      .montage_report_emit_qc_summary(qc_tbl, is_html = isTRUE(is_html))
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

  out <- data.frame(
    Cluster = tbl$cluster_id,
    Sign = tbl$sign,
    `N Voxels` = tbl$n_voxels,
    `X (MNI)` = round(tbl$peak_mni_x, 1),
    `Y (MNI)` = round(tbl$peak_mni_y, 1),
    `Z (MNI)` = round(tbl$peak_mni_z, 1),
    `Peak Stat` = round(tbl$max_stat, 2),
    Region = tbl$atlas_label,
    Hemisphere = tbl$hemisphere,
    Network = tbl$network,
    check.names = FALSE
  )
  out <- out[order(-abs(out$`Peak Stat`)), , drop = FALSE]
  .montage_report_drop_empty_cols(out)
}

.montage_report_emit_panel_image <- function(path, alt, is_html, empty = FALSE) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }
  if (!file.exists(path)) {
    cat("*Image not found:* `", path, "`\n\n", sep = "")
    return(invisible(NULL))
  }
  if (isTRUE(is_html)) {
    cat(
      "<figure class=\"nm-figure", if (isTRUE(empty)) " nm-figure-empty" else "",
      "\"><img src=\"", knitr::image_uri(path),
      "\" alt=\"", .montage_report_html_escape(alt),
      "\" loading=\"lazy\" /></figure>\n\n",
      sep = ""
    )
  } else {
    cat("![", alt, "](", path, ")\n\n", sep = "")
  }
  invisible(NULL)
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
    if (isTRUE(is_html)) {
      caption <- if (inherits(tbl, "montage_parcel_table")) {
        n_all <- n_total %||% nrow(tbl)
        paste0(n_all, if (n_all == 1L) " surviving parcel" else " surviving parcels",
               ", strongest first")
      }
      cat(.montage_report_html_table(formatted, class = "nm-table nm-peak-table",
                                     caption = caption),
          "\n", sep = "")
      if (!is.null(n_total) && n_total > nrow(tbl)) {
        cat("<p class=\"nm-table-note\">Strongest ", nrow(tbl), " of ",
            n_total, " surviving parcels.</p>\n", sep = "")
      }
    } else {
      cat(knitr::kable(formatted, format = "pipe", row.names = FALSE),
          sep = "\n")
    }
  } else {
    print(tbl)
  }
  cat("\n\n")
  invisible(NULL)
}

# Minimal accessible HTML table: numeric columns are right-aligned via a
# class rather than by column position.
.montage_report_html_table <- function(df, class = "nm-table", caption = NULL) {
  num <- vapply(df, is.numeric, logical(1))
  esc <- .montage_report_html_escape
  # One decimal count per column so values align on the point.
  digits <- vapply(df, function(col) {
    if (!is.numeric(col)) return(0L)
    col <- col[is.finite(col)]
    if (!length(col) || all(col == round(col))) return(0L)
    if (all(col * 10 == round(col * 10))) 1L else 2L
  }, integer(1))
  cell <- function(value, is_num, k) {
    text <- if (is.na(value)) "" else if (is_num) {
      formatC(value, format = "f", digits = digits[[k]], big.mark = ",")
    } else {
      as.character(value)
    }
    if (is_num) text <- sub("^-", "\u2212", text)
    paste0("<td", if (is_num) " class=\"num\"" else "", ">", esc(text),
           "</td>")
  }
  head <- paste0(
    "<thead><tr>",
    paste0("<th scope=\"col\"", ifelse(num, " class=\"num\"", ""), ">",
           esc(names(df)), "</th>", collapse = ""),
    "</tr></thead>"
  )
  body <- vapply(seq_len(nrow(df)), function(r) {
    paste0("<tr>", paste0(vapply(seq_along(df), function(k) {
      cell(df[[k]][[r]], num[[k]], k)
    }, ""), collapse = ""), "</tr>")
  }, "")
  paste0("<div class=\"nm-table-wrap\"><table class=\"", class, "\">",
         if (!is.null(caption)) paste0("<caption>", esc(caption), "</caption>"),
         head, "<tbody>", paste(body, collapse = ""), "</tbody></table></div>")
}

.montage_report_emit_metadata <- function(row, is_html) {
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
      parts <- c(parts, paste0(toupper(distribution), "-statistic"))
    } else if (!(length(profile_label) == 1L && is.na(profile_label))) {
      parts <- c(parts, as.character(profile_label))
    } else {
      parts <- c(parts, .profile_title(as.character(quantity)))
    }
  } else if (!(length(stat) == 1L && is.na(stat))) {
    stat <- as.character(stat)
    parts <- c(parts, paste0(
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
    parts <- c(parts, value)
  } else if (identical(as.character(display_mode), "continuous") &&
             !(length(lower) == 1L && is.na(lower)) &&
             !(length(upper) == 1L && is.na(upper))) {
    parts <- c(parts, paste0(
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
    parts <- c(parts, value)
  }
  if (!(length(n) == 1L && is.na(n))) {
    parts <- c(parts, paste0("N = ", .montage_report_fmt_val(n)))
  }
  if (!(length(units) == 1L && is.na(units)) &&
      nzchar(as.character(units))) {
    parts <- c(parts, paste0("units: ", as.character(units)))
  }
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

.montage_report_emit_qc_summary <- function(qc_tbl, is_html) {
  if (!.montage_report_qc_has_values(qc_tbl)) {
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
    cat("\n\n<section class=\"nm-qc-section\">\n<h2>Effective N / QC</h2>\n",
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
      "<h2 class=\"nm-panel-title\">",
      .montage_report_html_escape(label),
      "</h2>\n</div>\n\n",
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
    n <- nrow(table)
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
