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
#'
#' @return A named list of formatter and emitter functions.
#' @export
montage_report_formatters <- function(manifest = NULL,
                                      layout = character(),
                                      is_html = FALSE) {
  layout_state <- new.env(parent = emptyenv())
  layout <- layout %||% character()
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
    emit_report_overview = function() {
      .montage_report_emit_report_overview(
        manifest = manifest,
        layout = layout,
        is_html = isTRUE(is_html)
      )
    },
    emit_panel_image = function(path, alt) {
      .montage_report_emit_panel_image(path, alt, is_html = isTRUE(is_html))
    },
    emit_table = function(tbl) {
      .montage_report_emit_table(tbl, is_html = isTRUE(is_html))
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
    emit_panel_heading = function(i, label) {
      .montage_report_emit_panel_heading(
        i = i,
        label = label,
        manifest = manifest,
        layout = layout,
        layout_state = layout_state,
        is_html = isTRUE(is_html)
      )
    },
    emit_layout_headings = function(i) {
      .montage_report_emit_layout_headings(
        i = i,
        manifest = manifest,
        layout = layout,
        layout_state = layout_state
      )
    }
  )
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
  cat(
    "<style>\n",
    ".nm-report-overview{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:10px;margin:0.75rem 0 1.6rem;}\n",
    ".nm-overview-card{border:1px solid #d7dee8;border-left:4px solid #2f6f73;border-radius:6px;background:#f8fafc;padding:0.7rem 0.85rem;}\n",
    ".nm-overview-label{display:block;color:#5b6876;font-size:0.72rem;font-weight:700;letter-spacing:0.04em;text-transform:uppercase;}\n",
    ".nm-overview-value{display:block;margin-top:0.18rem;color:#1f2d3d;font-size:1.12rem;font-weight:700;}\n",
    ".nm-qc-section{margin:1.35rem 0 2rem;}\n",
    ".nm-qc-section h2{margin-bottom:0.65rem;}\n",
    ".nm-table{width:100%;border-collapse:collapse;margin:0.35rem 0 1.25rem;font-size:0.95rem;}\n",
    ".nm-table th{border-bottom:2px solid #d7dee8;color:#2b3d4f;font-weight:700;padding:0.45rem 0.55rem;text-align:left;}\n",
    ".nm-table td{border-bottom:1px solid #e7ebf0;padding:0.45rem 0.55rem;vertical-align:top;}\n",
    ".nm-table td:nth-child(n+2):not(:last-child),.nm-table th:nth-child(n+2):not(:last-child){text-align:right;}\n",
    ".nm-status{display:inline-block;border-radius:999px;font-size:0.78rem;font-weight:700;padding:0.16rem 0.55rem;white-space:nowrap;}\n",
    ".nm-status-ok{background:#e7f4ed;color:#17633a;}\n",
    ".nm-status-warning{background:#fff1dc;color:#884b08;}\n",
    ".nm-status-muted{background:#edf0f4;color:#5f6f80;}\n",
    ".nm-panel-heading{margin:1.8rem 0 0.5rem;}\n",
    ".nm-layout-path{color:#68798a;font-size:0.78rem;font-weight:700;letter-spacing:0.04em;text-transform:uppercase;}\n",
    ".nm-panel-title{margin:0.18rem 0 0.25rem;color:#243447;}\n",
    ".nm-panel-meta{color:#526272;font-size:0.95rem;font-style:normal;margin:0.25rem 0 0.75rem;}\n",
    ".nm-caution{border-left:4px solid #d97706;background:#fff7ed;border-radius:4px;color:#743a04;margin:0.75rem 0 1rem;padding:0.65rem 0.8rem;}\n",
    ".nm-caution strong{margin-right:0.4rem;}\n",
    ".nm-peak-table{font-size:0.92rem;}\n",
    "@media print{.nm-report-overview{display:block}.nm-overview-card{margin-bottom:0.5rem}}\n",
    "</style>\n\n",
    sep = ""
  )
  invisible(NULL)
}

.montage_report_emit_report_overview <- function(manifest, layout, is_html) {
  map_count <- if (is.data.frame(manifest)) nrow(manifest) else 0L
  layout_value <- if (length(layout) > 0L) {
    paste(layout, collapse = " / ")
  } else {
    "flat"
  }

  if (isTRUE(is_html)) {
    cat(
      "<div class=\"nm-report-overview\">",
      "<div class=\"nm-overview-card\"><span class=\"nm-overview-label\">Maps</span>",
      "<span class=\"nm-overview-value\">", map_count, "</span></div>",
      "<div class=\"nm-overview-card\"><span class=\"nm-overview-label\">Layout</span>",
      "<span class=\"nm-overview-value\">",
      .montage_report_html_escape(layout_value),
      "</span></div></div>\n\n",
      sep = ""
    )
  } else {
    cat(
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

.montage_report_format_peak_table <- function(tbl) {
  if (!is.data.frame(tbl) || nrow(tbl) == 0L) {
    return(tbl)
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
  .montage_report_drop_empty_cols(out)
}

.montage_report_emit_panel_image <- function(path, alt, is_html) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }
  if (!file.exists(path)) {
    cat("*Image not found:* `", path, "`\n\n", sep = "")
    return(invisible(NULL))
  }
  if (isTRUE(is_html)) {
    cat(
      "<img src=\"", knitr::image_uri(path), "\" alt=\"",
      .montage_report_html_escape(alt),
      "\" style=\"max-width:100%;height:auto;\" />\n\n",
      sep = ""
    )
  } else {
    cat("![", alt, "](", path, ")\n\n", sep = "")
  }
  invisible(NULL)
}

.montage_report_emit_table <- function(tbl, is_html) {
  if (is.null(tbl)) {
    return(invisible(NULL))
  }
  cat("\n")
  if (is.data.frame(tbl)) {
    formatted <- .montage_report_format_peak_table(tbl)
    table_format <- if (isTRUE(is_html)) "html" else "pipe"
    table_attr <- if (isTRUE(is_html)) {
      "class=\"nm-table nm-peak-table\""
    } else {
      NULL
    }
    cat(
      knitr::kable(
        formatted,
        format = table_format,
        table.attr = table_attr,
        row.names = FALSE
      ),
      sep = "\n"
    )
  } else {
    print(tbl)
  }
  cat("\n\n")
  invisible(NULL)
}

.montage_report_emit_metadata <- function(row, is_html) {
  stat <- .montage_report_scalar(row, "stat_kind")
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
  if (!(length(stat) == 1L && is.na(stat))) {
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
  }
  if (!(length(connectivity) == 1L && is.na(connectivity))) {
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
  if (length(parts) == 0L) {
    return(invisible(NULL))
  }

  if (isTRUE(is_html)) {
    cat(
      "<p class=\"nm-panel-meta\">",
      paste(.montage_report_html_escape(parts), collapse = " / "),
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
    cat("\n\n<section class=\"nm-qc-section\">\n<h2>Effective N / QC</h2>\n")
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
    cat("\n</section>\n\n")
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

.montage_report_emit_panel_heading <- function(i,
                                               label,
                                               manifest,
                                               layout,
                                               layout_state,
                                               is_html) {
  if (isTRUE(is_html)) {
    row <- manifest[i, , drop = FALSE]
    layout_values <- .montage_report_layout_values(row, layout)
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
    layout_state = layout_state
  )
  panel_hashes <- paste(rep("#", min(length(layout) + 2L, 6L)), collapse = "")
  cat("\n\n", panel_hashes, " ", label, "\n\n", sep = "")
  invisible(NULL)
}

.montage_report_emit_layout_headings <- function(i,
                                                 manifest,
                                                 layout,
                                                 layout_state) {
  if (length(layout) == 0L || !is.data.frame(manifest) || nrow(manifest) == 0L) {
    return(invisible(NULL))
  }
  changed <- FALSE
  for (depth in seq_along(layout)) {
    field <- layout[[depth]]
    if (!field %in% names(manifest)) {
      next
    }
    value <- as.character(manifest[[field]][[i]])
    if (is.na(value) || !nzchar(value)) {
      next
    }
    previous <- layout_state[[field]]
    if (changed || is.null(previous) || !identical(previous, value)) {
      changed <- TRUE
      hashes <- paste(rep("#", min(depth + 1L, 6L)), collapse = "")
      cat("\n\n", hashes, " ", value, "\n\n", sep = "")
    }
    layout_state[[field]] <- value
  }
  invisible(NULL)
}
