# Quality-preserving report packaging and on-demand size diagnostics.

.report_builtin_template <- function(template, stem) {
  name <- paste0(stem, ".", tools::file_ext(template))
  reference <- system.file("templates", name, package = "neuromosaic")
  if (!nzchar(reference) || !file.exists(reference)) return(FALSE)
  identical(digest::digest(file = template, algo = "sha256", serialize = FALSE),
            digest::digest(file = reference, algo = "sha256", serialize = FALSE))
}

.report_assets_mode <- function(assets, output_file, space = NULL) {
  assets <- match.arg(assets, c("auto", "embed", "bundle"))
  if (identical(tolower(tools::file_ext(output_file)), "pdf") && assets != "auto") {
    stop("'assets' packaging applies to HTML/QMD, not PDF.", call. = FALSE)
  }
  if (assets == "embed" && !is.null(space)) {
    stop("Space requires bundle assets; use assets = 'bundle' or 'auto'.",
         call. = FALSE)
  }
  assets
}

.report_copy_asset <- function(source, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  hash <- digest::digest(file = source, algo = "sha256", serialize = FALSE)
  name <- paste0(substr(hash, 1L, 16L), ".", tolower(tools::file_ext(source)))
  target <- file.path(directory, name)
  if (file.exists(target)) {
    if (!identical(digest::digest(file = target, algo = "sha256",
                                  serialize = FALSE), hash)) {
      stop("Existing report asset does not match its content hash: ", target,
           call. = FALSE)
    }
  } else if (!file.copy(source, target)) {
    stop("Could not copy report asset: ", source, call. = FALSE)
  }
  name
}

.report_relative <- function(path, root) {
  root <- paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, root)) {
    stop("Report asset is outside the report directory: ", path, call. = FALSE)
  }
  substring(path, nchar(root) + 1L)
}

.report_declared_files <- function(rd, output_file) {
  root <- dirname(output_file)
  # The scene and runtime identity are already in the HTML. These duplicate
  # build records are not fetched by the viewer and need not be uploaded.
  volume_files <- rd$interactive$bundle_files %||% character()
  volume_files <- volume_files[!basename(volume_files) %in% c("scene.json", "runtime.json")]
  files <- c(volume_files, rd$space$bundle_files)
  for (assets in list(rd$interactive$scene$assets, rd$surface$assets)) {
    for (asset in assets) {
      if (identical(asset$location$kind, "relative")) {
        files <- c(files, asset$location$ref)
      }
    }
  }
  unique(vapply(files, function(path) {
    if (!grepl("^(/|[A-Za-z]:)", path)) path <- file.path(root, path)
    .report_relative(path, root)
  }, character(1), USE.NAMES = FALSE))
}

.report_registry_html <- function(files, complete = TRUE) {
  json <- jsonlite::toJSON(list(version = 1L, complete = complete,
                               files = unname(as.list(files))), auto_unbox = TRUE)
  json <- gsub("<", "\\u003c", as.character(json), fixed = TRUE)
  paste0('<script type="application/json" id="nm-report-assets">',
         json, "</script>\n")
}

.report_append_registry <- function(output_file, files, complete = TRUE) {
  con <- file(output_file, open = "ab")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0("\n", .report_registry_html(files, complete))), con)
}

.report_qmd_packaging <- function(lines, assets, files, complete = TRUE) {
  if (assets != "auto") {
    # Parse and update only the HTML output option; retain custom template options.
    delimiters <- which(trimws(lines) == "---")
    if (length(delimiters) < 2L) stop("QMD template requires YAML front matter.",
                                    call. = FALSE)
    first <- delimiters[[1L]]; last <- delimiters[[2L]]
    header <- yaml::yaml.load(paste(lines[seq.int(first + 1L, last - 1L)],
                                   collapse = "\n"), eval.expr = FALSE)
    format <- header$format
    if (is.character(format)) format <- stats::setNames(list(list()), format)
    if (is.null(format)) format <- list()
    html <- format$html
    if (!is.list(html)) html <- list()
    html$`embed-resources` <- identical(assets, "embed")
    format$html <- html
    header$format <- format
    # yaml's default yes/no literals are strings under Quarto's YAML 1.2.
    yaml_header <- yaml::as.yaml(header, handlers = list(logical = function(x) {
      structure(ifelse(x, "true", "false"), class = "verbatim")
    }))
    lines <- c("---", strsplit(yaml_header, "\n", fixed = TRUE)[[1]],
               "---", lines[-seq_len(last)])
  }
  c(lines, "", "```{=html}", .report_registry_html(files, complete), "```")
}

.report_resource_refs <- function(doc) {
  nodes <- xml2::xml_find_all(doc, paste0(
    "//script[@src] | //img | //source | //link | //iframe[@src] | ",
    "//object[@data] | //video | //audio | //a[@download]"))
  attrs <- c("src", "href", "data", "poster", "srcset", "data-nm-src",
             "data-nm-srcset")
  refs <- unlist(lapply(nodes, function(node) {
    values <- xml2::xml_attrs(node)
    unlist(lapply(intersect(names(values), attrs), function(attr) {
      value <- unname(values[[attr]])
      if (attr %in% c("srcset", "data-nm-srcset")) {
        if (!startsWith(value, "data:")) value <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
        value <- sub("[[:space:]]+[0-9.]+[wx]$", "", value)
      }
      value
    }), use.names = FALSE)
  }), use.names = FALSE)
  refs[!is.na(refs) & nzchar(refs)]
}

.report_css_refs <- function(css) {
  hits <- regmatches(css, gregexpr("url\\([^)]*\\)", css, perl = TRUE))[[1]]
  if (!length(hits)) return(character())
  trimws(sub("\\)$", "", sub("^url\\(", "", hits)), whitespace = "[\\s'\"]")
}

.report_category <- function(path) {
  if (grepl("\\.html?$", path, ignore.case = TRUE)) return("html")
  if (grepl("\\.(png|jpe?g|webp|svg)$", path, ignore.case = TRUE)) return("figures")
  if (grepl("\\.nii(\\.gz)?$", path)) return("voxel_data")
  if (grepl("/surfaces/.*\\.(bin|gz)$", path)) return("surface_data")
  if (basename(path) %in% c("adapter.js", "display.js", "geometry.js", "space-host.js", "bridge.js")) {
    return("adapters_styles")
  }
  if (grepl("(neuroimjs|surfview|/runtime/).*\\.js$", path)) return("runtimes")
  if (grepl("\\.(js|css|woff2?|ttf|eot)$", path)) return("adapters_styles")
  "other"
}

#' Measure the Files Required to View a Report
#'
#' Measures actual HTML and companion-file bytes without counting source maps,
#' caches or unrelated files in the output directory. New built-in reports
#' declare dynamic viewer assets; older or custom reports have a partial inventory.
#' No resources are downloaded. Requires the optional package `xml2`.
#' @param path Path to a rendered HTML report.
#' @return A list with `files` (relative paths, categories, bytes, SHA-256 hashes),
#'   `html_bytes`, `companion_bytes`, `total_bytes`, `by_category`, `embedded`
#'   (non-additive base64 payload diagnostics), `duplicate_file_bytes`,
#'   `duplicate_embedded_bytes`, `complete`,
#'   and `remote_resources`. Embedded payload bytes are already part of HTML.
#'   Decoded binary bytes are not a browser-memory measurement.
#' @export
report_size <- function(path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required for report size diagnostics.", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = TRUE)
  root <- dirname(path)
  doc <- xml2::read_html(path, options = c("RECOVER", "NOERROR", "NOWARNING", "NONET", "HUGE"))
  registry_node <- xml2::xml_find_first(doc, "//script[@id='nm-report-assets']")
  registry <- if (!inherits(registry_node, "xml_missing")) {
    jsonlite::fromJSON(xml2::xml_text(registry_node), simplifyVector = FALSE)
  } else list()
  refs <- .report_resource_refs(doc)
  inline_css <- xml2::xml_text(xml2::xml_find_all(doc, "//style"))
  refs <- as.character(c(refs, unlist(lapply(inline_css, .report_css_refs), use.names = FALSE)))
  queue <- c(as.list(refs), unname(registry$files))
  bases <- rep(root, length(queue))
  found <- path; remote <- character()
  i <- 1L
  while (i <= length(queue)) {
    ref <- trimws(queue[[i]]); base <- bases[[i]]; i <- i + 1L
    if (!nzchar(ref) || startsWith(ref, "#") || startsWith(ref, "data:")) next
    if (grepl("^([A-Za-z][A-Za-z0-9+.-]*:|//)", ref)) {
      remote <- c(remote, ref); next
    }
    ref <- utils::URLdecode(sub("[?#].*$", "", ref))
    candidate <- file.path(base, ref)
    relative <- .report_relative(candidate, root)
    candidate <- file.path(root, relative)
    if (candidate %in% found) next
    if (dir.exists(candidate)) stop("Report asset is a directory: ", ref, call. = FALSE)
    found <- c(found, candidate)
    if (tolower(tools::file_ext(candidate)) == "css") {
      extra <- .report_css_refs(paste(readLines(candidate, warn = FALSE), collapse = "\n"))
      queue <- c(queue, as.list(extra)); bases <- c(bases, rep(dirname(candidate), length(extra)))
    }
  }
  relative <- vapply(found, .report_relative, character(1), root = root)
  files <- data.frame(path = unname(relative),
    category = vapply(relative, .report_category, character(1), USE.NAMES = FALSE),
    bytes = as.numeric(file.size(found)),
    sha256 = vapply(found, function(f) digest::digest(file = f, algo = "sha256",
                                                     serialize = FALSE), character(1), USE.NAMES = FALSE))
  # Inspect data URIs and embedded binary declarations separately from totals.
  payloads <- refs[startsWith(refs, "data:")]
  binaries <- xml2::xml_find_all(doc, "//script[@type='application/octet-stream']")
  payloads <- payloads[grepl(";base64,", payloads, fixed = TRUE)]
  categories <- c(ifelse(startsWith(payloads, "data:image/"), "figures", "other"),
    ifelse(startsWith(xml2::xml_attr(binaries, "id") %||% character(), "nm-volume-"),
           "voxel_data", "surface_data"))
  categories[is.na(categories)] <- "other"
  b64 <- c(sub("^[^,]*,", "", payloads),
           xml2::xml_text(binaries))
  embedded <- if (length(b64)) do.call(rbind, lapply(b64, function(value) {
    bytes <- jsonlite::base64_dec(value)
    data.frame(encoded_bytes = nchar(value, type = "bytes"), binary_bytes = length(bytes),
               sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE))
  })) else data.frame(encoded_bytes = numeric(), binary_bytes = numeric(), sha256 = character())
  embedded$category <- categories
  list(files = files, html_bytes = files$bytes[[1]],
       companion_bytes = sum(files$bytes[-1]), total_bytes = sum(files$bytes),
       by_category = stats::aggregate(files["bytes"], list(category = files$category), sum),
       embedded = embedded,
       duplicate_file_bytes = sum(files$bytes[duplicated(files$sha256)]),
       duplicate_embedded_bytes = sum(embedded$encoded_bytes[duplicated(embedded$sha256)]),
       complete = identical(registry$version, 1L) && isTRUE(registry$complete) && !length(remote),
       remote_resources = unique(remote))
}

#' Copy a Complete Report to a Clean Upload Directory
#' @param path Path to a rendered built-in neuromosaic HTML report.
#' @param directory New or empty destination directory. Existing files are never
#'   overwritten. The HTML filename and relative companion paths are preserved.
#' @return The copied HTML path, invisibly. Requires `xml2` via [report_size()].
#' @export
write_report_bundle <- function(path, directory) {
  size <- report_size(path)
  if (!isTRUE(size$complete)) {
    stop("Report inventory is incomplete or uses remote resources; re-render with a built-in template.",
         call. = FALSE)
  }
  if (dir.exists(directory) && length(list.files(directory, all.files = TRUE, no.. = TRUE))) {
    stop("Bundle destination must be new or empty.", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  root <- dirname(normalizePath(path, mustWork = TRUE))
  for (i in seq_len(nrow(size$files))) {
    relative <- size$files$path[[i]]
    source <- file.path(root, relative); target <- file.path(directory, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!identical(digest::digest(file = source, algo = "sha256", serialize = FALSE),
                   size$files$sha256[[i]]) || !file.copy(source, target, overwrite = FALSE) ||
        !identical(digest::digest(file = target, algo = "sha256", serialize = FALSE),
                   size$files$sha256[[i]])) {
      stop("Could not copy verified report asset: ", relative, call. = FALSE)
    }
  }
  invisible(normalizePath(file.path(directory, basename(path)), mustWork = TRUE))
}
