space_report_inputs <- function() {
  toy <- make_toy_cluster_report_inputs()
  volume <- function(x) neuroim2::NeuroVol(array(x, dim(toy$stat_map)), neuroim2::space(toy$stat_map))
  n <- length(toy$stat_map)
  manifest <- data.frame(map_id = c("s1_t", "s1_beta", "s1_se", "s2_t", "s2_beta", "s2_se"),
    analysis_id = rep(c("a", "b"), each = 3), role = rep(c("primary", "auxiliary", "auxiliary"), 2),
    quantity = rep(c("test_statistic", "estimate", "standard_error"), 2),
    distribution = rep(c("t", NA, NA), 2), df = rep(c(30, NA, NA), 2),
    threshold = rep(c(2, NA, NA), 2), label = rep(c("T", "Estimate", "SE"), 2))
  manifest$stat_map <- I(list(volume(seq_len(n) / n), volume(c(0, rep(.1, n - 1))),
    volume(rep(.2, n)), volume(rev(seq_len(n)) / n), volume(c(0, rep(.3, n - 1))), volume(rep(.4, n))))
  members <- data.frame(map_id = manifest$map_id, member_id = rep(c("sub-01", "sub-02"), each = 3))
  set <- montage_map_set("subjects", members, volume(rep(1, n)), member_kind = "subject")
  list(manifest = manifest, set = set, bg = toy$stat_map)
}

test_that("Space packages raw comparison values and exact reverse bindings", {
  x <- space_report_inputs()
  # Primary threshold excludes every t value. Comparison estimates still retain
  # their finite subthreshold values and shared zeros.
  out <- tempfile("space-package-"); dir.create(out)
  report <- .prepare_montage_space_report(x$manifest, montage_space(x$set), x$bg,
    file.path(out, "report.html"))
  set <- report$scene$sets[[1]]
  expect_identical(set$initial_state$statistic, "t")
  expect_identical(set$initial_state$threshold, 0)
  expect_identical(set$common_channels, c("t", "estimate"))
  expect_identical(vapply(set$bindings, `[[`, "", "map_id"), x$manifest$map_id)
  beta <- neuroim2::read_vol(file.path(out, set$members[[1]]$beta$location$ref))
  expect_equal(as.numeric(beta), c(0, rep(.1, length(beta) - 1)))
  expect_identical(set$mask$datatype, "uint8")
  expect_true(all(file.exists(report$bundle_files)))
  expect_false(any(c("stat_map", "path", "recipe") %in% names(set)))
  portable <- .montage_qmd_portable_report_data(
    list(manifest = x$manifest, panels = list(), space = report), file.path(out, "report.qmd"))
  expect_false(any(startsWith(portable$space$bundle_files, "/")))
  expect_false("stat_map" %in% names(portable$manifest))
  html <- paste(capture.output(montage_space_report_hooks(report, TRUE)$emit_host()), collapse = "\n")
  expect_match(html, "Open comparison", fixed = TRUE)
  expect_match(html, "sub-01 / t / s1_t", fixed = TRUE)
  expect_match(html, '<details class="nm-space-members" open>', fixed = TRUE)
  expect_match(html, paste0('href="#', .montage_report_dom_id("nm-variant", "a", "s1_beta"), '"'), fixed = TRUE)
})

test_that("Space validates capabilities, budget and initial channel before writing assets", {
  x <- space_report_inputs(); out <- tempfile("not-created-")
  prepare <- function(config) .prepare_montage_space_report(x$manifest, config, x$bg,
    file.path(out, "report.html"))
  expect_error(prepare(montage_space(x$set, max_data_mb = .0001)), "max_data_mb")
  expect_false(dir.exists(out))
  generic <- x$set; generic$member_kind <- "map"
  expect_error(prepare(montage_space(generic)), "generic map labels")
  expect_error(prepare(montage_space(x$set, statistic = "z")), "not common")
  x$manifest$distribution[x$manifest$quantity == "test_statistic"] <- "z"
  expect_error(prepare(montage_space(x$set)), "z statistics")
  expect_false(dir.exists(out))
})

test_that("Space hooks escape labels and provide a static member list", {
  x <- space_report_inputs(); x$set$label <- '<script>alert("x")</script>'
  report <- .prepare_montage_space_report(x$manifest, montage_space(x$set), x$bg,
    tempfile(fileext = ".pdf"), is_html = FALSE)
  html <- paste(capture.output(montage_space_report_hooks(report, TRUE)$emit_host()), collapse = "\n")
  expect_false(grepl('<script>alert', html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  static <- paste(capture.output(montage_space_report_hooks(report, FALSE)$emit_host()), collapse = "\n")
  expect_match(static, "sub-02 / estimate / s2_beta", fixed = TRUE)
  expect_match(static, paste0("](#", .montage_report_map_anchor("b", "s2_beta"), ")"), fixed = TRUE)
  expect_false(grepl("iframe", static, fixed = TRUE))
  expect_length(report$bundle_files, 0L)
})

test_that("loaded public validation enforces the payload cap including its boundary", {
  x <- space_report_inputs()
  loaded <- validate_montage_map_sets(x$manifest, x$set, x$bg, load_maps = TRUE)
  bytes <- loaded$sets$subjects$payload_bytes
  exact <- montage_space(x$set, max_data_mb = bytes / 1024^2)
  expect_true(validate_montage_map_sets(x$manifest, exact, x$bg, TRUE)$checks$payload_limit)
  too_small <- montage_space(x$set, max_data_mb = (bytes - 1) / 1024^2)
  expect_error(validate_montage_map_sets(x$manifest, too_small, x$bg, TRUE), "max_data_mb")
  expect_false(validate_montage_map_sets(x$manifest, too_small)$checks$payload_limit)
})

test_that("provenance distinguishes source values and inherited analysis support", {
  x <- space_report_inputs()
  mask <- x$set$mask; mask[] <- 0; mask[1] <- 1
  x$manifest$mask <- I(c(list(mask), rep(list(NULL), nrow(x$manifest) - 1L)))
  prepare <- function() .prepare_montage_space_report(x$manifest, montage_space(x$set), x$bg,
    file.path(tempdir(), "space-provenance", "report.html"))
  first <- prepare(); a <- first$scene$sets[[1]]
  expect_identical(a$bindings[[2]]$provenance$analysis_mask$map_id, "s1_t")
  expect_true(a$bindings[[2]]$provenance$analysis_mask$inherited)
  expect_match(a$comparison_mask$hash, "^[a-f0-9]{64}$")
  expect_length(first$scene$provenance$adapter$files, 2L)
  x$manifest$stat_map[[1]][2] <- 999 # outside the primary/inherited support
  b <- prepare()$scene$sets[[1]]
  expect_false(identical(a$bindings[[1]]$provenance$source$hash, b$bindings[[1]]$provenance$source$hash))
  expect_identical(a$members[[1]]$t$hash, b$members[[1]]$t$hash)
  expect_identical(a$bindings[[2]]$provenance$analysis_mask,
                   b$bindings[[2]]$provenance$analysis_mask)
})
