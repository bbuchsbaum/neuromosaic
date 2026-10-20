# Smaller portable report exports

Status: implemented and locally verified for the quality-preserving scope.
Source reviewed: neuromosaic `6b4407a`, 2026-10-20.

## Outcome and boundaries

Make smaller reports from a headless HPC through neuromosaic's R API and CLI.
Support both a self-contained HTML file and a complete static-site directory
that can be uploaded to here.now or another static host.

Keep this work inside neuromosaic. Reuse the installed neuroimjs and neurosurf /
surfview runtimes without changing their source repositories. No publishing is
part of this implementation. Preserve existing defaults and scientific results.
The priority is removing redundant bytes while preserving decoded data, figure
pixels, all light/dark and responsive variants, print/download quality, and
interactive behavior. Prefer exact deduplication and lossless packaging.
Lower-resolution figures, fewer figure variants, additional palette
quantization, approximate voxel storage, resampling, mesh decimation, CDN
dependencies, and upstream custom runtime builds are outside this plan.

The useful measures are HTML bytes, total files required for viewing, initial
browser transfer, and decoded browser memory. Report them separately: moving a
file outside HTML or delaying its download does not eliminate its bytes.

## Existing foundation

- `R/montage_render.R` selects non-self-contained HTML when volume, surface,
  or Space assets require a bundle. Static-only reports lack a direct switch.
- `R/montage_report_format.R::.montage_report_image_src()` already stores bundle
  images by content hash. Download links already avoid repeating inline images.
- Volume and surface data already use gzip and content-addressed deduplication.
  Reuse those implementations.
- `R/montage_interactive_report.R` loads the external neuroimjs runtime on first
  use in bundle mode; its small adapters and styles are still inline.
- `R/montage_surface_interactive.R` obtains the surfview runtime from neurosurf,
  verifies its hash, and attaches it once for loading on demand. Keep this
  dependency mechanism and integrity check.
- Static montage figures can include light/dark and wide/narrow variants.
  Thumbnail quantization is already optional; colour-key figures stay full colour.
- Report figure specs and surface strips have their own sizing paths. Merely
  changing the outer PNG device arguments will not consistently resize them.
- VolumeScene currently requires float64 overlays. Float32 storage requires a
  deliberate local contract change and proof that the pinned browser reader
  supports it, even when the numeric values are exactly representable.

## 1. Establish size accounting and fixed specimens

Extend the existing asset benchmarks with complete report measurements. Reuse
the small synthetic volume/surface fixtures and add a bounded many-map specimen.
Cover static-only, volume-interactive, surface-interactive, and combined reports.
Include duplicate maps/shared geometry and both dense and sparse data.
Include repeated figures and identical dependencies referenced by several
panels. Measure what is already deduplicated so existing savings are not
presented as new improvements.

Add an opt-in asset manifest and a proposed `report_size(path)` diagnostic for
neuromosaic outputs. Record relative path or embedded asset identity, category,
content hash, stored bytes, and data's decoded bytes where known. Separate figures,
voxel data, surface data, runtimes, adapters/styles, and remaining HTML.

Reconcile the exact HTML-plus-required-files total against the filesystem.
Distinguish unique payload bytes from repeated embedded bytes; don't count an
embedded payload twice in the total. Older outputs without a manifest may return
coarser diagnostics explicitly labelled as such.

The manifest must include the complete dependency closure, including htmltools
attachments and Space chunks/workers. Exclude source maps, caches, source data,
QMD build sidecars, and obsolete files unless needed to view that particular
output. Do not delete unrelated files to achieve a smaller measured total.

Acceptance: category totals reconcile; a duplicate map does not duplicate its
already-deduplicated data; a fresh export folder contains precisely the declared
viewing files. Save baseline measurements before optimization and set size
regression budgets from those specimens rather than guessing a saving percentage.

## 2. Expose report-wide packaging

Proposed API: append `assets = c("auto", "embed", "bundle")` to
`render_montage_report()` and `render_cluster_report()`, and propagate it through
their public callers and CLI (`--assets`).

- `auto` preserves existing behavior and existing viewer-specific choices.
- An explicit `bundle` or `embed` is authoritative for the report and enabled
  viewers, without changing their scientific display settings.
- `bundle` works for static-only reports and uses relative companion references.
- `embed` produces one complete HTML file for configurations that support it.
  Reject incompatible features, such as bundle-only Space, before expensive
  rendering rather than silently emitting a partially embedded report.

Carry this policy through QMD generation and later HTML rendering. Keep PDF
static; reject explicitly HTML-only packaging requests for PDF with a clear
message. Resolve custom-template behavior explicitly and preserve template
styling instead of replacing its entire output configuration.

Reuse the existing figure copying, dependency attachment, and lazy-load paths.
Make the manifest usable to stage a clean upload directory from an existing
report; copy the declared files and retain their relative paths. The documented
workflow should render `index.html` directly into a dedicated directory.

Acceptance: move the complete output directory to a new location and serve it
with source inputs unavailable. All figures, viewer data, runtime chunks, and
downloads resolve locally. Embedded fixtures work from a local file. Existing
calls retain their behavior.

## 3. Eliminate exact duplicate assets and finish runtime packaging

Use the measured duplicate inventory to extend the existing content-addressed
stores across report assembly. Prioritize repeated figures, backgrounds, masks,
surface geometry, data arrays, runtimes, adapters, and styles. Share physical
bytes while retaining each map's identity, labels, units, scientific metadata,
and display state.

Deduplicate only assets with identical content and compatible interpretation.
Include required encoding/type information in identity checks, verify an
existing file before reusing it, and retain the current integrity checks.
Different library versions or geometry/affine metadata must not be treated as
interchangeable merely because names or numeric arrays match.

For embedded outputs, reuse existing shared binary payload declarations where
possible. Do not introduce a JavaScript-only static-image loader to save bytes:
no-JavaScript viewing, printing, and downloads remain acceptance requirements.
Where exact sharing cannot preserve those behaviors simply, apply that
optimization to bundle output only.

For bundle exports, externalize neuromosaic's own viewer adapters and styles
where useful, recording them in the same dependency manifest. Retain one copy of
each required runtime per document and its current lazy initialization.
Static-only reports must not acquire volume/surface libraries.

Keep the pinned upstream runtime bytes and hashes. Audit actual generated
reports for duplicate dependencies before adding deduplication machinery.
Treat Space's existing module graph as one supported distribution: copy only its
declared runtime files, but do not merge incompatible neuroimjs builds.

Measure local adapter minification as a bounded build-time optimization. Adopt
it only if it saves meaningful bytes and passes behavior checks. Keep readable
sources and deterministic generated assets. R users on the HPC must not need
Node, a CDN, or a JavaScript build step.

Acceptance: identical assets occupy one physical file or shared embedded payload
where supported, while maps remain independently selectable with correct
metadata. Decoded figures and scientific values match baseline. Request tracing
shows no unused viewer runtime on initial load and
one successful load on first use; switching maps and reopening viewers still
works. Missing-runtime failures retain the static figures. Hash verification
continues to reject mismatched runtime files.

## 4. Evaluate lossless asset compression

Keep PNG dimensions, full colour, transparency, colour interpretation, and every
existing variant. Evaluate lossless PNG recompression on fixed specimens,
retaining the candidate only if it is smaller and decoded pixels are identical.
Preserve colour-profile/gamma and resolution metadata that affect display or
printing. Recompute content identities after optimization so deduplication and
integrity checks refer to the delivered bytes.

Keep gzip for volume and surface payloads; first check whether there is any
remaining lossless compression opportunity beyond the existing settings.
Avoid adding a required HPC executable or heavyweight dependency for a marginal
saving. Optional optimizers must have a transparent fallback to the original.
Retain PNG downloads; a format migration is unnecessary for this phase.

Acceptance: actual upload bytes decrease on the selected specimens with exact
decoded pixels/data and unchanged dimensions and colour behavior. Verify dark
mode, mobile layouts, printing, captioned/plain PNG downloads, and no-JavaScript
fallback. Report specimens where recompression brings no benefit and retain
their original files.

## 5. Evaluate exact compact volume storage

This is a deferred, optional phase after deduplication and lossless packaging.
It is not required to complete the primary work. Include it only if measurements
show a worthwhile additional saving and compatibility can be proved with the
existing pinned neuroimjs reader. The simpler packaging changes take priority
over a new scientific storage contract.

Proposed opt-in `montage_interactive(storage = "auto_lossless")`, with the current
float64 overlay behavior retained as the default. Detect float32-exact values
through an actual round trip, serialize, and verify the resulting asset before
acceptance. Otherwise keep float64. Define and test missing/nonfinite values,
signed zero, overflow/underflow, masks, and scaling explicitly.

Update the local scene validator and documented/versioned contract as needed.
Record selected storage and transformations in provenance. Compare candidate
gzip sizes as well as decoded size; smaller element width alone does not promise
a smaller compressed upload.

Acceptance: exact finite values and defined missing-value semantics survive
R serialization and browser decoding; affine/geometry and display thresholds
remain identical. Include true double-precision adverse examples that must
fall back. If the pinned runtime cannot support the candidate format, defer
this phase and report the upstream boundary.

## 6. Integrate, document, and qualify

Update R documentation, CLI parsing/help/dry-run descriptions, README, and the
montage vignette. Describe portable folder upload and single-file output,
measured size differences, and the preserved image/data quality. Avoid coupling
export code to any hosting provider or adding credentials/publishing behavior.

Extend existing R tests around render packaging, figure assets, QMD portability,
runtime attachments, CLI forwarding, and volume storage. Add browser coverage
to the existing `e2e/` suite for relocated bundles, blocked external network,
lazy runtime requests, print/download behavior, and embedded local-file viewing.
Optional dependency cases must be reported explicitly, including availability
of `magick`, neurosurf, and Quarto.

Commands already supported by this repository:

```sh
Rscript -e 'devtools::document()'
RGL_USE_NULL=TRUE Rscript -e 'devtools::test()'
npm run test:js
node ~/.local/share/agent-policy/browser-automation-guard.mjs --audit
npm run test:e2e -- e2e/interactive-volume.spec.js e2e/interactive-surface.spec.js e2e/map-selector.spec.js e2e/parcel-visual.spec.js e2e/space-report.spec.js
node ~/.local/share/agent-policy/browser-automation-guard.mjs --audit
RGL_USE_NULL=TRUE Rscript -e 'rcmdcheck::rcmdcheck()'
```

Run focused checks during development and the full R suite before completion.
Run `rcmdcheck` before a PR or release. Browser tests use the project's Playwright
Chromium and must close their owned processes. Run the relevant new export tests
alongside existing specimens; require real browser evidence for loading and
rendering claims. Document environmental or pre-existing failures separately.

Final evidence: a before/after table of HTML bytes, required upload bytes, and
initial browser requests; numerical equality checks for any storage change;
exact decoded-pixel checks for recompressed figures plus screenshots/print/
download checks; relocated-folder and embedded-file success. Retain all
baseline figure variants and resolutions. Do not promise a universal
compression ratio.

## Delivery order and next action

Deliver in reviewable increments: (1) baseline/accounting, (2) exact
deduplication, report packaging, and runtime assets, (3) worthwhile lossless
compression. Consider exact compact storage separately after those are complete.
Complete documentation and relevant checks within each increment.

The unrelated untracked `tests/testthat/helper-round2-offline.R` was preserved.

## Implementation and acceptance receipts

Implemented report-wide `assets` for montage and cluster renderers, their public
callers and CLI, with explicit QMD HTML policies. Built-in HTML/QMD outputs carry
a small dynamic-dependency inventory. Explicit copies of the shipped templates
are recognized by their content hash. Custom/legacy outputs receive a partial
inventory rather than an unsupported completeness claim.

`report_size()` reconciles HTML and companion bytes, reports categories and
duplicate payloads, and handles large embedded assets and relative CSS resources.
`write_report_bundle()` stages only declared/referenced assets into a new or
empty directory, checking copied hashes and rejecting escaping paths or remote
dependencies. Figure copies use the existing short content-addressed filenames
with full SHA-256 validation before reuse. Volume adapters/styles now reference
their existing companion files instead of also being inlined. Duplicate volume
scene/runtime build metadata are excluded from staged viewing bundles.

All PNG bytes, variants, resolution, numerical storage and runtime pins remain
unchanged. Lossless PNG recompression of the fixed specimen saved only 2,688
bytes (about 0.15%), so no optimizer or compression dependency was added. Compact
numeric storage remains deferred. `xml2` is optional and used only by the size
diagnostic and staging helper; normal report generation does not require it.

The fixed eight-map benchmark retained four byte-identical PNG variants:

| Output | HTML bytes | Total required bytes | Companion files |
|---|---:|---:|---:|
| Baseline default embedded | 19,839,488 | 19,839,488 | 0 |
| Explicit embedded | 19,839,674 | 19,839,674 | 0 |
| Bundle | 132,872 | 1,981,049 | 5 |

The small embedded increase is dependency-inventory metadata. The approximately
90% total saving in this deliberately repetitive specimen comes from storing
each figure once and avoiding base64 expansion; it is not a general compression
ratio. A second relocated bundle contains only the viewing files. Before/after
PNG equality is recorded in `figure-equality.json` below.

Local verification receipts are under
`/private/tmp/neuromosaic-report-exports-20261020/`:

- `full-r-02.log`: 3,716 assertions passed, no failures, 128 font/cluster-related
  warnings, and one missing-specimen thumbnail skip. Final affected checks are
  additionally covered by `export-final-tests.log` (328 assertions, no warnings)
  and `qmd-policy-tests.log` (44 assertions, no warnings).
- `js-01.log`: 76 JavaScript tests passed.
- `browser-authorized.log`: 49 of 64 browser cases passed, including all five
  new export cases; 15 Space cases could not navigate because the repository
  configuration lacked a base URL. After adding that URL,
  `browser-space-final.log` passed all 16 Space cases (one overlaps the first
  run). All 64 distinct selected cases therefore passed across the runs.
- Browser evidence covers local-file embedded output, relocated static/volume/
  surface bundles, blocked external requests, runtime loading on demand, print
  preparation, both PNG downloads, responsive/dark variants, no-JavaScript
  fallback, existing parcel visual baselines, and Space/Quarto integration.
- `quarto-policy-final.log`: actual Quarto renders from relocated sources passed
  for both explicit modes. Embedded output required zero companion files;
  bundled output used 16 companions. This integration check caught and fixed
  YAML 1.2 boolean serialization; regression assertions require true/false.
- `package-check-final.log`: no errors with tests/manual/vignette rebuilding
  omitted; tests were run separately. Two warnings concern missing built
  vignettes, and one note concerns existing long Space runtime paths. The
  initial missing optional flextable dependency was installed only into a
  temporary check library, leaving the user's R library unchanged.
- `final-benchmark.log`, `final-benchmark/sizes.csv`, and
  `figure-equality.json`: complete size and exact figure-byte witnesses.

Quarto rendering needed authorized system access because sandboxed architecture
queries failed. The browser run resumed from its freshly rendered R fixtures
and retained the required Quarto build step. The project's installed Playwright
Chromium was the only browser backend; the final ownership audit found no
automated top-level browser processes. No reports were published during
implementation verification.
