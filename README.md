# neuromosaic

`neuromosaic` generates atlas-annotated cluster reports from neuroimaging
statistical maps and provides an interactive Shiny explorer for drilling into
clusters, parcels, and design-linked signal plots.

## Installation

Install the package from GitHub:

```r
# install.packages("pak")
pak::pak("bbuchsbaum/neuromosaic")
```

## CLI Installation

The package ships with a CLI wrapper at `exec/neuromosaic`. After installing
the package, the easiest way to make `neuromosaic` available on your shell
`PATH` is to symlink that wrapper into a user bin directory.

```sh
mkdir -p ~/.local/bin
ln -sf "$(Rscript -e 'cat(system.file(\"exec\", \"neuromosaic\", package = \"neuromosaic\"))')" \
  ~/.local/bin/neuromosaic
chmod +x ~/.local/bin/neuromosaic
```

If `~/.local/bin` is not already on your `PATH`, add this to your shell startup
file such as `~/.zshrc` or `~/.bashrc`:

```sh
export PATH="$HOME/.local/bin:$PATH"
```

Then open a new shell and check:

```sh
neuromosaic --help
```

### Development Checkout

If you are running from a local checkout instead of an installed package, you
can link the repo script directly:

```sh
ln -sf "$PWD/exec/neuromosaic" ~/.local/bin/neuromosaic
chmod +x ~/.local/bin/neuromosaic
```

The script still expects the `neuromosaic` package to be installed in your R
library.

## CLI Examples

Generate a standard table-style report directly from a thresholded statistic
map:

```sh
neuromosaic report \
  --stat-map stats/z_map.nii.gz \
  --atlas Schaefer400 \
  --threshold 3.1 \
  --min-cluster-size 10 \
  --out report.html
```

Write a PDF report instead:

```sh
neuromosaic report \
  --stat-map stats/z_map.nii.gz \
  --atlas Glasser \
  --threshold 3.1 \
  --min-cluster-size 10 \
  --out report.pdf
```

Write a Quarto source document plus report data sidecar:

```sh
neuromosaic report \
  --stat-map stats/z_map.nii.gz \
  --atlas ASEG \
  --out report.qmd
```

Run a dataset-backed report from an ad hoc table of images:

```sh
neuromosaic report \
  --design design.tsv \
  --feature AUC \
  --path-template 'sub-{subject}/maps/AUC.nii.gz' \
  --stat-map stats/measure_t.nii.gz \
  --atlas Schaefer400x17 \
  --formula 'AUC ~ measure + group' \
  --out auc-report.html
```

Launch the interactive explorer:

```sh
neuromosaic explore \
  --design design.tsv \
  --feature AUC \
  --path-template 'sub-{subject}/maps/AUC.nii.gz' \
  --stat-map stats/measure_t.nii.gz \
  --atlas Schaefer400 \
  --plot-formula 'AUC ~ measure + group'
```

## Grouped map variants

A montage analysis can have one primary test-statistic map plus related effect,
uncertainty, or custom diagnostic maps. Give the rows one `analysis_id`, mark
one `role = "primary"`, and describe values with the generic `quantity` field:

```r
maps <- data.frame(
  analysis_id = "faces",
  map_id = c("faces_z", "faces_estimate", "faces_se"),
  path = c("faces_z.nii.gz", "faces_beta.nii.gz", "faces_se.nii.gz"),
  role = c("primary", "auxiliary", "auxiliary"),
  quantity = c("test_statistic", "estimate", "standard_error"),
  distribution = c("z", NA, NA),
  label = c("Z statistic", "Effect estimate", "Standard error")
)

render_montage_report(maps, "faces.html", bg = anatomical_volume)
```

HTML uses tabs for two to four variants and a select control for larger groups.
PDF, print, and no-JavaScript output show every map in primary-first order.
Built-in profiles choose thresholding, robust limits, and palette families;
`montage_map_profile()` supports namespaced custom quantities without a global
registry. See `vignette("montage-report", package = "neuromosaic")`.

For optional voxel-level exploration, keep the same report call and add an
interactive volume configuration:

```r
render_montage_report(
  maps,
  "faces.html",
  bg = anatomical_volume,
  interactive = montage_interactive(
    assets = "embed",  # one file that can be opened directly from an HPC system
    controls = c("threshold", "palette", "opacity")
  )
)
```

The static montage remains the report default and the print/failure fallback.
Opening the lazy orthogonal viewer exposes the same primary and subsidiary maps,
resolved limits, threshold, palette, and opacity; reader changes are exploratory
and reset exactly for each map. The static and interactive selectors stay in
sync, while each map retains its own temporary controls for that browser page.
Hiding or leaving the page disposes the viewer and its cached browser resources;
reopening starts from the currently selected map's report defaults.

Use `assets = "bundle"` for a hosted report with smaller HTML and a complete
`<report>_files/interactive/` companion tree; serve the HTML and directory
together over HTTP. Embedded reports enforce `max_embed_mb` against the actual
base64 payload. Neither mode requires a CDN or other external browser request.
Both modes disclose that voxel values are recoverable from the report artifact.
Static intensity-dependent alpha ramps are approximated by uniform interactive
opacity, so the contract is semantic parity rather than pixel identity and the
static montage remains authoritative.

As a reproducible reference, one background plus eight dense FLOAT64 maps used
29.39 MiB after gzip and base64 on a `91 x 109 x 91` lattice and 234.88 MiB at
`182 x 218 x 182`. With the default two-map cache, measured Chromium backing
storage plateaued at 21.00 MiB and 167.01 MiB respectively when switching from
four to eight maps. See the benchmark scripts under `tools/`; real data can
compress differently and the browser measure excludes driver-specific GPU
memory.

Repository browser checks use the installed Playwright Chromium and generate
their reports from current R sources:

```sh
node ~/.local/share/agent-policy/browser-automation-guard.mjs --audit
npm run verify:interactive
npm run test:e2e
node ~/.local/share/agent-policy/browser-automation-guard.mjs --audit
```

`npm run test:e2e` covers both embedded `file://` output and companion assets
served from a local static HTTP server. It owns and closes that server and its
browser processes for the duration of the run.

The larger payload/timing benchmark is reproducible with
`npm run benchmark:interactive`; run the same browser guard audits around it.
Its generated CSV and JSON stay under the ignored
`e2e/.artifacts/benchmark/` directory.

## Atlas Specs

Built-in CLI atlas specs currently include:

- `Schaefer100`, `Schaefer200`, `Schaefer300`, `Schaefer400`, `Schaefer500`,
  `Schaefer600`, `Schaefer800`, `Schaefer1000`
- `Schaefer400x17` or `schaefer:400:17`
- `Glasser` / `Glasser360`
- `ASEG`
- `subcortical:cit168`
- `subcortical:hcp_thalamus`
- `subcortical:mdtb10`
- `subcortical:hcp_hippamyg`
- Any `.rds` file containing a saved atlas object

## Notes

- HTML and PDF reports render immediately.
- `.qmd` output writes a Quarto source file and a companion
  `_report-data.rds` file for later rendering/customization.
- PDF output requires a working Pandoc and LaTeX installation.

<!-- albersdown:theme-note:start -->
## Albers theme
This package uses the albersdown theme. Existing vignette theme hooks are replaced so `albers.css` and local `albers.js` render consistently on CRAN and GitHub Pages. The defaults are configured via `params$family` and `params$preset` (family = 'red', preset = 'homage'). The pkgdown site uses `template: { package: albersdown }` together with generated `pkgdown/extra.css` and `pkgdown/extra.js` so the theme is linked and activated on site pages.
<!-- albersdown:theme-note:end -->
