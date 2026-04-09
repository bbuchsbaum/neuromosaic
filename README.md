# neuromosaic

`neuromosaic` generates atlas-annotated cluster reports from neuroimaging
statistical maps and provides an interactive Shiny explorer for drilling into
clusters, parcels, and design-linked signal plots.

## Installation

Install the package from GitHub:

```r
# install.packages("pak")
pak::pak("bbuchsbaum/clusterreport")
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
