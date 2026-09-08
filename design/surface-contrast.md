# Cortical surface contrast

Use the FreeSurfer-style preset for a binary folding underlay and saturated
signed heat colors. Positive values use red/yellow; negative values use
blue/cyan. The interactive preset starts at full overlay opacity.

```r
render_montage_report(
  manifest,
  output_file = "report.html",
  surfatlas = surfatlas,
  surface_args = list(appearance = "freesurfer"),
  surface = montage_surface(preset = "freesurfer")
)
```

For a softer anatomical underlay, set `anatomy_style = "continuous"` in both
`surface_args` and `montage_surface()`. The static appearance controls apply
to vertex overlays rendered by the CPU backend. Parcel-colored static panels
retain their parcel rendering path.

The ordinary interactive default now resolves anatomy automatically while
retaining the light background and authored map palette. Set
`montage_surface(anatomy_style = "none")` for an untextured surface.

Anatomy is resolved by `neuroatlas::surface_anatomy()`: explicit metrics take
precedence over atlas metrics, followed by curvature from matching white
geometry. Derived curvature receives five mesh-adjacency averaging steps to
reduce tessellation noise. Supplied anatomy metrics are not smoothed. Missing
or incompatible white geometry produces neutral anatomy and recorded fallback
provenance, rather than curvature calculated on an inflated mesh.

`neurosurf::normalize_surface_anatomy()` centers the metric on its median and
scales it robustly for the browser. For a native signed metric, set
`anatomy_midpoint = 0` and choose `anatomy_invert` according to its sign
convention. Matching hemisphere, surface space, density, and vertex ordering
remain necessary for supplied metrics; vector length alone cannot establish
correspondence. Static and interactive renderers share this display transform.

Statistical values, thresholds, projection settings, and coordinates are not
changed by the anatomy display options. Geometry lighting remains specific to
the renderer; the CPU raster and WebGL viewer are not pixel-identical.

## Reproduce the comparison

After installing the matching neurosurf and neuroatlas versions, run from the
neuromosaic root:

```sh
LC_ALL=C LANG=C RGL_USE_NULL=TRUE Rscript tools/render-surface-contrast.R
node tools/verify-surface-contrast.cjs
node tools/verify-surface-contrast-report.cjs
```

The fixture uses the real bilateral fsaverage6 inflated mesh (40,962 vertices
per hemisphere), a deterministic synthetic signed scalar field, limits of
`[-5, 5]`, and a threshold of `|value| >= 2`. It writes HTML, PNG, and JSON
evidence under `e2e/.artifacts/surface-contrast/`. This is a rendering comparison,
not analysis of the screenshot's subject data. The scripts use Playwright's
installed Chromium and close the browser after each run.

Set `NM_SURFACE_BASELINE_RUNTIME` to an older `surfview.embed.iife.js` to reproduce
the original shipped viewer as the untextured baseline. Without that variable,
the baseline uses the current viewer with anatomy disabled.

The neurosurf browser bundle is built from its recorded upstream revision plus
`tools/surfview-runtime.patch`. The patch includes the DataLayer opacity fix
already present in the current surfviewjs working source: the compositor applies
opacity once. `tools/rebuild-surfview-runtime.py` reproduces and verifies the
bundle without incorporating unrelated working-tree changes.
