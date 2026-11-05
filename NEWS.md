# neuromosaic 0.2.1

## New features

* Unthresholded maps fade by |value| in every view with one rule: static
  slices, the static surface, the slice viewer and the 3D surface. Map
  profiles choose it with `alpha_mode` (`"ramp"`, `"proportional"` or the
  self-tuning `"soft"`); every colour key draws the fade. Thresholded report
  figures stay opaque above threshold.
* `nm_magnitude_opacity()` returns that rule for callers that render static
  figures themselves (for example `surf_montage(magnitude_opacity =)`).
* `nm_standardize_map()` expresses a map in z units of spatial spread (map /
  robust SD, zero kept), dividing matching surface values by the same scale.
* The slice viewer offers a threshold control for unthresholded maps, and the
  surface display panel a threshold slider. Threshold and colour range are
  shared per map between Slices and 3D surface.
* `montage_surface(variants =, variant =)` adds an Inflated / Pial / White
  (or any named) surface switch to the 3D viewer.
* Report volume figures tag each cluster's peak (P1, N1, ...) on its nearest
  slice, in the sign's colour, so the figure and the cluster table share one
  vocabulary; `stat_montage(peak_labels =)` exposes the same tags.
* Cluster tables add the estimate and standard error at each peak, sampled
  from the analysis's own estimate and SE maps.
* Each volume figure links its unthresholded map ("Map (NIfTI, viewer
  copy)"), and provenance records the SHA-256 of every input map file.

## Changes

* Shared slice sets are placed from the thresholded maps and span every
  reported cluster, so each tabulated peak lies within half a slice step;
  unthresholded companions no longer spread slices over empty anatomy.
* Results at a glance shows glass brains for thresholded maps and, for
  unthresholded maps, their values at the analysis's peaks, by sign. QC
  collapses to one header line (N per analysis) when every map passes.
* Methods states each fact once, in paragraphs with run-in heads: thresholds
  as fixed and uncorrected, the extent rule as a display filter, one colour
  cap per quantity, and what QC "ok" means. Spec lines mark fixed thresholds
  "uncorrected".
* In grouped analyses the Static / Slices / 3D switch and its viewers sit
  under the map tabs. The 3D viewer opens on the Overview pose by default
  (`montage_surface(view = "oblique")`; was `"left"`).
* Cached static surface figures are keyed on the installed neurosurf
  version, so an upgrade no longer reuses figures drawn by the old renderer.
* Static and 3D surfaces share a soft gyral/sulcal two-tone underlay (sulcal
  depth, display greys 0.66-0.90) instead of a faint linear shading; the
  static medial wall is drawn at the light gyral tone.
* Diverging colour ranges authored symmetric about zero stay symmetric when
  either end is edited.
* The 3D colour key is shown for custom palettes.
* Captions no longer mention clusters on unthresholded maps or print internal
  palette identities, legends never repeat units ("z (z)"), and colour caps
  keep a meaningful second decimal (4.25, not 4.2).
* Requires the surfview 3.2.0 runtime bundled by neurosurf 0.1.0.9001 and
  neuroatlas 0.2.0.9001; older versions fall back to the previous displays.
