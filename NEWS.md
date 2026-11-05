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

## Changes

* Static and 3D surfaces share a soft gyral/sulcal two-tone underlay (sulcal
  depth, display greys 0.66-0.90) instead of a faint linear shading; the
  static medial wall is drawn at the light gyral tone.
* Diverging colour ranges authored symmetric about zero stay symmetric when
  either end is edited.
* The 3D colour key is shown for custom palettes.
* Captions no longer mention clusters on unthresholded maps or print internal
  palette identities, legends never repeat units ("z (z)"), and colour caps
  keep a meaningful second decimal (4.25, not 4.2).
* Requires the surfview 3.2.0 runtime bundled by neurosurf 0.1.0.9002 (whose
  dependency label, recorded in report provenance, names that version) and
  neuroatlas 0.2.0.9001.
