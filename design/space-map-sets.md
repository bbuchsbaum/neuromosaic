# Map sets and Space in montage reports

Status: S1/S2 implemented locally, 2026-10-14; broader S3/S4 work remains.
The subject-set path is available; generic maps and z statistics remain gated.
This extends the
[montage-report PRD, section 20](prd-multimap-montage-report.md#20-map-sets-and-space).
Work is tracked by Mote `bd-01M4XVE9PJ1SYZ3GVJM3JXB9X5`.

## 1. Decision

Add **map sets** to the existing montage report. A set names maps to compare;
Space is an optional interactive view of that set. The render manifest remains
the source of map identity, data, scientific quantity, and report presentation.
The same set interface supports collections of contrasts, alternative models,
and subjects. Callers state membership explicitly.

Keep the following concepts distinct:

| Concept | Question it answers | Owner |
|---|---|---|
| Manifest row, `map_id` | Which map, with what quantity and source? | Existing render manifest |
| Analysis group, `analysis_id` | Which primary and auxiliary maps describe one analysis? | Existing manifest and map selector |
| Map set, `set_id` | Which entities and map variants should be compared together? | New membership specification referencing `map_id` |
| Report layout | Where are analyses arranged in the document? | Existing `layout`, labeller, and policy |

A map can belong to several sets. Joining a set never changes its analysis group,
primary/auxiliary role, static figure, threshold, or position in the report.
Set membership does not follow the report's current search/filter state.

The reader's task is to compare a collection, identify a member worth inspecting,
and reach that member's existing figures, slices, surface, and cluster table.
This is an extension of the report, with a shared identity contract and explicit
navigation; it does not require a second manifest of file paths or a Shiny app.

## 2. Proposed author interface

Use two value objects, following `montage_interactive()` and `montage_surface()`:

```r
montage_map_set(
  id, members, mask,
  label = NULL,
  member_kind = c("map", "subject"),
  template = NULL,
  atlas = NULL,
  covariates = NULL
)

montage_space(
  sets,
  view = c("space", "montage", "facet", "ortho"),
  initial_set = NULL,
  statistic = NULL,
  world_coord = NULL,
  height = NULL,
  max_data_mb = 256
)

validate_montage_map_sets(manifest, sets, bg = NULL, load_maps = FALSE)
```

`render_montage_report()` gains a trailing `space = NULL` argument. All existing
calls retain their behavior. `space` is a sibling of `interactive` and `surface`;
`montage_interactive(view = "space")` is not overloaded because its host and
data contract describe one analysis, while a set can span analyses.

An author comparing existing report maps writes:

```r
# Proposed API. These map IDs already exist in manifest.
contrasts <- montage_map_set(
  id = "group-contrasts",
  label = "Compare group contrasts",
  members = c("faces_t", "scenes_t", "objects_t"),
  mask = brain_mask
)

render_montage_report(
  manifest,
  output_file = "report.html",
  bg = template_2mm,
  interactive = montage_interactive(assets = "bundle"),
  surface = montage_surface(assets = "bundle"),  # with the usual surfatlas
  surfatlas = cortical_atlas,
  space = montage_space(sets = list(contrasts))
)
```

This generic-map example is the target interface. Shipping it requires the
upstream labeling extension in section 8; current 0.6.0 uses subject terminology.
The first implementation uses `member_kind = "subject"`, after addressing the
explicit-mask issue documented in that section.

### Membership and paired quantities

`members` accepts a character vector of `map_id`s or a data frame:

| Column | Meaning and default |
|---|---|
| `map_id` | Required foreign key into the render manifest. No paths or arrays here. |
| `member_id` | Entity being compared, such as `sub-01` or `faces`. Required in the table form. |
| `model` | Optional grouping key; default `"model"`. |
| `contrast` | Optional grouping key; default `"maps"`. |
| `label` | Optional member label, consistent wherever that member appears. For subjects, default `member_id`; for maps, the single map's manifest label, or `member_id` when paired. |

A character vector expands to one member per map, with `member_id = map_id`.
It does **not** silently pair maps sharing `analysis_id`. Pairing uses the table
form, which also supports the same subject across models and contrasts:

```r
members <- data.frame(
  member_id = rep(c("sub-01", "sub-02", "sub-03"), each = 3),
  map_id = c("s01_t", "s01_beta", "s01_se",
             "s02_t", "s02_beta", "s02_se",
             "s03_t", "s03_beta", "s03_se"),
  model = "first-level",
  contrast = "faces"
)
subjects <- montage_map_set(
  id = "faces-subjects", label = "Faces: subjects",
  members = members, member_kind = "subject", mask = brain_mask
)
space <- montage_space(list(subjects), statistic = "t")
```

The renderer resolves each referenced row's normalized `quantity` and
`distribution` into a channel: t statistic, z statistic, estimate, or standard
error. There is no second, independently editable statistic declaration in the
membership table. Each `(member_id, model, contrast, channel)` is unique. A
`map_id` occurs at most once within a set, so reverse navigation is unambiguous
and one map cannot accidentally count as several members. Reuse across sets is
allowed. A standard-error map must accompany an estimate in the same tuple and have
compatible units. A variance map is never silently square-rooted.

All members of a model/contrast comparison must share at least one channel.
`statistic` accepts `"t"`, `"z"`, or `"estimate"`; it sets the initial channel of
the initial set/model/contrast, where it must be common to all members. `NULL`
chooses the first common channel in the fixed order t, z, estimate. On later
model/contrast changes, keep that channel if common; otherwise choose the first
common channel and announce the change. Comparisons require compatible quantity,
distribution, and declared units; unknown units remain unknown. No mixed t/z
channel, automatic z-to-t conversion, or arbitrary diagnostic-as-beta adapter.
Standard error is supporting data, not a selectable similarity channel.

Existing `analysis_id` identifies associated variants; it does not assert that
the same subject occurs in another model. `member_id` supplies that relation.
Changing model/contrast retains the focused member only when present; otherwise
the focus clears and the reader is told why. It never selects an unrelated
member merely because it occupies the same row number. This target behavior
needs an upstream correction before enabling multi-analysis sets in Ortho;
section 8 distinguishes it from current behavior.

### Configuration rules

- `sets` accepts one set or a list with unique set IDs. List order controls the
  chooser; `initial_set = NULL` means the first set. Membership order controls
  initial model/contrast order. All referenced IDs must resolve.
- A set's `label = NULL` defaults to its ID. IDs are stable join keys, not paths
  or CSS selectors; labels are escaped text, not executable HTML.
- `mask` is required: a `NeuroVol` or NIfTI path defining the comparison domain.
  It is never inferred from suprathreshold voxels or nonzero background values.
- `template = NULL` uses report `bg` only if it satisfies the set's exact grid
  contract. An explicit set template accommodates a 2 mm set in a report whose
  slice underlay is 1 mm. `interactive$underlay` does not override it.
- `atlas` is optional explicit label-volume/region-table input on that grid.
  Do not assume an atlas used for peak lookup is a compatible full-grid atlas.
  `covariates` is an optional table keyed by `member_id`, with numeric,
  categorical, or missing values. They describe members, not map rows.
- `world_coord` is an initial anatomical cursor in millimetres. `height = NULL`
  uses a responsive report stage; a supplied height follows `montage_surface()`.
- `max_data_mb` limits the uncompressed typed-array input of one complete set,
  including all models, contrasts, channels, template, mask, and atlas. The
  proposed default is 256 MiB. This is a payload guard, **not a bound on browser
  memory**: upstream copies arrays and creates worker/rendering buffers.
- Space is bundle-only. There is no misleading `assets = "embed"` option.
  It can coexist with embedded ordinary slices/surfaces in the same report.

Construction checks local argument shape. `validate_montage_map_sets()` checks
references and semantic compatibility after ordinary manifest normalization.
With `load_maps = TRUE`, it also checks data, geometry, and payload size. It does
not execute deferred recipes, launch a browser, or compute an embedding; recipes
must first be materialized by the existing report pipeline. Its result states
which checks ran. Required set checks still run if report `validate = FALSE`.
Errors name the set, member, map, and incompatible field; requested invalid sets
are not silently omitted.

## 3. Raw data and spatial contract

Resolve set data from the materialized manifest before display clipping,
thresholding, minimum-cluster filtering, or primary-support restriction.
Reuse `.montage_profile_values_with_analysis_masks()`'s mask semantics: a row's
own analysis mask, otherwise its primary analysis's mask. Outside that support,
represent values as missing. Within it, finite zero remains a valid measurement.

Do not take comparison data from `panels[[map_id]]$volume$overlay` or blindly
reuse interactive NIfTI assets. `.montage_render_support_masks()` can restrict
an auxiliary map to its primary map's thresholded support. That is useful for
display and unsuitable as an implicit whole-map comparison domain. Reuse an
existing asset only when source values, mask transformation, geometry, precision,
and content hash establish that it is the required comparison input.

For the first implementation, every map, set mask, template, and optional atlas
must share dimensions and the complete affine. The frame requires axis-aligned
RAS+ with positive diagonal voxel sizes. Reject flips, permutations, obliquity,
shear, incompatible spacing, and non-overlapping origins before asset emission.
No restamping or implicit resampling. A later lossless axis-reordering helper can
be considered separately, with voxel/world-coordinate evidence.

Use full-grid, x-fastest arrays. Preserve source data on disk at their declared
precision using the existing content-addressed NIfTI writer. Convert at the
Space boundary to Float32 as required upstream, record that conversion, and
reject finite-to-infinite overflow. Preserve NaN and finite zero. The comparison
uses the converted values; it does not claim double-precision equivalence.
The anatomical template must be finite; the mask must be nonempty; standard
errors must be nonnegative where finite.

The 0.6.0 basis module uses signed Pearson correlation on common finite support
among eligible members, with distance `1 - r`. Its internal defaults exclude
members below 90% of median coverage and require 30 common-support voxels.
However, the current loader first removes voxels that are zero or missing in
every member, even under an explicit mask. That preprocessing violates the
finite-zero contract. The bundled runtime applies the retained U1 correction;
the native probe and source pin are recorded in section 8. Record the pinned runtime
and the actual mask/basis policy; do not expose internal fitting options as
configurable R arguments until the public embedding API supports them. Slices, thresholds,
and display filters must not alter the default similarity input. A deliberately
chosen upstream region comparison is a separate exploratory operation.

Insufficient support, constant maps, and too few usable members produce a visible
"not placed" state while inspection remains available where upstream supports
it. A wholly unusable initial analysis must produce a visible loading/preparation
error rather than invented coordinates; the report maps remain available. Exclusion does
not delete the member from the report. Distances and clusters are descriptive;
they do not establish significance or subject subtypes.

## 4. Reader interface and state

Place an **Explore map sets** section after the results overview and before
the map panels. Preserve the existing report typography, spacing, dark/light
behavior, and print structure. The closed section shows a set chooser when
needed, member count/kind, available channels, comparison-mask description,
data size, and an **Open comparison** button. It loads no maps or frame until
opened. The existing overview and per-analysis Slices/Surface controls remain.

```text
Results overview

Explore map sets
  [ Faces: subjects                 v ]  24 subjects · t / estimate
  Whole analysis mask · spatial-pattern comparison       [Open comparison]

  [ Space | Montage | Facet | Ortho ]       [Close comparison]
  ┌────────────────────────────────────────────────────────────────────┐
  │ Upstream viewer: maps, shared cursor, controls, diagnostics          │
  └────────────────────────────────────────────────────────────────────┘
  Selected: sub-07 · Faces · t             [Show in report]
  Similarity uses unthresholded maps; display controls are exploratory.

Maps
  Existing analysis groups, variants, figures, slices, surfaces, tables
```

Use upstream's four view controls inside its frame, without duplicating them
outside. The host owns set choice, opening/closing, loading/errors, and the
**Show in report** action. The proposed R `view` selects the initial upstream
view, not another report layout. The schematic describes hierarchy, not a new
visual design system.

Selection is linked, navigation is deliberate. Focusing a Space member updates
the host's selected-map readout. **Show in report** activates its exact map
variant through the existing `nm-map-request` route and then moves focus to the
analysis heading. Merely hovering or focusing a member does not scroll the page.
A report `nm-map-change` updates the open set only when that map belongs to a
common selectable channel of its model/contrast. A supporting SE map, or an
estimate channel supplied for only some members, leaves the active comparison
unchanged and receives a short explanation. Never send SE as a selectable
statistic or silently substitute its paired estimate. An unrelated map does not
switch sets or refit anything. Handle single-map analysis groups too, which
need no variant tabs.

Maintain a reversible lookup from `(set, model, contrast, member, channel)` to
`map_id` and `analysis_id`. Internal upstream IDs may differ from report IDs;
visible labels must survive that mapping. Tag programmatic changes and compare
state before forwarding to prevent notification loops. Explicit movement from
Space to ordinary slices can carry the world cursor through a small shared
navigation helper; pointer movement does not continuously drive other viewers.

Space owns its exploratory threshold, symmetric range, palette, and advanced
controls. It starts with threshold zero and the runtime's own range, stated in
its legend. It does not overwrite map-local controls or report profiles. Custom
report RGB/RGBA tables remain authoritative in static figures and ordinary
viewers; 0.6.0 cannot ingest those palettes through `SpaceState`. Do not promise
visual parity, silently substitute report colors, or expose a nonfunctional R
palette option. An authored shared Space palette is an additive upstream task.

Only one set frame is active per report. Each replacement mounts a disposable
candidate through the public `mountSpace()` API, then promotes it after readiness.
A newer request immediately disposes the prior candidate. This is necessary
because `setData()` cannot cancel an in-flight swap while a newer set is still
downloading. A failed replacement keeps the prior usable set and chooser;
latest reader selection wins during rapid changes.
Close, page exit, or host removal disposes the iframe and workers and cancels
pending loading. Reopening restores only the supported state snapshot; lasso,
layout zoom, and advanced options are not claimed to persist in 0.6.0.

## 5. Accessibility, fallbacks, and delivery

Provide keyboard access to set choice, open/close, selected-member navigation,
and the upstream frame. Announce loading, fitting, readiness, failure, and focus
loss with concise status text; do not announce every cursor movement. Closing
returns focus to the launcher. Opening uses a bounded, responsive stage with internal vertical scrolling
on narrow screens. Respect reduced motion in the host. Verify
upstream keyboard behavior and mobile scrolling in the integrated report.

| Situation | Report behavior |
|---|---|
| HTML over HTTP(S) | Lazy, same-origin packaged frame and workers; local data only. |
| HTML opened through `file://` | Static report works; Space launcher explains that this comparison requires a local/static web server. No attempted cross-origin workaround. |
| JavaScript or WebGL unavailable; frame fails | Keep static figures and member-to-report links; show a useful retry/failure message. |
| PDF or print | Member list and comparison policy, plus existing static figures. No empty frame, hidden essential results, or unearned embedding snapshot. |
| `.qmd` export | Carry relative scene/runtime references in the sanitized sidecar. Rendering from another directory must not need original source paths. |
| Missing/constant/low-coverage members | Preserve report links and display upstream placement exclusions. |

The frame brings its own styles; host CSS cannot be assumed to theme it. Match
host presentation to the report and record any iframe theme limitation instead
of accessing private frame DOM. Initially, use an exclusive-active-viewer policy:
opening Space closes active ordinary volume/surface views through their existing
view-router/disposal paths; opening those views closes Space first. Preserve the
supported state needed for an explicit reopen. Measure the frame's own resource
use. Concurrent viewing can follow only after a public lifecycle/resource bridge
can account for its contexts in the existing page budget.

## 6. Integration with the existing implementation

1. Normalize and materialize the existing manifest through the current recipe,
   label, policy, and profile pipeline. Resolve map-set references once from
   those rows. Static rendering can then proceed unchanged.
2. Add focused `R/montage_map_set.R` and `R/montage_space.R` modules for the value
   objects and validation, plus `R/montage_space_report.R` for packaging/hooks.
   Extract only the shared mask/asset operations actually needed; no parallel
   recipe loader, image cache, or statistics engine.
3. Build a small versioned `MapSetScene` with set/member labels, channel-to-asset
   references, full geometry, template/mask references, report-navigation keys,
   initial state, and provenance. Keep existing `VolumeScene` v1 unchanged.
   Both scenes can reference the same verified content-addressed assets.
4. Attach `report_data$space`; store only report-safe values in `.qmd` sidecars.
   Strip set source objects/paths as well as manifest sources, and make Space
   bundle references relative. Add `montage_space_report_hooks(space, is_html)`
   with `emit_document()` and one `emit_host()` for the set chooser/stage,
   following the existing volume/surface hooks. Wire both Rmd and Qmd templates.
5. Add a small host adapter that loads the public `neuroimjs/space` entry,
   decodes the declared local assets, and calls `mountSpace()`. Never import
   internal analysis modules or copy the demo application into custom logic.
6. Extend the vendoring tool to copy the **entire matching frame directory**,
   including workers, and bundle the public host entry. Hash every shipped file,
   record source/package version, and load these assets only for requested sets.
   Keep the ordinary viewer's size test; give Space a separate measured budget.
7. Record ordered membership, source and derived hashes, masks, grid, conversion,
   declared comparison channels, runtime/adapter versions, and input policy in
   report provenance. Live fit diagnostics are runtime results; 0.6.0 does not
   expose them in its supported state snapshot. Do not invent saved fit evidence.

Space has its own pinned runtime manifest and reproducible vendoring script;
the existing ordinary-volume runtime remains at 0.5.0. Its older npm development
dependency is not used to build Space. The Space build requires the exact source
tree plus the retained correction patch; a package version alone is insufficient.
No CDN, external fonts, synthetic demonstration data, or study-specific file
discovery is part of the report runtime.

## 7. What remains where

| Responsibility | Owner |
|---|---|
| Map sources, recipes, labels, quantity/units, primary/auxiliary identity | Existing neuromosaic manifest/pipeline |
| Comparison membership and links back into reports | Neuromosaic map sets |
| Static thresholding, surface projection, clusters, peak tables, report palettes | Existing neuromosaic renderers/policies |
| Space fitting, exclusions, comparison tools, linked upstream views | Neuroimjs public Space application |
| Frame packaging, loading, lifecycle, safe data conversion, outer-page navigation | Neuromosaic Space adapter |
| Statistical inference or a new R implementation of NMDS | Outside this integration |

Surface-only parcel sets, automatic volume resampling, author-configurable
embedding algorithms, static embedding exports, and a new CLI configuration
language are deferred. R API support comes first. A later CLI adapter should
deserialize the same set contract and validator, rather than fork them. This
does not replace the current Shiny montage explorer.

## 8. Upstream capability boundary

Verified against neuroimjs main `18cd39ff8fa63a37cab16bb8cf204900c448b77e`
(package version 0.6.0), whose tree matches local `c4d23a4`. npm 0.6.0 is now
published. The integration builds the pinned tree with a retained patch because
the released source still has U1. Relevant primary sources:
[Space guide](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/docs/guide/volume-set-space.md),
[public types](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/src/space/types.ts),
[validation](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/src/space/validation.ts),
[similarity basis](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/src/volumeset/basis.ts).

| Capability | 0.6.0 public contract | Consumer decision |
|---|---|---|
| Mount/dispose, replace data, events, four linked views | Supported | Use directly; validate through real packaged consumer tests. |
| Subject t/beta collections, optional SE/atlas/covariates | Supported data shape | First implementation after the explicit-mask correction; channel `estimate` maps to upstream `beta`. |
| Preserve finite zeros throughout the declared mask | Loader intersects the mask with the union of nonzero finite values | Required upstream correction before the first usable integration. No data perturbation or private-module bypass. |
| Clear a missing member when changing analysis in Ortho | Current source selects the first ordered member when selection is absent | Required before multi-analysis sets: clear selection and return to Space, or show an explicit empty Ortho state. |
| Arbitrary member nouns and human-readable labels | Subject-oriented fields/UI; no label metadata | Small upstream extension before generic report-map release. Preserve internal API compatibility, add labels/nouns and neutral accessible text. No DOM rewriting. |
| z statistics | Only `t` and `beta` slots | Add an honest public z channel upstream before accepting z sets. Do not send z values through the t slot. |
| Generic probability/diagnostic maps | No corresponding channel | Reject with a capability error; defer beyond t/z/estimate scope. |
| Arbitrary affine or differently sampled template | Rejected | Validate strictly; caller supplies aligned data. |
| Author palette tables, full tail/range semantics | Not in public Space state | Keep exploratory Space display explicit; upstream additive work for authored parity. |
| Fitted coordinates, diagnostics, advanced-state export | Not in public state snapshot | Keep in the live viewer; no reproducibility/export claim for unexported state. |

Before shipping the subject path, verify that displayed IDs are the intended
subject labels. If an upstream-safe ID transformation would change a visible
label, require the label extension rather than displaying opaque replacement
tokens. Arbitrary existing report `map_id`s can still be retained in the
consumer's reverse lookup; they need not become upstream subject IDs.

### Required upstream corrections and evidence

**U1, before S2: preserve the explicit comparison mask.** The embedded
[`loadSet()`](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/examples/volume-set/data.ts)
uses
[`VolumeSet.fromVolumes()`](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/src/volumeset/VolumeSet.ts),
whose nonzero-union filter currently overrides explicit mask membership. Add a
supported path preserving the declared mask, including finite zeros, and make
embedded Space use it. Keep implicit-mask behavior separately named if needed.

An independently executed native-loader probe used a 60-voxel all-one mask and
three arrays formed by repeating `[0, 1, 2]`, `[0, 2, 1]`, and `[0, 1, 3]` twenty
times. The declared-mask Pearson correlation of the first two arrays is `+0.5`.
Unpatched `fromVolumes()` retains 40 voxels and the correlation becomes `-1`.
The bundled correction retains all 60 and preserves `+0.5`.
This is a change of comparison domain, not a harmless storage optimization.
The probe exercises the actual loader constructor with an independent Pearson
calculation; it is not an end-to-end browser result. The correction needs both
that regression and a packaged Space consumer test.

**U2, before multi-analysis support: preserve selection semantics.** Source
review of
[`main.ts`](https://github.com/bbuchsbaum/neuroimjs/blob/18cd39ff8fa63a37cab16bb8cf204900c448b77e/examples/volume-set/main.ts)
shows `applyEmbeddedState()` clearing a missing subject and `renderOrtho()`
then selecting `order[0]`. Test disjoint memberships through both `setState()`
and the frame's model/contrast controls. The committed state must have no
unrelated selection; returning to Space with an explanation is an acceptable
solution. A host correction after an unrelated selection event is too late.
The packaged public-API disjoint-Ortho regression now passes with the retained
patch. Multi-analysis report sets remain gated until frame-control transitions
and the broader S3 channel/identity contract are qualified.

**U3/U4, before S3: neutral labels and z statistics.** Extend the supported API
with member nouns/display labels and an honest z channel. Keep stable identity
separate from labels, and update accessible text as well as visible controls.
These are bounded upstream changes, not permission to fork the Space app.

## 9. Delivery sequence and acceptance

| Step | Concrete output | Evidence required before advancing |
|---|---|---|
| S0: Contract and upstream boundaries | Constructors/validator specification, membership examples, U1–U4 requirements, pinned usable artifact | Identity traces for report and subject sets; native zero-support probe; packaged U1 regression before S2. Artifact contains host entry, frame, and workers. |
| S1: Resolve and package sets | R objects, raw-data/mask path, `MapSetScene`, assets, provenance, size/geometry checks | R tests for joins, paired channels, duplicate tuples, missing IDs, masks, unsupported geometry/statistics, Float32 conversion, payload guard, and sidecar portability. No browser required. |
| S2: Working subject-set report | Lazy host/frame in both templates, initially one model/contrast, subject t/beta support, disposal, member-to-report navigation | U1 verified; R-generated report runs against packaged API; no demo/network data; selection/cursor round trips; failed replacement preserves prior view; close leaves no frame/workers. |
| S3: General report-map comparisons | U2–U4 adopted; multiple models/contrasts, neutral labels and z; report-map example works honestly | Disjoint memberships clear focus; SE/incomplete channels never trigger invalid navigation; t/z labels stay distinct; member labels/identity mapping survive. |
| S4: Qualification and documentation | Examples/vignette, accessibility/fallback checks, measured payload/memory results | Existing R/JS/browser regressions plus targeted Space browser checks, and `rcmdcheck::rcmdcheck()` on the exact candidate. Report existing failures separately. |

After U1 is fixed, one initial executable slice is sufficient: three subject members, each with
an existing t-map row, one shared mask/template, a lazily opened Space frame,
and **Show in report** selecting the correct panel. This establishes the full
route before adding paired channels, covariates, atlas contributions, or new
upstream capabilities. S2 is not completion of the generic-map interface.

Use independent numerical and boundary fixtures: known correlated/sign-reversed
patterns; finite zeros; one missing-coverage member; a constant member; an
asymmetric landmark volume detecting axis or index errors; and an auxiliary map
whose report display uses primary support. Verify that report threshold/extent
changes leave the exported comparison arrays unchanged. Verify source arrays
remain untouched. Where fit diagnostics are unavailable publicly, assert export
semantics and visible consumer behavior without claiming internal-fit equality.

Browser acceptance also covers keyboard navigation, narrow/desktop layouts,
multiple set switches, rapid close during load, error/retry, `file://`, print,
and coexistence with the existing volume/surface resource budget. Measure a
small real-grid fixture and a larger synthetic collection before finalizing
the proposed 256 MiB default. Do not claim capacity from compressed file size.

Design validation completed: proposed R examples parse; cross-checked identity,
asset, navigation, and template integration against current neuromosaic source;
independent contract review identified U1, U2, and the nonselectable-channel rule;
the U1 native-loader effect was independently reproduced. Implementation and
consumer evidence are recorded below.

## 10. Review decisions

The proposed decisions are: explicit membership; one shared interface for both
subject and report-map sets; raw comparison inputs; strict spatial validation;
bundle-only delivery; deliberate links into current report panels; and a staged
release that names upstream capability gaps. Naming, the initial payload limit,
and whether generic-map support must precede any public release remain review
choices. The private fixture can be prepared while U1 is addressed, but its
comparison results cannot be accepted until the mask contract is satisfied.

## 11. Implemented subject-set slice

The constructors, validator, `space` render argument, both template hooks,
content-addressed NIfTI bundles, and lazy report adapter are implemented. The
fixture in `e2e/build-space-fixture.R` exercises four synthetic subjects with
t/estimate/SE maps, an explicit mask, an atlas, and numeric/categorical/missing
covariates. It also builds single-map groups and portable Quarto output.

```r
# The manifest already contains these subject t maps.
subjects <- montage_map_set(
  "faces",
  data.frame(map_id = c("s01_t", "s02_t", "s03_t"),
             member_id = c("sub-01", "sub-02", "sub-03")),
  mask = brain_mask, member_kind = "subject"
)
render_montage_report(manifest, "report.html", bg = template_2mm,
                      space = montage_space(subjects))
# Serve report.html together with report_files/ over HTTP(S).
```

The runtime is built from tree `69ebc65e37b26a3ab2a33984c492165ce6bde6e8`.
`tools/patches/neuroimjs-space-0.6.0.patch` retains the explicit-mask correction,
the public-API missing-Ortho-selection correction, mobile sidebar layout, and
removal of the optional external-font path. `runtime.json` records the patch
and every artifact hash. `tools/vendor-neuroimjs-space.mjs` rebuilds from a clean
pinned checkout. It installs dependencies afresh with `npm ci` from that tree's
lockfile, retaining Node/npm versions, platform, lockfile and installed-tree
hashes, and the vendoring script hash. Ambient `node_modules` is not used:

```sh
NEUROIMJS_SPACE_SOURCE=/path/to/neuroimjs node tools/vendor-neuroimjs-space.mjs
```

The Space artifact is 1,194,871 bytes against a separate 2 MiB runtime budget.
Two independent clean dependency installations/builds produced identical
artifacts and runtime manifests on darwin/arm64, Node 26.7.0 / npm 11.19.0.
This verifies repeatability on that toolchain; it is not a cross-platform claim.
The 256 MiB input default is a configurable payload guard; larger real-grid
capacity and peak browser memory have not been measured.

Current boundaries: subject sets only, one model/contrast per set, t/estimate
channels with optional SE, numeric pixel height, strict RAS+ grids, and bundled
HTTP delivery. Generic nouns/labels, z, multiple comparisons per set, cursor
transfer into ordinary slices, and a release-level capacity/check run remain
S3/S4 work. The retained upstream patch has not been published upstream.

Verification for this slice (2026-10-14):

- Focused R tests: 57 assertions passed, covering raw values, masks, geometry,
  units/channels, factor atlas IDs, payload-limit boundaries in public validation,
  exact Float32 endpoints, provenance, escaping and portable sidecars.
- Full R suite rerun: 3,545 assertions passed, zero failures or errors,
  128 warnings and one existing specimen-thumbnail skip. The two pre-existing
  `test-cr-brain-plot.R` failures were fixed by explicitly requesting
  `assemble = FALSE` from upstream `plot_ortho()`, preserving the public named
  axial/coronal/sagittal views. Regression checks render each view and verify
  its cluster title.
- JavaScript suite: 75 tests passed with the qualified neurosurf dependency.
  Running against the machine's older default neurosurf reproduces its shader
  assertion failure; this is an environment dependency, not a Space result.
- Patched upstream VolumeSet: 27 tests and typecheck passed. The independent
  explicit-mask probe retains 60 voxels and Pearson r = +0.5.
- Sixteen Space browser tests pass, covering lazy loading, all four views, exact report navigation,
  SE handling, optional atlas/covariates, singleton groups, atomic replacement,
  a deterministic superseded-candidate race, rapid close, desktop/mobile,
  no-JavaScript links/printing, rapid selection reversal, cancellation during
  runtime/data/constructor loading, pending surface readiness, keyboard focus,
  and the public Ortho fix. All 59 selected browser tests pass, including the
  existing volume, surface, map-selector and explicit-palette regressions.
- The normal Playwright setup requires Quarto, installs the current package in
  a temporary library, relocates the QMD/sidecar/bundle, and renders it afresh.
  The browser test has no optional-fixture skip or stale-HTML fallback.

### Fresh-review corrections

The fresh review identified atlas factor coercion, rapid report-selection
reversals, pending ordinary-viewer activation, and fallback navigation/printing
defects. Atlas IDs now retain their numeric labels; the loaded public validator
enforces the same payload cap as rendering. Report selection sends every latest
request through the upstream command queue. Volume activation checks cancellation
after each asynchronous boundary and disposes a late constructor result; the
surface bridge also fences pending runtime/readiness work. The volume adapter
version is 1.5.2, within its existing runtime size budget.

Included-map lists render open without JavaScript, have exact map anchors, and
remain printable. Enhancement collapses the list; the report's existing map
router reveals linked auxiliary variants. PDF headings also receive stable
map anchors. Opening Space transfers keyboard focus to its close control.

Each binding now records SHA-256 of the loaded source values plus grid before
support masking, and the origin map ID, inheritance flag and SHA-256 of any
normalized analysis support. The explicit comparison mask is also hashed.
These semantic hashes use R serialization v2; they are not hashes of original
NIfTI file bytes or paths. Derived NIfTI hashes and the exact JS/CSS adapter
hashes complete the exported transformation trail.

No release/PR has been created. A new `rcmdcheck` release gate and measured
large-data memory/performance qualification are outside this completed S1/S2
slice; the previous package-check environment lacked suggested `flextable`.
