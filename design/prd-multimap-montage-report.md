# PRD — Multi-Map Montage Report

| | |
|---|---|
| **Status** | Draft for review (rev 2 — incorporates code-review findings 2026-06-03) |
| **Author** | bbuchsbaum (with Claude) |
| **Date** | 2026-06-03 |
| **Tracking issue** | [#1 — RFE: multi-map montage report mode](https://github.com/bbuchsbaum/neuromosaic/issues/1) |
| **Home package** | `neuromosaic` (rescoped) |

---

## 1. TL;DR

Add a **second report family** to `neuromosaic`: a *multi-map montage report* that
renders a grid of thresholded statistical maps as **volume + surface montages**,
driven by a first-class **render manifest** (one row per renderable map). The display
primitives already exist in `neuroim2` (`plot_overlay`) and `neurosurf`
(`vol_to_surf`), and much of the report scaffolding (a quarto/Rmd render path, atlas
peak annotation, `--design`/`--path-template` addressing, a Shiny explorer) already
exists in `neuromosaic`. We add a frozen render engine, lift the volume-montage glue
`fmrireport` already proved out, add a surface montage, and feed the engine from a
storage-agnostic manifest that adapters fill from `fmrigds` (group), `neurotabs`
(subject), or glob/TSV.

**Crucially, the first milestone is *contract-first*** (per the 2026-06-03 review):
the CLI dispatch contract, the manifest schema + validator, and the renderer/template
contract land *before* any plotting code — because the current `report` path is
hard-wired to a single cluster-defining stat map and to `cluster_report()`, and the
HTML/PDF render path does **not** use the `_report-data.rds` sidecar the way the RFE
assumed. The bespoke 30% (discovery, labels, threshold/layout policy) stays confined
to plug-points; the generic 70% (overlay, montage, tables, atlas peaks, assembly, QC)
is frozen and shared.

---

## 2. Problem & motivation

We have hand-built essentially the same study-level GLM HTML report **twice** across
two projects — a subject-level + 2-subject-contrast report, and a group report
(6 contrasts × 2 group models × 2 smoothing variants). Each time ~70% of the code was
identical and ~30% was a bespoke shell. The bespoke part was always the **same four
knobs**: how maps are discovered/addressed, how they're labelled, how they're
grouped/nested, and the threshold/stat policy.

Proof-case implementations to mine for the engine:
`sdam/scripts/weiner_bros/{render_reports.R,R/report_plots.R,reports/*.qmd}` and
`sdam/scripts/glm/{render_group_reports.R,R/report_plots.R,reports/group_recog_report.qmd}`.

This duplication is also a symptom of a larger split: **two report front-ends already
exist** — `fmrireport` (PDF, `fmrireg`-coupled, already calls `plot_overlay` + a
montage + cluster/peak tables) and `neuromosaic` (HTML, clusters, Shiny explorer) —
carrying **overlapping cluster-table code**. A shared, frozen render engine is the
long-term fix for both the duplication *and* the front-end split.

---

## 3. Goals & non-goals

### Goals
- A `render_montage_report(manifest, labeller, policy, out)` that renders a nested grid
  of thresholded volume (+ optional surface) montages from a manifest.
- A **storage-agnostic render manifest** as the pivot abstraction, with a **formal
  schema and a validator** (`validate_manifest()`), fillable by adapters.
- A **CLI dispatch contract** that routes a montage report through a *separate* spec and
  executor — no single cluster-defining stat map required.
- A **renderer/template contract** defining `render_montage_report()`'s template
  selector and output strategy across HTML/PDF/`.qmd`.
- **Atlas peak annotation** for montage panels, reusing `neuromosaic`'s existing
  `neuroatlas` machinery (the bridge that justifies cohabitation with the cluster report).
- QC invariants that turn each historically-costly bug into a loud failure.
- A render engine structured so `fmrireport` can later depend on it and shed duplicated
  rendering code (migration-friendly, not a one-way door).

### Non-goals (this milestone)
- Replacing the existing single-map cluster-table report (it stays; montage *complements* it).
- Replacing the existing `neuromosaic manifest` (nftab) verb or the `--design` mechanism.
- A bespoke statistics/fitting engine — the report is a **pure renderer** of finished maps.
- 2-D matrix layouts, non-`p`→threshold policies (TFCE/FDR), `fmrireport` migration, and
  PDF polish — served by *plug-points* now, first-class later.

---

## 4. Where it lives — the layering decision

**Decision: build the rendering core *inside* `neuromosaic`, rescoped from "cluster
reports" to "statistical-map reports."** Not a new package; not `fmrireport`.

### The ecosystem, as layers

```
display:   neuroim2 (plot_overlay)  ·  neurosurf (vol_to_surf)  ·  neuroatlas (labels)
core:      RENDER ENGINE — overlay/surf montage · tables · atlas peaks · QC · assembly
source:    fmrigds (group GDS)  ·  neurotabs (subject NFTab)  ·  glob/TSV   →  MANIFEST
frontends: fmrireport (PDF/GLM)        ·        neuromosaic (HTML/clusters/explorer)
```

### Why `neuromosaic` (scorecard)

| RFE requirement | neuromosaic | fmrireport | new pkg |
|---|---|---|---|
| quarto/Rmd render path + `.qmd` rds sidecar | ✅ (see §13) | ❌ (PDF/Rmd) | build |
| Atlas peak annotation machinery | ✅ | partial | build |
| `--design` / `--path-template` addressing | ✅ | ❌ | build |
| Shiny explorer to wire the manifest into | ✅ | ❌ | build |
| Imports neuroim2 **+** neurosurf **+** neuroatlas | ✅ all three | partial | wire up |
| HTML output (the RFE target) | ✅ native | ❌ PDF-first | build |
| Volume montage glue (`plot_overlay`) | ❌ (uses `plot_ortho`) | ✅ already | build |
| Existing fsaverage surface projection layer | ✅ `ce_overlay.R` | — | build |
| License | MIT | **GPL** | — |

### Why not the alternatives
- **`fmrireport` as the core** — wrong base: PDF-first, welded to `fmrireg`, and **GPL**
  (a dependency of MIT packages is a licensing headache). Better it *depends on* the core.
- **A new rendering package** — "clean on paper," but you'd refactor *both* front-ends
  when `neuromosaic` already holds most of the infra. Pay that tax only with a *third* front-end.
- **`fmrigds` / `neurotabs`** — wrong layer; they are manifest *sources*, not renderers.

### Not a one-way door
Structure the engine as a clean internal layer (`stat_montage`, `surf_montage`,
`prepare_overlay`, peak-atlas table, QC) with a stable contract, *as if* extractable.
Later, `fmrireport` can depend on `neuromosaic` for these primitives and delete its
duplicated cluster/peak-table code — without paying the new-package tax now.

---

## 5. Architecture — three layers, manifest as the pivot

```
 STUDY CONFIG (small, bespoke):   manifest builder · labeller · policy/layout
            │  emits a normalized RENDER MANIFEST (tidy table, schema-validated)
 RENDER ENGINE (generic, frozen): prepare_overlay · vol montage · surf montage
            ·  tables · atlas peak labels · assembly · QC invariants
```

Everything **upstream** of the manifest is study-specific discovery; everything
**downstream** is generic and frozen. The bespoke knobs survive only as the *builder*,
the *labeller*, and the *policy* — each a typed plug-point.

---

## 6. The render manifest (the keystone)

One row per renderable statistic map. Distinct from the per-observation
`nftab`/`--design` manifest (different granularity); adapters bridge them (§11).
**Schema parity with the existing single-map report (`cluster_report()`,
`R/cr_report.R:88`) is required** — omitting these fields risks silent disagreement
with current report behavior.

### Identity & map
| field | example | role |
|---|---|---|
| `map_id` (**required, stable**) | `grp_vivid_onesample_s2` | join key, cache key, figure anchor, table key |
| `path` **or** `recipe` | `…/t_coef_Intercept.nii.gz` / `function()…` | the map, or a thunk that computes it (§12) |
| `space` / `template` | `MNI152NLin2009cAsym:res-02` | background + grid reconciliation (§10.1) |
| `mask` | `…/mask.nii.gz` | optional analysis mask; restricts suprathreshold counts |

### Statistic semantics
| field | example | role |
|---|---|---|
| `stat_kind` | `t` / `z` / `beta` / `cope` | colorbar label, p→threshold, legend wording |
| `df` | 31 | t→p threshold |
| `units` | `t` / `a.u.` | colorbar unit |
| `signed` | `TRUE` | diverging vs sequential; enables "warm = A>B" semantics |

### Threshold / cluster policy (per-map overrides; defaults from policy §8)
| field | example | role |
|---|---|---|
| `p` **or** `threshold` | `0.005` / `3.1` | per-map override of the global policy |
| `tail` | `two_sided` / `positive` / `negative` | matches `cluster_report(tail=)` |
| `connectivity` | `18-connect` | matches `cluster_report(connectivity=)` |
| `min_cluster_size` | `10` | matches `cluster_report(min_cluster_size=)` |

### Layout / labelling / provenance
| field | example | role |
|---|---|---|
| `level` | subject / group | |
| grouping keys | `contrast=vividness`, `model=onesample`, `variant=smooth2` | layout nesting + cap scope |
| `label` (**required**) | "Vividness modulation — visualization" | human-supplied heading |
| `description` | markdown | injected above the figure |
| `n` / `subjects` | 27 | shown; flags dropped subjects (§10.4) |

`build_manifest()` accepts pluggable sources (§11) and supports **hybrid
parse-with-overrides** (auto-parse filename entities, then override columns).
`validate_manifest()` enforces required fields + the QC assertions (§10) and is the
contract both the programmatic API and the CLI `--validate`/dry-run share.

---

## 7. Labeller contract — supplied AND validated

```
labeller(entities) -> { title, short, description, legend_semantics }
```

The engine **must fail loudly when any manifest row lacks a label** (label-coverage
assertion, §10.3) — that assertion *is* how we guarantee "correct labels" instead of a
raw `phase_phase.cue` leaking into a heading. Difference-map wording ("warm = A > B")
lives in `legend_semantics`.

---

## 8. Policy — declarative *with function escape hatches*

Policy is **function-valued**, not pure config, so the next bespoke case lands at a
plug-point instead of forcing an engine fork. Policy supplies **defaults**; the manifest
may override per map (§6).

- **threshold**: `f(stat_kind, df) -> thr` (default: t → `qt(1-p/2, df)`, z → `qnorm`);
  user sets one `p`. A custom function covers TFCE/FDR/cluster-extent later.
- **cluster definition**: default `tail`, `connectivity`, `min_cluster_size` for
  suprathreshold counts/peaks (parity with `cluster_report()`); per-map override via §6.
- **cap scope**: which grouping key shares a symmetric color cap, e.g.
  `cap_within = c(contrast, model)` (we shared across `variant` so smoothed/unsmoothed
  are visually comparable).
- **layout**: ordered nesting `c(contrast, model, variant)` → H2/H3/panel. One generic
  template renders any nesting; a custom layout function is the escape hatch for 2-D matrices.

---

## 9. Render engine primitives (frozen)

- `prepare_overlay(bg, stat)` — reconcile grids (§10.1), return aligned pair.
- `stat_montage()` — `neuroim2::plot_overlay(style = "report")`: diverging signed map,
  symmetric limits, `ov_alpha_mode = "soft"`, threshold-marked colorbar, brain crop,
  and report-style overlay assembly (lift from `fmrireport::report.R` ~746–789).
- `surf_montage()` — sample at midthickness → inflated surface, lateral + medial, both
  hemispheres, **same cap as the volume**. **Surface-layer decision (§14, §16):**
  generalize the *existing* `ce_overlay.R` projection + geometry layer (currently
  fsaverage) to also serve fsLR-32k, reusing its `.overlay_geom_cache` — do **not** add a
  parallel surface path or a parallel cache (`R/AGENTS.md:30`).
- `stat_summary_tbl()` — suprathreshold counts + peaks (honors `tail`/`connectivity`/`min_cluster_size`).
- **Atlas peak annotation** — optionally annotate each panel's peaks with atlas labels via
  the existing `enrich_cluster_table` → `neuroatlas::query_point` machinery, generalized
  to a per-panel peak table. The bridge between the cluster-table and montage reports.

---

## 10. QC invariants the engine must enforce (each was a real, costly bug)

1. **Grid reconciliation** — never assume bg and stat share a `NeuroSpace`; check
   dims+affine, re-stamp/resample or error. (A first-level→group writer dropped the
   affine and silently broke overlay.)
2. **Non-empty overlay** — zero finite suprathreshold voxels must warn/error, not emit a
   blank PNG (an all-NaN map once rendered a "clean" empty figure).
3. **Label coverage** — every manifest row has a label, or hard error (§7).
4. **Effective-N surfacing** — show N per map and flag dropped subjects (e.g. 32 → 27).

These live in `validate_manifest()` and the engine, and are shared by the CLI's
`--validate`/dry-run path (§13).

---

## 11. Manifest sources / adapters

The engine is **storage-agnostic**: it consumes a validated manifest; adapters live at the edges.

- **glob + filename-entity parser** (BIDS entities) — MVP escape hatch.
- **TSV/CSV adapter** (e.g. `coefficient_niftis.csv`, `--design`) — MVP.
- **`neurotabs` (NFTab) adapter** — subject-level proof case.
- **`fmrigds` adapter** — group-level: a group-reduced GDS *is* "maps × contrast × model
  × variant," matching the group proof case. Kept **optional (`Suggests`)**.

---

## 12. Derived-map recipes

A manifest row may carry a `recipe` (function + inputs) instead of a `path`; the engine
materializes & caches it to disk. This subsumes the contrast report's inline z-difference
`z = (βA − βB)/√(seA² + seB²)` and future contrast/conjunction maps, keeping the report a
**pure renderer**. Default: **precompute-to-disk** (testable/cacheable); in-report thunks
are the escape hatch.

---

## 13. CLI, render strategy & Shiny explorer

### CLI dispatch contract (HIGH — current dispatch cannot host this as-is)
Today `.cli_prepare_report` (`R/cli.R:116`) hard-requires `--stat-map` and
`.cli_execute_command` (`R/cli.R:73`) always calls `cluster_report()`. A montage report
has **no single cluster-defining stat map**, so we add an explicit `--style` branch:

```
neuromosaic report --style montage --render-manifest m.tsv --labels labels.tsv \
  --layout 'contrast/model/variant' --p 0.005 --surface --atlas Schaefer400 --out report.html
```

- `--style cluster` (default) → existing `.cli_prepare_report` / `cluster_report()` path, unchanged.
- `--style montage` → a **separate** `montage` spec + executor calling
  `render_montage_report()`; **must not require `--stat-map`**.
- Add `--validate` / dry-run (runs `validate_manifest()` + QC, renders nothing) and
  dedicated `--help` for the montage style.
- Tests must prove a montage report builds with **no** single stat map and that
  `--validate` fails loudly on a label-less / empty / grid-mismatched manifest.
- The existing `neuromosaic manifest {create,validate,show}` verb stays nftab-scoped; the
  new artifact is a **render manifest** (`--render-manifest`), never conflated.
- Reuse `--path-template` / `--design` to *build* the render manifest.

### Renderer / template contract (HIGH — the rds "split" is `.qmd`-only)
`render_cluster_report` (`R/cr_render.R`) only writes the `_report-data.rds` sidecar for
**`.qmd` source output**; HTML/PDF render via `rmarkdown::render(params = list(...))`,
and qmd templates are **rejected** for HTML/PDF (cr_render.R:44–49). So:

- `render_montage_report()` gets its **own template selector**, parallel to
  `render_cluster_report`: a montage `.Rmd` for HTML/PDF (`rmarkdown::render`,
  `params = list(report_data = ...)`) and a montage `.qmd` for `.qmd` source export
  (with the `_report-data.rds` sidecar + `__REPORT_DATA_FILE__` substitution).
- **Image strategy:** surface montages write to a PNG *device* (not a ggplot object), so
  the template inlines them as base64 `<img>` in a `results='asis'` loop to preserve
  heading↔image order. Use `knitr::image_uri()` (or `base64enc`) — see §14.
- One generic template renders **any** layout nesting from the policy list (§8).

### Shiny explorer & assets
- **`explore`**: the same manifest drives the explorer (pick a row → montage + cluster
  table + design-linked signal plot).
- **Backgrounds/surfaces**: MNI152NLin2009cAsym res-02 + fsLR-32k (midthickness/inflated)
  fetched via **neuroatlas's** template API (`create_templateflow()` / `get_template()`),
  which itself wraps the **`templateflow`** package (`tf_get`/`tf_templates`). Its
  S3-backed cache (`templateflow::tf_home()` / `tf_cache_*`) is the **single canonical
  asset cache** — the engine manages **none**.

---

## 14. Dependencies & implementation notes

- Pin `neuroim2 (>= 0.16)` in DESCRIPTION (currently unpinned) for
  `plot_overlay(style = "report")`. Add `gifti`. For base64 image inlining,
  prefer `knitr::image_uri()` (already in the dependency tree via `knitr`); add
  `base64enc` only if a knitr-free path is needed.
- **fsLR availability (confirmed 2026-06-03):** `neuroatlas` fetches fsLR-32k via the
  **`templateflow`** package (`tf_get`/`tf_templates`); the engine calls neuroatlas's
  template API and relies on `templateflow`'s S3-backed cache (`tf_home()`) — no second
  cache here. `templateflow` is the asset backend (transitive via `neuroatlas`).
- **Surface layer / cache boundary:** the package already has a `vol_to_surf` projection
  layer with geometry caching in `R/ce_overlay.R` (defaulting to **fsaverage**, with an
  `.overlay_geom_cache` env). `R/AGENTS.md:30` forbids parallel caching logic. Decision
  (§9, §16): **generalize that layer to fsLR and reuse its cache**, rather than introduce
  a separate fsLR path/cache.
- `neurosurf`'s GIFTI reader is unreliable for TemplateFlow fsLR files — read via
  `gifti::readgii` + `SurfaceGeometry`.
- **Performance**: surface renders dominate; cache by `(map-hash, threshold, cap)`.
  Derived maps cache to disk.

---

## 15. Phased plan / milestones

**Milestone 1 is contract-first (per the 2026-06-03 review): the dispatch, schema, and
render contracts land before plotting.**

- **Phase 0 — Foundations & contracts.**
  - **P0** Rescope DESCRIPTION (Title/Description "clusters" → "statistical maps", keep
    cluster report first-class); pin `neuroim2 >= 0.16`; add `gifti`.
  - **C1 — CLI dispatch contract.** `--style` branch; separate montage spec + executor;
    montage path requires no `--stat-map`; `--validate`/dry-run; help; dispatch tests.
  - **C2 — Manifest schema + `validate_manifest()`.** Formal schema (§6, incl. parity
    fields) + the QC assertions (§10) as the shared contract.
  - **C3 — Renderer/template contract.** `render_montage_report()` template selector +
    HTML/PDF/`.qmd` output strategy + base64 image strategy (§13); skeleton template that
    renders an empty/fixture manifest end-to-end.
- **Phase 1 — Volume vertical slice (atlas-free), on top of the contracts.**
  `build_manifest()` (glob + TSV, conforms to C2); labeller + label-coverage;
  function-valued policy; `prepare_overlay()` + grid-reconcile; `stat_montage()` (lift
  `fmrireport`) + non-empty assertion; the generic montage template (C3) wired to real
  montages; `render_montage_report()` reachable via `report --style montage` (C1). **Proves the spine.**
- **Phase 2 — Surface montage.** `surf_montage()` by **generalizing `ce_overlay.R`** to
  fsLR (shared cap, single geometry cache); base64 `results='asis'` assembly; assets via neuroatlas.
- **Phase 3 — Atlas peak bridge.** Generalize peak→atlas labeling to a per-panel peak
  table; effective-N surfacing.
- **Phase 4 — Adapters.** `neurotabs` (subject); optional `fmrigds` (group).
- **Phase 5 — Reach.** Derived-map recipes + disk cache; Shiny explorer wiring; PDF polish.

**MVP cut** = Phase 0 (P0 + C1 + C2 + C3) + Phase 1: a volume-only, atlas-free montage
report, dispatched correctly, schema-validated, with the three assertions firing.

---

## 16. Open decisions

### Resolved
- **Home** → `neuromosaic`, rescoped (§4).
- **Manifest relationship** → distinct concept, adapter-sourced; never unified with nftab (§6, §11).
- **CLI** → `report --style montage` via a real dispatch branch + separate executor (§13).
- **Render strategy** → montage-specific template selector; HTML/PDF via `rmarkdown::render`
  params, `.qmd` via the rds sidecar; base64 via `knitr::image_uri()` (§13, §14).
- **Derived maps** → precompute-to-disk default; thunks as escape hatch (§12).
- **Layout / combined-vs-split docs** → a layout flag (function-valued), not a code change (§8).
- **Policy expressiveness** → function-valued plug-points; manifest carries per-map overrides (§8, §6).
- **`fmrireport` migration** onto the shared core → **out of scope this milestone**; follow-up (2026-06-03).
- **fsLR/TemplateFlow asset caching** → reuse **neuroatlas's** TemplateFlow cache; no separate cache (§13, §14) (2026-06-03).
- **Surface projection layer** → **generalize `ce_overlay.R` (fsaverage) to fsLR and reuse
  its geometry cache**; no parallel surface path/cache (§9, §14) (2026-06-03, per review).
- **Atlas annotation** → Phase 3, **not the MVP** (confirmed 2026-06-03).
- **fsLR retrieval / asset cache** → fetched via `neuroatlas`'s template API, which wraps
  the **`templateflow`** package; its S3-backed cache (`tf_home()`) is the single canonical
  asset cache — engine adds none (§13, §14) (confirmed 2026-06-03).

### Remaining (for review)
- None blocking — all prior open decisions resolved 2026-06-03. (Future, non-blocking:
  2-D matrix layouts, TFCE/FDR threshold policies, `fmrireport` migration — all deferred by design.)

---

## 17. Success metrics / acceptance

### MVP accepted (Phase 0 + Phase 1)
- `report --style montage` builds a volume-only report from a TSV/glob manifest with **no
  `--stat-map`** and renders HTML (the dispatch contract holds).
- `validate_manifest()` / `--validate` **fails loudly** on a label-less, empty-overlay, or
  grid-mismatched manifest (QC invariants 1–3 fire).
- A third study report can be produced with **zero engine edits** — only a builder,
  labeller, and policy (the duplication test passes) for the volume case.
- `R CMD check` clean; engine has **zero Shiny dependency** (preserves the existing split).

### Full PRD accepted
- Surface montages render with shared caps via the generalized `ce_overlay` layer (one cache).
- Per-panel atlas peak tables match the single-map report's labels for the same map.
- Effective-N (QC #4) surfaces dropped subjects.
- Both proof cases (subject-level contrast; 6×2×2 group) reproduce via the engine and adapters.

---

## 18. Risks

- **Surface rendering fragility** (PNG device, GIFTI reader, fsLR availability) — mitigated
  by isolating it to Phase 2 behind a stable `surf_montage()` contract built on the existing layer.
- **Dispatch/identity creep** — the `--style` branch must leave `--style cluster` behavior
  byte-for-byte unchanged; rescoping `neuromosaic` must keep the cluster report first-class.
- **Schema drift vs the single-map report** — parity fields (§6) and shared QC keep the two
  report families in agreement; the manifest is the firewall against absorbing stat logic.

---

## 19. Review history

- **2026-06-03 (rev 2):** Incorporated a code-review pass. Confirmed against source: the
  `report` verb hard-requires `--stat-map` and always calls `cluster_report()`
  (`R/cli.R:73,116`); the `_report-data.rds` sidecar is `.qmd`-only while HTML/PDF use
  `rmarkdown::render(params=)` (`R/cr_render.R:44–49,71–83`); `cluster_report()` exposes
  first-class `tail`/`connectivity`/`min_cluster_size` (`R/cr_report.R:88–108`); an
  fsaverage `vol_to_surf` + geometry-cache layer already exists (`R/ce_overlay.R:227–270`)
  under a no-parallel-cache rule (`R/AGENTS.md:30`). Restructured Milestone 1 to be
  **contract-first** (C1 dispatch, C2 schema+validator, C3 render/template), extended the
  manifest schema for parity, split MVP vs full acceptance, and resolved the surface-layer
  and base64 decisions.
