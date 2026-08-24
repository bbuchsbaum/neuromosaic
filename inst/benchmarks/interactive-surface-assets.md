# Interactive surface asset benchmark

Recorded 2026-08-24 on macOS arm64 with R 4.5.1. Reproduce from the repository
root with:

```sh
RGL_USE_NULL=TRUE Rscript tools/benchmark-interactive-surface-assets.R
```

The deterministic fixture is bilateral and fsaverage6-sized: 40,962 vertices
and 81,920 faces per hemisphere, with four full-vertex maps. It is a payload
benchmark, not an anatomical or frame-rate benchmark. Identical face topology
is deliberately reused between hemispheres, exercising content-addressed
deduplication.

| Measure | Result |
|---|---:|
| Unique assets | 11 |
| Geometry, raw | 1.88 MiB |
| Geometry, gzip | 0.98 MiB |
| Four maps, raw | 1.25 MiB |
| Four maps, gzip | 1.10 MiB |
| Total, raw | 3.13 MiB |
| Total, gzip bundle | 2.08 MiB |
| Total, gzip plus base64 embedding | 2.77 MiB |
| SurfaceScene serialization | 0.026 s |
| Gzip level 9 preparation | 0.140 s |

These numbers cover the typed arrays only. The surfview runtime is recorded and
hashed separately in report provenance. Browser mounting remains lazy and is
covered by Playwright; runtime and HTML dependency bytes are therefore not
charged to the per-scene data figures above.
