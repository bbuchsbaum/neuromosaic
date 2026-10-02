const test = require("node:test");
const assert = require("node:assert/strict");
const { createHash } = require("node:crypto");
const { readFileSync, statSync } = require("node:fs");
const { resolve } = require("node:path");

const runtimeDir = resolve(
  __dirname, "../../inst/htmlwidgets/lib/neuromosaic-volume"
);
const manifest = JSON.parse(readFileSync(
  resolve(runtimeDir, "runtime.json"), "utf8"
));

test("the committed interactive runtime matches its reproducibility manifest", () => {
  for (const file of manifest.files) {
    const path = resolve(runtimeDir, file.path);
    const bytes = readFileSync(path);
    assert.equal(statSync(path).size, file.bytes, `${file.path} byte size`);
    assert.equal(
      createHash("sha256").update(bytes).digest("hex"),
      file.sha256,
      `${file.path} SHA-256`
    );
  }
  const bundle = manifest.files.find((file) => file.path === manifest.bundle);
  assert.equal(bundle.sha256, manifest.sha256);
});

test("the inline browser runtime remains within its reviewed size envelope", () => {
  const total = manifest.files
    .filter((file) => !file.path.startsWith("LICENSE"))
    .reduce((sum, file) => sum + file.bytes, 0);
  // Reviewed envelope, raised in round 4 from 1.1 MiB to 1.12 MiB
  // (r3 1,142,306 B -> r4 1,167,666 B; neuroimjs itself is 1,017,278 B).
  // The growth pays for the WebGL park/resume lifecycle, the underlay's
  // light figure ground and smoothed rim, cap triangles and the threshold
  // note in exported keys, colour-range validation and the below-threshold
  // readout. In bundle mode the runtime loads only when a reader first opens
  // Slices, so first-load weight is unaffected.
  assert.ok(total < 1.12 * 1024 * 1024, `runtime is ${total} bytes`);
});

test("the running adapter version agrees with its reproducibility manifest", () => {
  const { runInNewContext } = require("node:vm");
  const context = {
    document: { readyState: "loading", addEventListener() {} },
    window: { addEventListener() {} }
  };
  runInNewContext(readFileSync(resolve(runtimeDir, "adapter.js"), "utf8"), context);
  assert.equal(context.NeuroMosaicVolumeReport.version, manifest.adapter_version);
  assert.equal(context.NeuroMosaicVolumeReport.runtimeVersion, manifest.version);
});
