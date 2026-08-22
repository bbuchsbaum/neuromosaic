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
  assert.ok(total < 1.1 * 1024 * 1024, `runtime is ${total} bytes`);
});
