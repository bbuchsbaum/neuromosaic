// Re-record sizes and SHA-256 hashes of the shipped volume runtime files after
// editing adapter/display/geometry sources (the neuroimjs bundle is unchanged).
import { createHash } from "node:crypto";
import { readFileSync, statSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";

const runtimeDir = resolve(process.cwd(), "inst/htmlwidgets/lib/neuromosaic-volume");
const manifestPath = resolve(runtimeDir, "runtime.json");
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
for (const file of manifest.files) {
  const path = resolve(runtimeDir, file.path);
  file.bytes = statSync(path).size;
  file.sha256 = createHash("sha256").update(readFileSync(path)).digest("hex");
}
const bundle = manifest.files.find((file) => file.path === manifest.bundle);
if (!bundle || bundle.sha256 !== manifest.sha256) {
  throw new Error("The neuroimjs bundle changed; re-vendor it with tools/vendor-neuroimjs.mjs.");
}
writeFileSync(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`);
