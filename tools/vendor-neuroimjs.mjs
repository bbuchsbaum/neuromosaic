import { createHash } from "node:crypto";
import { copyFileSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";

const root = process.cwd();
const runtimeDir = resolve(
  root, "inst/htmlwidgets/lib/neuromosaic-volume"
);
const manifestPath = resolve(runtimeDir, "runtime.json");
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const packageRoot = resolve(root, "node_modules/neuroimjs");
const metadata = JSON.parse(readFileSync(
  resolve(packageRoot, "package.json"), "utf8"
));

if (metadata.version !== manifest.version) {
  throw new Error(
    `Expected neuroimjs ${manifest.version}, found ${metadata.version}.`
  );
}

copyFileSync(
  resolve(packageRoot, "dist/neuroimjs.umd.js"),
  resolve(runtimeDir, manifest.bundle)
);
copyFileSync(
  resolve(packageRoot, "LICENSE"),
  resolve(runtimeDir, "LICENSE-neuroimjs")
);

for (const file of manifest.files) {
  const path = resolve(runtimeDir, file.path);
  const contents = readFileSync(path);
  file.bytes = statSync(path).size;
  file.sha256 = createHash("sha256").update(contents).digest("hex");
}
const bundle = manifest.files.find((file) => file.path === manifest.bundle);
if (!bundle) throw new Error("Runtime manifest does not list its UMD bundle.");
manifest.sha256 = bundle.sha256;
manifest.source_state = `npm:neuroimjs@${metadata.version}`;
writeFileSync(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`);
