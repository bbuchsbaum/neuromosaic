import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { copyFileSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";

const root = process.cwd();
const runtimeDir = resolve(
  root, "inst/htmlwidgets/lib/neuromosaic-volume"
);
const manifestPath = resolve(runtimeDir, "runtime.json");
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
// NEUROIMJS_SOURCE=/path/to/neuroimjs vendors a local checkout (after
// `npm run build:vite` there) instead of the npm package, e.g. before a
// release is published; the manifest then records the exact commit.
const localSource = process.env.NEUROIMJS_SOURCE;
const packageRoot = localSource ?
  resolve(localSource) :
  resolve(root, "node_modules/neuroimjs");
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
manifest.source_state = localSource ?
  gitSourceState(packageRoot, metadata.version) :
  `npm:neuroimjs@${metadata.version}`;
writeFileSync(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`);

function gitSourceState(dir, version) {
  const git = (...args) => execFileSync("git", ["-C", dir, ...args], {
    encoding: "utf8"
  }).trim();
  const commit = git("rev-parse", "HEAD");
  const dirty = git("status", "--porcelain", "--untracked-files=no") !== "";
  return `git:neuroimjs@${version}+${commit}${dirty ? "-dirty" : ""}`;
}
