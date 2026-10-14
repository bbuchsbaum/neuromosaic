const test = require('node:test');
const assert = require('node:assert/strict');
const { createHash } = require('node:crypto');
const { readFileSync, statSync } = require('node:fs');
const { resolve } = require('node:path');
const { runInNewContext } = require('node:vm');

const runtimeDir = resolve(__dirname, '../../inst/htmlwidgets/lib/neuromosaic-space/runtime');
const manifest = JSON.parse(readFileSync(resolve(runtimeDir, 'runtime.json'), 'utf8'));

const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

test('the committed Space runtime matches its patch and file manifest', () => {
  assert.equal(manifest.name, 'neuroimjs-space');
  assert.equal(manifest.version, '0.6.0');
  assert.equal(manifest.source_state, `git:${manifest.source_commit}+${manifest.patch.sha256}`);
  assert.equal(manifest.source_tree, '69ebc65e37b26a3ab2a33984c492165ce6bde6e8');
  assert.equal(manifest.public_entry, 'src/space/index.ts');
  assert.equal(manifest.global, 'window.NeuroMosaicSpaceRuntime');
  assert.equal(manifest.toolchain.install, 'npm ci --include=dev --no-audit --no-fund');
  assert.match(manifest.toolchain.node, /^v\d+\./);
  assert.match(manifest.toolchain.npm, /^\d+\./);
  assert.match(manifest.toolchain.lockfile_sha256, /^[a-f0-9]{64}$/);
  assert.match(manifest.toolchain.installed_lock_sha256, /^[a-f0-9]{64}$/);
  assert.equal(manifest.toolchain.vendor_tool_sha256,
    sha256(readFileSync(resolve(__dirname, '../../tools/vendor-neuroimjs-space.mjs'))));
  assert.equal(
    manifest.patch.sha256,
    sha256(readFileSync(resolve(__dirname, '../../tools/patches/neuroimjs-space-0.6.0.patch'))),
    'upstream correction patch SHA-256'
  );
  for (const file of manifest.files) {
    const path = resolve(runtimeDir, file.path);
    const bytes = readFileSync(path);
    assert.equal(statSync(path).size, file.bytes, `${file.path} byte size`);
    assert.equal(sha256(bytes), file.sha256, `${file.path} SHA-256`);
  }
  const bundle = manifest.files.find((file) => file.path === manifest.bundle);
  assert.equal(bundle.sha256, manifest.sha256);
  const runtimeBytes = manifest.files
    .filter((file) => file.path !== 'LICENSE-neuroimjs')
    .reduce((total, file) => total + file.bytes, 0);
  assert.equal(manifest.runtime_bytes, runtimeBytes);
  assert.equal(manifest.budget_bytes, 2 * 1024 * 1024);
  assert.ok(runtimeBytes < manifest.budget_bytes, `Space runtime is ${runtimeBytes} bytes`);
});

test('the Space artifact includes its frame workers and public mountSpace host without demo data', () => {
  const paths = manifest.files.map((file) => file.path);
  assert.equal(manifest.bundle, 'space-host.js');
  assert.ok(paths.includes('frame/index.html'));
  assert.ok(paths.some((path) => /\.worker-[^/]+\.js$/.test(path)), 'frame worker bundle');
  assert.equal(paths.some((path) => /(?:^|\/)(?:templateflow|data)(?:\/|$)|\.(?:nii|nii\.gz|tsv)$/i.test(path)), false);
  assert.equal(paths.some((path) => readFileSync(resolve(runtimeDir, path), 'utf8').includes('fonts.googleapis.com')), false);
  const frame = readFileSync(resolve(runtimeDir, 'frame/index.html'), 'utf8');
  assert.match(frame, /@media \(max-width: 640px\)/);
  assert.match(frame, /grid-template-rows: minmax\(420px, 65vh\) auto/);
  assert.match(frame, /#side \{\s*border-left: 0;\s*border-top: 1px solid var\(--line\);\s*overflow: visible;/);

  const context = {};
  context.window = context;
  runInNewContext(readFileSync(resolve(runtimeDir, manifest.bundle), 'utf8'), context);
  assert.equal(typeof context.NeuroMosaicSpaceRuntime.mountSpace, 'function');
  assert.equal(context.window.NeuroMosaicSpaceRuntime, context.NeuroMosaicSpaceRuntime);
});
