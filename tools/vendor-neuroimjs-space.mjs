import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  cpSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const root = process.cwd();
const version = '0.6.0';
const upstreamMain = '18cd39ff8fa63a37cab16bb8cf204900c448b77e';
const expectedSourceTree = '69ebc65e37b26a3ab2a33984c492165ce6bde6e8';
const budgetBytes = 2 * 1024 * 1024;
const source = process.env.NEUROIMJS_SPACE_SOURCE;
if (!source) throw new Error('Set NEUROIMJS_SPACE_SOURCE to a clean neuroimjs 0.6.0 checkout.');

const sourceRoot = resolve(source);
const packageMetadata = JSON.parse(readFileSync(resolve(sourceRoot, 'package.json'), 'utf8'));
if (packageMetadata.version !== version) {
  throw new Error(`Expected neuroimjs ${version}, found ${packageMetadata.version}.`);
}
const baseCommit = git(sourceRoot, 'rev-parse', 'HEAD');
const sourceTree = git(sourceRoot, 'rev-parse', 'HEAD^{tree}');
if (sourceTree !== expectedSourceTree) {
  throw new Error(`Expected neuroimjs source tree ${expectedSourceTree}, found ${sourceTree}.`);
}
const dirty = git(sourceRoot, 'status', '--porcelain', '--untracked-files=no');
if (dirty) throw new Error('NEUROIMJS_SPACE_SOURCE must be clean so the patch applies reproducibly.');

const patchPath = resolve(root, 'tools/patches/neuroimjs-space-0.6.0.patch');
const patchSha256 = sha256(readFileSync(patchPath));
const staging = mkdtempSync(resolve(tmpdir(), 'neuromosaic-space-vendor-'));
const runtimeDir = resolve(staging, 'consumer-runtime');
const destination = resolve(process.env.NEUROIMJS_SPACE_OUTPUT || resolve(root, 'inst/htmlwidgets/lib/neuromosaic-space/runtime'));
const buildEnv = { ...process.env, NPM_CONFIG_CACHE: process.env.NPM_CONFIG_CACHE ?? '/tmp/neuromosaic-npm-cache' };

try {
  // Build only the pinned tracked tree, never incidental untracked files.
  const archive = resolve(staging, 'source.tar');
  execFileSync('git', ['-C', sourceRoot, 'archive', '--format=tar', '-o', archive, baseCommit]);
  execFileSync('tar', ['-xf', archive, '-C', staging]);
  rmSync(archive);
  execFileSync('git', ['apply', '--whitespace=error', patchPath], { cwd: staging, stdio: 'inherit' });
  // Never inherit an ambient dependency installation from the source checkout.
  execFileSync('npm', ['ci', '--include=dev', '--no-audit', '--no-fund'], {
    cwd: staging, stdio: 'inherit', env: buildEnv,
  });
  const toolchain = {
    node: process.version, npm: execFileSync('npm', ['--version'], { encoding: 'utf8' }).trim(),
    platform: process.platform, arch: process.arch,
    install: 'npm ci --include=dev --no-audit --no-fund',
    lockfile_sha256: sha256(readFileSync(resolve(staging, 'package-lock.json'))),
    installed_lock_sha256: sha256(readFileSync(resolve(staging, 'node_modules/.package-lock.json'))),
    vendor_tool_sha256: sha256(readFileSync(resolve(root, 'tools/vendor-neuroimjs-space.mjs'))),
  };

  execFileSync('npm', ['run', 'build:space'], {
    cwd: staging,
    stdio: 'inherit',
    env: buildEnv,
  });
  const { build } = await import(pathToFileURL(resolve(staging, 'node_modules/esbuild/lib/main.js')).href);

  rmSync(runtimeDir, { recursive: true, force: true });
  mkdirSync(runtimeDir, { recursive: true });
  await build({
    entryPoints: [resolve(staging, 'src/space/index.ts')],
    outfile: resolve(runtimeDir, 'space-host.js'),
    bundle: true,
    format: 'iife',
    globalName: 'NeuroMosaicSpaceRuntime',
    platform: 'browser',
    target: ['es2022'],
    minify: true,
    legalComments: 'none',
    footer: { js: 'window.NeuroMosaicSpaceRuntime = NeuroMosaicSpaceRuntime;' },
  });
  cpSync(resolve(staging, 'dist/space/frame'), resolve(runtimeDir, 'frame'), { recursive: true });
  cpSync(resolve(staging, 'LICENSE'), resolve(runtimeDir, 'LICENSE-neuroimjs'));

  const paths = listFiles(runtimeDir)
    .filter((path) => path !== 'runtime.json')
    .sort();
  if (paths.some((path) => /(?:^|\/)(?:templateflow|data)(?:\/|$)|\.(?:nii|nii\.gz|tsv)$/i.test(path))) {
    throw new Error('The Space artifact contains demo or external data.');
  }
  if (paths.some((path) => readFileSync(resolve(runtimeDir, path), 'utf8').includes('fonts.googleapis.com'))) {
    throw new Error('The Space artifact must not load fonts from a CDN.');
  }
  const files = paths.map((path) => {
    const bytes = readFileSync(resolve(runtimeDir, path));
    return { path, bytes: bytes.length, sha256: sha256(bytes) };
  });
  const host = files.find((file) => file.path === 'space-host.js');
  if (!host) throw new Error('The public Space host bundle is missing.');
  const runtimeBytes = files
    .filter((file) => file.path !== 'LICENSE-neuroimjs')
    .reduce((total, file) => total + file.bytes, 0);
  if (runtimeBytes >= budgetBytes) {
    throw new Error(`Space runtime is ${runtimeBytes} bytes; budget is ${budgetBytes} bytes.`);
  }
  writeFileSync(resolve(runtimeDir, 'runtime.json'), `${JSON.stringify({
    name: 'neuroimjs-space',
    version,
    source_state: `git:${baseCommit}+${patchSha256}`,
    source_commit: baseCommit,
    source_tree: sourceTree,
    toolchain,
    upstream_main: upstreamMain,
    patch: { path: 'tools/patches/neuroimjs-space-0.6.0.patch', sha256: patchSha256 },
    public_entry: 'src/space/index.ts',
    global: 'window.NeuroMosaicSpaceRuntime',
    bundle: 'space-host.js',
    sha256: host.sha256,
    budget_bytes: budgetBytes,
    runtime_bytes: runtimeBytes,
    files,
  }, null, 2)}\n`);
  // Only replace shipped artifacts after the complete build passes validation.
  rmSync(destination, { recursive: true, force: true });
  cpSync(runtimeDir, destination, { recursive: true });
} finally {
  rmSync(staging, { recursive: true, force: true });
}

function git(cwd, ...args) {
  return execFileSync('git', ['-C', cwd, ...args], { encoding: 'utf8' }).trim();
}

function sha256(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

function listFiles(directory, prefix = '') {
  const files = [];
  for (const name of readdirSync(directory)) {
    const absolute = resolve(directory, name);
    const path = prefix ? `${prefix}/${name}` : name;
    if (lstatSync(absolute).isDirectory()) files.push(...listFiles(absolute, path));
    else files.push(path);
  }
  return files;
}
