// Required, freshly rendered Quarto fixture from a relocated portable bundle.
const { cpSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } = require('node:fs');
const { tmpdir } = require('node:os');
const { resolve, delimiter } = require('node:path');
const { spawnSync } = require('node:child_process');

module.exports = function renderSpaceQuarto() {
  const root = resolve(__dirname, '..'), artifacts = resolve(__dirname, '.artifacts');
  const destination = resolve(artifacts, 'space-quarto');
  rmSync(destination, { recursive: true, force: true });
  rmSync(resolve(artifacts, 'space-report-quarto.html'), { force: true });
  const staging = mkdtempSync(resolve(tmpdir(), 'neuromosaic-quarto-'));
  const library = resolve(staging, 'library'), report = resolve(staging, 'report');
  mkdirSync(library); mkdirSync(report);
  const env = { ...process.env, RGL_USE_NULL: 'TRUE', LANG: 'en_US.UTF-8', LC_ALL: 'en_US.UTF-8',
    R_LIBS: [library, process.env.R_LIBS].filter(Boolean).join(delimiter) };
  const logs = [];
  const run = (command, args, cwd) => {
    const result = spawnSync(command, args, { cwd, env, encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 });
    logs.push([command, ...args].join(' '), result.stdout || '', result.stderr || '', `exit=${result.status}`);
    if (result.error || result.status !== 0) throw new Error(
      `Required Quarto fixture failed: ${command}. Install Quarto and inspect e2e/.artifacts/space-quarto-build.log. ${result.error || ''}`);
  };
  try {
    run('quarto', ['--version'], staging);
    run('R', ['CMD', 'INSTALL', '--library=' + library, root], staging);
    for (const path of ['space-report.qmd', 'space-report_report-data.rds', 'space-report_files']) {
      cpSync(resolve(artifacts, path), resolve(report, path), { recursive: true });
    }
    run('quarto', ['render', 'space-report.qmd', '--to', 'html', '--output', 'space-report.html'], report);
    cpSync(report, destination, { recursive: true });
  } finally {
    writeFileSync(resolve(artifacts, 'space-quarto-build.log'), logs.join('\n'));
    rmSync(staging, { recursive: true, force: true });
  }
};
if (require.main === module) module.exports();
