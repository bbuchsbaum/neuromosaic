const { existsSync } = require("node:fs");
const { resolve } = require("node:path");
const { spawnSync } = require("node:child_process");

module.exports = async function globalSetup() {
  const repoRoot = resolve(__dirname, "..");
  const fixtureScript = resolve(__dirname, "render-fixtures.R");
  const spaceFixtureScript = resolve(__dirname, "build-space-fixture.R");
  const fixedScalesScript = resolve(__dirname, "build-fixed-scales-fixture.R");
  const result = spawnSync("Rscript", [
    "-e",
    `source(${JSON.stringify(fixtureScript)}, chdir = FALSE); source(${JSON.stringify(spaceFixtureScript)}, chdir = FALSE); source(${JSON.stringify(fixedScalesScript)}, chdir = FALSE)`
  ], {
    cwd: repoRoot,
    encoding: "utf8",
    env: {
      ...process.env,
      LANG: "en_US.UTF-8",
      LC_ALL: "en_US.UTF-8",
      RGL_USE_NULL: "TRUE"
    }
  });

  if (result.status !== 0) {
    const details = [result.stdout, result.stderr].filter(Boolean).join("\n");
    throw new Error(`Unable to render browser-test fixtures.\n${details}`);
  }
  require('./render-space-quarto.cjs')();

  for (const name of [
    "selector-auto.html",
    "selector-none.html",
    "interactive-volume.html",
    "interactive-volume-bundle.html",
    "interactive-volume-fine.html",
    "interactive-surface.html",
    "interactive-surface-bundle.html",
    "parcel-surface-visual.html",
    "interactive-mixed.html",
    "fixed-scales.html",
    "space-report.html",
    "space-single.html",
    "space-quarto/space-report.html"
  ]) {
    const output = resolve(__dirname, ".artifacts", name);
    if (!existsSync(output)) {
      throw new Error(`Browser-test fixture was not created: ${output}`);
    }
  }
};
