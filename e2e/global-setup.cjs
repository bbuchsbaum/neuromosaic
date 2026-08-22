const { existsSync } = require("node:fs");
const { resolve } = require("node:path");
const { spawnSync } = require("node:child_process");

module.exports = async function globalSetup() {
  const repoRoot = resolve(__dirname, "..");
  const fixtureScript = resolve(__dirname, "render-fixtures.R");
  const result = spawnSync("Rscript", [
    "-e",
    `source(${JSON.stringify(fixtureScript)}, chdir = FALSE)`
  ], {
    cwd: repoRoot,
    encoding: "utf8",
    env: {
      ...process.env,
      LANG: "C",
      LC_ALL: "C"
    }
  });

  if (result.status !== 0) {
    const details = [result.stdout, result.stderr].filter(Boolean).join("\n");
    throw new Error(`Unable to render browser-test fixtures.\n${details}`);
  }

  for (const name of [
    "selector-auto.html",
    "selector-none.html",
    "interactive-volume.html",
    "interactive-volume-bundle.html"
  ]) {
    const output = resolve(__dirname, ".artifacts", name);
    if (!existsSync(output)) {
      throw new Error(`Browser-test fixture was not created: ${output}`);
    }
  }
};
