const test = require("node:test");
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { resolve } = require("node:path");
const display = require(
  "../../inst/htmlwidgets/lib/neuromosaic-volume/display.js"
);

const lib = resolve(__dirname, "../../inst/htmlwidgets/lib");

test("the report palette is the volume viewer's signed default", () => {
  const maps = ["Report (nm-signed)", "BlueRed", "Inferno"];
  const resolved = display.resolve({
    mode: "thresholded", scale: "diverging", limits: [-10, 10],
    threshold: 5, tail: "two_sided", palette: "nm-signed", alpha: 0.9
  }, [-12, 12], maps);
  assert.equal(resolved.colormapName, "Report (nm-signed)");
  assert.throws(() => display.paletteName(
    { palette: "nm-signed", scale: "sequential" }, maps
  ), /incompatible/);
});

test("the volume adapter curates presets and uses the report connectivity", () => {
  const adapter = readFileSync(resolve(lib, "neuromosaic-volume/adapter.js"), "utf8");
  for (const name of ["Set1", "Pastel1", "Paired", "Greys"]) {
    assert.ok(adapter.includes(`"${name}"`), `${name} is hidden`);
  }
  assert.match(adapter, /neighbourOffsets\(connectivity\)/);
  assert.doesNotMatch(adapter, /Display modified for " \+ state\.currentMapId/);
  assert.match(adapter, /nm-volume-hidden/);
  assert.match(adapter, /aria-valuetext/);
});

test("the surface bridge keeps the router in sync and curates colour maps", () => {
  const bridge = readFileSync(resolve(lib, "neuromosaic-surface/bridge.js"), "utf8");
  assert.match(bridge, /addEventListener\("nm-volume-hidden"/);
  assert.match(bridge, /data-nm-view-slot/);
  for (const name of ["jet", "hsv", "rainbow", "phase"]) {
    assert.ok(!new RegExp(`CURATED_COLOR_MAPS = \\[[^\\]]*"${name}"`).test(bridge),
      `${name} is not offered`);
  }
});
