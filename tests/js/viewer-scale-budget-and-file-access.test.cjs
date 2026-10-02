const test = require("node:test");
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { resolve } = require("node:path");
const { runInNewContext } = require("node:vm");

const lib = resolve(__dirname, "../../inst/htmlwidgets/lib");
const volumeDisplay = require(resolve(lib, "neuromosaic-volume/display.js"));
const surfaceDisplay = require(resolve(lib, "neuromosaic-surface/display.js"));

test("viewer scale numbers use the report's rounding (cap 22.2539 reads 22.3)", () => {
  const f = volumeDisplay.reportNumber;
  assert.equal(f(22.2539022445679), "22.3");
  assert.equal(f(-22.2539022445679), "−22.3");
  assert.equal(f(5), "5");
  assert.equal(f(-5), "−5");
  assert.equal(f(3.14159), "3.14");
  assert.equal(f(0.359), "0.359");
  assert.equal(f(0), "0");
  assert.equal(f(-0), "0");
  assert.equal(f(NaN), "");
});

test("an unlinked threshold band can never invert", () => {
  const band = volumeDisplay.orderThresholdBand;
  // Two-sided map: one cut per sign, low <= 0 <= high.
  assert.deepEqual(band([12, 8], "low", true), [0, 8]);
  assert.deepEqual(band([-5, -21], "high", true), [-5, 0]);
  assert.deepEqual(band([-6, 9], "low", true), [-6, 9]);
  // One-signed map: the edited end yields to the other.
  assert.deepEqual(band([12, 8], "low", false), [8, 8]);
  assert.deepEqual(band([3, 1], "high", false), [3, 3]);
  assert.deepEqual(band([2, 7], "high", false), [2, 7]);
});

test("reports opened from disk know which interactive views cannot load", () => {
  const bundled = { assets: [{ location: { kind: "relative", ref: "index_files/a.nii.gz" } }] };
  const embedded = { assets: [{ location: { kind: "embedded", ref: "nm-asset-1" } }] };
  assert.equal(volumeDisplay.needsServer("https:", bundled, true), false);
  assert.equal(volumeDisplay.needsServer("file:", bundled, false), true);
  assert.equal(volumeDisplay.needsServer("file:", embedded, true), true);
  assert.equal(volumeDisplay.needsServer("file:", embedded, false), false);

  const bundle = JSON.stringify({ assets: { v: { url: "index_files/interactive/surfaces/x.vertices.bin.gz" } } });
  const inline = JSON.stringify({ assets: { v: { url: "nm-surface-asset:nm-x" }, w: { url: "data:application/octet-stream;base64,AA" } } });
  assert.equal(surfaceDisplay.manifestNeedsServer("http:", bundle), false);
  assert.equal(surfaceDisplay.manifestNeedsServer("file:", bundle), true);
  assert.equal(surfaceDisplay.manifestNeedsServer("file:", inline), false);
});

test("surface hover values carry two decimals with a true minus", () => {
  const f = surfaceDisplay.formatFixed;
  assert.equal(f(-10), "−10.00");
  assert.equal(f(0.359), "0.36");
  assert.equal(f(-8.864), "−8.86");
  assert.equal(f(-0.001), "0.00");
  assert.equal(f(undefined), "");
});

// Load the volume adapter and the surface bridge into one page-like context.
function page() {
  const lost = new WeakSet();
  class FakeCanvas {
    getContext(type) {
      if (!/webgl/.test(type)) return null;
      if (!this.gl) {
        const canvas = this;
        this.gl = {
          isContextLost: () => lost.has(canvas),
          getExtension: (name) => name === "WEBGL_lose_context" ?
            { loseContext: () => lost.add(canvas) } : null
        };
      }
      return this.gl;
    }
  }
  const context = {
    HTMLCanvasElement: FakeCanvas,
    document: {
      readyState: "loading",
      addEventListener() {},
      querySelector() { return null; },
      querySelectorAll() { return []; }
    },
    location: { protocol: "http:" },
    console
  };
  context.window = context;
  context.window.addEventListener = () => {};
  runInNewContext(readFileSync(resolve(lib, "neuromosaic-volume/adapter.js"), "utf8"), context);
  runInNewContext(readFileSync(resolve(lib, "neuromosaic-surface/bridge.js"), "utf8"), context);
  return { context, FakeCanvas };
}

test("one WebGL budget is shared by the volume and surface viewers", () => {
  const { context } = page();
  const budget = context.NeuroMosaicGL;
  assert.ok(budget, "the adapter creates the page budget");
  assert.equal(budget.budget, 8);
  const released = [];
  const shown = new Set();
  const entry = (key, cost) => ({
    key, cost,
    shown: () => shown.has(key),
    release: () => released.push(key)
  });
  // Three Slices viewers (3 contexts each): the third evicts the oldest
  // hidden one.
  budget.touch(entry("volume:a", 3));
  budget.touch(entry("volume:b", 3));
  shown.add("volume:c");
  budget.touch(entry("volume:c", 3));
  assert.deepEqual(released, ["volume:a"]);
  assert.deepEqual(Array.from(budget.keys()), ["volume:b", "volume:c"]);
  // Re-using b makes it most recent; a surface (1 context) then fits.
  budget.touch(entry("volume:b", 3));
  budget.touch(entry("surface:a", 1));
  assert.deepEqual(released, ["volume:a"]);
  // A fourth viewer evicts the least recently used hidden one (c is shown).
  budget.touch(entry("volume:d", 3));
  assert.deepEqual(released, ["volume:a", "volume:b"]);
  assert.ok(Array.from(budget.keys()).includes("volume:c"));
  // Explicitly disposed viewers leave the budget without a release call.
  budget.drop("volume:d");
  assert.ok(!Array.from(budget.keys()).includes("volume:d"));
});

test("released viewers hand their WebGL contexts back to the browser", () => {
  const { context, FakeCanvas } = page();
  const budget = context.NeuroMosaicGL;
  const gl = new FakeCanvas();
  const plain = new FakeCanvas();
  gl.getContext("webgl2");
  assert.equal(gl.__nmGL, gl.gl, "contexts are remembered as they are created");
  assert.equal(budget.isLost([gl]), false);
  budget.loseContexts([gl, plain]);
  assert.equal(gl.__nmReleased, true);
  assert.equal(gl.gl.isContextLost(), true);
  // A canvas that never had a WebGL context is left alone (no context is
  // created just to release it).
  assert.equal(plain.gl, undefined);
  const container = { querySelectorAll: () => [gl] };
  assert.equal(budget.isLost(container), true);
});
