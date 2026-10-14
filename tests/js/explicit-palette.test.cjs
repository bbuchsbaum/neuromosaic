const test = require("node:test");
const assert = require("node:assert/strict");
const display = require("../../inst/htmlwidgets/lib/neuromosaic-volume/display.js");

const colors = ["#F7FCF5", "#74C47680", "#00441B"];
const spec = (extra = {}) => ({
  mode: "thresholded", scale: "sequential", limits: [0, 8], threshold: 3,
  tail: "positive", palette: "custom:character", palette_colors: colors,
  alpha: 0.7, ...extra
});

test("explicit tables preserve color order and alpha without preset lookup", () => {
  const out = display.resolve(spec(), [-10, 10], []);
  assert.equal(out.colormapName, "Custom");
  assert.deepEqual(out.colormapColors, [
    [247 / 255, 252 / 255, 245 / 255, 1],
    [116 / 255, 196 / 255, 118 / 255, 128 / 255],
    [0, 68 / 255, 27 / 255, 1]
  ]);
  assert.deepEqual(out.range, [0, 8]);
  assert.equal(out.alpha, 0.7);
  assert.deepEqual(colors, ["#F7FCF5", "#74C47680", "#00441B"]);
  for (const palette of ["Greens", "Blues", "Reds"]) {
    assert.deepEqual(display.resolve(spec({ palette }), [-10, 10], []).colormapColors,
      out.colormapColors, "explicit colors take precedence over names");
  }
});

test("custom colors retain all tail, range and continuous display rules", () => {
  for (const tail of ["positive", "negative", "two_sided"]) {
    const input = spec({ tail, scale: "diverging", limits: [-8, 8] });
    assert.deepEqual(display.resolve(input, [-10, 10], []).threshold,
      display.thresholdPair(input, [-10, 10]));
  }
  assert.deepEqual(display.resolve(spec({ mode: "continuous", threshold: null }),
    [-10, 10], []).threshold, [0, 0]);
  assert.throws(() => display.resolve(spec({ limits: [8, 0] }), [-10, 10], []),
    /Display limits/);
  assert.throws(() => display.resolve(spec({ tail: "bogus" }), [-10, 10], []),
    /Threshold tail/);
});

test("malformed tables fail instead of silently selecting another palette", () => {
  for (const palette_colors of [[], ["#FFFFFF"], "Greens", ["red", "blue"],
    ["#12345G", "#FFFFFF"], ["#000000", null], [[0, 0, 0], [1, 1, 1]]]) {
    assert.throws(() => display.resolve(spec({ palette_colors }), [-10, 10], []),
      /palette_colors/);
  }
  assert.throws(() => display.resolve(spec({ palette_colors: null }), [-10, 10], []),
    /no neuroimjs translation/);
});

test("the threshold-saturated report palette still uses its existing preset", () => {
  const out = display.resolve(spec({ palette: "nm-signed", scale: "diverging",
    limits: [-8, 8], tail: "two_sided" }), [-10, 10], []);
  assert.equal(out.colormapName, "Report (nm-signed)");
});
