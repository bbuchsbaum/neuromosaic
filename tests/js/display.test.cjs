const test = require("node:test");
const assert = require("node:assert/strict");
const display = require(
  "../../inst/htmlwidgets/lib/neuromosaic-volume/display.js"
);

const maps = ["BlueRed", "RdBu", "Viridis", "Inferno", "Grayscale"];

function spec(overrides = {}) {
  return {
    mode: "thresholded",
    scale: "diverging",
    limits: [-8, 8],
    threshold: 3.1,
    tail: "two_sided",
    palette: "blue-red",
    alpha: 0.7,
    ...overrides
  };
}

test("tail semantics preserve inclusive report thresholds", () => {
  assert.deepEqual(
    display.resolve(spec(), [-7, 9], maps).threshold,
    [-3.1, 3.1]
  );
  const positive = display.resolve(spec({ tail: "positive" }), [-7, 9], maps);
  assert.ok(positive.threshold[0] < -7);
  assert.equal(positive.threshold[1], 3.1);
  const negative = display.resolve(spec({ tail: "negative" }), [-7, 9], maps);
  assert.equal(negative.threshold[0], -3.1);
  assert.ok(negative.threshold[1] > 9);
  const constant = display.resolve(spec({ tail: "positive" }), [4.5, 4.5], maps);
  assert.ok(constant.threshold[0] < 3.1);
  assert.equal(constant.threshold[1], 3.1);
});

test("continuous maps disable threshold masking", () => {
  const resolved = display.resolve(spec({
    mode: "continuous",
    scale: "sequential",
    limits: [0, 2],
    threshold: null,
    palette: "inferno"
  }), [0, 1.5], maps);
  assert.deepEqual(resolved.threshold, [0, 0]);
  assert.equal(resolved.colormapName, "Inferno");
});

test("palette translation preserves sequential and diverging meaning", () => {
  assert.equal(display.paletteName(spec({ palette: "vik" }), maps), "BlueRed");
  assert.equal(display.paletteName(spec({ palette: "RdBu" }), maps), "RdBu");
  assert.throws(
    () => display.paletteName(spec({ palette: "Inferno" }), maps),
    /incompatible with diverging/
  );
  assert.throws(
    () => display.paletteName(spec({ palette: "custom:closure" }), maps),
    /no neuroimjs translation/
  );
});

test("non-finite and contradictory display states fail clearly", () => {
  assert.throws(() => display.resolve(spec({ limits: [0, Infinity] }), [-1, 1], maps),
    /Display limits/);
  assert.throws(() => display.resolve(spec({ threshold: 0 }), [-1, 1], maps),
    /positive finite threshold/);
  assert.throws(() => display.resolve(spec({ alpha: 2 }), [-1, 1], maps),
    /between zero and one/);
  assert.throws(() => display.resolve(spec({ tail: "upper" }), [-1, 1], maps),
    /Threshold tail/);
});
