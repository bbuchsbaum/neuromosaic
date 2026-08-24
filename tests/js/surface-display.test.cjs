const test = require("node:test");
const assert = require("node:assert/strict");
const path = require("node:path");

const display = require(path.resolve(
  __dirname,
  "../../inst/htmlwidgets/lib/neuromosaic-surface/display.js"
));

function layer(id = "z-map") {
  return {
    id,
    opacity: 0.85,
    scalarMapping: {
      colorMap: { id: "RdBu" },
      displayRange: { value: [-6, 6] },
      maskInterval: { value: [-3.1, 3.1] }
    }
  };
}

test("surface display defaults are captured once and reset exactly", () => {
  const store = display.createDefaultStore();
  const current = layer();
  const defaults = store.capture(current);

  current.opacity = 0.35;
  current.scalarMapping.colorMap.id = "viridis";
  current.scalarMapping.displayRange.value = [-8, 8];
  current.scalarMapping.maskInterval.value = [-4, 4];
  assert.equal(display.isModified(current, defaults), true);
  assert.strictEqual(store.capture(current), defaults);
  assert.deepEqual(store.resetUpdate("z-map"), {
    scalar: {
      colorMapId: "RdBu",
      displayRange: [-6, 6],
      maskInterval: [-3.1, 3.1]
    },
    opacity: 0.85
  });
});

test("surface display state is map-local", () => {
  const store = display.createDefaultStore();
  const z = layer("z");
  const se = layer("se");
  se.opacity = 1;
  se.scalarMapping.colorMap.id = "inferno";
  se.scalarMapping.displayRange.value = [0, 0.5];
  se.scalarMapping.maskInterval.value = [0, 0];

  store.capture(z);
  store.capture(se);
  assert.equal(store.get("z").colorMapId, "RdBu");
  assert.equal(store.get("se").colorMapId, "inferno");
  assert.equal(store.get("se").opacity, 1);
  assert.equal(store.get("missing"), null);
});

test("surface range validation fails closed", () => {
  assert.deepEqual(display.orderedFinitePair("-3", "4"), [-3, 4]);
  assert.equal(display.orderedFinitePair("5", "4"), null);
  assert.equal(display.orderedFinitePair("not-a-number", "4"), null);
  assert.equal(display.orderedFinitePair("-3", "Infinity"), null);
});

test("surface map routing accepts only declared string layer ids", () => {
  const mapping = { z: "layer-z", se: "layer-se", bad: 12 };
  assert.equal(display.layerIdForMap(mapping, "z"), "layer-z");
  assert.equal(display.layerIdForMap(mapping, "missing"), null);
  assert.equal(display.layerIdForMap(mapping, "bad"), null);
  assert.equal(display.layerIdForMap(null, "z"), null);
});

test("surface modification comparison tolerates numerical roundoff only", () => {
  const current = layer();
  const defaults = display.layerDefaults(current);
  current.scalarMapping.displayRange.value = [-6 - 1e-12, 6 + 1e-12];
  assert.equal(display.isModified(current, defaults), false);
  current.scalarMapping.displayRange.value = [-6.01, 6];
  assert.equal(display.isModified(current, defaults), true);
  assert.throws(() => display.layerDefaults({ id: "bad" }), /scalar surface layer/);
});
