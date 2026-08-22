const test = require("node:test");
const assert = require("node:assert/strict");
const geometry = require("../../inst/htmlwidgets/lib/neuromosaic-volume/geometry.js");

function sceneGeometry() {
  return {
    dimensions: [5, 6, 7],
    spacing: [2, 2, 2],
    origin: [-5, -6, -7],
    orientation: "RAS",
    affine: [
      2, 0, 0, 0,
      0, 2, 0, 0,
      0, 0, 2, 0,
      -5, -6, -7, 1
    ],
    fingerprint: "md5:fixture"
  };
}

test("decoded geometry matches the scene within tolerance", () => {
  const expected = sceneGeometry();
  const decoded = {
    dims: [5, 6, 7],
    affine: expected.affine.map((x, i) => i === 12 ? x + 5e-7 : x),
    fingerprint: expected.fingerprint
  };

  const normalized = geometry.assertGeometryMatch(
    expected, decoded, "map-z", geometry.DEFAULT_TOLERANCE
  );
  assert.deepEqual(normalized.dimensions, [5, 6, 7]);
  assert.equal(normalized.orientation, "RAS");
});

test("decoded geometry fails closed for every spatial invariant", () => {
  const expected = sceneGeometry();
  const cases = [
    ["dimensions", { dimensions: [4, 6, 7] }],
    ["spacing", { spacing: [3, 2, 2] }],
    ["origin", { origin: [-4, -6, -7] }],
    ["orientation", { orientation: "LAS" }],
    ["affine", { affine: expected.affine.map((x, i) => i === 0 ? 3 : x) }],
    ["fingerprint", { fingerprint: "md5:different" }]
  ];

  for (const [field, change] of cases) {
    const decoded = Object.assign({}, expected, change);
    assert.throws(
      () => geometry.assertGeometryMatch(expected, decoded, "map-se"),
      new RegExp(field)
    );
  }
});

test("missing decoded geometry never mounts", () => {
  assert.throws(
    () => geometry.assertGeometryMatch(sceneGeometry(), null, "map-beta"),
    /geometry is missing/
  );
});
