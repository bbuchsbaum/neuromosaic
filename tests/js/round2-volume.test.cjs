const test = require("node:test");
const assert = require("node:assert/strict");
const display = require(
  "../../inst/htmlwidgets/lib/neuromosaic-volume/display.js"
);

// A 6 x 6 x 6 volume in NIfTI order (i fastest), every voxel 0 or NaN unless
// set below.
const dims = [6, 6, 6];
const index = (i, j, k) => (k * dims[1] + j) * dims[0] + i;

function volume(cells) {
  const data = new Float64Array(dims[0] * dims[1] * dims[2]);
  for (const [i, j, k, v] of cells) data[index(i, j, k)] = v;
  return data;
}

function excludedCells(mask) {
  const out = [];
  if (!mask) return out;
  for (let n = 0; n < mask.length; n += 1) {
    if (!mask[n]) continue;
    out.push([n % dims[0], Math.floor(n / dims[0]) % dims[1],
      Math.floor(n / (dims[0] * dims[1]))]);
  }
  return out;
}

// Positive cluster A: a 2 x 2 x 1 block (4 voxels) at z = 0.
// Positive pair B: two voxels meeting only along an edge (18-, not
// 6-connected) at z = 3.
// Positive pair C: two voxels meeting only at a corner (26-connected only).
// Negative voxel D touches A face-on but has the other sign.
const cells = [
  [0, 0, 0, 5], [1, 0, 0, 5], [0, 1, 0, 5], [1, 1, 0, 6],
  [0, 0, 3, 4], [1, 1, 3, 4],
  [4, 4, 4, 7], [5, 5, 5, 7],
  [2, 0, 0, -6]
];

test("extent rule separates signs and follows the report connectivity", () => {
  const data = volume(cells);
  const at = (connectivity, k) => excludedCells(
    display.extentExcluded(data, dims, [-3, 3], k, connectivity)
  );
  // k = 2: single voxels are excluded. The negative voxel is alone (its
  // positive neighbour does not join it).
  assert.deepEqual(at("6-connect", 2), [
    [2, 0, 0], [0, 0, 3], [1, 1, 3], [4, 4, 4], [5, 5, 5]
  ]);
  assert.deepEqual(at("18-connect", 2), [[2, 0, 0], [4, 4, 4], [5, 5, 5]]);
  assert.deepEqual(at("26-connect", 2), [[2, 0, 0]]);
  // k = 5 removes every cluster, including the 4-voxel block.
  assert.equal(at("18-connect", 5).length, 9);
});

test("extent rule is evaluated at the threshold being displayed", () => {
  const data = volume(cells);
  // At |z| >= 5.5 only the 6 inside block A and the 7s survive threshold:
  // each is now a single voxel, so k = 2 removes all of them.
  assert.deepEqual(
    excludedCells(display.extentExcluded(data, dims, [-5.5, 5.5], 2, "18-connect")),
    [[2, 0, 0], [1, 1, 0], [4, 4, 4], [5, 5, 5]]
  );
  // Lowering the threshold below a voxel's value never leaves it excluded
  // while its cluster meets the extent (no holes at lower thresholds).
  const low = display.extentExcluded(data, dims, [-1, 1], 4, "18-connect");
  for (const [i, j, k] of [[0, 0, 0], [1, 0, 0], [0, 1, 0], [1, 1, 0]]) {
    assert.equal(low[index(i, j, k)], 0);
  }
});

test("extent rule includes the threshold value and ignores NaN support", () => {
  const data = volume([[0, 0, 0, 3], [1, 0, 0, 3], [2, 0, 0, NaN], [3, 0, 0, 3]]);
  const mask = display.extentExcluded(data, dims, [-3, 3], 2, "6-connect");
  assert.deepEqual(excludedCells(mask), [[3, 0, 0]]);
});

test("one-sided thresholds and trivial extents exclude nothing extra", () => {
  const data = volume(cells);
  // Positive tail: the low edge sits below the data, so negatives never form
  // clusters.
  const positive = excludedCells(
    display.extentExcluded(data, dims, [-1e9, 3], 2, "18-connect")
  );
  assert.ok(!positive.some(([i, j, k]) => i === 2 && j === 0 && k === 0));
  assert.equal(display.extentExcluded(data, dims, [-3, 3], 1, "18-connect"), null);
  assert.equal(display.extentExcluded(data, dims, [0, 0], 5, "18-connect"), null);
});

test("connectivity offsets match neuroim2 neighbourhood sizes", () => {
  assert.equal(display.neighbourOffsets("6-connect").length, 6);
  assert.equal(display.neighbourOffsets("18-connect").length, 18);
  assert.equal(display.neighbourOffsets("26-connect").length, 26);
});

test("colour-map menus are curated by display scale", () => {
  const diverging = display.colormapMenu("diverging", "Report (nm-signed)");
  assert.deepEqual(diverging, ["Report (nm-signed)", "BlueRed", "Purple–orange"]);
  assert.ok(!diverging.includes("RdBu"), "sign-inverting red-white-blue is not offered");
  assert.deepEqual(display.colormapMenu("sequential", "Inferno"),
    ["Inferno", "Viridis", "Grayscale"]);
  // A map whose report palette is outside the menu keeps it, first.
  assert.equal(display.colormapMenu("diverging", "RdBu")[0], "RdBu");
  assert.equal(display.menuAliases["Purple–orange"], "PuOr");
});

test("the report palette restretches so its ramp starts at the shown threshold", () => {
  // 41 samples over [-5, 5] (step 0.25); report threshold 2.
  const values = Array.from({ length: 41 }, (_, i) => -5 + i * 0.25);
  const name = (v) => (Math.abs(v) < 2 ? "g" : (v > 0 ? "p" : "n") + Math.abs(v));
  const colors = values.map(name);
  const idx = (v) => Math.round((v + 5) / 0.25);
  const same = display.resaturate(colors, [-5, 5], 2, [-2, 2]);
  // Unchanged except the bins one step below the threshold, which can hold
  // drawn values and take the threshold colour.
  values.forEach((v, i) => {
    const expected = Math.abs(v) === 1.75 ? name(Math.sign(v) * 2) : colors[i];
    assert.equal(same[i], expected, `v = ${v}`);
  });
  // Threshold 1: the ramp starts at 1 and still ends at the cap.
  const lower = display.resaturate(colors, [-5, 5], 2, [-1, 1]);
  assert.equal(lower[idx(0.5)], "g");
  assert.equal(lower[idx(1)], "p2");
  assert.equal(lower[idx(-1)], "n2");
  assert.equal(lower[idx(3)], "p3.5");
  assert.equal(lower[idx(5)], "p5");
  assert.equal(lower[idx(-5)], "n5");
  // Threshold 4: everything well below 4 is gap; 4 starts the ramp.
  const higher = display.resaturate(colors, [-5, 5], 2, [-4, 4]);
  assert.equal(higher[idx(3.5)], "g");
  assert.equal(higher[idx(-3)], "g");
  assert.equal(higher[idx(4)], "p2");
  assert.equal(higher[idx(4.5)], "p3.5");
  // Without a report threshold the palette is returned unchanged.
  assert.equal(display.resaturate(colors, [-5, 5], 0, [-1, 1]), colors);
});

test("a palette bin straddling the threshold takes the ramp colour, not the gap", () => {
  // 21 samples over [-5, 5] (step 0.5); report threshold 1.2 falls inside
  // the bin centred on 1.0, which R's sampling paints as gap.
  const values = Array.from({ length: 21 }, (_, i) => -5 + i * 0.5);
  const colors = values.map((v) => (Math.abs(v) < 1.2 ? "g" : (v > 0 ? "p" : "n") + Math.abs(v)));
  const out = display.resaturate(colors, [-5, 5], 1.2, [-1.2, 1.2]);
  assert.equal(out[10], "g");          // 0.0
  assert.equal(out[12], "p1.5");       // 1.0 bin holds values >= 1.2
  assert.equal(out[8], "n1.5");
  assert.equal(out[11], "g");          // 0.5 bin is wholly below 1.2
  assert.deepEqual(out.slice(13), colors.slice(13));
});
