const test = require("node:test");
const assert = require("node:assert/strict");
const path = require("node:path");

const display = require(path.resolve(
  __dirname,
  "../../inst/htmlwidgets/lib/neuromosaic-surface/display.js"
));

const SCHAEFER = "Schaefer-200-7networks";

test("hover regions read like the peak table, with the hemisphere", () => {
  const pfc = display.describeParcel(
    80, { label: "PFCl_4", full: "LH_Cont_PFCl_4", network: "Cont" }, SCHAEFER
  );
  assert.equal(pfc.name, "Control · PFCl 4 (L)");
  assert.equal(pfc.detail, "Schaefer-200, 7 networks");
  const names = [
    ["RH_Vis_3", "Vis", "Visual · 3 (R)"],
    ["LH_SomMot_2", "SomMot", "Somatomotor · 2 (L)"],
    ["RH_DorsAttn_Post_1", "DorsAttn", "Dorsal attention · Post 1 (R)"],
    ["RH_SalVentAttn_FrOperIns_1", "SalVentAttn",
      "Salience / ventral attention · FrOperIns 1 (R)"],
    ["LH_Limbic_TempPole_2", "Limbic", "Limbic · TempPole 2 (L)"],
    ["LH_Default_Temp_3", "Default", "Default · Temp 3 (L)"]
  ];
  names.forEach(([full, network, expected]) => {
    const label = full.replace(/^(LH|RH)_/, "");
    assert.equal(display.describeParcel(9, { label, full, network }, SCHAEFER).name, expected);
  });
  // A table entry whose label already drops the network still resolves.
  assert.equal(
    display.describeParcel(151, { label: "PrC_1", full: "RH_SalVentAttn_PrC_1",
      network: "SalVentAttn" }, SCHAEFER).name,
    "Salience / ventral attention · PrC 1 (R)"
  );
  // 17-network codes keep their letter.
  assert.equal(
    display.describeParcel(10, { label: "DefaultA_pCunPCC_3", network: "DefaultA" }, "x").name,
    "Default A · pCunPCC 3"
  );
});

test("unknown parcel formats and the medial wall are kept honest", () => {
  assert.deepEqual(display.describeParcel(0, null, SCHAEFER),
    { name: "Medial wall", detail: "Schaefer-200, 7 networks" });
  assert.deepEqual(display.describeParcel(5, { label: "V1" }, "Glasser"),
    { name: "V1", detail: "V1 · Glasser" });
  assert.equal(
    display.describeParcel(7, { label: "ctx-lh-precuneus", network: "Precuneus" }, "DK").name,
    "ctx-lh-precuneus"
  );
  assert.equal(display.atlasName("Schaefer-400-17networks"), "Schaefer-400, 17 networks");
  assert.equal(display.atlasName("Glasser360"), "Glasser360");
});

function option(id, label) {
  return { id, label: label || id, availability: { enabled: true } };
}

const AVAILABLE = [
  option("nm-signed-map-1", "nm-signed-map-1"), option("surface-heat", "Surface Heat"),
  option("surface-heat-positive", "Surface Heat Positive"), option("hot", "Hot"),
  option("viridis", "Viridis"), option("bluered", "bluered"), option("coolwarm"),
  option("jet"), option("greys"),
  { id: "RdBu", label: "RdBu", availability: { enabled: false } }
];

test("signed maps are offered the report palette and diverging maps only", () => {
  const menu = display.curateColorMaps(AVAILABLE, "nm-signed-map-1", true);
  assert.equal(menu[0].id, "nm-signed-map-1");
  assert.equal(menu[0].label, "Report (nm-signed)");
  const ids = menu.map((m) => m.id);
  ["surface-heat-positive", "hot", "viridis", "greys", "jet", "RdBu"].forEach((id) => {
    assert.ok(!ids.includes(id), id + " must not be offered for a signed map");
  });
  assert.ok(ids.includes("surface-heat"));
  // Reader-facing names, not preset ids.
  menu.forEach((m) => assert.doesNotMatch(m.label, /^[a-z]+(-[a-z]+)*$/));
});

test("unsigned maps get sequential palettes; an off-menu current map stays", () => {
  const ids = display.curateColorMaps(AVAILABLE, "viridis", false).map((m) => m.id);
  assert.ok(ids.includes("viridis"));
  assert.ok(!ids.includes("surface-heat"));
  const kept = display.curateColorMaps(AVAILABLE, "jet", true);
  assert.equal(kept[0].id, "jet");
});

test("thresholds clamp into the data range", () => {
  assert.equal(display.clampMagnitude("999", [-12.5, 21.6]), 21.6);
  assert.equal(display.clampMagnitude("-4", [-12.5, 21.6]), 4);
  assert.equal(display.clampMagnitude("4", null), 4);
  assert.equal(display.clampMagnitude("x", [-1, 1]), null);
  assert.deepEqual(display.clampInterval("-999", "999", [-12.5, 21.6]), [-12.5, 21.6]);
  assert.deepEqual(display.clampInterval("-3", "3", [-12.5, 21.6]), [-3, 3]);
  // A one-signed map still allows an interval that reaches zero.
  assert.deepEqual(display.clampInterval("-5", "5", [1, 9]), [0, 5]);
  assert.equal(display.clampInterval("5", "-5", [-1, 1]), null);
});

test("hex colours parse to bytes", () => {
  assert.deepEqual(display.hexRgb("#B40426"), [180, 4, 38]);
  assert.deepEqual(display.hexRgb("1d4e9e"), [29, 78, 158]);
  assert.equal(display.hexRgb("#B404"), null);
});

test("the hidden band of a threshold palette takes the threshold colours", () => {
  const pal = ["#A6E3F5", "#1D4E9E", "#C9CDD0", "#C9CDD0", "#C9CDD0", "#C9CDD0", "#B40426", "#FFE08A"];
  assert.deepEqual(display.bleedGap(pal), [
    "#A6E3F5", "#1D4E9E", "#1D4E9E", "#1D4E9E", "#B40426", "#B40426", "#B40426", "#FFE08A"
  ]);
  // A continuous palette (no neutral band) and degenerate input are unchanged.
  const ramp = ["#000000", "#111111", "#222222", "#333333"];
  assert.deepEqual(display.bleedGap(ramp), ramp);
  assert.deepEqual(display.bleedGap(["#C9CDD0", "#C9CDD0", "#C9CDD0"]), ["#C9CDD0", "#C9CDD0", "#C9CDD0"]);
  assert.deepEqual(display.bleedGap(null), []);
  // The input is not mutated.
  assert.equal(pal[2], "#C9CDD0");
});

test("a crowded symmetric threshold is labelled once as ±t", () => {
  const labels = (range, mask, gap) => display.legendLabels(range, mask, gap).map((t) => t.label);
  // Room for both edges: unchanged.
  assert.deepEqual(labels([-22.3, 22.3], [-5, 5], 0.16), ["−22.3", "−5", "5", "22.3"]);
  // Only one edge fits: one centred ±5 instead of a lone −5.
  assert.deepEqual(labels([-22.3, 22.3], [-5, 5], 0.245), ["−22.3", "±5", "22.3"]);
  assert.equal(display.legendLabels([-22.3, 22.3], [-5, 5], 0.245)[1].value, 0);
  // Asymmetric masks keep plain numeric labels.
  assert.deepEqual(labels([0, 10], [0, 3], 0.16), ["0", "3", "10"]);
});
