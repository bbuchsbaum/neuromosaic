// Volume viewer: a scripted focus (a peak jump) never hands the plain wheel
// to the slice pane; a rejected colour-range entry clears its invalid state
// after dispatch; and the threshold-split palette paints every shown value in
// its own sign's ramp for any map, threshold and colour range, including a
// range end set inside the threshold.
const test = require("node:test");
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { resolve } = require("node:path");
const { runInNewContext } = require("node:vm");

const lib = resolve(__dirname, "../../inst/htmlwidgets/lib/neuromosaic-volume");
const display = require(resolve(lib, "display.js"));
const adapterSource = readFileSync(resolve(lib, "adapter.js"), "utf8");
const reportPalette = require("./fixtures/report-palette.json");

// ---- wheel ownership -------------------------------------------------------------
test("a pane focused by script (a peak jump) lets the wheel scroll the page", () => {
  const w = display.wheelAction;
  assert.equal(w({ deltaY: 100, focused: true, byReader: false }), "page");
  assert.equal(w({ deltaY: 100, focused: true }), "page", "unknown origin is not the reader");
  assert.equal(w({ deltaY: 100, focused: true, byReader: true }), "runtime", "the reader's own focus");
  assert.equal(w({ deltaY: 100, focused: false, byReader: true }), "page");
  assert.equal(w({ deltaY: 100, focused: true, byReader: false, altKey: true }), 1, "modifiers still step");
});

test("the adapter tells the wheel whether the reader put focus in the pane", () => {
  // Reader intent is the last key or press anywhere on the page; a Tab or a
  // press/key inside the pane counts, a jump button elsewhere does not.
  const listeners = {};
  const context = {
    document: {
      readyState: "loading",
      addEventListener(type, fn) { (listeners[type] = listeners[type] || []).push(fn); }
    },
    window: { addEventListener() {} }
  };
  runInNewContext(adapterSource, context);
  assert.ok((listeners.keydown || []).length >= 1 && (listeners.pointerdown || []).length >= 1,
    "the adapter watches the reader's keys and presses");
  assert.match(adapterSource,
    /byReader: lastIntent\.tab \|\| !!\(lastIntent\.target && cell\.contains\(lastIntent\.target\)\)/);
});

// ---- rejected colour-range entry ---------------------------------------------------
function rangeInputFactory() {
  const start = adapterSource.indexOf("    var rangeInput = function (which, original) {");
  const end = adapterSource.indexOf("    panel.onRangeLowInput = rangeInput(", start);
  assert.ok(start > 0 && end > start, "rangeInput is present");
  return adapterSource.slice(start, end) + "\n    rangeInput;";
}

test("a rejected colour-range entry clears aria-invalid even after the event is gone", () => {
  const timers = [];
  const errors = [];
  const attrs = {};
  const field = {
    value: "",
    setAttribute(k, v) { attrs[k] = v; },
    removeAttribute(k) { delete attrs[k]; }
  };
  // Like an event from a shadow tree: target is null once dispatch is over.
  let dispatching = true;
  const event = { get target() { return dispatching ? field : null; } };
  const live = { textContent: "" };
  const panel = {
    range: [-22.3, 22.3],
    readField: () => 30,
    fmt: (v) => String(v),
    onRangeLowInput() { throw new Error("original handler must not run"); }
  };
  const ctx = {
    panel, state: { host: { querySelector: () => live } },
    displayApi: display,
    niceTick: (v) => String(v),
    setTimeout: (fn, ms) => { timers.push({ fn, ms }); return timers.length; }
  };
  const rangeInput = runInNewContext(rangeInputFactory(), ctx);
  const handler = rangeInput("low", panel.onRangeLowInput);
  handler(event);
  dispatching = false;
  assert.equal(attrs["aria-invalid"], "true", "the field is marked invalid");
  assert.equal(field.value, "-22.3", "the field returns to the kept value");
  assert.match(live.textContent, /Range low must stay below 22\.3; kept -22\.3\./);
  timers.forEach((t) => { try { t.fn(); } catch (err) { errors.push(err); } });
  assert.deepEqual(errors.map(String), [], "no error from the delayed reset");
  assert.equal("aria-invalid" in attrs, false, "aria-invalid is cleared");
});

// ---- palette grid ----------------------------------------------------------------
// A report palette sampled over `limits`: a negative ramp, the hidden-band
// (gap) colour inside ±t0, and a positive ramp, every ramp colour distinct.
function syntheticPalette(limits, t0, n = 256) {
  const colors = [];
  for (let i = 0; i < n; i++) {
    const v = limits[0] + (i * (limits[1] - limits[0])) / (n - 1);
    if (Math.abs(v) < t0) colors.push("#C9CDD0");
    else colors.push((v < 0 ? "#00" : "#FF") + i.toString(16).padStart(2, "0") + (v < 0 ? "FF" : "00"));
  }
  return { limits, threshold: t0, palette_colors: colors };
}

const maps = [
  ["report", reportPalette],
  ["symmetric 11", syntheticPalette([-11, 11], 3.1)],
  ["wide 30", syntheticPalette([-30, 30], 5)],
  ["asymmetric", syntheticPalette([-8.5, 26], 4)]
];
const thresholds = [3.1, 5, 7, 10];
const rangesFor = (lim) => [
  lim, [-22.3, 15], [-10, 22.3], [-22.3, 8], [-6, 6], [-30, 30], [-12, 9], [-4, 22.3], [-2, 2]
];

test("every shown value takes its own sign's ramp for any map, threshold and range", () => {
  let checked = 0;
  const failures = {};
  for (const [name, map] of maps) {
    const C = map.palette_colors;
    const n = C.length;
    const lim = map.limits.map(Number);
    const gap = C[Math.round(((0 - lim[0]) / (lim[1] - lim[0])) * (n - 1))];
    const neg = new Set(C.slice(0, Math.round(((0 - lim[0]) / (lim[1] - lim[0])) * (n - 1))).filter((c) => c !== gap));
    const pos = new Set(C.slice(Math.round(((0 - lim[0]) / (lim[1] - lim[0])) * (n - 1))).filter((c) => c !== gap));
    for (const T of thresholds) {
      if (!(Math.min(-lim[0], lim[1]) > T)) continue; // the map has no shown value of one sign
      for (const [rlo, rhi] of rangesFor(lim)) {
        const L = display.resaturate(C, lim, map.threshold, [-T, T], [rlo, rhi]);
        // Shown values: |v| >= T within the map's limits.
        for (let v = lim[0]; v <= lim[1]; v += 0.01) {
          if (Math.abs(v) < T) continue;
          for (const mode of ["round", "floor"]) {
            const x = Math.max(0, Math.min(1, (v - rlo) / (rhi - rlo))) * (n - 1);
            const i = Math.max(0, Math.min(n - 1, mode === "round" ? Math.round(x) : Math.floor(x)));
            const c = L[i];
            checked++;
            if (c === gap || (v < 0 && !neg.has(c)) || (v > 0 && !pos.has(c))) {
              const key = `${name} T${T} ${rlo}..${rhi} ${mode}`;
              failures[key] = (failures[key] || 0) + 1;
            }
          }
        }
        // Bins wholly inside the threshold keep the gap colour, except the
        // range ends (they hold clamped shown values). A bin within one step
        // of ±T may hold drawn values and takes the threshold colour.
        const step = (rhi - rlo) / (n - 1);
        for (let i = 1; i < n - 1; i++) {
          const vb = rlo + i * step;
          if (Math.abs(vb) + step >= T) continue;
          if (L[i] !== gap) {
            const key = `${name} T${T} ${rlo}..${rhi} sub-threshold`;
            failures[key] = (failures[key] || 0) + 1;
          }
        }
      }
    }
  }
  assert.ok(checked > 5e5, `${checked} samples`);
  assert.deepEqual(failures, {});
});

test("a positive range end inside the threshold paints positive peaks at the cap, not grey", () => {
  const map = reportPalette;
  const L = display.resaturate(map.palette_colors, map.limits, map.threshold, [-10, 10], [-22.3, 8]);
  const C = map.palette_colors;
  assert.equal(L[L.length - 1], C[C.length - 1], "values >= 10 clamp to the end bin: the cap colour");
  const L2 = display.resaturate(map.palette_colors, map.limits, map.threshold, [-5, 5], [-4, 22.3]);
  assert.equal(L2[0], C[0], "the negative end bin likewise takes the negative cap colour");
});

// ---- failure messages from disk -----------------------------------------------------
function messageFn(source, name, scope) {
  const start = source.indexOf("  function " + name + "(error) {");
  assert.ok(start > 0, name + " is present");
  const end = source.indexOf("\n  }\n", start) + 4;
  const fileMessage = source.match(/var FILE_MESSAGE = ("[^"]+");/)[1];
  return runInNewContext("var FILE_MESSAGE = " + fileMessage + ";\n" + source.slice(start, end) + "\n" + name + ";", scope);
}

test("a report opened from disk names HTTP only when a file could not be fetched", () => {
  const surfaceSource = readFileSync(resolve(lib, "../neuromosaic-surface/bridge.js"), "utf8");
  const disk = { protocol: "file:" };
  const volume = messageFn(adapterSource, "readerMessage", { root: { location: disk } });
  const surface = messageFn(surfaceSource, "surfaceMessage", { window: { location: disk } });
  for (const fn of [volume, surface]) {
    // Embedded data on disk: a WebGL or schema failure gives its own reason.
    assert.match(fn(new Error("WebGL initialization failed")), /could not load \(WebGL initialization failed\)\. The static figures are unaffected\./);
    assert.match(fn(new Error("Unsupported neuromosaic VolumeScene.")), /Unsupported neuromosaic VolumeScene/);
    // A blocked fetch from disk is the HTTP case.
    assert.match(fn(new TypeError("Failed to fetch")), /served over HTTP/);
  }
});
