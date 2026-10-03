// Round 4b corrections: print through the report (Ctrl/Cmd+P loads every
// light figure first), readout states, a palette split at 0 for any colour
// range, tie-aware viewer peaks, keyboard/wheel ownership, and a 3D threshold
// edge that colours every suprathreshold vertex without ghost fill.
const test = require("node:test");
const assert = require("node:assert/strict");
const { existsSync, readFileSync } = require("node:fs");
const { execFileSync } = require("node:child_process");
const { resolve } = require("node:path");
const { runInNewContext } = require("node:vm");

const lib = resolve(__dirname, "../../inst/htmlwidgets/lib");
const display = require(resolve(lib, "neuromosaic-volume/display.js"));
const bridgeSource = readFileSync(resolve(lib, "neuromosaic-surface/bridge.js"), "utf8");
const reportSource = readFileSync(resolve(__dirname, "../../inst/report/montage-report.js"), "utf8");

// ---- print ---------------------------------------------------------------
function reportPage(images, sources) {
  const listeners = { doc: {}, win: {} };
  const add = (bag) => (type, fn) => { (bag[type] = bag[type] || []).push(fn); };
  const ctx = {
    console, setTimeout, clearTimeout, setInterval, clearInterval, Promise,
    Event: class { constructor(type) { this.type = type; } },
    document: {
      readyState: "loading",
      addEventListener: add(listeners.doc),
      querySelectorAll: (sel) => {
        if (/source\[data-nm-theme\]/.test(sel)) return sources;
        if (/\.nm-figure img|nm-atlas-thumb img/.test(sel)) return images;
        return [];
      },
      querySelector: () => null,
      documentElement: { getAttribute: () => null },
      createElement: () => ({ setAttribute() {}, style: {} }),
      body: { appendChild() {} }
    },
    navigator: {}, location: { protocol: "http:", hash: "" }, history: {},
    getComputedStyle: () => ({})
  };
  ctx.window = ctx;
  ctx.addEventListener = add(listeners.win);
  ctx.dispatchEvent = (e) => { (listeners.win[e.type] || []).forEach((fn) => fn(e)); return true; };
  ctx.printed = [];
  ctx.print = () => ctx.printed.push(images.map((i) => i.currentSrc === i.src && i.decoded));
  runInNewContext(reportSource, ctx);
  return { ctx, listeners };
}

// An <img> in a <picture> that shows its narrow copy until the light source
// is selected and arrives.
function fakeImage(name) {
  return {
    src: "http://x/" + name + ".png", currentSrc: "http://x/" + name + "-narrow.png",
    complete: true, naturalWidth: 800, loading: "lazy", decoded: false,
    attrs: { src: name + ".png" },
    getAttribute(k) { return this.attrs[k] || null; },
    closest: () => null,
    addEventListener() {}, removeEventListener() {},
    decode() { this.decoded = this.currentSrc === this.src; return Promise.resolve(); }
  };
}

test("Ctrl/Cmd+P prints only after every light figure has loaded and decoded", async () => {
  const images = [fakeImage("a"), fakeImage("b"), fakeImage("c")];
  const sources = [0, 1, 2].map(() => ({
    media: "(max-width: 600px)", setAttribute(k, v) { this[k] = v; }, getAttribute(k) { return k in this ? this[k] : null; }
  }));
  const { ctx, listeners } = reportPage(images, sources);
  let prevented = 0;
  (listeners.doc.keydown || []).forEach((fn) => fn({
    ctrlKey: true, metaKey: false, altKey: false, shiftKey: false, key: "p",
    preventDefault() { prevented += 1; }
  }));
  assert.ok(prevented >= 1, "the browser's own print is intercepted");
  // Narrow and dark sources are switched off for paper.
  assert.ok(sources.every((s) => s.media === "not all"));
  await new Promise((r) => setTimeout(r, 150));
  assert.equal(ctx.printed.length, 0, "no print while light copies are missing");
  images.forEach((img) => { img.currentSrc = img.src; });
  await new Promise((r) => setTimeout(r, 300));
  assert.equal(ctx.printed.length, 1, "print is asked for exactly once");
  assert.deepEqual(ctx.printed[0], [true, true, true], "every figure shows its decoded light copy");
  assert.ok(images.every((i) => i.loading === "eager"), "lazy thumbnails are loaded for paper");
});

test("the page offers a visible Print / Save PDF control", () => {
  assert.match(reportSource, /b\.textContent = 'Print \/ Save PDF';/);
  assert.match(reportSource, /nmr\.printButton\('nm-print-minibar'\)/);
  assert.match(reportSource, /printButton\('nm-print-header'\)/);
});

// ---- readout -----------------------------------------------------------------
test("the readout says below threshold, hidden, outside map or the value", () => {
  const st = display.readoutState;
  for (const t of [3.1, 5, 7]) {
    const band = [-t, t];
    const below = st(t - 0.5, t - 0.5, band, 20);
    assert.equal(below.kind, "below");
    assert.equal(below.reason, "below threshold");
    assert.equal(below.value, t - 0.5);
    assert.equal(st(-(t - 0.01), -(t - 0.01), band, 20).kind, "below");
    assert.equal(st(t + 1, t + 1, band, 20).kind, "shown");
    assert.equal(st(NaN, t + 1, band, 20).reason, "hidden (cluster < 20 voxels)");
  }
  assert.equal(st(0, 0, [-5, 5], 20).reason, "outside map");
  assert.equal(st(null, NaN, [-5, 5], 20).kind, "outside");
  assert.equal(st(4.78, null, [-5, 5], 20).kind, "below", "falls back to the displayed value");
  assert.equal(st(1.2, 1.2, [0, 0], 20).kind, "shown", "no threshold: everything shown");
});

// ---- palette -----------------------------------------------------------------
test("an asymmetric colour range never paints shown values in the gap colour", () => {
  // 257 samples over [-22.3, 22.3]; gap below the report threshold 5.
  const n = 257, lim = [-22.3, 22.3];
  const vals = Array.from({ length: n }, (_, i) => lim[0] + (i * (lim[1] - lim[0])) / (n - 1));
  const colors = vals.map((v) => (Math.abs(v) < 5 ? "gap" : (v > 0 ? "p" : "n") + Math.round(Math.abs(v) * 10)));
  // The renderer spreads the palette linearly over the layer range.
  const colourAt = (pal, range, v) => {
    const idx = Math.round(((v - range[0]) / (range[1] - range[0])) * (pal.length - 1));
    return pal[Math.max(0, Math.min(pal.length - 1, idx))];
  };
  for (const [range, thr] of [[[-22.3, 15], 5], [[-22.3, 15], 7], [[-10, 22.3], 5], [[-22.3, 22.3], 6]]) {
    const pal = display.resaturate(colors, lim, 5, [-thr, thr], range);
    for (let v = range[0]; v <= range[1]; v += 0.05) {
      if (Math.abs(v) < thr) continue;
      const c = colourAt(pal, range, v);
      assert.notEqual(c, "gap", `v = ${v.toFixed(2)} shown at range ${range} thr ${thr}`);
      assert.equal(c[0], v > 0 ? "p" : "n", "sign kept");
    }
    // Each half ends at the cap colour.
    assert.equal(colourAt(pal, range, range[1]), "p223");
    assert.equal(colourAt(pal, range, range[0]), "n223");
  }
});

// ---- viewer peaks, keys and wheel -----------------------------------------------
test("viewer peaks use the table's tie digits", () => {
  const fallback = (v) => Number(v).toFixed(2).replace(/^-/, "−");
  assert.equal(display.peakText(-11.6329, 3, fallback), "−11.633");
  assert.equal(display.peakText(-11.631, 3, fallback), "−11.631");
  assert.equal(display.peakText(-11.6329, undefined, fallback), "−11.63");
  assert.equal(display.peakText(21.58, 2, fallback), "21.58");
});

test("the wheel scrolls the page unless the pane is focused or a modifier is held", () => {
  const w = display.wheelAction;
  assert.equal(w({ deltaY: 100 }), "page");
  assert.equal(w({ deltaY: 100, focused: true, byReader: true }), "runtime", "a pane the reader focused");
  assert.equal(w({ deltaY: 100, ctrlKey: true }), "runtime");
  assert.equal(w({ deltaY: 100, altKey: true }), 1);
  assert.equal(w({ deltaY: -100, shiftKey: true }), -1);
  assert.equal(w({ deltaX: 40, shiftKey: true }), 1, "Shift turns the wheel horizontal");
  assert.equal(w({ shiftKey: true }), "page");
});

// ---- 3D threshold edge ----------------------------------------------------------
// The one-sided threshold edge and per-vertex visibility signs are surfview's
// own behaviour (surfviewjs 1dfed43, shipped by neurosurf >= 0.1.0.9004); the
// bridge no longer patches the shader. Test the expression the runtime ships.
function surfviewRuntime() {
  if (process.env.NEUROMOSAIC_SURFVIEW_RUNTIME) return process.env.NEUROMOSAIC_SURFVIEW_RUNTIME;
  try {
    return execFileSync("Rscript", ["-e",
      "cat(system.file('htmlwidgets/lib/neurosurface/surfview.embed.iife.js', package = 'neurosurf'))"
    ], { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim();
  } catch (error) {
    return "";
  }
}
// Evaluate the GLSL ternary that defines surfviewInside.
function glsl(expr) {
  const smoothstep = (a, b, x) => {
    const t = Math.min(1, Math.max(0, (x - a) / (b - a)));
    return t * t * (3 - 2 * t);
  };
  return (edge, width) => runInNewContext(expr, {
    vSurfviewEdge: edge, surfviewWidth: width, smoothstep, max: Math.max
  });
}

const runtimePath = surfviewRuntime();
const runtimeSkip = runtimePath && existsSync(runtimePath) ? false :
  "neurosurf's surfview runtime is not installed";

test("surfview draws the 3D threshold edge one-sided at zero with a half-colour floor", { skip: runtimeSkip }, () => {
  const runtime = readFileSync(runtimePath, "utf8");
  const m = runtime.match(/float surfviewInside = ((?:[^;]|\\n)+);/);
  assert.ok(m, "the runtime defines surfviewInside");
  const expr = m[1].replace(/\\n/g, " ").replace(/\s+/g, " ").trim();
  assert.doesNotMatch(expr, /-0\.5 \* surfviewWidth/, "the symmetric ramp is gone");
  const inside = glsl(expr);
  const w = 0.4;
  assert.equal(inside(-1e-6, w), 0, "below threshold: no colour");
  assert.equal(inside(-0.5 * w, w), 0, "no ghost fill half a pixel below");
  assert.ok(inside(0, w) >= 0.5, "at threshold: coloured");
  assert.ok(inside(0.01 * w, w) >= 0.5);
  assert.equal(inside(w, w), 1, "full colour one pixel inside");
  // With per-vertex signs (+1 shown, -1 hidden) the contour runs midway
  // between a shown and a hidden vertex: a suprathreshold vertex is coloured
  // at its own pixel wherever the pixel centre falls, a sub-threshold one never.
  const at = (e0, e1, off, spacing) => e0 + (e1 - e0) * (off / spacing);
  for (const off of [0, 0.25, 0.5]) {
    assert.ok(inside(at(1, -1, off, 3), 2 / 3) >= 0.5, `supra at offset ${off}`);
    assert.equal(inside(at(-1, 1, off, 3), 2 / 3), 0, `sub at offset ${off}`);
  }
});

test("the bridge leaves the threshold edge, picking and edge signs to surfview", () => {
  for (const patch of ["SYMMETRIC_EDGE", "ONE_SIDED_EDGE", "crispThreshold", "vertexEdges",
                       "pickVisible", "onBeforeCompile"]) {
    assert.ok(!bridgeSource.includes(patch), patch + " is gone");
  }
  assert.match(bridgeSource, /viewer\.pick\(\{ x: clientX, y: clientY, useGPU: false \}\)/);
});
