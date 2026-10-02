// Round 4b corrections: print through the report (Ctrl/Cmd+P loads every
// light figure first), readout states, a palette split at 0 for any colour
// range, tie-aware viewer peaks, keyboard/wheel ownership, and a 3D threshold
// edge that colours every suprathreshold vertex without ghost fill.
const test = require("node:test");
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
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

test("the runtime's arrow keys never act on a focused control", () => {
  const ok = display.runtimeKeyAllowed;
  assert.equal(ok({ isPage: true }), true);
  assert.equal(ok({ tag: "INPUT" }), false, "range slider / number field");
  assert.equal(ok({ tag: "SELECT" }), false);
  assert.equal(ok({ tag: "BUTTON", inPane: true }), false);
  assert.equal(ok({ tag: "DIV", editable: true, inPane: true }), false);
  assert.equal(ok({ tag: "DIV", inPane: true }), true, "a focused slice pane");
  assert.equal(ok({ tag: "DIV" }), false, "focus elsewhere on the page");
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
const shaderLine = (name) => {
  const m = bridgeSource.match(new RegExp("var " + name + ' = "([^"]+)";'));
  assert.ok(m, name + " present");
  return m[1];
};
// Evaluate the GLSL expression of a "float surfviewInside = ...;" line.
function glsl(line) {
  const expr = line.replace(/^float surfviewInside = /, "").replace(/;$/, "");
  const smoothstep = (a, b, x) => {
    const t = Math.min(1, Math.max(0, (x - a) / (b - a)));
    return t * t * (3 - 2 * t);
  };
  return (edge, width) => runInNewContext(expr, {
    vSurfviewEdge: edge, surfviewWidth: width, smoothstep, max: Math.max
  });
}

test("the shipped edge expression is one-sided at zero with a half-colour floor", () => {
  const inside = glsl(shaderLine("ONE_SIDED_EDGE"));
  const w = 0.4;
  assert.equal(inside(-1e-6, w), 0, "below threshold: no colour");
  assert.equal(inside(-0.5 * w, w), 0, "no ghost fill half a pixel below");
  assert.ok(inside(0, w) >= 0.5, "at threshold: coloured");
  assert.ok(inside(0.01 * w, w) >= 0.5);
  assert.equal(inside(w, w), 1, "full colour one pixel inside");
  // The rewrite targets surfview's symmetric line verbatim.
  assert.match(shaderLine("SYMMETRIC_EDGE"),
    /smoothstep\( -0\.5 \* surfviewWidth, 0\.5 \* surfviewWidth, vSurfviewEdge \)/);
});

function bridgeHooks() {
  const context = {
    HTMLCanvasElement: class { getContext() { return null; } },
    innerHeight: 1000,
    document: {
      readyState: "loading", documentElement: { getAttribute: () => null },
      addEventListener() {}, querySelector() { return null; }, querySelectorAll() { return []; }
    },
    location: { protocol: "http:" },
    setTimeout: () => 0,
    IntersectionObserver: class { observe() {} },
    console
  };
  context.window = context;
  context.window.addEventListener = () => {};
  runInNewContext(bridgeSource, context);
  return context.NeuroMosaicSurfaceBridge;
}

test("vertex edges keep only the sign, so a suprathreshold vertex is coloured at its own pixel", () => {
  const hooks = bridgeHooks();
  const edges = new Float32Array([0.02, -3, -3, -0.02, 3, 3]);
  const attr = { array: edges, needsUpdate: false };
  const surface = {
    mesh: { geometry: { getAttribute: (k) => (k === "surfviewEdge" ? attr : null) } },
    applyThresholdEdges() { edges.set([0.5, -0.1, 2, -4, 0, 1]); return true; }
  };
  hooks.vertexEdges(surface);
  assert.deepEqual(Array.from(edges), [1, -1, -1, -1, 1, 1]);
  assert.equal(attr.needsUpdate, true);
  // A threshold change rewrites the buffer; the signs are re-applied.
  surface.applyThresholdEdges();
  assert.deepEqual(Array.from(edges), [1, -1, 1, -1, 1, 1]);

  // Projection model: a vertex 0.02 above threshold whose neighbours are at
  // -3, 3 px away; its pixel centre lies up to 0.5 px off the vertex.
  const inside = glsl(shaderLine("ONE_SIDED_EDGE"));
  const at = (e0, e1, off, spacing) => e0 + (e1 - e0) * (off / spacing);
  for (const off of [0, 0.25, 0.5]) {
    // Raw magnitudes (r4): the vertex vanishes once the pixel is off-centre.
    if (off >= 0.25) assert.equal(inside(at(0.02, -3, off, 3), 3.02 / 3), 0);
    // Signs: coloured at its pixel; the sub-threshold mirror case is not.
    assert.ok(inside(at(1, -1, off, 3), 2 / 3) >= 0.5, `supra at offset ${off}`);
    assert.equal(inside(at(-1, 1, off, 3), 2 / 3), 0, `sub at offset ${off}`);
  }
});
