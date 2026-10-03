// Round 4 interactive viewers: a WebGL lifecycle that cannot loop (viewers
// off screen are released and parked, rebuilt only on screen and within the
// budget), export notes, colour-range edits, slider steps and the one-sided
// threshold edge on the 3D surface.
const test = require("node:test");
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { resolve } = require("node:path");
const { runInNewContext } = require("node:vm");

const lib = resolve(__dirname, "../../inst/htmlwidgets/lib");
const adapterSource = readFileSync(resolve(lib, "neuromosaic-volume/adapter.js"), "utf8");
const bridgeSource = readFileSync(resolve(lib, "neuromosaic-surface/bridge.js"), "utf8");
const volumeDisplay = require(resolve(lib, "neuromosaic-volume/display.js"));

function budgetBlock(source) {
  const start = source.indexOf("// <nm-gl-budget>");
  const end = source.indexOf("// </nm-gl-budget>");
  assert.ok(start >= 0 && end > start, "the budget block is delimited");
  return source.slice(start, end);
}

test("the volume and surface scripts carry the same WebGL budget", () => {
  assert.equal(budgetBlock(adapterSource), budgetBlock(bridgeSource));
});

// A page-like context with a viewport, elements at given positions and an
// IntersectionObserver whose callback the test drives.
function page() {
  const observers = [];
  const timers = [];
  class FakeCanvas { getContext() { return null; } }
  const context = {
    HTMLCanvasElement: FakeCanvas,
    innerHeight: 1000,
    document: {
      readyState: "loading",
      documentElement: { getAttribute: () => null },
      addEventListener() {},
      querySelector() { return null; },
      querySelectorAll() { return []; }
    },
    location: { protocol: "http:" },
    setTimeout: (fn) => { timers.push(fn); return timers.length; },
    IntersectionObserver: class {
      constructor(callback) { this.callback = callback; this.targets = []; observers.push(this); }
      observe(target) { this.targets.push(target); }
    },
    console
  };
  context.window = context;
  context.window.addEventListener = () => {};
  runInNewContext(adapterSource, context);
  runInNewContext(bridgeSource, context);
  const element = (top) => ({
    top,
    hidden: false,
    getClientRects: () => [1],
    getBoundingClientRect() { return { top: this.top, bottom: this.top + 600, width: 800, height: 600 }; }
  });
  const flush = () => { while (timers.length) timers.shift()(); };
  return { GL: context.NeuroMosaicGL, observers, element, flush };
}

test("viewers scrolled out of view are released even while they are the chosen view", () => {
  const { GL, element } = page();
  assert.equal(GL.version, 2);
  const released = [];
  const entry = (key, el) => ({
    key, cost: 3, element: el, shown: () => true, release: () => released.push(key)
  });
  // Slices left open on map after map, scrolling down: every viewer stays
  // "shown", but only the ones on screen keep their contexts.
  const els = [0, 1, 2, 3].map((i) => element(i * 1500));
  els.forEach((el, i) => {
    els.forEach((other, j) => { other.top = (j - i) * 1500 + 100; });
    GL.touch(entry("volume:" + i, el));
  });
  assert.deepEqual(released, ["volume:0", "volume:1"]);
  assert.ok(GL.live() <= GL.budget);
  // Nothing on screen is ever released to make room.
  // Over budget only by visible viewers: nothing is released, and a fifth
  // Slices view would pass the hard limit (Chrome's pool) so it must wait.
  els.forEach((el) => { el.top = 100; });
  GL.touch(entry("volume:4", element(100)));
  GL.touch(entry("volume:5", element(100)));
  assert.deepEqual(released, ["volume:0", "volume:1"]);
  assert.equal(GL.live(), 12);
  assert.equal(GL.reserve("volume:x", 3), false);
  assert.equal(GL.reserve("volume:x", 1), true);
  assert.deepEqual(released, ["volume:0", "volume:1"]);
});

test("a parked viewer resumes only when it is back on screen", () => {
  const { GL, observers, element, flush } = page();
  const el = element(5000);
  let resumed = 0;
  GL.park({ key: "volume:a", element: el, resume: () => { resumed += 1; } });
  assert.equal(GL.isParked("volume:a"), true);
  const io = observers[0];
  assert.ok(io && io.targets.includes(el), "parked viewers are observed");
  // Entering the viewport margin while a fast scroll moves on: no rebuild.
  io.callback([{ target: el, isIntersecting: true }]);
  el.top = 4000;
  flush();
  assert.equal(resumed, 0);
  // Settled on screen: one rebuild.
  el.top = 200;
  io.callback([{ target: el, isIntersecting: true }]);
  flush();
  assert.equal(resumed, 1);
  // Unparked viewers are not resumed again.
  GL.unpark("volume:a");
  io.callback([{ target: el, isIntersecting: true }]);
  flush();
  assert.equal(resumed, 1);
});

test("exported keys state the display that was shown", () => {
  const note = volumeDisplay.exportNote;
  assert.equal(
    note({ modified: true, threshold: [-9, 9], extent: 20, symbol: "z", lo: -22.2539, hi: 22.2539 }),
    "Display modified in the browser (exploratory): shown where |z| ≥ 9, clusters ≥ 20 voxels; " +
      "colour scale ±22.3. The Static view is the figure of record."
  );
  assert.equal(
    note({ modified: false, threshold: [-5, 5], extent: 20, symbol: "z", lo: -22.2539, hi: 22.2539 }),
    "Shown where |z| ≥ 5, clusters ≥ 20 voxels; colour scale ±22.3 (report settings)."
  );
  assert.equal(
    note({ modified: true, threshold: [-2, 7], extent: 1, symbol: "t", lo: 0, hi: 10 }),
    "Display modified in the browser (exploratory): hidden between −2 and 7; colour scale 0 to 10. " +
      "The Static view is the figure of record."
  );
  assert.match(note({ modified: false, threshold: null, extent: 20, symbol: "z", lo: -3, hi: 3 }), /^No threshold;/);
});

test("a colour-range end typed past the other end is rejected", () => {
  const reject = volumeDisplay.rejectCrossedRange;
  assert.deepEqual(reject([3, 10], "low", 12), [3, 10]);
  assert.deepEqual(reject([3, 10], "low", 10), [3, 10]);
  assert.deepEqual(reject([3, 10], "high", 2), [3, 10]);
  assert.equal(reject([3, 10], "low", 4), null);
  assert.equal(reject([3, 10], "high", 11), null);
  assert.deepEqual(reject([3, 10], "high", NaN), [3, 10]);
});

test("slider steps land on round values", () => {
  const grid = volumeDisplay.sliderGrid;
  assert.deepEqual(grid(-22.2539022445679, 22.2539022445679), { step: 0.1, min: -22.3, max: 22.3 });
  assert.deepEqual(grid(-29.567, 21.1), { step: 0.1, min: -29.6, max: 21.1 });
  assert.deepEqual(grid(0, 3.333), { step: 0.01, min: 0, max: 3.34 });
  // Every arrow step from the minimum is a whole number of steps.
  const g = grid(-22.2539, 22.2539);
  for (const v of [5, 5.1, 21.6]) {
    const k = (v - g.min) / g.step;
    assert.ok(Math.abs(k - Math.round(k)) < 1e-9, `${v} is on the grid`);
  }
});

test("viewer exports use the report's file stem", () => {
  assert.match(adapterSource, /closest\("\[data-nm-file-stem\]"\)/);
  assert.match(adapterSource, /stem \+ "_slices"/);
  assert.match(bridgeSource, /"_surface-" \+ \(VIEW_SLUGS\[view\] \|\| view\)/);
});
