// The report's print path: a capped wait with visible progress and a
// "Print now" escape, a second Ctrl/Cmd+P that prints at once, a page that
// leaves print state when print() returns without an afterprint event,
// glance thumbnails that never reach paper dark, failed figures that are
// called unavailable (not "still loading"), and a header control that never
// starts a line with its separator.
const test = require("node:test");
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { resolve } = require("node:path");
const { runInNewContext } = require("node:vm");

const reportSource = readFileSync(resolve(__dirname, "../../inst/report/montage-report.js"), "utf8");

const flush = () => new Promise((r) => setImmediate(r));

// A manual clock: timers run only when the test advances time.
function clock() {
  let now = 0;
  let seq = 0;
  const queue = [];
  const setTimeout = (fn, ms) => {
    const id = ++seq;
    queue.push({ id, at: now + (ms || 0), fn });
    return id;
  };
  const clearTimeout = (id) => {
    for (let i = queue.length - 1; i >= 0; i--) if (queue[i].id === id) queue.splice(i, 1);
  };
  const setInterval = (fn, ms) => {
    const id = ++seq;
    const again = () => { queue.push({ id, at: now + ms, fn: again }); fn(); };
    queue.push({ id, at: now + ms, fn: again });
    return id;
  };
  async function advance(ms) {
    const end = now + ms;
    for (;;) {
      await flush();
      queue.sort((a, b) => a.at - b.at);
      const next = queue[0];
      if (!next || next.at > end) break;
      queue.shift();
      now = next.at;
      next.fn();
    }
    now = end;
    await flush();
  }
  return { setTimeout, clearTimeout, setInterval, clearInterval: clearTimeout, advance, now: () => now };
}

function element(tag, classes = []) {
  const listeners = {};
  const attrs = {};
  const el = {
    tagName: tag.toUpperCase(),
    nodeType: 1,
    children: [],
    childNodes: [],
    parentNode: null,
    hidden: false,
    textContent: "",
    style: {},
    listeners,
    classList: {
      set: new Set(classes),
      add(c) { this.set.add(c); },
      remove(c) { this.set.delete(c); },
      contains(c) { return this.set.has(c); },
      toggle(c, on) { if (on) this.set.add(c); else this.set.delete(c); }
    },
    setAttribute(k, v) { attrs[k] = String(v); },
    getAttribute(k) { return k in attrs ? attrs[k] : null; },
    removeAttribute(k) { delete attrs[k]; },
    hasAttribute(k) { return k in attrs; },
    addEventListener(type, fn) { (listeners[type] = listeners[type] || []).push(fn); },
    removeEventListener(type, fn) {
      listeners[type] = (listeners[type] || []).filter((f) => f !== fn);
    },
    appendChild(child) {
      child.parentNode = el;
      el.childNodes.push(child);
      if (child.nodeType === 1) el.children.push(child);
      return child;
    },
    get lastElementChild() { return el.children[el.children.length - 1] || null; },
    get nextSibling() {
      if (!el.parentNode) return null;
      const sibs = el.parentNode.childNodes;
      return sibs[sibs.indexOf(el) + 1] || null;
    },
    closest: () => null,
    querySelector: () => null,
    querySelectorAll: () => []
  };
  Object.defineProperty(el, "className", {
    get() { return Array.from(el.classList.set).join(" "); },
    set(v) { el.classList.set = new Set(String(v).split(/\s+/).filter(Boolean)); }
  });
  return el;
}

function textNode(text) {
  return {
    nodeType: 3, textContent: text, parentNode: null,
    get nextSibling() {
      const sibs = this.parentNode ? this.parentNode.childNodes : [];
      return sibs[sibs.indexOf(this) + 1] || null;
    }
  };
}

// An <img> in a <picture> showing its dark copy until the light source is
// selected and arrives (`arrive()`), or fails (`fail()`).
function picture(name, boxClass) {
  const img = {
    tagName: "IMG",
    src: "http://x/" + name + ".png",
    currentSrc: "http://x/" + name + "-dark.png",
    complete: true, naturalWidth: 800, loading: "lazy",
    attrs: { src: name + ".png" },
    listeners: {},
    getAttribute(k) { return this.attrs[k] || null; },
    closest: () => null,
    addEventListener(type, fn) { (this.listeners[type] = this.listeners[type] || []).push(fn); },
    removeEventListener(type, fn) {
      this.listeners[type] = (this.listeners[type] || []).filter((f) => f !== fn);
    },
    decode() { return Promise.resolve(); },
    arrive() { this.currentSrc = this.src; this.naturalWidth = 800; (this.listeners.load || []).slice().forEach((f) => f({})); },
    fail() { this.currentSrc = this.src; this.naturalWidth = 0; (this.listeners.error || []).slice().forEach((f) => f({})); }
  };
  const box = element("figure", [boxClass]);
  box.querySelector = (sel) => (/img/.test(sel) ? img : null);
  return { img, box };
}

function reportPage({ figures = 3, thumbs = 1, header = null } = {}) {
  const time = clock();
  const docListeners = {};
  const winListeners = {};
  const items = [];
  for (let i = 0; i < figures; i++) items.push(picture("fig" + i, "nm-figure"));
  for (let i = 0; i < thumbs; i++) items.push(picture("thumb" + i, "nm-atlas-thumb"));
  const images = items.map((p) => p.img);
  const boxes = items.map((p) => p.box);
  const sources = images.map(() => element("source"));
  sources.forEach((s) => s.setAttribute("media", "(prefers-color-scheme: dark)"));
  const body = element("body");
  const ctx = {
    console, Promise,
    setTimeout: time.setTimeout, clearTimeout: time.clearTimeout,
    setInterval: time.setInterval, clearInterval: time.clearInterval,
    performance: { now: () => time.now() },
    Event: class { constructor(type) { this.type = type; } },
    document: {
      readyState: "loading",
      addEventListener(type, fn) { (docListeners[type] = docListeners[type] || []).push(fn); },
      querySelectorAll: (sel) => {
        if (/source\[data-nm-theme\]/.test(sel)) return sources;
        if (sel === ".nm-figure img, .nm-atlas-thumb img") return images;
        if (sel === ".nm-figure, .nm-atlas-thumb") return boxes;
        if (/^\.nm-figure$/.test(sel)) return boxes.filter((b) => b.classList.contains("nm-figure"));
        if (/nm-print-pending/.test(sel)) {
          return boxes.filter((b) => b.classList.contains("nm-print-pending") || b.classList.contains("nm-print-failed"));
        }
        return [];
      },
      querySelector: (sel) => (sel === ".nm-report-header" ? header : null),
      documentElement: { getAttribute: () => null },
      createElement: (tag) => element(tag),
      createTextNode: (text) => textNode(text),
      body
    },
    navigator: {}, location: { protocol: "http:", hash: "" }, history: {},
    getComputedStyle: () => ({})
  };
  ctx.window = ctx;
  ctx.addEventListener = (type, fn) => { (winListeners[type] = winListeners[type] || []).push(fn); };
  ctx.dispatchEvent = (event) => { (winListeners[event.type] || []).forEach((fn) => fn(event)); return true; };
  ctx.printed = [];
  ctx.print = () => {
    ctx.printed.push({
      at: time.now(),
      light: images.map((i) => i.currentSrc === i.src),
      pending: boxes.map((b) => b.classList.contains("nm-print-pending")),
      failed: boxes.map((b) => b.classList.contains("nm-print-failed"))
    });
  };
  runInNewContext(reportSource, ctx);
  const press = () => (docListeners.keydown || []).forEach((fn) => fn({
    ctrlKey: true, metaKey: false, altKey: false, shiftKey: false, key: "p", code: "KeyP",
    preventDefault() {}
  }));
  const domReady = () => (docListeners.DOMContentLoaded || []).forEach((fn) => {
    try { fn(); } catch (err) { /* other sections need a fuller page */ }
  });
  return { ctx, time, images, boxes, sources, body, press, domReady, winListeners };
}

const sheetOf = (body) => body.children.find((c) => c.classList.contains("nm-print-sheet"));

test("a stalled figure delays Ctrl/Cmd+P by at most ~10 s, with visible progress and Print now", async () => {
  const page = reportPage({ figures: 3, thumbs: 0 });
  page.press();
  page.images[0].arrive();
  page.images[1].arrive();
  // images[2] never answers.
  await page.time.advance(400);
  const sheet = sheetOf(page.body);
  assert.ok(sheet && !sheet.hidden, "a progress note is on screen while the print waits");
  assert.match(sheet.children[0].textContent, /2 of 3 figures loaded/);
  const now = sheet.children.find((c) => c.tagName === "BUTTON");
  assert.equal(now && now.textContent, "Print now");
  assert.equal(page.ctx.printed.length, 0);
  await page.time.advance(9700);
  assert.equal(page.ctx.printed.length, 1, "printing starts once the cap is reached");
  assert.ok(page.ctx.printed[0].at <= 10500, `printed at ${page.ctx.printed[0].at} ms`);
  assert.deepEqual(page.ctx.printed[0].pending, [false, false, true], "the missing figure is marked on paper");
  assert.equal(sheet.hidden, true, "the progress note goes when the dialog opens");
});

test("a second Ctrl/Cmd+P, or Print now, prints at once", async () => {
  const page = reportPage({ figures: 2, thumbs: 0 });
  page.press();
  await page.time.advance(500);
  assert.equal(page.ctx.printed.length, 0);
  page.press();
  await page.time.advance(0);
  assert.equal(page.ctx.printed.length, 1, "the second press is not swallowed");

  const other = reportPage({ figures: 2, thumbs: 0 });
  other.press();
  await other.time.advance(400);
  const now = sheetOf(other.body).children.find((c) => c.tagName === "BUTTON");
  now.listeners.click.forEach((fn) => fn({}));
  await other.time.advance(0);
  assert.equal(other.ctx.printed.length, 1, "Print now prints");
});

test("a print() that returns without afterprint still leaves print state", async () => {
  const page = reportPage({ figures: 1, thumbs: 0 });
  let themed = 0;
  page.ctx.NeuroMosaicReport.applyTheme = () => { themed += 1; };
  page.images[0].arrive();
  page.press();
  await page.time.advance(100);
  assert.equal(page.ctx.printed.length, 1);
  assert.equal(page.ctx.NeuroMosaicReport.isPrinting(), true);
  // No dialog, no afterprint (RStudio Viewer, sandboxed iframe).
  await page.time.advance(1500);
  assert.equal(page.ctx.NeuroMosaicReport.isPrinting(), false, "print state is left");
  assert.ok(themed >= 1, "the theme (dark figures) is re-applied");
  // A later press prints again rather than being swallowed.
  page.press();
  await page.time.advance(100);
  assert.equal(page.ctx.printed.length, 2);
});

test("a menu print never puts a dark glance thumbnail on paper", () => {
  const page = reportPage({ figures: 1, thumbs: 2 });
  page.images[0].arrive();
  (page.winListeners.beforeprint || []).forEach((fn) => fn({}));
  assert.ok(page.sources.every((s) => s.getAttribute("media") === "not all"), "dark sources are switched off");
  const thumbs = page.boxes.filter((b) => b.classList.contains("nm-atlas-thumb"));
  assert.ok(thumbs.every((b) => b.classList.contains("nm-print-pending")),
    "thumbnails still showing their dark copy are held back from paper");
  page.images[1].arrive();
  assert.equal(thumbs[0].classList.contains("nm-print-pending"), false, "the light copy clears the hold");
  (page.winListeners.afterprint || []).forEach((fn) => fn({}));
  assert.ok(page.boxes.every((b) => !b.classList.contains("nm-print-pending")));
});

test("a figure that failed to load prints as unavailable, not as still loading", async () => {
  const page = reportPage({ figures: 2, thumbs: 0 });
  page.images[0].arrive();
  page.images[1].fail();
  page.press();
  await page.time.advance(100);
  assert.equal(page.ctx.printed.length, 1, "a broken figure does not hold the print");
  assert.deepEqual(page.ctx.printed[0].failed, [false, true]);
  assert.deepEqual(page.ctx.printed[0].pending, [false, false]);
  const css = readFileSync(resolve(__dirname, "../../inst/report/montage-report.css"), "utf8");
  assert.match(css, /\.nm-figure\.nm-print-failed::before \{\s*content: "This figure could not be loaded/);
});

test("the header print control keeps its separator with the line's last words", () => {
  const header = element("header", ["nm-report-header"]);
  const labels = element("p", ["nm-report-labels"]);
  const tail = element("span", ["nm-nowrap"]);
  labels.appendChild(textNode("Peak labels: Schaefer-200, ASEG; "));
  labels.appendChild(tail);
  labels.appendChild(textNode("\n"));
  header.appendChild(labels);
  header.querySelector = (sel) => (sel === ".nm-report-labels" ? labels : null);
  const page = reportPage({ figures: 0, thumbs: 0, header });
  page.domReady();
  const button = tail.children.find((c) => c.tagName === "BUTTON");
  assert.ok(button, "the control joins the unbreakable run");
  const sep = tail.children[tail.children.indexOf(button) - 1];
  assert.equal(sep.textContent.trim(), "·");
  assert.equal(sep.getAttribute("aria-hidden"), "true");
  const css = readFileSync(resolve(__dirname, "../../inst/report/montage-report.css"), "utf8");
  assert.doesNotMatch(css, /\.nm-print-header::before/, "no generated dot that can start a line");
  assert.match(css, /@media \(max-width: 600px\), \(pointer: coarse\) \{\s*\.nm-print-action \{ min-height: 32px; \}/);
});
