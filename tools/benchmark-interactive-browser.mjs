// Measure the generated bundle reports in the project Playwright Chromium.
// Run the browser automation guard before and after this script.

import http from "node:http";
import { readFile, writeFile } from "node:fs/promises";
import { extname, join, resolve } from "node:path";
import { chromium } from "@playwright/test";

const root = resolve("e2e/.artifacts/benchmark");
const executablePath = process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH;
const allFixtures = [
  ["2mm", "sparse"],
  ["2mm", "dense"],
  ["1mm", "sparse"],
  ["1mm", "dense"]
];
const selectedGrids = new Set(
  (process.env.NM_BENCH_GRIDS || "2mm,1mm").split(",").map((x) => x.trim())
);
const selectedDensities = new Set(
  (process.env.NM_BENCH_DENSITIES || "sparse,dense")
    .split(",").map((x) => x.trim())
);
const fixtures = allFixtures.filter(([grid, density]) =>
  selectedGrids.has(grid) && selectedDensities.has(density)
);
const mime = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json",
  ".gz": "application/gzip",
  ".png": "image/png"
};

const server = http.createServer(async (request, response) => {
  try {
    const pathname = decodeURIComponent(new URL(
      request.url, "http://127.0.0.1"
    ).pathname).replace(/^\/+/, "");
    const path = join(root, pathname);
    if (!path.startsWith(root)) throw new Error("unsafe path");
    const body = await readFile(path);
    response.writeHead(200, {
      "content-type": mime[extname(path)] || "application/octet-stream",
      "content-length": body.length,
      "cache-control": "no-store"
    });
    response.end(body);
  } catch {
    response.writeHead(404);
    response.end("not found");
  }
});

await new Promise((resolveListen) => server.listen(0, "127.0.0.1", resolveListen));
const port = server.address().port;
const browser = await chromium.launch(executablePath ? { executablePath } : {});
const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });

async function heapSnapshot(cdp) {
  await cdp.send("HeapProfiler.collectGarbage");
  const heap = await cdp.send("Runtime.getHeapUsage");
  return {
    js_heap_mib: heap.usedSize / 1024 ** 2,
    embedder_heap_mib: heap.embedderHeapUsedSize / 1024 ** 2,
    backing_store_mib: heap.backingStorageSize / 1024 ** 2
  };
}

async function measure(grid, density) {
  const page = await context.newPage();
  const cdp = await context.newCDPSession(page);
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  const url = `http://127.0.0.1:${port}/interactive-benchmark-${grid}-${density}.html`;
  const navigationStart = performance.now();
  await page.goto(url, { waitUntil: "load", timeout: 120_000 });
  const navigation_ms = performance.now() - navigationStart;
  const host = page.locator("[data-nm-volume-host]");
  const baseline = await heapSnapshot(cdp);

  const activationStart = performance.now();
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();
  await host.waitFor({ state: "visible" });
  await page.waitForFunction(() => {
    const node = document.querySelector("[data-nm-volume-host]");
    return node && node.dataset.nmVolumeReady === "true";
  }, null, { timeout: 120_000 });
  const activation_ms = performance.now() - activationStart;

  const samples = [];
  async function sample(overlays, switch_ms) {
    const memory = await heapSnapshot(cdp);
    const state = await host.evaluate((node) => {
      const current = globalThis.NeuroMosaicVolumeReport.getState(node);
      return {
        cache_maps: current.overlayCache.size,
        current_map: current.currentMapId
      };
    });
    const resources = await page.evaluate(() => performance
      .getEntriesByType("resource")
      .filter((entry) => entry.name.endsWith(".nii.gz"))
      .reduce((total, entry) => total + entry.encodedBodySize, 0));
    samples.push({
      grid,
      density,
      overlays,
      navigation_ms,
      activation_ms,
      switch_ms,
      transferred_nifti_mib: resources / 1024 ** 2,
      cache_maps: state.cache_maps,
      current_map: state.current_map,
      js_heap_delta_mib: memory.js_heap_mib - baseline.js_heap_mib,
      embedder_heap_delta_mib:
        memory.embedder_heap_mib - baseline.embedder_heap_mib,
      backing_store_delta_mib:
        memory.backing_store_mib - baseline.backing_store_mib
    });
  }
  await sample(1, 0);

  const select = host.getByRole("combobox", { name: "Interactive map" });
  let switchStart = performance.now();
  for (let index = 2; index <= 4; index += 1) {
    await select.selectOption(`map_${index}`);
    await page.waitForFunction((mapId) => {
      const node = document.querySelector("[data-nm-volume-host]");
      const state = globalThis.NeuroMosaicVolumeReport.getState(node);
      return state && state.currentMapId === mapId;
    }, `map_${index}`, { timeout: 120_000 });
  }
  await sample(4, performance.now() - switchStart);

  switchStart = performance.now();
  for (let index = 5; index <= 8; index += 1) {
    await select.selectOption(`map_${index}`);
    await page.waitForFunction((mapId) => {
      const node = document.querySelector("[data-nm-volume-host]");
      const state = globalThis.NeuroMosaicVolumeReport.getState(node);
      return state && state.currentMapId === mapId;
    }, `map_${index}`, { timeout: 120_000 });
  }
  await sample(8, performance.now() - switchStart);
  if (errors.length) throw new Error(errors.join("\n"));
  await cdp.detach();
  await page.close();
  return samples;
}

try {
  const results = [];
  for (const [grid, density] of fixtures) {
    results.push(...await measure(grid, density));
  }
  await writeFile(
    join(root, "browser-results.json"),
    JSON.stringify(results, null, 2) + "\n"
  );
  process.stdout.write(JSON.stringify(results, null, 2) + "\n");
} finally {
  await context.close();
  await browser.close();
  await new Promise((resolveClose) => server.close(resolveClose));
}
