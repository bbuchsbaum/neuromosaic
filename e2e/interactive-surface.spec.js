const { test, expect } = require("@playwright/test");
const { resolve } = require("node:path");
const { pathToFileURL } = require("node:url");

const fixture = pathToFileURL(resolve(
  __dirname, ".artifacts", "interactive-surface.html"
)).href;
const bundleFixture =
  "http://127.0.0.1:4173/interactive-surface-bundle.html";
const mixedFixture = pathToFileURL(resolve(
  __dirname, ".artifacts", "interactive-mixed.html"
)).href;

async function openSurfaceReport(page, url = fixture) {
  await page.goto(url, { waitUntil: "load" });
  await expect(page.getByRole("heading", {
    name: "Neuromosaic interactive surface browser fixture",
    exact: true
  })).toBeVisible();
  return page.locator(
    '[data-nm-surface-analysis="surface-interactive"]'
  );
}

async function activateSurface(host) {
  const router = host.locator(
    'xpath=preceding-sibling::*[@data-nm-view-router][1]'
  );
  await router.getByRole("radio", { name: "Surface", exact: true }).check();
  await expect(host.locator("[data-nm-surface-status]")).toContainText(
    "Interactive surface ready",
    { timeout: 20_000 }
  );
  await expect(host.locator("canvas")).toBeVisible();
}

async function surfaceState(host) {
  return host.evaluate((node) => {
    const widget = node.querySelector(".surfwidget");
    const handle = widget && widget.__surfviewHandle;
    const snapshot = handle && handle.controlTarget &&
      handle.controlTarget.getSnapshot();
    const displayedLayerId = snapshot &&
      snapshot.capabilities.exclusiveMap.displayedLayerId;
    let layer = null;
    if (snapshot && displayedLayerId) {
      for (const surface of snapshot.surfaces) {
        layer = surface.layers.find((candidate) =>
          candidate.id === displayedLayerId
        );
        if (layer) break;
      }
    }
    return {
      mapId: node.getAttribute("data-nm-surface-map"),
      displayedLayerId,
      camera: handle && handle.viewer && handle.viewer.getCameraState(),
      display: layer && {
        colorMapId: layer.scalarMapping.colorMap.id,
        displayRange: Array.from(layer.scalarMapping.displayRange.value),
        maskInterval: Array.from(layer.scalarMapping.maskInterval.value),
        opacity: layer.opacity
      }
    };
  });
}

test("the static surface remains authoritative until disclosure", async ({
  page
}) => {
  const host = await openSurfaceReport(page);
  await expect(page.getByRole("img", {
    name: "Surface Z statistic surface montage"
  })).toBeVisible();
  await expect(host.locator("details")).not.toHaveAttribute("open", "");
  await expect(host).toBeHidden();
  const router = host.locator(
    'xpath=preceding-sibling::*[@data-nm-view-router][1]'
  );
  await expect(router.getByRole("radio", { name: "Static" })).toBeChecked();
  await expect(router.getByRole("radio", { name: "Surface" })).toBeVisible();
  await expect(router.getByRole("radio", { name: "Slices" })).toHaveCount(0);
  await expect(host.locator("canvas")).toHaveCount(0);
  await expect(host).toContainText("static montage above remains authoritative");

  await activateSurface(host);
  await expect(host.getByRole("combobox", {
    name: "Displayed surface map"
  })).toBeVisible();
  const lateral = host.getByRole("radio", { name: "Lateral" });
  await expect(lateral).toBeEnabled();
  await lateral.check();
  await expect(lateral).toBeChecked();
});

test("static and surface map selectors synchronize in both directions", async ({
  page
}) => {
  const host = await openSurfaceReport(page);
  const group = page.locator('[data-nm-map-group="surface-interactive"]');

  await group.getByRole("tab", { name: "Estimate" }).click();
  await expect(host).toHaveAttribute("data-nm-surface-map", "surface_estimate");
  await expect(host.locator("[data-nm-surface-target]")).toHaveText("Estimate");

  await activateSurface(host);
  const mapping = await host.evaluate((node) => JSON.parse(
    node.getAttribute("data-nm-surface-map-to-layer")
  ));
  await expect.poll(async () => (await surfaceState(host)).displayedLayerId)
    .toBe(mapping.surface_estimate);

  await host.getByRole("combobox", {
    name: "Displayed surface map"
  }).selectOption(mapping.surface_se);
  await expect(host).toHaveAttribute("data-nm-surface-map", "surface_se");
  await expect(group.getByRole("tab", { name: "SE" })).toHaveAttribute(
    "aria-selected", "true"
  );
  await expect(group.locator('[data-nm-map-id="surface_se"]')).toBeVisible();
});

test("map switching preserves the reader camera", async ({ page }) => {
  const host = await openSurfaceReport(page);
  await activateSurface(host);
  const group = page.locator('[data-nm-map-group="surface-interactive"]');
  const before = await host.evaluate((node) => {
    const handle = node.querySelector(".surfwidget").__surfviewHandle;
    handle.viewer.setCameraState({
      position: [2.75, 3.5, 6.25],
      target: [0.25, 0.1, 0.2]
    });
    return handle.viewer.getCameraState();
  });

  await group.getByRole("tab", { name: "Reliability" }).click();
  await expect(host).toHaveAttribute(
    "data-nm-surface-map", "surface_reliability"
  );
  await expect.poll(async () => (await surfaceState(host)).camera).toEqual(before);
});

test("display controls retain map-local exploration and reset exactly", async ({
  page
}) => {
  const host = await openSurfaceReport(page);
  await activateSurface(host);
  const group = page.locator('[data-nm-map-group="surface-interactive"]');
  const zDefaults = (await surfaceState(host)).display;
  const palette = host.getByRole("combobox", { name: "Surface colormap" });
  const opacity = host.getByRole("slider", { name: "Surface opacity" });
  const thresholdLow = host.getByLabel("Surface threshold low");
  const thresholdHigh = host.getByLabel("Surface threshold high");
  const rangeLow = host.getByLabel("Surface range low");
  const rangeHigh = host.getByLabel("Surface range high");

  await expect(palette).toHaveValue("RdBu");
  await expect(thresholdLow).toHaveValue("-3.1");
  await expect(thresholdHigh).toHaveValue("3.1");
  await palette.selectOption("viridis");
  await opacity.evaluate((node) => {
    node.value = "0.35";
    node.dispatchEvent(new Event("input", { bubbles: true }));
  });
  await thresholdLow.fill("-4");
  await thresholdLow.dispatchEvent("change");
  await thresholdHigh.fill("4");
  await thresholdHigh.dispatchEvent("change");
  await rangeLow.fill("-6");
  await rangeLow.dispatchEvent("change");
  await rangeHigh.fill("6");
  await rangeHigh.dispatchEvent("change");
  await expect.poll(async () => (await surfaceState(host)).display).toEqual({
    colorMapId: "viridis",
    displayRange: [-6, 6],
    maskInterval: [-4, 4],
    opacity: 0.35
  });
  await expect(host).toHaveAttribute("data-nm-surface-modified", "true");
  await expect(host.locator("[data-nm-surface-status]")).toContainText(
    "exploratory only"
  );

  await group.getByRole("tab", { name: "SE" }).click();
  await expect(host).toHaveAttribute("data-nm-surface-map", "surface_se");
  await expect(palette).toHaveValue("inferno");
  await expect(host).toHaveAttribute("data-nm-surface-modified", "false");
  await palette.selectOption("magma");
  await opacity.evaluate((node) => {
    node.value = "0.25";
    node.dispatchEvent(new Event("input", { bubbles: true }));
  });

  await group.getByRole("tab", { name: "Z" }).click();
  await expect.poll(async () => (await surfaceState(host)).display).toEqual({
    colorMapId: "viridis",
    displayRange: [-6, 6],
    maskInterval: [-4, 4],
    opacity: 0.35
  });
  await host.getByRole("button", { name: "Reset map display" }).click();
  await expect.poll(async () => (await surfaceState(host)).display).toEqual(
    zDefaults
  );
  await expect(host).toHaveAttribute("data-nm-surface-modified", "false");
});

test("embedded surface reports make no external requests and hide in print", async ({
  page
}) => {
  const requested = [];
  const errors = [];
  page.on("request", (request) => requested.push(request.url()));
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  const host = await openSurfaceReport(page);
  await activateSurface(host);

  expect(requested.filter((url) => /^https?:/.test(url))).toEqual([]);
  expect(errors).toEqual([]);
  await page.emulateMedia({ media: "print" });
  await expect(host).toBeHidden();
  await expect(page.getByRole("img", {
    name: "Surface Z statistic surface montage"
  })).toBeVisible();
});

test("bundled surface data stay relative, deduplicated, and lazy", async ({
  page
}) => {
  const requested = [];
  page.on("request", (request) => requested.push(request.url()));
  const host = await openSurfaceReport(page, bundleFixture);
  expect(requested.filter((url) => /\.bin\.gz(?:$|[?#])/.test(url))).toEqual([]);
  await expect(host).toContainText("recoverable per-vertex surface data");

  await activateSurface(host);
  const assets = requested.filter((url) => /\.bin\.gz(?:$|[?#])/.test(url));
  expect(assets.length).toBeGreaterThan(0);
  expect(new Set(assets).size).toBe(assets.length);
  expect(assets.every((url) => url.startsWith(
    "http://127.0.0.1:4173/interactive-surface-bundle_files/"
  ))).toBe(true);
  expect(requested.filter((url) =>
    /^https?:/.test(url) && !url.startsWith("http://127.0.0.1:4173/")
  )).toEqual([]);
});

test("surface viewers release browser resources on pagehide", async ({ page }) => {
  const host = await openSurfaceReport(page);
  await activateSurface(host);
  const disposed = await host.evaluate((node) => {
    const handle = node.__nmSurfaceHandle;
    window.dispatchEvent(new Event("pagehide"));
    return {
      viewerReleased: handle.viewer === null,
      canvasCount: node.querySelectorAll("canvas").length
    };
  });
  expect(disposed).toEqual({ viewerReleased: true, canvasCount: 0 });
});

test("mixed reports expose only eligible static, slice, and surface routes", async ({
  page
}) => {
  await page.goto(mixedFixture, { waitUntil: "load" });
  const surface = page.locator(
    '[data-nm-surface-analysis="surface-interactive"]'
  );
  const volume = page.locator(
    '[data-nm-volume-analysis="surface-interactive"]'
  );
  const router = page.locator(
    '[data-nm-view-router="surface-interactive"]'
  );
  const staticChoice = router.getByRole("radio", { name: "Static" });
  const sliceChoice = router.getByRole("radio", { name: "Slices" });
  const surfaceChoice = router.getByRole("radio", { name: "Surface" });

  await expect(staticChoice).toBeChecked();
  await expect(surface).toBeHidden();
  await expect(volume).toBeHidden();
  await sliceChoice.check();
  await expect(volume).toBeVisible();
  await expect(surface).toBeHidden();
  await expect(volume.locator("[data-nm-volume-shell]")).toBeVisible();
  await surfaceChoice.check();
  await expect(surface).toBeVisible();
  await expect(volume).toBeHidden();
  await expect(surface.locator("canvas")).toBeVisible({ timeout: 20_000 });
  await staticChoice.check();
  await expect(surface).toBeHidden();
  await expect(volume).toBeHidden();
  await expect(page.getByRole("img", {
    name: "Surface Z statistic surface montage"
  })).toBeVisible();
});

test("corrupt surface payloads fail visibly without displacing static figures", async ({
  page
}) => {
  const host = await openSurfaceReport(page);
  await page.locator('script[id^="nm-surface-asset-values-"]').first()
    .evaluate((node) => { node.textContent = "AAAA"; });
  const router = host.locator(
    'xpath=preceding-sibling::*[@data-nm-view-router][1]'
  );
  await router.getByRole("radio", { name: "Surface" }).check();
  await expect(host.locator("[data-nm-surface-status]")).toContainText(
    "Interactive surface unavailable",
    { timeout: 20_000 }
  );
  await expect(host.locator("[data-nm-surface-author-fallback]")).toBeVisible();
  await expect(page.getByRole("img", {
    name: "Surface Z statistic surface montage"
  })).toBeVisible();
});

test.describe("without JavaScript", () => {
  test.use({ javaScriptEnabled: false });

  test("the static surfaces and explicit fallback remain readable", async ({
    page
  }) => {
    const host = await openSurfaceReport(page);
    await expect(page.getByRole("img", {
      name: "Surface Z statistic surface montage"
    })).toBeVisible();
    await expect(host.getByText(
      "JavaScript is disabled; use the static surface montage above.",
      { exact: true }
    )).toBeVisible();
    await expect(host.locator("canvas")).toHaveCount(0);
  });
});
