const { test, expect } = require("@playwright/test");
const { resolve } = require("node:path");
const { pathToFileURL } = require("node:url");

const fixture = pathToFileURL(resolve(
  __dirname, ".artifacts", "interactive-volume.html"
)).href;
const noSelectorFixture = pathToFileURL(resolve(
  __dirname, ".artifacts", "interactive-volume-none.html"
)).href;
const bundleFixture =
  "http://127.0.0.1:4173/interactive-volume-bundle.html";

async function openInteractiveReport(page, url = fixture) {
  await page.goto(url, { waitUntil: "load" });
  await expect(page.getByRole("heading", {
    name: "Neuromosaic interactive volume browser fixture",
    exact: true
  })).toBeVisible();
  return page.locator(
    '[data-nm-volume-analysis="faces-interactive"]'
  );
}

async function activate(page, host) {
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();
  await expect(host).toHaveAttribute("data-nm-volume-ready", "true", {
    timeout: 20_000
  });
  await expect(host.locator("[data-nm-volume-shell]")).toBeVisible();
}

async function viewerState(host) {
  return host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    return {
      currentMapId: state.currentMapId,
      worldCoord: state.viewer.getWorldCoord(),
      controls: state.controls.getState("neuromosaic-overlay")
    };
  });
}

test("the static report is authoritative until the reader opens the viewer", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  const staticPanel = page.locator(
    '[data-nm-map-id="interactive_z"]'
  );

  await expect(staticPanel.getByRole("img", {
    name: "Z statistic volume montage"
  })).toBeVisible();
  await expect(host.locator("[data-nm-volume-shell]")).toBeHidden();
  await expect(host.locator("canvas")).toHaveCount(0);
  await expect(host).toContainText("static montage is the report default");
  await expect(host).toContainText("recoverable voxel data");

  await activate(page, host);
  await expect(host.locator("canvas")).toHaveCount(3);
  await expect(host.locator('[data-view="axial"]')).toBeVisible();
  await expect(host.locator('[data-view="coronal"]')).toBeVisible();
  await expect(host.locator('[data-view="sagittal"]')).toBeVisible();
  await expect(host.locator("[data-nm-volume-readout]")).toContainText(
    "Standardized association:"
  );
  await expect(host.getByRole("combobox", { name: "Colormap" })).toHaveValue(
    "BlueRed"
  );
  await expect(host.getByLabel("Threshold low")).toHaveValue("-3.10");
  await expect(host.getByLabel("Threshold high")).toHaveValue("3.10");
});

test("map switching preserves map-local exploration and exact report resets", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);

  const mapSelect = host.getByRole("combobox", { name: "Interactive map" });
  const palette = host.getByRole("combobox", { name: "Colormap" });
  const opacity = host.getByLabel("Opacity value");

  await palette.selectOption("Viridis");
  await opacity.fill("0.31");
  await opacity.press("Enter");
  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    colormap: "Viridis",
    opacity: 0.31
  });
  await expect(host).toHaveAttribute("data-nm-volume-modified", "true");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "exploratory only"
  );

  await mapSelect.selectOption("interactive_se");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "Interactive view ready"
  );
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_se"
  );
  await expect(palette).toHaveValue("Inferno");
  await expect(host.getByText("Threshold", { exact: true })).toHaveCount(0);
  await expect(host.getByText("Range", { exact: true })).toBeVisible();
  await expect(host.locator("[data-nm-volume-readout]")).toContainText(
    "Standard error:"
  );

  const defaults = await viewerState(host);
  await palette.selectOption("Viridis");
  await opacity.fill("0.23");
  await opacity.press("Enter");
  await host.getByRole("button", { name: "Reset to defaults" }).click();
  await expect.poll(async () => (await viewerState(host)).controls).toEqual(
    defaults.controls
  );

  await mapSelect.selectOption("interactive_z");
  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    colormap: "Viridis",
    opacity: 0.31
  });
  await expect(host).toHaveAttribute("data-nm-volume-modified", "true");

  await host.getByRole("button", { name: "Reset to defaults" }).click();
  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    colormap: "BlueRed",
    threshold: [-3.1, 3.1],
    opacity: 0.7
  });
  await expect(host).toHaveAttribute("data-nm-volume-modified", "false");
});

test("static and interactive selectors stay synchronized before and after activation", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  const group = page.locator('[data-nm-map-group="faces-interactive"]');
  await group.getByRole("tab", { name: "SE" }).click();
  await expect(host).toHaveAttribute("data-nm-volume-map", "interactive_se");
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("SE");

  await activate(page, host);
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_se"
  );
  await expect(host.getByRole("combobox", {
    name: "Interactive map"
  })).toHaveValue("interactive_se");

  await host.getByRole("combobox", {
    name: "Interactive map"
  }).selectOption("interactive_estimate");
  await expect(group.locator(
    '[data-nm-map-id="interactive_estimate"]'
  )).toBeVisible();
  await expect(group.getByRole("tab", {
    name: "Estimate"
  })).toHaveAttribute("aria-selected", "true");

  await host.getByRole("combobox", {
    name: "Interactive map"
  }).selectOption("interactive_reliability");
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_reliability"
  );
  await expect(host.getByRole("combobox", { name: "Colormap" })).toHaveValue(
    "Inferno"
  );
  await expect(host.getByLabel("Range low")).toHaveValue("0.00");
  await expect(host.getByLabel("Range high")).toHaveValue("1.00");
  await expect(host.locator("[data-nm-volume-readout]")).toContainText(
    "agreement"
  );
  await expect(group.locator(
    '[data-nm-map-id="interactive_reliability"]'
  )).toBeVisible();
  await expect(host.locator("[data-nm-volume-target]")).toHaveText(
    "Reliability"
  );
});

test("expanded static mode identifies and opens every map in one viewer", async ({
  page
}) => {
  const host = await openInteractiveReport(page, noSelectorFixture);
  const group = page.locator('[data-nm-map-group="faces-interactive"]');
  await expect(group.locator('[data-nm-map-variant]')).toHaveCount(4);
  for (const name of [
    "Z statistic volume montage",
    "Beta estimate volume montage",
    "Standard error volume montage",
    "Reliability volume montage"
  ]) {
    await expect(group.getByRole("img", { name })).toBeVisible();
  }
  await expect(group.locator('[role="tablist"]')).toHaveCount(0);
  await expect(group.getByRole("combobox", { name: "Map variant" })).toHaveCount(0);
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("Z");

  await activate(page, host);
  const mapSelect = host.getByRole("combobox", { name: "Interactive map" });
  await expect(mapSelect.locator("option")).toHaveCount(4);
  await mapSelect.selectOption("interactive_se");
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_se"
  );
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("SE");
  await expect(group.getByRole("img", {
    name: "Z statistic volume montage"
  })).toBeVisible();
  await expect(group.getByRole("img", {
    name: "Standard error volume montage"
  })).toBeVisible();
});

test("launcher and display controls are keyboard operable", async ({ page }) => {
  const host = await openInteractiveReport(page);
  const launcher = host.getByRole("button", {
    name: "Explore volume interactively"
  });
  await launcher.focus();
  await expect(launcher).toBeFocused();
  expect(await launcher.evaluate((node) => {
    const style = getComputedStyle(node);
    return [style.outlineStyle, style.outlineWidth];
  })).toEqual(["solid", "3px"]);
  await launcher.press("Enter");
  await expect(host).toHaveAttribute("data-nm-volume-ready", "true", {
    timeout: 20_000
  });

  const palette = host.getByRole("combobox", { name: "Colormap" });
  const initialPalette = await palette.inputValue();
  await palette.focus();
  await expect(palette).toBeFocused();
  expect(await palette.evaluate((node) => getComputedStyle(node).boxShadow))
    .not.toBe("none");
  await palette.press("v");
  await expect.poll(() => palette.inputValue()).not.toBe(initialPalette);
  await expect(host).toHaveAttribute("data-nm-volume-modified", "true");

  const reset = host.getByRole("button", { name: "Reset to defaults" });
  await reset.focus();
  await expect(reset).toBeFocused();
  await reset.press("Enter");
  await expect(host).toHaveAttribute("data-nm-volume-modified", "false");
  await expect(palette).toHaveValue(initialPalette);
});

test("an empty overlay opens with an explicit status and static fallback", async ({
  page
}) => {
  await openInteractiveReport(page);
  const host = page.locator(
    '[data-nm-volume-analysis="empty-interactive"]'
  );
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("Empty Z");
  const staticImage = page.getByRole("img", {
    name: "Empty Z statistic volume montage"
  });
  await expect(staticImage).toBeVisible();

  await activate(page, host);
  await expect(host.locator("canvas")).toHaveCount(3);
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "has no displayable voxels"
  );
  await expect(staticImage).toBeVisible();
});

test("coordinate navigation updates raw readout and resets to report position", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);
  const initial = (await viewerState(host)).worldCoord;

  await host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    state.viewer.setWorldCoord([-4, -3, -2]);
  });
  await expect(host.locator("[data-nm-volume-readout]")).toContainText(
    "x -4.0, y -3.0, z -2.0 mm"
  );
  await host.getByRole("button", {
    name: "Reset to report position"
  }).click();
  await expect.poll(async () => (await viewerState(host)).worldCoord).toEqual(
    initial
  );
});

test("threshold and palette widgets update all orthogonal views", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);
  const before = await viewerState(host);
  const canvases = host.locator("canvas");
  const beforeCanvases = await Promise.all(
    [0, 1, 2].map((index) => canvases.nth(index).screenshot())
  );

  const low = host.getByLabel("Threshold low");
  const high = host.getByLabel("Threshold high");
  await low.fill("-4.25");
  await low.press("Enter");
  await high.fill("4.25");
  await high.press("Enter");
  await host.getByRole("combobox", { name: "Colormap" }).selectOption(
    "Viridis"
  );

  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    threshold: [-4.25, 4.25],
    colormap: "Viridis"
  });
  await page.evaluate(() => new Promise((resolveFrame) => {
    requestAnimationFrame(() => requestAnimationFrame(resolveFrame));
  }));
  const afterCanvases = await Promise.all(
    [0, 1, 2].map((index) => canvases.nth(index).screenshot())
  );
  expect(afterCanvases.map((image, index) =>
    !image.equals(beforeCanvases[index])
  )).toEqual([true, true, true]);

  await host.getByRole("button", { name: "Reset to defaults" }).click();
  await expect.poll(async () => (await viewerState(host)).controls).toEqual(
    before.controls
  );
});

test("viewer failure remains local and leaves the static panel readable", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await page.evaluate(() => {
    document.querySelectorAll('script[type="application/octet-stream"]')[0]
      .remove();
  });
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();

  await expect(host).toHaveAttribute("data-nm-volume-failed", "true");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "static panel remains authoritative"
  );
  await expect(page.locator(
    '[data-nm-map-id="interactive_z"] img'
  )).toBeVisible();
});

test("unsupported scene versions fail closed without disturbing static output", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await page.evaluate(() => {
    const node = document.querySelector('script[type="application/json"]');
    const scene = JSON.parse(node.textContent);
    scene.schema_version = 999;
    node.textContent = JSON.stringify(scene);
  });
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();
  await expect(host).toHaveAttribute("data-nm-volume-failed", "true");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "Unsupported neuromosaic VolumeScene"
  );
  await expect(page.locator(
    '[data-nm-map-id="interactive_z"] img'
  )).toBeVisible();
});

test("embedded file reports make no network requests", async ({ page }) => {
  const requested = [];
  const errors = [];
  page.on("request", (request) => requested.push(request.url()));
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });

  const host = await openInteractiveReport(page);
  await activate(page, host);

  expect(requested.filter((url) => /^https?:/.test(url))).toEqual([]);
  expect(errors).toEqual([]);
});

test("bundle mode lazily fetches only local NIfTI assets over HTTP", async ({
  page
}) => {
  const requested = [];
  const errors = [];
  page.on("request", (request) => requested.push(request.url()));
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  const host = await openInteractiveReport(page, bundleFixture);
  expect(requested.filter((url) => url.endsWith(".nii.gz"))).toEqual([]);

  await activate(page, host);
  await expect.poll(() => new Set(requested.filter((url) =>
    url.endsWith(".nii.gz")
  )).size).toBe(2);
  await host.getByRole("combobox", {
    name: "Interactive map"
  }).selectOption("interactive_se");
  await expect.poll(() => new Set(requested.filter((url) =>
    url.endsWith(".nii.gz")
  )).size).toBe(3);
  const networkRequests = requested.filter((url) => /^https?:/.test(url));
  expect(networkRequests.every((url) =>
    new URL(url).hostname === "127.0.0.1"
  )).toBe(true);
  expect(errors).toEqual([]);
});

test("embedded and bundled delivery initialize the same report state", async ({
  page
}) => {
  let host = await openInteractiveReport(page);
  await activate(page, host);
  const embedded = await viewerState(host);

  host = await openInteractiveReport(page, bundleFixture);
  await activate(page, host);
  const bundled = await viewerState(host);
  expect(bundled).toEqual(embedded);
});

test("an HTTP 404 produces a localized fallback", async ({ page }) => {
  await page.route("**/*.nii.gz", (route) => route.fulfill({
    status: 404,
    contentType: "text/plain",
    body: "missing"
  }));
  const host = await openInteractiveReport(page, bundleFixture);
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();
  await expect(host).toHaveAttribute("data-nm-volume-failed", "true");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "HTTP 404"
  );
  await expect(page.locator(
    '[data-nm-map-id="interactive_z"] img'
  )).toBeVisible();
});

test("a corrupt bundled NIfTI produces a localized fallback", async ({ page }) => {
  const host = await openInteractiveReport(page, bundleFixture);
  const primaryRef = await page.evaluate(() => {
    const scene = JSON.parse(document.querySelector(
      'script[type="application/json"]'
    ).textContent);
    const analysis = scene.analyses[0];
    const primary = analysis.maps.find((map) =>
      map.map_id === analysis.primary_map_id
    );
    return scene.assets.find((asset) =>
      asset.asset_id === primary.asset_id
    ).location.ref;
  });
  await page.route(`**/${primaryRef.split("/").pop()}`, (route) =>
    route.fulfill({ status: 200, body: "not a nifti file" })
  );
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();
  await expect(host).toHaveAttribute("data-nm-volume-failed", "true");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "static panel remains authoritative"
  );
});

test("viewer initialization failures retain the static report", async ({ page }) => {
  const host = await openInteractiveReport(page);
  await page.evaluate(() => {
    globalThis.neuroimjs.SimpleOrthogonalViewer.create = async () => {
      throw new Error("WebGL initialization failed");
    };
  });
  await host.getByRole("button", {
    name: "Explore volume interactively"
  }).click();
  await expect(host).toHaveAttribute("data-nm-volume-failed", "true");
  await expect(host.locator("[data-nm-volume-status]")).toContainText(
    "WebGL initialization failed"
  );
  await expect(page.locator(
    '[data-nm-map-id="interactive_z"] img'
  )).toBeVisible();
});

test("viewer resources can be explicitly disposed", async ({ page }) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);
  await host.evaluate((node) => {
    globalThis.NeuroMosaicVolumeReport.dispose(node);
  });
  await expect.poll(() => host.evaluate((node) =>
    globalThis.NeuroMosaicVolumeReport.getState(node)
  )).toBeUndefined();
  await expect(host.locator("canvas")).toHaveCount(0);
  await expect(host.locator("[data-nm-volume-shell]")).toBeHidden();
  await expect(host).not.toHaveAttribute("data-nm-volume-ready", "true");
  await expect(host.getByRole("button", {
    name: "Explore volume interactively"
  })).toHaveAttribute("aria-expanded", "false");
});

test("hiding tears down the viewer and reopening uses the selected map", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);
  await host.getByRole("combobox", {
    name: "Interactive map"
  }).selectOption("interactive_se");
  await expect(host).toHaveAttribute("data-nm-volume-map", "interactive_se");

  await host.getByRole("button", { name: "Hide interactive view" }).click();
  await expect.poll(() => host.evaluate((node) =>
    globalThis.NeuroMosaicVolumeReport.getState(node)
  )).toBeUndefined();
  await expect(host.locator("canvas")).toHaveCount(0);
  await expect(host.locator("[data-nm-volume-controls]")).toBeEmpty();

  await activate(page, host);
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_se"
  );
  await expect(host.getByRole("combobox", { name: "Colormap" })).toHaveValue(
    "Inferno"
  );
});

test("the interactive controls fit a narrow report viewport and disappear in print", async ({
  page
}) => {
  await page.setViewportSize({ width: 390, height: 844 });
  const host = await openInteractiveReport(page);
  await activate(page, host);
  await host.scrollIntoViewIfNeeded();
  await expect(host).toBeInViewport();
  await expect.poll(() => page.evaluate(() =>
    document.documentElement.scrollWidth <=
      document.documentElement.clientWidth + 1
  )).toBe(true);
  const hostBox = await host.boundingBox();
  expect(hostBox.x + hostBox.width).toBeLessThanOrEqual(391);

  await page.emulateMedia({ media: "print" });
  await expect(host).toBeHidden();
  await expect(page.locator(
    '[data-nm-map-id="interactive_z"] img'
  )).toBeVisible();
});

test.describe("without JavaScript", () => {
  test.use({ javaScriptEnabled: false });

  test("the report retains static maps and its explicit fallback message", async ({
    page
  }) => {
    const host = await openInteractiveReport(page);
    await expect(page.locator(
      '[data-nm-map-id="interactive_z"] img'
    )).toBeVisible();
    await expect(host.getByText(
      "JavaScript is disabled; use the static montage above.",
      { exact: true }
    )).toBeVisible();
    await expect(host.locator("[data-nm-volume-shell]")).toBeHidden();
  });
});
