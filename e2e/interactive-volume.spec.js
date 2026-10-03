const { test, expect } = require("@playwright/test");

// Figure alt text names the map, then describes the figure (e.g. "Z statistic:
// slice montage at z = ...").
const escapeRe = (s) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
const volumeAlt = (label) => new RegExp("^" + escapeRe(label) + ": slice montage");
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
    name: volumeAlt("Z statistic")
  })).toBeVisible();
  await expect(host.locator("[data-nm-volume-shell]")).toBeHidden();
  await expect(host.locator("canvas")).toHaveCount(0);
  await expect(host).toContainText("static montage is the report default");
  await expect(host).toContainText("recoverable voxel data");

  await activate(page, host);
  await expect(host.locator("[data-view] canvas")).toHaveCount(3);
  await expect(host.locator('[data-view="axial"]')).toBeVisible();
  await expect(host.locator('[data-view="coronal"]')).toBeVisible();
  await expect(host.locator('[data-view="sagittal"]')).toBeVisible();
  await expect(host.locator("[data-nm-volume-readout]")).toContainText(
    "Standardized association"
  );
  // The viewer opens on the static figure's palette and the report's
  // threshold, printed with the report's rounding.
  await expect(host.getByRole("combobox", { name: "Colormap" })).toHaveValue(
    "Report (nm-signed)"
  );
  await expect(host.getByRole("textbox", { name: "Threshold low" })).toHaveValue("−3.1");
  await expect(host.getByRole("textbox", { name: "Threshold high" })).toHaveValue("3.1");
});

test("map switching preserves map-local exploration and exact report resets", async ({
  page
}) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);

  const mapSelect = host.getByRole("combobox", { name: "Map", exact: true });
  const palette = host.getByRole("combobox", { name: "Colormap" });
  const opacity = host.getByLabel("Opacity value");

  // Signed maps offer the report palette and diverging maps only.
  await palette.selectOption("BlueRed");
  await opacity.fill("0.31");
  await opacity.press("Enter");
  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    colormap: "BlueRed",
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
    "Standard error"
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
    colormap: "BlueRed",
    opacity: 0.31
  });
  await expect(host).toHaveAttribute("data-nm-volume-modified", "true");

  await host.getByRole("button", { name: "Reset to defaults" }).click();
  // Report settings: the report palette at the figure's opacity.
  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    colormap: "Report (nm-signed)",
    threshold: [-3.1, 3.1],
    opacity: 0.92
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
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("Standard error");

  await activate(page, host);
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_se"
  );
  await expect(host.getByRole("combobox", {
    name: "Map", exact: true
  })).toHaveValue("interactive_se");

  await host.getByRole("combobox", {
    name: "Map", exact: true
  }).selectOption("interactive_estimate");
  await expect(group.locator(
    '[data-nm-map-id="interactive_estimate"]'
  )).toBeVisible();
  await expect(group.getByRole("tab", {
    name: "Estimate"
  })).toHaveAttribute("aria-selected", "true");

  await host.getByRole("combobox", {
    name: "Map", exact: true
  }).selectOption("interactive_reliability");
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_reliability"
  );
  await expect(host.getByRole("combobox", { name: "Colormap" })).toHaveValue(
    "Inferno"
  );
  // Range fields use the report's rounding (no trailing zeros).
  await expect(host.getByRole("textbox", { name: "Range low" })).toHaveValue("0");
  await expect(host.getByRole("textbox", { name: "Range high" })).toHaveValue("1");
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
    "Z statistic",
    "Beta estimate",
    "Standard error",
    "Reliability"
  ]) {
    await expect(group.getByRole("img", { name: volumeAlt(name) })).toBeVisible();
  }
  await expect(group.locator('[role="tablist"]')).toHaveCount(0);
  await expect(group.getByRole("combobox", { name: "Map variant" })).toHaveCount(0);
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("Z statistic");

  await activate(page, host);
  const mapSelect = host.getByRole("combobox", { name: "Map", exact: true });
  await expect(mapSelect.locator("option")).toHaveCount(4);
  await mapSelect.selectOption("interactive_se");
  await expect.poll(async () => (await viewerState(host)).currentMapId).toBe(
    "interactive_se"
  );
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("Standard error");
  await expect(group.getByRole("img", {
    name: volumeAlt("Z statistic")
  })).toBeVisible();
  await expect(group.getByRole("img", {
    name: volumeAlt("Standard error")
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
  })).toEqual(["solid", "2px"]);
  await launcher.press("Enter");
  await expect(host).toHaveAttribute("data-nm-volume-ready", "true", {
    timeout: 20_000
  });

  const palette = host.getByRole("combobox", { name: "Colormap" });
  const initialPalette = await palette.inputValue();
  await palette.focus();
  await expect(palette).toBeFocused();
  // A visible focus indicator: an outline ring or a box-shadow ring.
  expect(await palette.evaluate((node) => {
    const style = getComputedStyle(node);
    return style.outlineStyle !== "none" && parseFloat(style.outlineWidth) > 0 ||
      style.boxShadow !== "none";
  })).toBe(true);
  // Type-ahead to "Blue–white–red" (signed maps offer diverging maps only).
  await palette.press("b");
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
  await expect(host.locator("[data-nm-volume-target]")).toHaveText("Empty Z statistic");
  const staticImage = page.getByRole("img", {
    name: volumeAlt("Empty Z statistic")
  });
  await expect(staticImage).toBeVisible();

  await activate(page, host);
  await expect(host.locator("[data-view] canvas")).toHaveCount(3);
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
    "x −4.0 y −3.0 z −2.0 mm"
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
  const canvases = host.locator("[data-view] canvas");
  const beforeCanvases = await Promise.all(
    [0, 1, 2].map((index) => canvases.nth(index).screenshot())
  );

  // Each end has a text field and a slider; type into the field.
  const low = host.getByRole("textbox", { name: "Threshold low" });
  const high = host.getByRole("textbox", { name: "Threshold high" });
  await low.fill("-4.25");
  await low.press("Enter");
  await high.fill("4.25");
  await high.press("Enter");
  await host.getByRole("combobox", { name: "Colormap" }).selectOption(
    "BlueRed"
  );

  await expect.poll(async () => (await viewerState(host)).controls).toMatchObject({
    threshold: [-4.25, 4.25],
    colormap: "BlueRed"
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
    "The static figures are unaffected."
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
    const host = document.querySelector('[data-nm-volume-analysis="faces-interactive"]');
    const node = document.getElementById(host.dataset.nmVolumeScene);
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
    name: "Map", exact: true
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
    const host = document.querySelector('[data-nm-volume-analysis="faces-interactive"]');
    const scene = JSON.parse(document.getElementById(
      host.dataset.nmVolumeScene
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
    "The static figures are unaffected."
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
    name: "Map", exact: true
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

test("a pane focused by script lets the wheel scroll; a pane the reader focused steps", async ({
  page
}) => {
  await page.setViewportSize({ width: 1200, height: 700 });
  const host = await openInteractiveReport(page);
  await activate(page, host);
  const axial = host.locator('[data-nij-view="axial"]');
  await axial.scrollIntoViewIfNeeded();
  const z = async () => (await viewerState(host)).worldCoord[2];

  // A peak jump: the reader presses a control elsewhere, then the page moves
  // focus into the pane by script.
  await host.getByRole("button", { name: "Reset to report position" }).click();
  await axial.evaluate((node) => node.focus({ preventScroll: true }));
  await expect(axial).toBeFocused();
  const z0 = await z();
  const y0 = await page.evaluate(() => window.scrollY);
  const box = await axial.boundingBox();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.wheel(0, 200);
  await expect.poll(() => page.evaluate(() => window.scrollY)).toBeGreaterThan(y0);
  expect(await z()).toBe(z0);

  // The reader clicks the pane: now the plain wheel steps its slice.
  const box2 = await axial.boundingBox();
  await page.mouse.click(box2.x + box2.width / 2, box2.y + box2.height / 2);
  await expect(axial).toBeFocused();
  const z1 = await z();
  const y1 = await page.evaluate(() => window.scrollY);
  await page.mouse.wheel(0, 100);
  await expect.poll(z).not.toBe(z1);
  expect(await page.evaluate(() => window.scrollY)).toBe(y1);
});

// ---- neuroimjs 0.5 behaviour the adapter now relies on ---------------------
const fineFixture = pathToFileURL(resolve(
  __dirname, ".artifacts", "interactive-volume-fine.html"
)).href;

function collectErrors(page) {
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  return errors;
}

test("the Ground switch restyles the open viewer in place", async ({ page }) => {
  const errors = collectErrors(page);
  const host = await openInteractiveReport(page);
  await activate(page, host);
  await host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    state.viewer.setWorldCoord([-4, -3, -2]);
    node.__nmViewerBefore = state.viewer;
    node.querySelector("[data-view] canvas").__nmMark = true;
  });
  const before = await host.getAttribute("data-nm-volume-ground");
  const other = before === "light" ? "dark" : "light";
  await host.locator(`.nm-volume-ground[data-ground="${other}"]`).click();
  await expect(host).toHaveAttribute("data-nm-volume-ground", other);
  await expect(host.locator(`.nm-volume-ground[data-ground="${other}"]`))
    .toHaveAttribute("aria-pressed", "true");
  const after = await host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    return {
      sameViewer: state.viewer === node.__nmViewerBefore,
      sameCanvas: node.querySelector("[data-view] canvas").__nmMark === true,
      coord: state.viewer.getWorldCoord(),
      canvasBackground: node.querySelector("[data-view] canvas").style.background
    };
  });
  expect(after.sameViewer).toBe(true);
  expect(after.sameCanvas).toBe(true);
  expect(after.coord).toEqual([-4, -3, -2]);
  expect(after.canvasBackground).toMatch(other === "dark" ? /17, 22, 25|#111619/ : /250, 250, 247|#fafaf7/);
  expect(errors).toEqual([]);
});

test("arrow keys in a form control never step the slices", async ({ page }) => {
  const host = await openInteractiveReport(page);
  await activate(page, host);
  const coord = async () => (await viewerState(host)).worldCoord;
  const axial = host.locator('[data-nij-view="axial"]');
  await axial.scrollIntoViewIfNeeded();
  const box = await axial.boundingBox();
  // The pointer rests on a pane while the reader edits the threshold.
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  const start = await coord();
  const field = host.getByRole("textbox", { name: "Threshold low" });
  await field.focus();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2 + 1);
  await expect(axial).toHaveAttribute("data-nij-hover", "");
  await page.keyboard.press("ArrowLeft");
  await page.keyboard.press("ArrowRight");
  await page.waitForTimeout(150);
  expect(await coord()).toEqual(start);
  // With focus on the page itself, the runtime steps the hovered pane.
  await page.evaluate(() => document.activeElement && document.activeElement.blur());
  await page.mouse.move(box.x + box.width / 2 + 1, box.y + box.height / 2);
  await page.keyboard.press("ArrowRight");
  await expect.poll(async () => (await coord())[2]).not.toBe(start[2]);
});

test("a viewer disposed right after a resize does not draw into freed graphics", async ({
  page
}) => {
  const errors = collectErrors(page);
  const host = await openInteractiveReport(page);
  await activate(page, host);
  await page.setViewportSize({ width: 900, height: 700 });
  await host.evaluate((node) => {
    window.dispatchEvent(new Event("resize"));
    globalThis.NeuroMosaicVolumeReport.dispose(node);
  });
  await page.waitForTimeout(300);
  await expect(host.locator("canvas")).toHaveCount(0);
  expect(errors).toEqual([]);
});

test("a finer underlay is drawn on its own grid under the maps", async ({ page }) => {
  const errors = collectErrors(page);
  const host = await openInteractiveReport(page, fineFixture);
  await activate(page, host);
  const grids = await host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    return {
      underlay: Array.from(state.stack.getLayer(0).volume.space.dim),
      overlay: Array.from(state.stack.getLayerById("neuromosaic-overlay").volume.space.dim)
    };
  });
  expect(grids.underlay).toEqual([25, 27, 29]);
  expect(grids.overlay).toEqual([12, 13, 14]);

  // A peak jump lands on the exact peak voxel and reads its value.
  await host.evaluate((node) => {
    globalThis.NeuroMosaicVolumeReport.getState(node).viewer.setWorldCoord([-4, -3, -2]);
  });
  const peak = await host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    return {
      coord: state.viewer.getWorldCoord(),
      value: state.viewer.getValue("neuromosaic-overlay")
    };
  });
  expect(peak.coord).toEqual([-4, -3, -2]);
  expect(peak.value).toBeCloseTo(7.75, 6);
  await expect(host.locator("[data-nm-volume-readout]")).toContainText("7.75");

  // A click lands on the centre of the underlay texel under the pointer and
  // the readout reports the map voxel containing it.
  const axial = host.locator('[data-nij-view="axial"] canvas');
  const box = await axial.boundingBox();
  await page.mouse.click(box.x + box.width * 0.45, box.y + box.height * 0.55);
  await page.waitForTimeout(100);
  const clicked = await host.evaluate((node) => {
    const state = globalThis.NeuroMosaicVolumeReport.getState(node);
    const coord = state.viewer.getWorldCoord();
    const under = state.stack.getLayer(0).volume.space.coordToGrid(coord);
    const overGrid = state.stack.getLayerById("neuromosaic-overlay").volume.space
      .coordToGrid(coord);
    const over = overGrid.map(Math.round);
    const vol = state.stack.getLayerById("neuromosaic-overlay").volume;
    return {
      coord,
      under,
      overGrid,
      value: state.viewer.getValue("neuromosaic-overlay"),
      expected: vol.getAt(over[0], over[1], over[2])
    };
  });
  // The click selects the map voxel under the pointer: the cursor sits on its
  // centre in the pane's plane, which is also an underlay texel centre.
  await expect.poll(() => host.evaluate((node) =>
    globalThis.NeuroMosaicVolumeReport.getState(node).viewer.getWorldCoord()[2])).toBe(-2);
  for (const axis of [0, 1]) {
    expect(Math.abs(clicked.overGrid[axis] - Math.round(clicked.overGrid[axis]))).toBeLessThan(1e-6);
    expect(Math.abs(clicked.under[axis] - Math.round(clicked.under[axis]))).toBeLessThan(1e-6);
  }
  if (clicked.expected === null || !Number.isFinite(clicked.expected)) {
    expect(clicked.value).toBeNull();
  } else {
    expect(clicked.value).toBeCloseTo(clicked.expected, 6);
  }
  expect(errors).toEqual([]);
});
