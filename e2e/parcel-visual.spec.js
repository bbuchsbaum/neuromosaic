const { test, expect } = require("@playwright/test");
const { resolve } = require("node:path");
const { pathToFileURL } = require("node:url");

const fixture = pathToFileURL(resolve(
  __dirname, ".artifacts", "parcel-surface-visual.html"
)).href;

async function openParcelReport(page) {
  await page.goto(fixture, { waitUntil: "load" });
  await expect(page.getByRole("heading", {
    name: "Neuromosaic parcel visual QA fixture",
    exact: true
  })).toBeVisible();
  return {
    group: page.locator('[data-nm-map-group="parcel-qa"]'),
    host: page.locator('[data-nm-surface-analysis="parcel-qa"]'),
    router: page.locator('[data-nm-view-router="parcel-qa"]')
  };
}

async function settleFrames(page) {
  await page.evaluate(() => new Promise((resolveFrame) => {
    requestAnimationFrame(() => requestAnimationFrame(resolveFrame));
  }));
}

test("parcel report primary and auxiliary static panels match baselines", async ({
  page
}) => {
  const { group } = await openParcelReport(page);
  const zPanel = group.locator('[data-nm-map-id="parcel-qa_z_stat"]');
  const betaPanel = group.locator('[data-nm-map-id="parcel-qa_beta"]');

  await expect(group.getByRole("tablist", { name: "Map variant" })).toBeVisible();
  await expect(zPanel).toBeVisible();
  await expect(betaPanel).toBeHidden();
  await expect(zPanel.getByRole("img", {
    name: "Parcel Z statistic surface montage"
  })).toBeVisible();
  await expect(group).toHaveScreenshot("parcel-primary-static.png", {
    animations: "disabled",
    caret: "hide",
    maxDiffPixelRatio: 0.01
  });

  await group.getByRole("tab", { name: "Parcel beta estimate" }).click();
  await expect(zPanel).toBeHidden();
  await expect(betaPanel).toBeVisible();
  await expect(betaPanel).toContainText("ROI-table display derived from beta");
  await expect(group).toHaveScreenshot("parcel-beta-static.png", {
    animations: "disabled",
    caret: "hide",
    maxDiffPixelRatio: 0.01
  });
});

test("parcel report surface view matches the selected metric baseline", async ({
  page
}) => {
  const { group, host, router } = await openParcelReport(page);
  const scene = await page.locator('script[id^="nm-surface-scene-"]').evaluate(
    (node) => JSON.parse(node.textContent)
  );
  expect(scene.geometries.left).toMatchObject({
    vertexCount: 642,
    faceCount: 1280
  });
  expect(scene.geometries.right).toMatchObject({
    vertexCount: 642,
    faceCount: 1280
  });
  await group.getByRole("tab", { name: "Parcel standard error" }).click();
  await expect(host).toHaveAttribute(
    "data-nm-surface-map", "parcel-qa_standard_error"
  );

  await router.getByRole("radio", { name: "Surface", exact: true }).check();
  await expect(host.locator("[data-nm-surface-status]")).toContainText(
    "Interactive surface ready",
    { timeout: 20_000 }
  );
  await expect(host.locator("canvas")).toBeVisible();
  await expect(host.getByRole("combobox", {
    name: "Surface colormap"
  })).toHaveValue("inferno");
  await settleFrames(page);

  await expect(host).toHaveScreenshot("parcel-se-surface.png", {
    animations: "disabled",
    caret: "hide",
    maxDiffPixelRatio: 0.02
  });
});
