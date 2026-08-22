const { test, expect } = require("@playwright/test");
const { resolve } = require("node:path");
const { pathToFileURL } = require("node:url");

function fixtureUrl(name) {
  return pathToFileURL(resolve(__dirname, ".artifacts", name)).href;
}

async function openFixture(page, name = "selector-auto.html") {
  await page.goto(fixtureUrl(name), { waitUntil: "load" });
  await expect(page.getByRole("heading", {
    name: "Neuromosaic map selector browser fixture",
    exact: true
  })).toBeVisible();
}

function groupFor(page, analysisId) {
  return page.locator(`[data-nm-map-group="${analysisId}"]`);
}

function variantFor(group, mapId) {
  return group.locator(`[data-nm-map-id="${mapId}"]`);
}

test("tabs switch the complete map panel and support keyboard navigation", async ({
  page
}, testInfo) => {
  await openFixture(page);
  const group = groupFor(page, "faces");
  const zPanel = variantFor(group, "faces_z");
  const estimatePanel = variantFor(group, "faces_estimate");
  const sePanel = variantFor(group, "faces_se");

  await expect(group.getByRole("tablist", { name: "Map variant" })).toBeVisible();
  await expect(zPanel).toBeVisible();
  await expect(estimatePanel).toBeHidden();
  await expect(sePanel).toBeHidden();

  const estimateTab = group.getByRole("tab", { name: "Estimate" });
  await estimateTab.click();

  await expect(estimateTab).toHaveAttribute("aria-selected", "true");
  await expect(zPanel).toBeHidden();
  await expect(estimatePanel).toBeVisible();
  await expect(estimatePanel).toContainText("DESCRIPTION faces_estimate");
  await expect(estimatePanel).toContainText("TABLE faces_estimate");
  await expect(estimatePanel).toContainText("2 subject(s) dropped");
  await expect(estimatePanel.locator(".nm-panel-meta")).toContainText("Estimate");

  const image = estimatePanel.getByRole("img", {
    name: "Estimate volume montage"
  });
  await expect(image).toBeVisible();
  await expect.poll(() => image.evaluate((node) =>
    node.complete && node.naturalWidth > 0
  )).toBe(true);

  await estimateTab.focus();
  await estimateTab.press("ArrowRight");
  const seTab = group.getByRole("tab", { name: "SE" });
  await expect(seTab).toBeFocused();
  await expect(seTab).toHaveAttribute("aria-selected", "true");
  await expect(estimatePanel).toBeHidden();
  await expect(sePanel).toBeVisible();

  await testInfo.attach("tabs-selected-panel", {
    body: await group.screenshot(),
    contentType: "image/png"
  });
});

test("auto mode uses a select control for five map variants", async ({ page }) => {
  await openFixture(page);
  const group = groupFor(page, "working-memory");
  const select = group.getByRole("combobox", { name: "Map variant" });

  await expect(select).toBeVisible();
  await expect(variantFor(group, "wm_z")).toBeVisible();
  await expect(variantFor(group, "wm_probability")).toBeHidden();

  await select.selectOption({ label: "Probability" });

  const probabilityPanel = variantFor(group, "wm_probability");
  await expect(probabilityPanel).toBeVisible();
  await expect(variantFor(group, "wm_z")).toBeHidden();
  await expect(probabilityPanel).toContainText("DESCRIPTION wm_probability");
  await expect(probabilityPanel).toContainText("TABLE wm_probability");
});

test("the selector remains usable without horizontal overflow on a narrow screen", async ({
  page
}) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await openFixture(page);
  const group = groupFor(page, "faces");
  const tabs = group.getByRole("tablist", { name: "Map variant" });

  await expect(tabs).toBeVisible();
  await tabs.scrollIntoViewIfNeeded();
  await expect(tabs).toBeInViewport();
  await expect.poll(() => page.evaluate(() =>
    document.documentElement.scrollWidth <=
      document.documentElement.clientWidth + 1
  )).toBe(true);
});

test("print media expands every variant and hides interactive selectors", async ({ page }) => {
  await openFixture(page);
  await page.emulateMedia({ media: "print" });

  for (const analysisId of ["faces", "working-memory"]) {
    const group = groupFor(page, analysisId);
    const variants = group.locator("[data-nm-map-variant]");
    for (let i = 0; i < await variants.count(); i += 1) {
      await expect(variants.nth(i)).toBeVisible();
    }
    await expect(group.locator(".nm-map-selector")).toBeHidden();
  }
});

test.describe("without JavaScript", () => {
  test.use({ javaScriptEnabled: false });

  test("progressive enhancement leaves every variant readable", async ({ page }) => {
    await openFixture(page);
    const variants = page.locator("[data-nm-map-variant]");
    for (let i = 0; i < await variants.count(); i += 1) {
      await expect(variants.nth(i)).toBeVisible();
    }
    const selectors = page.locator(".nm-map-selector");
    for (let i = 0; i < await selectors.count(); i += 1) {
      await expect(selectors.nth(i)).toBeHidden();
    }
  });
});

test("map_selector none renders an expanded report without controls", async ({ page }) => {
  await openFixture(page, "selector-none.html");
  const group = groupFor(page, "faces");
  await expect(group.locator(".nm-map-selector")).toHaveCount(0);

  const variants = group.locator("[data-nm-map-variant]");
  await expect(variants).toHaveCount(3);
  for (let i = 0; i < await variants.count(); i += 1) {
    await expect(variants.nth(i)).toBeVisible();
  }
});
