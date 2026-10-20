const { test, expect } = require("@playwright/test");
const { resolve } = require("node:path");
const { pathToFileURL } = require("node:url");
const { readFile } = require("node:fs/promises");

const origin = "http://127.0.0.1:4173/";
const bundle = origin + "report-assets/relocated/index.html";
const embedded = pathToFileURL(resolve(__dirname, ".artifacts/report-assets/embed/index.html")).href;

async function localOnly(page) {
  const external = [];
  await page.route("**/*", (route) => {
    const url = route.request().url();
    if (/^(file:|data:|blob:)/.test(url) || url.startsWith(origin)) return route.continue();
    external.push(url);
    return route.abort();
  });
  return external;
}

for (const [mode, url] of [["embedded", embedded], ["relocated bundle", bundle]]) {
  test(`${mode} retains responsive figures, print images and PNG downloads`, async ({ page }) => {
    const external = await localOnly(page);
    const errors = [];
    page.on("pageerror", (e) => errors.push(e.message));
    await page.goto(url);
    await page.evaluate(() => NeuroMosaicReport.preparePrint());
    const figures = page.locator("figure.nm-figure-volume");
    await expect(figures).toHaveCount(4);
    await expect(figures.locator("source")).toHaveCount(12);
    expect(await figures.locator("picture img").evaluateAll((images) =>
      images.every((img) => img.complete && img.naturalWidth === 88))).toBe(true);

    const first = figures.first();
    const source = await first.locator("picture img").getAttribute("src");
    const expected = source.startsWith("data:")
      ? Buffer.from(source.slice(source.indexOf(",") + 1), "base64")
      : await readFile(resolve(__dirname, ".artifacts", new URL(source, url).pathname.slice(1)));
    const downloadEvent = page.waitForEvent("download");
    await first.getByRole("link", { name: "image only", exact: true }).click();
    const download = await downloadEvent;
    expect(await readFile(await download.path())).toEqual(expected);
    const captionedEvent = page.waitForEvent("download");
    await first.getByRole("link", { name: "Download PNG", exact: true }).click();
    const captioned = await captionedEvent;
    expect((await readFile(await captioned.path())).subarray(0, 8)).toEqual(
      Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]));

    await page.emulateMedia({ colorScheme: "dark" });
    await page.setViewportSize({ width: 390, height: 844 });
    await expect(first).toBeVisible();
    await page.screenshot({ path: test.info().outputPath("dark-mobile.png") });
    expect(external).toEqual([]);
    expect(errors).toEqual([]);
  });
}

test("the relocated bundle retains figures with JavaScript disabled", async ({ browser }) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    const external = await localOnly(page);
    await page.goto(bundle);
    const images = page.locator("figure.nm-figure-volume noscript img");
    await expect(images).toHaveCount(4);
    expect(await images.evaluateAll((nodes) => nodes.every((img) => img.complete && img.naturalWidth === 88))).toBe(true);
    expect(external).toEqual([]);
  } finally {
    await context.close();
  }
});

test("a staged volume bundle carries its runtime and data and loads the runtime once", async ({ page }) => {
  const external = await localOnly(page);
  const runtimeRequests = [];
  page.on("request", (r) => { if (/neuroimjs-.*\.umd\.js/.test(r.url())) runtimeRequests.push(r.url()); });
  await page.goto(origin + "report-assets/relocated-volume/interactive-volume-bundle.html");
  expect(runtimeRequests).toHaveLength(0);
  const host = page.locator('[data-nm-volume-analysis="faces-interactive"]');
  await host.getByRole("button", { name: "Explore volume interactively" }).click();
  await expect(host).toHaveAttribute("data-nm-volume-ready", "true", { timeout: 20000 });
  await expect(host.locator("[data-view] canvas")).toHaveCount(3);
  expect(runtimeRequests).toHaveLength(1);
  expect(external).toEqual([]);
});

test("a staged surface bundle carries its runtime and typed arrays", async ({ page }) => {
  const external = await localOnly(page);
  const runtimeRequests = [];
  page.on("request", (r) => { if (/surfview.*\.js/.test(r.url())) runtimeRequests.push(r.url()); });
  await page.goto(origin + "report-assets/relocated-surface/interactive-surface-bundle.html");
  expect(runtimeRequests).toHaveLength(0);
  const host = page.locator('[data-nm-surface-analysis="surface-interactive"]');
  const router = host.locator('xpath=preceding-sibling::*[@data-nm-view-router][1]');
  await router.getByRole("radio", { name: "3D surface", exact: true }).check();
  await expect(host).toHaveAttribute("data-nm-surface-ready", "true", { timeout: 20000 });
  await expect(host.locator(".nm-surface-widget canvas")).toBeVisible();
  expect(runtimeRequests).toHaveLength(1);
  expect(external).toEqual([]);
});
