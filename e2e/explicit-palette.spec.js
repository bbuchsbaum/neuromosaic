const { test, expect } = require("@playwright/test");
const { resolve } = require("node:path");
const runtime = require("../inst/htmlwidgets/lib/neuromosaic-volume/runtime.json");

test("the native ColorMap preserves an explicit mixed RGB/RGBA table", async ({ page }) => {
  await page.goto("http://127.0.0.1:4173/health");
  const lib = resolve(__dirname, "../inst/htmlwidgets/lib/neuromosaic-volume");
  await page.addScriptTag({ path: resolve(lib, runtime.bundle) });
  await page.addScriptTag({ path: resolve(lib, "display.js") });
  const result = await page.evaluate(() => {
    const display = NeuroMosaicVolumeDisplay.resolve({
      palette: "custom:character", palette_colors: ["#00112240", "#CCDDEE"],
      mode: "continuous", scale: "sequential", limits: [0, 8],
      threshold: null, tail: "positive", alpha: 0.7
    }, [-10, 10], []);
    const map = new neuroimjs.ColorMap(display.colormapColors, {
      range: display.range, threshold: display.threshold, name: "Custom"
    });
    return [map.getColor(0), map.getColor(8)];
  });
  expect(result).toEqual([[0, 17 / 255, 34 / 255, 64 / 255],
    [204 / 255, 221 / 255, 238 / 255, 1]]);
});

test("custom report colors survive threshold edits, map switches and reset", async ({ page }) => {
  await page.goto("http://127.0.0.1:4173/interactive-volume-bundle.html");
  const host = page.locator('[data-nm-volume-analysis="faces-interactive"]');
  const tables = [["#F7FCF5", "#00441B80"], ["#F7FBFF", "#08306B"]];
  const ids = await host.evaluate((node, colors) => {
    const script = document.getElementById(node.dataset.nmVolumeScene);
    const scene = JSON.parse(script.textContent);
    const maps = scene.analyses.find(a => a.analysis_id === node.dataset.nmVolumeAnalysis).maps;
    maps.slice(0, 2).forEach((map, i) => {
      map.display.palette = "custom:character";
      map.display.palette_colors = colors[i];
    });
    script.textContent = JSON.stringify(scene);
    return maps.slice(0, 2).map(map => map.map_id);
  }, tables);
  await host.getByRole("button", { name: "Explore volume interactively" }).click();
  await expect(host).toHaveAttribute("data-nm-volume-ready", "true", { timeout: 20_000 });
  const actual = () => host.evaluate(node => {
    const state = NeuroMosaicVolumeReport.getState(node);
    return state.stack.getLayerById("neuromosaic-overlay").colorMap.getColorMap();
  });
  const green = [[247 / 255, 252 / 255, 245 / 255, 1], [0, 68 / 255, 27 / 255, 128 / 255]];
  const blue = [[247 / 255, 251 / 255, 1, 1], [8 / 255, 48 / 255, 107 / 255, 1]];
  await expect.poll(actual).toEqual(green);
  await host.evaluate(node => {
    const state = NeuroMosaicVolumeReport.getState(node);
    state.controls.applyState({ threshold: [-2, 2], range: [-10, 10] });
  });
  await expect.poll(actual).toEqual(green);
  await host.getByRole("combobox", { name: "Colormap" }).selectOption("BlueRed");
  await host.getByRole("button", { name: "Reset to defaults" }).click();
  await expect.poll(actual).toEqual(green);
  const select = host.getByRole("combobox", { name: "Map", exact: true });
  await select.selectOption(ids[1]);
  await expect.poll(actual).toEqual(blue);
  await select.selectOption(ids[0]);
  await expect.poll(actual).toEqual(green);
});
