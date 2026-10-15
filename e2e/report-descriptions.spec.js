const { test, expect } = require('@playwright/test');
const { stat } = require('node:fs/promises');

for (const width of [1450, 980, 640, 390]) {
  test(`volume readout and actions remain separate at viewport ${width}`, async ({ page }) => {
    await page.setViewportSize({ width, height: 1000 });
    await page.goto('http://127.0.0.1:4173/interactive-volume-bundle.html');
    const host = page.locator('[data-nm-volume-analysis="faces-interactive"]');
    await host.getByRole('button', { name: 'Explore volume interactively' }).click();
    await expect(host).toHaveAttribute('data-nm-volume-ready', 'true');
    const title = host.locator('.nm-volume-readout-title');
    const reset = host.getByRole('button', { name: 'Reset to report position', exact: true });
    const save = host.getByRole('button', { name: 'Save view as PNG', exact: true });
    for (const label of ['Selected Spt membership', 'Conjunction z-equivalent', 'LongUnbrokenQuantityTitleWithUnitsAndAdditionalDescription']) {
      await reset.focus();
      await host.evaluate((node, label) => {
        const state = NeuroMosaicVolumeReport.getState(node);
        state.analysis.maps.find(m => m.map_id === state.currentMapId).legend_title = label;
        const xyz = state.viewer.getWorldCoord();
        state.viewer.setWorldCoord([xyz[0] + 1, xyz[1], xyz[2]]);
      }, label);
      await expect(title).toHaveText(label);
      await expect(reset).toBeFocused();
      const bounds = await host.evaluate(node => {
        const box = selector => {
          const r = node.querySelector(selector).getBoundingClientRect();
          return { x: r.x, y: r.y, right: r.right, bottom: r.bottom, width: r.width, height: r.height };
        };
        return { title: box('.nm-volume-readout-title'), actions: box('.nm-volume-legend-head'),
          cell: box('[data-nij-legend]'), scale: box('.nm-volume-scale') };
      });
      expect(bounds.actions.y).toBeGreaterThanOrEqual(bounds.title.bottom);
      expect(bounds.actions.x).toBeGreaterThanOrEqual(bounds.cell.x);
      expect(bounds.actions.right).toBeLessThanOrEqual(bounds.cell.right + 1);
      expect(bounds.scale.bottom).toBeLessThanOrEqual(bounds.cell.bottom + 1);
      expect(bounds.scale.y).toBeGreaterThanOrEqual(bounds.actions.bottom);
      await expect(save).toBeVisible();
    }
    await host.locator('[data-nm-volume-shell]').screenshot({ path: `/tmp/neuromosaic-34-${width}.png` });
  });
}

for (const [fixture, phrase] of [
  ['parcel-surface-visual.html', 'Parcel values mapped directly'],
  ['interactive-mixed.html', 'Precomputed vertex values mapped']
]) {
  test(`surface PNG describes the actual source in ${fixture}`, async ({ page }) => {
    await page.goto('http://127.0.0.1:4173/' + fixture);
    const host = page.locator('[data-nm-surface-analysis]').first();
    const router = host.locator('xpath=preceding-sibling::*[@data-nm-view-router][1]');
    await router.getByRole('radio', { name: '3D surface', exact: true }).check();
    await expect(host).toHaveAttribute('data-nm-surface-ready', 'true', { timeout: 20000 });
    const downloadEvent = page.waitForEvent('download');
    await host.evaluate(node => NeuroMosaicSurfaceBridge.exportPNG(node));
    const download = await downloadEvent;
    expect((await stat(await download.path())).size).toBeGreaterThan(1000);
    const note = await host.evaluate(node => node.__nmLastExportNote);
    expect(note).toContain(phrase);
    expect(note).not.toMatch(/averag|peaks are lower/i);
  });
}

test('a pending surface edit never changes the next map after the debounce interval', async ({ page }) => {
  await page.goto('http://127.0.0.1:4173/interactive-surface-bundle.html');
  const host = page.locator('[data-nm-surface-analysis="surface-interactive"]');
  await host.locator('xpath=preceding-sibling::*[@data-nm-view-router][1]')
    .getByRole('radio', { name: '3D surface', exact: true }).check();
  await expect(host).toHaveAttribute('data-nm-surface-ready', 'true');
  const toggle = host.getByRole('button', { name: 'Colour, range and threshold' });
  if (await toggle.getAttribute('aria-expanded') !== 'true') await toggle.click();
  await host.getByLabel('Colour range high').evaluate(input => {
    input.value = '7';
    input.dispatchEvent(new Event('input', { bubbles: true }));
  });
  await page.locator('[data-nm-map-group="surface-interactive"]').getByRole('tab', { name: 'SE' })
    .evaluate(button => button.click());
  await expect(host).toHaveAttribute('data-nm-surface-map', 'surface_se');
  // Deliberately cross the 220ms edit timer; an immediate assertion misses it.
  await page.waitForTimeout(350);
  const state = await host.evaluate(node => ({
    actual: NeuroMosaicSurfaceDisplay.layerDefaults(node.__nmSurfaceLayer),
    defaults: node.__nmSurfaceDisplayStore.get(node.__nmSurfaceLayer.id)
  }));
  expect(state.actual).toEqual(state.defaults);
  await expect(host).toHaveAttribute('data-nm-surface-modified', 'false');
});
