const { test, expect } = require('@playwright/test');

test('mixed quantities keep fixed ranges above observed maxima through switching and reset', async ({ page }) => {
  await page.goto('http://127.0.0.1:4173/fixed-scales.html');
  const host = page.locator('[data-nm-volume-analysis="mixed"]');
  const ranges = await host.evaluate(node => {
    const scene = JSON.parse(document.getElementById(node.dataset.nmVolumeScene).textContent);
    return scene.analyses[0].maps.map(map => map.display.limits);
  });
  expect(ranges).toEqual([[0, 100], [0, 17], [0, 1]]);
  await host.getByRole('button', { name: 'Explore volume interactively' }).click();
  await expect(host).toHaveAttribute('data-nm-volume-ready', 'true', { timeout: 20000 });
  const actual = () => host.evaluate(node => {
    const state = NeuroMosaicVolumeReport.getState(node);
    return Array.from(state.stack.getLayerById('neuromosaic-overlay').colorMap.getRange());
  });
  const select = host.getByRole('combobox', { name: 'Map', exact: true });
  for (const [id, upper] of [['prevalence', 100], ['coverage', 17], ['reference', 1], ['prevalence', 100]]) {
    await select.selectOption(id);
    await expect.poll(actual).toEqual([0, upper]);
    await host.getByRole('button', { name: 'Reset to defaults' }).click();
    await expect.poll(actual).toEqual([0, upper]);
  }
});
