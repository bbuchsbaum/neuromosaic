const { test, expect } = require('@playwright/test');
const { stat } = require('node:fs/promises');

async function chooseView(page, id, name) {
  await page.locator(`[data-nm-view-router="${id}"]`)
    .getByRole('radio', { name, exact: true }).check();
}

for (const width of [1440, 390]) {
  for (const grouped of [false, true]) {
    test(`analysis context survives view and map switches: ${grouped ? 'grouped' : 'singletons'} at ${width}`, async ({ page }, testInfo) => {
      await page.setViewportSize({ width, height: 1000 });
      await page.addInitScript(() => {
        window.exportText = [];
        const fillText = CanvasRenderingContext2D.prototype.fillText;
        CanvasRenderingContext2D.prototype.fillText = function (text, ...args) {
          window.exportText.push(String(text));
          return fillText.call(this, text, ...args);
        };
      });
      await page.goto(grouped ? '/context-grouped.html' : '/context-singletons.html');
      if (grouped) {
        await expect(page.locator('.nm-summary-note')).toHaveText(
          'Unthresholded descriptive maps. Select a cell to open that map.');
        await expect(page.locator('.nm-summary-table a')).toHaveCount(4);
      } else {
        for (const id of ['A', 'B', 'C', 'D', 'E', 'F']) {
          await expect(page.getByRole('heading', {
            name: `Analysis ${id} — All eligible runs`, exact: true
          })).toHaveCount(1);
        }
      }
      const volume = page.locator('[data-nm-volume-analysis="a"]');
      const surface = page.locator('[data-nm-surface-analysis="a"]');
      // The router owns lazy activation; both hosts must retain the same analysis.
      await chooseView(page, 'a', 'Slices');
      await expect(volume).toHaveAttribute('data-nm-volume-ready', 'true');
      await expect(volume.locator('.nm-volume-analysis-label')).toHaveText('Analysis A');
      await expect(volume.locator('.nm-volume-analysis-label')).toBeVisible();
      await expect(volume.locator('[data-nm-volume-viewer]')).toHaveAttribute('aria-label', /Analysis A.*All eligible runs/);
      await volume.screenshot({ path: testInfo.outputPath('volume.png') });
      if (grouped) {
        await page.locator('[data-nm-map-group="a"]').getByRole('tab', { name: 'Subset', exact: true }).click();
        await expect(volume).toHaveAttribute('data-nm-volume-map', 'a_2');
        await expect(volume.locator('[data-nm-volume-viewer]')).toHaveAttribute('aria-label', /Analysis A.*Subset/);
      }
      const variant = grouped ? 'Subset' : 'All eligible runs';
      await page.evaluate(() => { window.exportText = []; });
      const volumeDownload = page.waitForEvent('download');
      await volume.getByRole('button', { name: 'Save view as PNG', exact: true }).click();
      expect((await stat(await (await volumeDownload).path())).size).toBeGreaterThan(1000);
      expect(await page.evaluate(() => window.exportText)).toEqual(expect.arrayContaining([
        expect.stringContaining(`Analysis A — ${variant}`)
      ]));
      await chooseView(page, 'a', '3D surface');
      await expect(surface).toHaveAttribute('data-nm-surface-ready', 'true', { timeout: 20000 });
      await expect(surface.locator('.nm-surface-analysis-label')).toHaveText('Analysis A');
      await expect(surface.locator('.nm-surface-analysis-label')).toBeVisible();
      await expect(surface).toHaveAttribute('aria-label', new RegExp(`Analysis A.*${variant}`));
      await surface.screenshot({ path: testInfo.outputPath('surface.png') });
      await page.evaluate(() => { window.exportText = []; });
      const surfaceDownload = page.waitForEvent('download');
      await surface.getByRole('button', { name: 'Save view as PNG', exact: true }).click();
      expect((await stat(await (await surfaceDownload).path())).size).toBeGreaterThan(1000);
      expect(await page.evaluate(() => window.exportText)).toEqual(expect.arrayContaining([
        expect.stringContaining(`Analysis A — ${variant}`)
      ]));
      if (grouped) {
        await page.locator('[data-nm-map-group="a"]').getByRole('tab', { name: 'All eligible runs', exact: true }).click();
        await expect(surface).toHaveAttribute('aria-label', /Analysis A.*All eligible runs/);
        await expect(surface.locator('[data-nm-surface-display-controls]')).toHaveAttribute('aria-label', /Analysis A.*All eligible runs/);
      }
    });
  }
}
