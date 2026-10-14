const { test, expect } = require('@playwright/test');
const { resolve } = require('node:path');

const hostSelector = '[data-nm-space-host]';
async function openSpace(page) {
  await page.getByRole('button', { name: 'Open comparison', exact: true }).click();
  await expect(page.locator(hostSelector)).toHaveAttribute('data-nm-space-ready', 'true', { timeout: 30000 });
}
const state = page => page.locator(hostSelector).evaluate(el => el.__nmSpace.getState());
const setState = (page, patch) => page.locator(hostSelector).evaluate((el, value) => el.__nmSpace.setState(value), patch);

test('Space loads lazily, shares exact map identity and closes for ordinary slices', async ({ page }) => {
  const requests = [], errors = [];
  page.on('request', req => requests.push(req.url()));
  page.on('pageerror', error => errors.push(error.message));
  await page.goto('/space-report.html');
  const host = page.locator(hostSelector);
  await expect(host.getByText('4 subjects', { exact: false })).toBeVisible();
  expect(requests.filter(url => url.includes('/space/'))).toEqual([]);
  await expect(host.locator('iframe')).toHaveCount(0);
  await openSpace(page);
  await expect(host.locator('iframe')).toHaveCount(1);
  await setState(page, { selectedMemberId: 'member-2' });
  await expect(host.locator('[data-nm-space-selection]')).toContainText('sub-02');
  await host.getByRole('button', { name: 'Show in report' }).click();
  const group = page.locator('[data-nm-map-group="sub-02"]');
  await expect(group.locator('[data-nm-map-id="sub-02_t"]')).toBeVisible();
  await group.getByRole('tab', { name: 'Estimate', exact: true }).click();
  await expect.poll(async () => (await state(page)).statistic).toBe('beta');
  await group.getByRole('tab', { name: 'SE', exact: true }).click();
  await expect(host.locator('[data-nm-space-status]')).toContainText('not a shared comparison channel');
  expect((await state(page)).statistic).toBe('beta');
  for (const mode of ['montage', 'facet', 'ortho', 'space']) {
    await setState(page, { mode });
    expect((await state(page)).mode).toBe(mode);
  }
  await host.scrollIntoViewIfNeeded();
  await host.screenshot({ path: '/tmp/neuromosaic-space-desktop.png' });
  const volume = page.locator('[data-nm-volume-analysis="sub-02"]');
  await volume.getByRole('button', { name: 'Explore volume interactively' }).click();
  await expect(host.locator('iframe')).toHaveCount(0);
  await expect(volume).toHaveAttribute('data-nm-volume-ready', 'true', { timeout: 20000 });
  await openSpace(page);
  await expect(volume).not.toHaveAttribute('data-nm-volume-ready', 'true');
  await host.getByRole('button', { name: 'Close comparison' }).click();
  await expect(host.locator('iframe')).toHaveCount(0);
  expect(errors).toEqual([]);
});

test('set replacement preserves the old frame on failure and latest choice wins', async ({ page }) => {
  await page.goto('/space-report.html');
  await openSpace(page);
  const host = page.locator(hostSelector), select = host.getByRole('combobox');
  await page.route('**/space/volumes/*', route => route.abort());
  await select.selectOption('subset');
  await expect(host.locator('[data-nm-space-status]')).toContainText('Could not open comparison');
  await expect(select).toHaveValue('faces');
  await expect(host.locator('iframe')).toHaveCount(1);
  await page.unroute('**/space/volumes/*');
  await select.selectOption('subset');
  await expect(host.locator('[data-nm-space-status]')).toHaveText('Comparison ready.', { timeout: 30000 });
  await expect(select).toHaveValue('subset');
  await expect(host.locator('iframe')).toHaveCount(1);
  await select.selectOption('faces');
  await select.selectOption('subset');
  await expect(host.locator('[data-nm-space-status]')).toHaveText('Comparison ready.', { timeout: 30000 });
  await expect(select).toHaveValue('subset');
  await expect(host.locator('iframe')).toHaveCount(1);
  await host.getByRole('button', { name: 'Close comparison' }).click();
  await expect(host.locator('iframe')).toHaveCount(0);
});

test('Space keeps an accessible mobile summary and an offline/print fallback', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto('/space-report.html');
  const host = page.locator(hostSelector);
  await openSpace(page);
  await host.screenshot({ path: '/tmp/neuromosaic-space-mobile.png' });
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  const frame = page.frameLocator('[data-nm-space-stage] iframe');
  expect(await frame.locator('#view').evaluate(el => el.clientWidth)).toBeGreaterThan(300);
  expect(await frame.locator('#view').evaluate(el => el.clientHeight)).toBeGreaterThan(300);
  expect(await frame.locator('#side').evaluate(el => el.offsetTop)).toBeGreaterThan(400);
  await page.evaluate(() => window.dispatchEvent(new Event('beforeprint')));
  await page.emulateMedia({ media: 'print' });
  await expect(host.locator('[data-nm-space-stage]')).toBeHidden();
  await expect(host.getByText('sub-01 / t / sub-01_t', { exact: true }).first()).toBeVisible();
  await page.emulateMedia({ media: 'screen' });
  await page.evaluate(() => window.dispatchEvent(new Event('afterprint')));
  await page.goto('file://' + resolve(__dirname, '.artifacts/space-report.html'));
  await expect(page.getByRole('button', { name: 'Open comparison', exact: true })).toBeDisabled();
  await expect(page.locator('[data-nm-space-status]')).toContainText('Serve this report');
  await expect(page.locator('iframe')).toHaveCount(0);
});

test('closing during a pending data fetch prevents a late frame mount', async ({ page }) => {
  await page.goto('/space-report.html');
  let release;
  const pending = new Promise(resolve => { release = resolve; });
  await page.route('**/space/volumes/*', async route => { await pending; await route.continue().catch(() => {}); });
  await page.getByRole('button', { name: 'Open comparison', exact: true }).click();
  await page.getByRole('button', { name: 'Close comparison', exact: true }).click();
  release();
  await expect(page.locator('[data-nm-space-stage] iframe')).toHaveCount(0);
  await expect(page.getByRole('button', { name: 'Open comparison', exact: true })).toBeVisible();
  await page.unroute('**/space/volumes/*');
  await openSpace(page);
  await expect(page.locator('[data-nm-space-stage] iframe')).toHaveCount(1);
});

test('a superseded pending frame cannot replace the active set after the newest load fails', async ({ page }) => {
  await page.goto('/space-report.html');
  // Instrument the public runtime boundary to delay candidate readiness. This
  // makes the A -> pending B -> failing C race deterministic, without sleeps.
  await page.addScriptTag({ url: '/space-report_files/space/runtime/space-host.js' });
  await page.evaluate(() => {
    const mount = NeuroMosaicSpaceRuntime.mountSpace;
    let calls = 0;
    window.spaceDelayed = {};
    window.NeuroMosaicSpaceRuntime = { mountSpace: (container, options) => {
      const h = mount(container, options);
      if (++calls !== 2) return h;
      window.spaceDelayed.started = true;
      const deferred = new Promise(resolve => { window.spaceDelayed.release = resolve; });
      const ready = h.ready.then(() => { window.spaceDelayed.nativeReady = true; return deferred; });
      return { ...h, ready, dispose() { window.spaceDelayed.disposed = true; h.dispose(); } };
    } };
  });
  await openSpace(page); // A
  const host = page.locator(hostSelector), select = host.getByRole('combobox');
  await select.selectOption('subset'); // B reaches mount, but readiness is held
  await expect.poll(() => page.evaluate(() => !!window.spaceDelayed.started)).toBe(true);
  await expect.poll(() => page.evaluate(() => !!window.spaceDelayed.nativeReady)).toBe(true);
  await setState(page, { selectedMemberId: 'member-4' });
  await expect(host.locator('[data-nm-space-selection]')).toContainText('sub-04');
  await page.route('**/space/volumes/*', route => route.abort());
  await select.selectOption('faces'); // C request fails before reaching mount
  await expect(host.locator('[data-nm-space-status]')).toContainText('Could not open comparison');
  expect(await page.evaluate(() => window.spaceDelayed.disposed)).toBe(true);
  await page.evaluate(() => window.spaceDelayed.release());
  await expect(select).toHaveValue('faces');
  await expect(host.locator('iframe')).toHaveCount(1);
  // Only A contains member-4; successful selection proves data and bindings agree.
  await setState(page, { selectedMemberId: 'member-4' });
  await expect(host.locator('[data-nm-space-selection]')).toContainText('sub-04');
});

test('the packaged public API clears a missing Ortho member on model switch', async ({ page }) => {
  await page.goto('/space-report.html');
  await openSpace(page);
  await page.locator(hostSelector).evaluate(el => el.__nmSpace.close());
  const result = await page.evaluate(async () => {
    const size = 125, values = k => Float32Array.from({ length: size }, (_, i) => Math.sin(i * .3 + k) + Math.cos(i * .7) * k);
    const members = [0, 1, 2].flatMap(i => [
      { id: 'a-' + i, subject: 's' + i, model: 'first', contrast: 'cue', t: values(i + 1) },
      { id: 'b-' + i, subject: 'x' + i, model: 'other', contrast: 'disjoint', beta: values(i + 2) }
    ]);
    const data = { id: 'native-regression', grid: { dimensions: [5, 5, 5], affine: [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]] },
      members, template: new Float32Array(size).fill(1), mask: new Uint8Array(size).fill(1) };
    const container = document.createElement('div'); container.style.height = '650px'; document.body.append(container);
    const handle = NeuroMosaicSpaceRuntime.mountSpace(container, { assetBaseUrl: new URL('space-report_files/space/runtime/frame/', location.href).href, data });
    try {
      await handle.ready;
      await handle.setState({ mode: 'ortho', selectedMemberId: 'a-0' });
      return await handle.setState({ model: 'other' });
    } finally { handle.dispose(); container.remove(); }
  });
  expect(result).toMatchObject({ mode: 'space', model: 'other', contrast: 'disjoint', statistic: 'beta', selectedMemberId: null });
});

test('single-map report navigation focuses the correct heading without variant tabs', async ({ page }) => {
  await page.goto('/space-single.html');
  await openSpace(page);
  await setState(page, { selectedMemberId: 'member-3' });
  await page.locator(hostSelector).getByRole('button', { name: 'Show in report' }).click();
  const article = page.locator('[data-nm-map-article="sub-03"]');
  await expect(article.locator('h3')).toBeFocused();
  await expect(article.getByRole('tab')).toHaveCount(0);
});

test('the rendered Quarto source opens its companion Space bundle', async ({ page }) => {
  await page.goto('/space-quarto/space-report.html');
  await openSpace(page);
  await setState(page, { selectedMemberId: 'member-1' });
  await expect(page.locator('[data-nm-space-selection]')).toContainText('sub-01');
});

test('rapid report channel and member reversals retain the latest selection', async ({ page }) => {
  await page.goto('/space-report.html');
  await page.addScriptTag({ url: '/space-report_files/space/runtime/space-host.js' });
  await page.evaluate(() => {
    const mount = NeuroMosaicSpaceRuntime.mountSpace;
    window.spaceRequests = [];
    window.NeuroMosaicSpaceRuntime = { mountSpace: (container, options) => {
      const handle = mount(container, options);
      return { ...handle, setState(patch) {
        const request = handle.setState(patch); window.spaceRequests.push(request); return request;
      } };
    } };
  });
  await openSpace(page);
  await setState(page, { selectedMemberId: 'member-2', statistic: 't' });
  await page.evaluate(async () => {
    const tab = (subject, name) => [...document.querySelectorAll(`[data-nm-map-group="${subject}"] [role=tab]`)]
      .find(node => node.textContent.trim() === name);
    window.spaceRequests = [];
    tab('sub-02', 'Estimate').click(); tab('sub-02', 'T').click();
    await Promise.all(window.spaceRequests);
  });
  expect(await state(page)).toMatchObject({ selectedMemberId: 'member-2', statistic: 't' });
  await page.evaluate(async () => {
    const tab = subject => [...document.querySelectorAll(`[data-nm-map-group="${subject}"] [role=tab]`)]
      .find(node => node.textContent.trim() === 'T');
    window.spaceRequests = [];
    tab('sub-03').click(); tab('sub-02').click();
    await Promise.all(window.spaceRequests);
  });
  expect(await state(page)).toMatchObject({ selectedMemberId: 'member-2', statistic: 't' });
  await page.getByRole('button', { name: 'Show in report', exact: true }).click();
  await expect(page.locator('[data-nm-map-group="sub-02"] [data-nm-map-id="sub-02_t"]')).toBeVisible();
});

for (const boundary of ['runtime', 'data', 'constructor']) {
  test(`opening Space cancels ordinary slices pending at ${boundary}`, async ({ page }) => {
    await page.goto('/space-report.html');
    let release, started = false;
    const gate = new Promise(resolve => { release = resolve; });
    if (boundary === 'constructor') {
      await page.evaluate(async () => {
        await NeuroMosaicVolumeReport.ensureRuntime();
        const create = neuroimjs.SimpleOrthogonalViewer.create.bind(neuroimjs.SimpleOrthogonalViewer);
        let calls = 0;
        neuroimjs.SimpleOrthogonalViewer.create = async (...args) => {
          const viewer = await create(...args);
          if (++calls === 1) {
            window.volumeConstructorStarted = true;
            await new Promise(resolve => { window.releaseVolumeConstructor = resolve; });
          }
          return viewer;
        };
      });
    } else {
      const runtime = await page.locator('script[data-nm-volume-runtime-src]').evaluate(el => JSON.parse(el.textContent).src);
      const matches = boundary === 'runtime' ? new URL(runtime, page.url()).href : /\/interactive\/.*\.nii(?:\.gz)?$/;
      await page.route(matches, async route => { started = true; await gate; await route.continue().catch(() => {}); });
    }
    const volume = page.locator('[data-nm-volume-analysis="sub-02"]');
    await volume.getByRole('button', { name: 'Explore volume interactively' }).click();
    if (boundary === 'constructor') await expect.poll(() => page.evaluate(() => !!window.volumeConstructorStarted)).toBe(true);
    else await expect.poll(() => started).toBe(true);
    await volume.evaluate(el => { window.pendingVolume = el.__nmActivating; });
    await openSpace(page);
    if (boundary === 'constructor') await page.evaluate(() => window.releaseVolumeConstructor());
    else release();
    await page.evaluate(() => window.pendingVolume);
    await expect(volume).not.toHaveAttribute('data-nm-volume-ready', 'true');
    await expect(volume.locator('[data-nm-volume-shell]')).toBeHidden();
    await expect(volume.locator('[data-nm-volume-viewer] canvas')).toHaveCount(0);
    expect(await volume.evaluate(el => NeuroMosaicVolumeReport.getState(el) == null)).toBe(true);
    expect(await page.evaluate(() => NeuroMosaicGL.keys())).not.toContain('volume:sub-02');
    await expect(page.locator(hostSelector + ' iframe')).toHaveCount(1);
    // A later deliberate open still works after the cancelled activation.
    await volume.getByRole('button', { name: 'Explore volume interactively' }).click();
    await expect(volume).toHaveAttribute('data-nm-volume-ready', 'true', { timeout: 30000 });
    await expect(page.locator(hostSelector + ' iframe')).toHaveCount(0);
  });
}

test('no-JavaScript fallback links and printing retain every included map', async ({ browser }) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  try {
    const page = await context.newPage();
    await page.goto('http://127.0.0.1:4173/space-report.html');
    const host = page.locator(hostSelector);
    const links = host.locator('.nm-space-members a');
    await expect(links).toHaveCount(21);
    await host.getByRole('link', { name: 'sub-02 / estimate / sub-02_beta', exact: true }).first().click();
    const target = await page.evaluate(() => decodeURIComponent(location.hash.slice(1)));
    await expect(page.locator(`[id="${target}"]`)).toHaveAttribute('data-nm-map-id', 'sub-02_beta');
    await expect(page.locator(`[id="${target}"]`)).toBeVisible();
    await page.emulateMedia({ media: 'print' });
    for (const link of await links.all()) await expect(link).toBeVisible();
    await page.screenshot({ path: '/tmp/neuromosaic-space-fixed-no-js-print.png', fullPage: true });
  } finally { await context.close(); }
});

test('fallback links reveal exact variants after Space fails and opening preserves keyboard focus', async ({ page }) => {
  await page.goto('/space-report.html');
  await page.route('**/space/runtime/space-host.js', route => route.abort());
  const host = page.locator(hostSelector);
  await host.getByRole('button', { name: 'Open comparison', exact: true }).focus();
  await page.keyboard.press('Enter');
  await expect(host.locator('[data-nm-space-status]')).toContainText('Could not open comparison');
  await host.locator('summary').click();
  await host.getByRole('link', { name: 'sub-02 / estimate / sub-02_beta', exact: true }).first().click();
  await expect(page.locator('[data-nm-map-group="sub-02"] [data-nm-map-id="sub-02_beta"]')).toBeVisible();
  await page.unroute('**/space/runtime/space-host.js');
  await host.getByRole('button', { name: 'Open comparison', exact: true }).focus();
  await page.keyboard.press('Enter');
  await expect(host).toHaveAttribute('data-nm-space-ready', 'true');
  await expect(host.getByRole('button', { name: 'Close comparison', exact: true })).toBeFocused();
  await page.keyboard.press('Enter');
  await expect(host.getByRole('button', { name: 'Open comparison', exact: true })).toBeFocused();
});

for (const boundary of ['runtime', 'ready']) {
  test(`surface release cancels a mount pending at ${boundary}`, async ({ page }) => {
    await page.goto('/interactive-surface-bundle.html');
    const host = page.locator('[data-nm-surface-analysis="surface-interactive"]');
    let release, started = false;
    const gate = new Promise(resolve => { release = resolve; });
    const src = await page.locator('#surfview-runtime-attachment').getAttribute('href');
    if (boundary === 'runtime') {
      const body = await page.evaluate(async src => (await fetch(src)).text(), src);
      await page.locator('#surfview-runtime-attachment').evaluate(el => { el.href = '/held-surface-runtime.js'; });
      await page.route('**/held-surface-runtime.js', async route => {
        started = true; await gate; await route.fulfill({ contentType: 'text/javascript', body });
      });
    } else {
      await page.addScriptTag({ url: src });
      await page.evaluate(() => {
        const mount = surfview.mountSurfView;
        window.surfview = { ...surfview, mountSurfView(...args) {
          const handle = mount(...args);
          const ready = handle.ready.then(() => {
            window.surfaceNativeReady = true;
            return new Promise(resolve => { window.releaseSurfaceReady = resolve; });
          });
          return new Proxy(handle, { get(target, key) { return key === 'ready' ? ready : Reflect.get(target, key); } });
        } };
      });
    }
    await page.locator('[data-nm-view-router="surface-interactive"]').getByRole('radio', { name: '3D surface', exact: true }).check();
    if (boundary === 'runtime') await expect.poll(() => started).toBe(true);
    else await expect.poll(() => page.evaluate(() => !!window.surfaceNativeReady)).toBe(true);
    await host.evaluate(el => { window.pendingSurface = el.__nmSurfaceMount; NeuroMosaicSurfaceBridge.release(el); });
    if (boundary === 'runtime') release();
    else await page.evaluate(() => window.releaseSurfaceReady());
    await page.evaluate(() => window.pendingSurface);
    await expect(host).not.toHaveAttribute('data-nm-surface-ready', 'true');
    await expect(host.locator('.surfwidget canvas')).toHaveCount(0);
    expect(await page.evaluate(() => NeuroMosaicGL.keys())).not.toContain('surface:surface-interactive');
  });
}
