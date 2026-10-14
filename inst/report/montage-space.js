// Progressive Space integration. No data or frame is loaded before Open.
(function (root) {
  'use strict';
  function list(value) { return Array.isArray(value) ? value : value == null ? [] : [value]; }
  function runtimeChannel(channel) { return channel === 'estimate' ? 'beta' : channel; }

  // Deliberately restricted to canonical NIfTI assets written by this package.
  // World geometry comes from the separately validated scene, never restamped.
  function decodeNifti(buffer, asset, count, target) {
    var view = new DataView(buffer);
    if (buffer.byteLength < 352 || view.getInt32(0, true) !== 348) throw new Error('Invalid canonical NIfTI header.');
    if (view.getInt16(40, true) !== 3 ||
        view.getInt16(42, true) * view.getInt16(44, true) * view.getInt16(46, true) !== count) {
      throw new Error('NIfTI dimensions do not match the comparison grid.');
    }
    var types = { uint8: [2, 1, 'getUint8'], int16: [4, 2, 'getInt16'], float64: [64, 8, 'getFloat64'] };
    var type = types[asset.datatype];
    if (!type || view.getInt16(70, true) !== type[0]) throw new Error('Unexpected comparison asset datatype.');
    var offset = view.getFloat32(108, true), slope = view.getFloat32(112, true), intercept = view.getFloat32(116, true);
    if (!Number.isInteger(offset) || offset < 352 || buffer.byteLength !== offset + count * type[1] ||
        (slope !== 0 && slope !== 1) || (slope !== 0 && intercept !== 0)) {
      throw new Error('Unexpected canonical NIfTI scaling or byte length.');
    }
    var out = target === 'mask' ? new Uint8Array(count) : target === 'atlas' ? new Int32Array(count) : new Float32Array(count);
    for (var i = 0; i < count; i++) {
      var value = view[type[2]](offset + i * type[1], true);
      if (target === 'mask' && value !== 0 && value !== 1) throw new Error('Comparison mask is not binary.');
      if (target === 'atlas' && (!Number.isInteger(value) || value < 0 || value > 2147483647)) throw new Error('Invalid atlas label.');
      if (target === 'template' && !Number.isFinite(value)) throw new Error('Template contains missing values.');
      out[i] = value;
      if (Number.isFinite(value) && !Number.isFinite(out[i])) throw new Error('Map value overflows Float32.');
      if (!Number.isFinite(value) && !Number.isNaN(value)) throw new Error('Map contains an infinite value.');
    }
    return out;
  }
  if (typeof module !== 'undefined' && module.exports) module.exports = { decodeNifti: decodeNifti, runtimeChannel: runtimeChannel };
  if (!root.document) return;
  var document = root.document, runtimePromise;
  function sameOrigin(ref) {
    var url = new URL(ref, document.baseURI);
    if (url.origin !== location.origin || !/^https?:$/.test(url.protocol)) throw new Error('Space assets must be served from the report’s origin.');
    return url.href;
  }
  function ensureRuntime(ref) {
    if (root.NeuroMosaicSpaceRuntime) return Promise.resolve(root.NeuroMosaicSpaceRuntime);
    if (!runtimePromise) runtimePromise = new Promise(function (resolve, reject) {
      var script = document.createElement('script');
      script.src = sameOrigin(ref);
      script.onload = function () {
        if (root.NeuroMosaicSpaceRuntime) resolve(root.NeuroMosaicSpaceRuntime);
        else { script.remove(); runtimePromise = null; reject(new Error('Space runtime did not initialize.')); }
      };
      script.onerror = function () { script.remove(); runtimePromise = null; reject(new Error('Could not load the Space runtime.')); };
      document.head.appendChild(script);
    });
    return runtimePromise;
  }
  async function loadDataset(scene, signal) {
    var count = scene.grid.dimensions.reduce(function (a, b) { return a * b; }, 1);
    var cache = new Map();
    async function load(asset, target) {
      var key = asset.location.ref + ':' + target;
      if (!cache.has(key)) cache.set(key, (async function () {
        var response = await fetch(sameOrigin(asset.location.ref), { signal: signal });
        if (!response.ok) throw new Error('Comparison data could not load (HTTP ' + response.status + ').');
        var buffer = await response.arrayBuffer();
        if (asset.compression === 'gzip') {
          if (!root.DecompressionStream) throw new Error('This browser cannot decompress comparison data. Use a current browser.');
          buffer = await new Response(new Blob([buffer]).stream().pipeThrough(new DecompressionStream('gzip'))).arrayBuffer();
        }
        return decodeNifti(buffer, asset, count, target);
      }()));
      return cache.get(key);
    }
    var dataset = { id: scene.id, grid: scene.grid, members: [] };
    dataset.template = await load(scene.template, 'template');
    dataset.mask = await load(scene.mask, 'mask');
    // Sequential fetch bounds simultaneous decompressed intermediates; all
    // final channel arrays are required by the supported upstream API.
    for (var member of scene.members) {
      var row = { id: member.id, subject: member.subject, model: member.model, contrast: member.contrast };
      for (var key of ['t', 'beta', 'se']) if (member[key]) row[key] = await load(member[key], 'map');
      dataset.members.push(row);
    }
    if (scene.atlas) dataset.atlas = { name: scene.atlas.name, regions: scene.atlas.regions, labels: await load(scene.atlas.labels, 'atlas') };
    if (scene.covariates) dataset.covariates = scene.covariates;
    return dataset;
  }
  function closeOtherViewers() {
    document.querySelectorAll('[data-nm-view-router] input[value="static"]').forEach(function (radio) {
      if (!radio.checked) { radio.checked = true; radio.dispatchEvent(new Event('change', { bubbles: true })); }
    });
    document.querySelectorAll('[data-nm-volume-host]').forEach(function (host) {
      var toggle = host.querySelector('[data-nm-volume-toggle][aria-expanded="true"]');
      if (toggle) toggle.click();
      if (root.NeuroMosaicVolumeReport) root.NeuroMosaicVolumeReport.dispose(host);
    });
    document.querySelectorAll('[data-nm-surface-analysis]').forEach(function (host) {
      if (root.NeuroMosaicSurfaceBridge) root.NeuroMosaicSurfaceBridge.release(host);
    });
  }
  function init() {
    var host = document.querySelector('[data-nm-space-host]'), config = document.getElementById('nm-space-scene');
    if (!host || !config || host.__nmSpace) return;
    var scene = JSON.parse(config.textContent);
    var part = function (name) { return host.querySelector('[data-nm-space-' + name + ']'); };
    var select = part('set'), open = part('open'), close = part('close'), stage = part('stage'), show = part('show');
    var handle = null, activeStage = null, pendingHandle = null, pendingStage = null;
    var active = null, current = null, focused = null, generation = 0, abort = null, busy = false, fromSpace = false;
    var states = new Map();
    function chosen() { return scene.sets.find(function (set) { return set.id === select.value; }); }
    function status(text) { part('status').textContent = text || ''; }
    function summary() {
      var set = chosen();
      var size = set.payload_bytes < 1048576 ? Math.ceil(set.payload_bytes / 1024) + ' KiB' : (set.payload_bytes / 1048576).toFixed(1) + ' MiB';
      part('summary').textContent = set.members.length + ' subjects · ' + list(set.common_channels).join(', ') +
        ' · explicit mask · ' + size + ' input';
    }
    function updateSelection(state) {
      current = state;
      if (active) states.set(active.id, state);
      focused = active && active.bindings.find(function (row) {
        return row.member_id === state.selectedMemberId && row.model === state.model &&
          row.contrast === state.contrast && runtimeChannel(row.channel) === state.statistic;
      });
      part('selection').textContent = focused ? focused.subject + (focused.label !== focused.subject ? ' · ' + focused.label : '') + ' · ' + focused.channel : 'Select a subject to inspect its report map.';
      show.disabled = !focused;
    }
    function dispose(message) {
      generation++; if (abort) abort.abort(); abort = null;
      cancelPending();
      if (handle) handle.dispose(); handle = null;
      activeStage = null;
      active = null; current = null; focused = null; busy = false;
      stage.replaceChildren(); stage.hidden = true;
      open.hidden = false; close.hidden = true; open.setAttribute('aria-expanded', 'false');
      host.querySelector('.nm-space-selection').hidden = true;
      host.removeAttribute('data-nm-space-ready'); status(message);
    }
    function cancelPending() {
      if (pendingHandle) pendingHandle.dispose(); pendingHandle = null;
      if (pendingStage) pendingStage.remove(); pendingStage = null;
    }
    async function activate() {
      if (!/^https?:$/.test(location.protocol)) { status('Serve this report and its companion folder over HTTP to open Space. The report maps remain available.'); return; }
      var request = ++generation, candidate = chosen();
      cancelPending();
      if (abort) abort.abort(); abort = new AbortController();
      var controller = abort;
      busy = true; closeOtherViewers();
      var moveFocus = document.activeElement === open;
      open.hidden = true; close.hidden = false; stage.hidden = false;
      if (moveFocus) close.focus();
      open.setAttribute('aria-expanded', 'true');
      status('Loading ' + candidate.label + '…');
      try {
        var runtime = await ensureRuntime(scene.runtime_script);
        if (request !== generation) return;
        var dataset = await loadDataset(candidate, controller.signal);
        if (request !== generation) return;
        var initial = states.get(candidate.id) || candidate.initial_state;
        // A candidate has its own disposable public handle. setData() on the
        // active handle cannot cancel a pending swap while a newer set's files
        // are downloading; that could leave old report bindings on new data.
        var candidateStage = document.createElement('div');
        candidateStage.style.cssText = 'position:absolute;inset:0;height:100%;visibility:hidden;pointer-events:none';
        candidateStage.setAttribute('aria-hidden', 'true');
        stage.appendChild(candidateStage); pendingStage = candidateStage;
        var candidateHandle = runtime.mountSpace(candidateStage, { assetBaseUrl: sameOrigin(scene.runtime_base), data: dataset, initialState: initial });
        pendingHandle = candidateHandle;
        await candidateHandle.ready;
        if (request !== generation) return;
        var loadedState = await candidateHandle.getState();
        if (request !== generation) return;
        if (handle) handle.dispose();
        if (activeStage) activeStage.remove();
        handle = candidateHandle; activeStage = candidateStage;
        pendingHandle = null; pendingStage = null;
        candidateStage.style.cssText = 'position:relative;height:100%';
        candidateStage.removeAttribute('aria-hidden');
        active = candidate; busy = false;
        handle.on('statechange', function (event) { if (handle === candidateHandle && active) updateSelection(event.state); });
        handle.on('error', function (event) { if (handle === candidateHandle && !busy) status('Comparison error: ' + event.error.message + ' Close and reopen to retry.'); });
        handle.on('statuschange', function (event) { if (handle === candidateHandle && !busy) status(event.status === 'fitting' ? 'Fitting the comparison…' : event.status === 'ready' ? 'Comparison ready.' : event.message); });
        updateSelection(loadedState);
        host.querySelector('.nm-space-selection').hidden = false;
        host.setAttribute('data-nm-space-ready', 'true'); status('Comparison ready.');
      } catch (error) {
        if (request !== generation) return;
        cancelPending();
        busy = false;
        if (!active) {
          var recoverFocus = document.activeElement === close;
          dispose();
          if (recoverFocus) open.focus();
        }
        else { select.value = active.id; summary(); }
        status('Could not open comparison: ' + error.message + ' Choose a set and retry.');
      }
    }
    open.addEventListener('click', activate);
    close.addEventListener('click', function () { dispose('Comparison closed.'); open.focus(); });
    select.addEventListener('change', function () { summary(); if (handle || busy) activate(); });
    show.addEventListener('click', function () {
      if (!focused) return;
      var binding = focused;
      var group = Array.from(document.querySelectorAll('[data-nm-map-group]')).find(function (node) { return node.getAttribute('data-nm-map-group') === binding.analysis_id; });
      var article = group || Array.from(document.querySelectorAll('[data-nm-map-article]')).find(function (node) { return node.getAttribute('data-nm-map-article') === binding.analysis_id; });
      if (!article) { status('This map is not present in the report layout.'); return; }
      fromSpace = true;
      article.dispatchEvent(new CustomEvent('nm-map-request', { bubbles: true, detail: { analysisId: binding.analysis_id, mapId: binding.map_id, source: 'space' } }));
      fromSpace = false;
      var variant = Array.from(article.querySelectorAll('[data-nm-map-id]')).find(function (node) { return node.getAttribute('data-nm-map-id') === binding.map_id; });
      var heading = (variant || article).querySelector('h2,h3,h4') || article;
      heading.setAttribute('tabindex', '-1'); heading.focus({ preventScroll: true });
      if (root.NeuroMosaicReport && root.NeuroMosaicReport.holdTarget) root.NeuroMosaicReport.holdTarget(heading, 2000, true);
      else heading.scrollIntoView({ block: 'start' });
    });
    document.addEventListener('nm-map-change', function (event) {
      if (fromSpace || !handle || !active || busy) return;
      var binding = active.bindings.find(function (row) { return row.map_id === event.detail.mapId && row.analysis_id === event.detail.analysisId; });
      if (!binding) return;
      if (!list(active.common_channels).includes(binding.channel)) { status('This report map is not a shared comparison channel; the current Space selection is unchanged.'); return; }
      var statistic = runtimeChannel(binding.channel);
      // Forward the latest intent even when it matches the last acknowledged
      // state: an earlier request may still be queued inside the iframe.
      var request = generation;
      handle.setState({ model: binding.model, contrast: binding.contrast, statistic: statistic, selectedMemberId: binding.member_id })
        .then(function (state) { if (request === generation) updateSelection(state); })
        .catch(function (error) { if (request === generation) status(error.message); });
    });
    document.addEventListener('nm-view-change', function (event) {
      if (event.detail && event.detail.view !== 'static' && (handle || busy)) dispose('Comparison closed while the map viewer is open.');
    });
    document.addEventListener('click', function (event) {
      var toggle = event.target.closest && event.target.closest('[data-nm-volume-toggle]');
      if (toggle && toggle.getAttribute('aria-expanded') !== 'true' && (handle || busy)) dispose('Comparison closed while the map viewer is open.');
    }, true);
    root.addEventListener('pagehide', function () { dispose(); });
    var observer = new MutationObserver(function () { if (!host.isConnected) { dispose(); observer.disconnect(); } });
    observer.observe(document.body, { childList: true, subtree: true });
    host.__nmSpace = { close: dispose, open: activate, getState: function () { return handle ? handle.getState() : Promise.resolve(null); },
      setState: function (patch) { return handle ? handle.setState(patch) : Promise.reject(new Error('Comparison is closed.')); } };
    summary();
    // The server-rendered list stays open without scripting, including print.
    host.querySelector('.nm-space-members').open = false;
    if (!/^https?:$/.test(location.protocol)) { open.disabled = true; status('Serve this report and its companion folder over HTTP to open Space. The report maps remain available.'); }
    var printDetails = [];
    root.addEventListener('beforeprint', function () {
      printDetails = Array.from(host.querySelectorAll('details:not([open])'));
      printDetails.forEach(function (detail) { detail.open = true; });
    });
    root.addEventListener('afterprint', function () { printDetails.forEach(function (detail) { detail.open = false; }); printDetails = []; });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init); else init();
}(typeof globalThis !== 'undefined' ? globalThis : this));
