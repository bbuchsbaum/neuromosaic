(function (root) {
  "use strict";

  var ADAPTER_VERSION = "1.5.0";
  var RUNTIME_VERSION = "0.5.0";
  var OVERLAY_LAYER_ID = "neuromosaic-overlay";
  var BACKGROUND_LAYER_ID = "neuromosaic-background";
  var UNDERLAY_HEADROOM = 1.3;
  // Stage grounds: "light" matches the static figures, "dark" is the
  // instrument stage. Theme picks the default; a reader's choice is kept.
  var GROUNDS = {
    dark: {
      stage: 0x111619, gutter: "#22282c", rampLow: 0x111619, rampHigh: 0xffffff, hideOutside: false,
      cross: 0xffffff, crossAlpha: 0.58, halo: 0x000000, haloAlpha: 0.35,
      label: 0xe6eaec, labelAlpha: 0.6, labelShadow: 0.9,
      hist: { hidden: "rgba(255, 255, 255, 0.10)", base: "rgba(255, 255, 255, 0.18)",
        guide: "rgba(255, 255, 255, 0.55)", caret: "rgba(255, 255, 255, 0.95)" }
    },
    light: {
      stage: 0xfafaf7, gutter: "#e3e5e2", rampLow: 0x16191b, rampHigh: 0xf6f6f3, hideOutside: true,
      cross: 0x1c2226, crossAlpha: 0.55, halo: 0xffffff, haloAlpha: 0.6,
      label: 0x3e4952, labelAlpha: 0.9, labelShadow: 0,
      hist: { hidden: "rgba(28, 34, 38, 0.10)", base: "rgba(28, 34, 38, 0.2)",
        guide: "rgba(28, 34, 38, 0.5)", caret: "rgba(28, 34, 38, 0.9)" }
    }
  };
  var GROUND_KEY = "neuromosaic.volume.ground";

  function pageIsDark() {
    var forced = document.documentElement.getAttribute("data-theme");
    if (forced === "dark") return true;
    if (forced === "light") return false;
    return typeof matchMedia === "function" && matchMedia("(prefers-color-scheme: dark)").matches;
  }

  function groundOf(host) {
    var chosen = host.dataset.nmVolumeGroundChoice;
    if (!chosen) {
      try { chosen = root.localStorage && root.localStorage.getItem(GROUND_KEY); } catch (error) { chosen = null; }
    }
    if (chosen === "light" || chosen === "dark") return chosen;
    return pageIsDark() ? "dark" : "light";
  }
  var MINUS = "\u2212";
  var REPORT_PALETTE = "Report (nm-signed)";
  // Qualitative, rainbow and duplicate presets have no place in a menu for
  // statistical maps; the report palette leads the list.
  var HIDDEN_PRESETS = [
    "Set1", "Set2", "Set3", "Accent", "Dark2", "Paired", "Pastel1",
    "Pastel2", "Greys", "Spectral", "RdYlGn"
  ];
  var states = new WeakMap();

  // <nm-gl-budget> -- identical in neuromosaic-volume/adapter.js and
  // neuromosaic-surface/bridge.js; whichever script runs first installs it.
  // Browsers kill the oldest WebGL context past a page-wide pool (16 in
  // Chrome). Keep at most BUDGET live by releasing least recently used
  // viewers that are not visible; a released chosen view is "parked" (keeps
  // its height) and is rebuilt only on screen and within the budget.
  var GL = (function (scope) {
    var VERSION = 2;
    var existing = scope.NeuroMosaicGL;
    if (existing && existing.version >= VERSION) return existing;
    var BUDGET = 8;
    var HARD = 14;
    var MARGIN = 120;
    var live = [];
    var parked = [];
    var observer = null;
    var proto = typeof HTMLCanvasElement !== "undefined" && HTMLCanvasElement.prototype;
    // Remember each canvas's WebGL context so releasing it never has to ask
    // getContext() (which would create a context on a bare canvas).
    if (proto && !proto.getContext.__nmTracked) {
      var getContext = proto.getContext;
      var tracked = function (type) {
        var context = getContext.apply(this, arguments);
        if (context && /webgl/.test(String(type)) && !this.__nmGL) this.__nmGL = context;
        return context;
      };
      tracked.__nmTracked = true;
      proto.getContext = tracked;
    }
    // On screen: displayed and within MARGIN px of the viewport.
    function onScreen(element) {
      if (!element || element.hidden || !element.getClientRects().length) return false;
      var box = element.getBoundingClientRect();
      var height = scope.innerHeight || (document.documentElement && document.documentElement.clientHeight) || 0;
      return box.width > 0 && box.height > 0 && box.bottom > -MARGIN && box.top < height + MARGIN;
    }
    // Visible: the chosen view and (when the entry names its element) on
    // screen.
    function visible(entry) {
      try {
        return Boolean(entry.shown()) && (!entry.element || onScreen(entry.element));
      } catch (error) {
        return false;
      }
    }
    function total(exceptKey) {
      return live.reduce(function (sum, entry) {
        return entry.key === exceptKey ? sum : sum + entry.cost;
      }, 0);
    }
    function find(list, key) {
      for (var i = 0; i < list.length; i += 1) if (list[i].key === key) return list[i];
      return null;
    }
    function drop(key) {
      live = live.filter(function (entry) { return entry.key !== key; });
    }
    function unpark(key) {
      parked = parked.filter(function (entry) { return entry.key !== key; });
    }
    // Release least recently used viewers that are not visible until `need`
    // more contexts fit under BUDGET. `keep` is never released; its own live
    // contexts count unless it is about to be rebuilt (`replacing`).
    function trim(need, keep, replacing) {
      var sum = total(replacing ? keep : null);
      for (var i = 0; sum + need > BUDGET && i < live.length;) {
        var entry = live[i];
        if (entry.key === keep || visible(entry)) { i += 1; continue; }
        live.splice(i, 1);
        sum -= entry.cost;
        try { entry.release(); } catch (error) { /* already gone */ }
      }
      return sum;
    }
    function watch(element) {
      if (!element || typeof IntersectionObserver === "undefined") return;
      if (!observer) {
        observer = new IntersectionObserver(function (records) {
          records.forEach(function (record) {
            if (record.isIntersecting) {
              var waiting = parked.filter(function (entry) { return entry.element === record.target; });
              waiting.forEach(function (entry) {
                // Let a fast scroll pass by before rebuilding anything.
                setTimeout(function () {
                  if (find(parked, entry.key) === entry && onScreen(entry.element)) {
                    try { entry.resume(); } catch (error) { /* reported by the viewer */ }
                  }
                }, 150);
              });
            } else if (total() > BUDGET) {
              trim(0, null);
            }
          });
        }, { rootMargin: MARGIN + "px 0px" });
      }
      if (!element.__nmGLWatched) {
        element.__nmGLWatched = true;
        observer.observe(element);
      }
    }
    var api = scope.NeuroMosaicGL = {
      version: VERSION,
      budget: BUDGET,
      hard: HARD,
      onScreen: onScreen,
      // Register (or refresh) a live viewer as most recently used:
      // { key, cost, element, shown(), release() }.
      touch: function (entry) {
        drop(entry.key);
        unpark(entry.key);
        live.push(entry);
        watch(entry.element);
        trim(0, entry.key);
      },
      drop: drop,
      // Make room for `cost` new contexts for `key`. True when they fit
      // without passing HARD (visible viewers are never released for it).
      reserve: function (key, cost) {
        return trim(cost, key, true) + cost <= HARD;
      },
      // A released viewer that is still the chosen view waits here until it
      // is back on screen: { key, element, resume() }.
      park: function (entry) {
        unpark(entry.key);
        parked.push(entry);
        watch(entry.element);
      },
      unpark: unpark,
      isParked: function (key) { return Boolean(find(parked, key)); },
      live: function () { return total(null); },
      keys: function () { return live.map(function (entry) { return entry.key; }); },
      isLost: function (canvases) {
        if (!canvases) return false;
        if (typeof canvases.querySelectorAll === "function") canvases = canvases.querySelectorAll("canvas");
        return Array.prototype.some.call(canvases, function (canvas) {
          return Boolean(canvas.__nmGL && canvas.__nmGL.isContextLost());
        });
      },
      // Hand each context back to the browser now rather than at garbage
      // collection, so the pool is free for the next viewer.
      loseContexts: function (canvases) {
        if (!canvases) return;
        if (typeof canvases.querySelectorAll === "function") canvases = canvases.querySelectorAll("canvas");
        Array.prototype.forEach.call(canvases, function (canvas) {
          var context = canvas.__nmGL;
          if (!context || context.isContextLost()) return;
          canvas.__nmReleased = true;
          var ext = context.getExtension("WEBGL_lose_context");
          if (ext) ext.loseContext();
        });
      }
    };
    return api;
  }(typeof globalThis !== "undefined" ? globalThis : window));
  // </nm-gl-budget>

  function fail(message) {
    throw new Error(message);
  }

  var FILE_MESSAGE = "Interactive views need the report served over HTTP — open the shared link, or ask the author for the single-file version.";

  // Companion assets cannot be fetched from a report opened from disk.
  function needsServer(scene) {
    var displayApi = root.NeuroMosaicVolumeDisplay;
    return Boolean(displayApi && root.location && displayApi.needsServer(
      root.location.protocol, scene,
      Boolean(document.querySelector("script[data-nm-volume-runtime-src]"))
    ));
  }

  // From disk, HTTP is named only when a file could not be fetched; any other
  // failure (a WebGL error, an unsupported scene) states its own reason.
  function readerMessage(error) {
    var reason = (error && error.message) || "unknown error";
    if (reason === FILE_MESSAGE || (root.location && root.location.protocol === "file:" &&
        /Failed to fetch|NetworkError|Load failed/.test(reason))) return FILE_MESSAGE;
    return "The interactive view could not load (" + reason + "). The static figures are unaffected.";
  }

  function byId(id) {
    return document.getElementById(id);
  }

  function sceneFor(host) {
    var node = byId(host.dataset.nmVolumeScene);
    if (!node) fail("Interactive VolumeScene metadata is missing.");
    var scene = JSON.parse(node.textContent || "null");
    if (!scene || scene.schema !== "org.neuromosaic.volume-scene" ||
        scene.schema_version !== 1) {
      fail("Unsupported neuromosaic VolumeScene.");
    }
    if (!scene.engine || scene.engine.name !== "neuroimjs" ||
        scene.engine.version !== RUNTIME_VERSION ||
        scene.engine.adapter_version !== ADAPTER_VERSION) {
      fail("The report's neuroimjs runtime/adapter versions do not match its VolumeScene.");
    }
    return scene;
  }

  function analysisFor(scene, id) {
    var analysis = scene.analyses.find(function (candidate) {
      return candidate.analysis_id === id;
    });
    if (!analysis) fail("Unknown interactive analysis: " + id);
    return analysis;
  }

  function assetFor(scene, id) {
    var asset = scene.assets.find(function (candidate) {
      return candidate.asset_id === id;
    });
    if (!asset) fail("Unknown interactive volume asset: " + id);
    return asset;
  }

  function decodeBase64(text) {
    var clean = String(text || "").replace(/\s+/g, "");
    var binary = atob(clean);
    var bytes = new Uint8Array(binary.length);
    for (var i = 0; i < binary.length; i += 1) {
      bytes[i] = binary.charCodeAt(i);
    }
    return bytes.buffer;
  }

  async function assetBytes(asset) {
    if (asset.location.kind === "embedded") {
      var payload = byId(asset.location.ref);
      if (!payload) fail("Embedded volume payload is missing: " + asset.asset_id);
      return decodeBase64(payload.textContent);
    }
    try {
      var response = await fetch(new URL(asset.location.ref, document.baseURI));
      if (!response.ok) fail("HTTP " + response.status);
      return await response.arrayBuffer();
    } catch (error) {
      if (window.location.protocol === "file:") fail(FILE_MESSAGE);
      throw error;
    }
  }

  // Bundle reports ship the ~1 MB neuroimjs runtime as a companion file and
  // load it on the first open; embedded reports inline it. One promise per
  // page, cleared on failure so a later open can retry.
  var runtimePromise = null;

  function runtimeSource() {
    var node = document.querySelector("script[data-nm-volume-runtime-src]");
    if (!node) return null;
    try {
      var spec = JSON.parse(node.textContent || "null");
      return spec && typeof spec.src === "string" ? spec.src : null;
    } catch (error) {
      return null;
    }
  }

  function ensureRuntime() {
    if (root.neuroimjs) return Promise.resolve(root.neuroimjs);
    if (runtimePromise) return runtimePromise;
    var src = runtimeSource();
    if (!src) return Promise.reject(new Error("The bundled neuroimjs runtime did not load."));
    runtimePromise = new Promise(function (resolve, reject) {
      var script = document.createElement("script");
      script.src = new URL(src, document.baseURI).href;
      script.async = true;
      script.dataset.nmVolumeRuntimeLoaded = "";
      script.onload = function () {
        if (root.neuroimjs) resolve(root.neuroimjs);
        else reject(new Error("The neuroimjs runtime loaded but did not initialise."));
      };
      script.onerror = function () {
        script.remove();
        reject(new Error("The viewer runtime (" + src + ") could not be loaded."));
      };
      document.head.appendChild(script);
    });
    runtimePromise.catch(function () { runtimePromise = null; });
    return runtimePromise;
  }

  async function decodeAsset(state, assetId, isBackground) {
    if (!isBackground && state.overlayCache.has(assetId)) {
      var cached = state.overlayCache.get(assetId);
      state.overlayCache.delete(assetId);
      state.overlayCache.set(assetId, cached);
      return cached;
    }

    var api = root.neuroimjs;
    var geometryApi = root.NeuroMosaicVolumeGeometry;
    if (!api || !geometryApi) fail("The bundled neuroimjs runtime did not load.");
    var asset = assetFor(state.scene, assetId);
    var volume = api.readNiftiArrayBuffer(await assetBytes(asset));
    geometryApi.assertGeometryMatch(
      asset.geometry,
      api.getVolumeGeometry(volume),
      asset.asset_id
    );

    // The underlay is cached page-wide, prepared (underlayFor).
    if (isBackground) return volume;
    state.overlayCache.set(assetId, volume);
    while (state.overlayCache.size > state.scene.view.cache_maps) {
      state.overlayCache.delete(state.overlayCache.keys().next().value);
    }
    return volume;
  }

  // Curate the shared preset registry once: drop qualitative/rainbow maps and
  // the duplicate "Greys" (shown as a second "Grayscale"), and put the report
  // palette first.
  function curatePresets(api) {
    var ColorMap = api.ColorMap;
    if (!ColorMap || ColorMap.__nmCurated) return;
    var presets = ColorMap.presetMaps;
    if (!presets || typeof presets !== "object") return;
    var ordered = {};
    ordered[REPORT_PALETTE] = presets[REPORT_PALETTE] || presets.BlueRed;
    Object.keys(presets).forEach(function (name) {
      if (name === REPORT_PALETTE || HIDDEN_PRESETS.indexOf(name) >= 0) return;
      ordered[name] = presets[name];
    });
    var displayApi = root.NeuroMosaicVolumeDisplay;
    var aliases = (displayApi && displayApi.menuAliases) || {};
    Object.keys(aliases).forEach(function (label) {
      if (presets[aliases[label]]) ordered[label] = presets[aliases[label]];
    });
    if (Object.prototype.hasOwnProperty.call(ColorMap, "_presetMaps")) {
      ColorMap._presetMaps = ordered;
    } else {
      Object.keys(presets).forEach(function (name) { delete presets[name]; });
      Object.keys(ordered).forEach(function (name) { presets[name] = ordered[name]; });
    }
    ColorMap.__nmCurated = true;
  }

  // The report palette (from R, per map) restretched to the live threshold
  // and range. The preset registry is page-wide, so each viewer re-registers
  // its colours before its panel reads them.
  function registerReportPalette(api, display, threshold, range) {
    curatePresets(api);
    var colors = display && display.palette_colors;
    if (!Array.isArray(colors) || colors.length < 2) return;
    if (display.mode === "thresholded") {
      var t = Number(display.threshold);
      threshold = threshold || (display.tail === "positive" ? [-Infinity, t] :
        (display.tail === "negative" ? [-t, Infinity] : [-t, t]));
      colors = root.NeuroMosaicVolumeDisplay.resaturate(
        colors, display.limits, display.threshold, threshold, range
      );
    }
    api.ColorMap.presetMaps[REPORT_PALETTE] = colors.slice();
  }

  function syncReportPalette(state, apply) {
    var map = currentMap(state);
    var layer = overlayVolLayer(state);
    if (!map || !layer) return;
    var api = root.neuroimjs;
    var range = layer.getRange().map(Number);
    registerReportPalette(api, map.display, layer.getThreshold().map(Number), range);
    state.paletteKey = thresholdKey(layer.getThreshold()) + "|" + thresholdKey(range);
    if (!apply || !layer.colorMap || layer.colorMap.name !== REPORT_PALETTE) return;
    state.viewer.updateLayer(OVERLAY_LAYER_ID, {
      colormap: api.ColorMap.fromPreset(REPORT_PALETTE, { existingColorMap: layer.colorMap })
    });
    // Redraw the panel's colour-map swatch from the restretched colours.
    if (state.controls && typeof state.controls.requestUpdate === "function") {
      state.controls.requestUpdate();
    }
  }

  function mapDisplay(map, volume) {
    var api = root.neuroimjs;
    registerReportPalette(api, map.display);
    var displayApi = root.NeuroMosaicVolumeDisplay;
    if (!displayApi) fail("The bundled display translator did not load.");
    var resolved = displayApi.resolve(
      map.display,
      volume.getRange(),
      api.ColorMap.getAvailableMaps()
    );
    return {
      range: resolved.range,
      threshold: resolved.threshold,
      alpha: resolved.alpha,
      colormap: api.ColorMap.fromPreset(resolved.colormapName),
      colormapName: resolved.colormapName
    };
  }

  function copyControlState(value) {
    return {
      layerId: value.layerId,
      range: value.range.slice(),
      threshold: value.threshold.slice(),
      colormap: value.colormap,
      opacity: value.opacity,
      visible: value.visible
    };
  }

  function samePair(left, right) {
    return Array.isArray(left) && Array.isArray(right) &&
      left.length === 2 && right.length === 2 &&
      left[0] === right[0] && left[1] === right[1];
  }

  function sameControlState(left, right) {
    return Boolean(left && right &&
      samePair(left.range, right.range) &&
      samePair(left.threshold, right.threshold) &&
      left.colormap === right.colormap &&
      left.opacity === right.opacity &&
      left.visible === right.visible);
  }

  // Background floor: well below Otsu's tissue split of a strided sample.
  function backgroundFloor(volume, range) {
    var data = typeof volume.getData === "function" ? volume.getData() : null;
    var lo = range[0], span = range[1] - range[0];
    if (!data || !data.length || !(span > 0)) return lo;
    var bins = new Float64Array(256);
    var stride = Math.max(1, Math.floor(data.length / 50000));
    var n = 0;
    for (var i = 0; i < data.length; i += stride) {
      var v = data[i];
      if (!Number.isFinite(v)) continue;
      bins[Math.min(255, Math.max(0, Math.floor(((v - lo) / span) * 255)))] += 1;
      n += 1;
    }
    if (!n) return lo;
    var total = 0;
    for (var t = 0; t < 256; t += 1) total += t * bins[t];
    var wB = 0, sumB = 0, best = 0, split = 0;
    for (var k = 0; k < 256; k += 1) {
      wB += bins[k];
      if (!wB || wB === n) continue;
      sumB += k * bins[k];
      var mB = sumB / wB, mF = (total - sumB) / (n - wB);
      var between = wB * (n - wB) * (mB - mF) * (mB - mF);
      if (between > best) { best = between; split = k; }
    }
    volume.__nmOtsuSplit = lo + span * (split / 255);
    return lo + span * Math.min(0.2, (0.4 * split) / 255);
  }

  // Bounding box (voxel indices) of everything brighter than a fraction of the
  // Otsu tissue split, i.e. the head, padded by a few voxels.
  function headBounds(volume) {
    var data = typeof volume.getData === "function" ? volume.getData() : null;
    var dim = volume.space && volume.space.dim;
    var cut = volume.__nmOtsuSplit;
    if (!data || !dim || !Number.isFinite(cut)) return null;
    cut = cut * 0.6;
    var nx = dim[0], ny = dim[1], nz = dim[2];
    var min = [nx, ny, nz], max = [-1, -1, -1];
    for (var k = 0; k < nz; k += 1) {
      for (var j = 0; j < ny; j += 1) {
        var row = (k * ny + j) * nx;
        for (var i = 0; i < nx; i += 1) {
          if (!(data[row + i] > cut)) continue;
          if (i < min[0]) min[0] = i; if (i > max[0]) max[0] = i;
          if (j < min[1]) min[1] = j; if (j > max[1]) max[1] = j;
          if (k < min[2]) min[2] = k; if (k > max[2]) max[2] = k;
        }
      }
    }
    if (max[0] < 0) return null;
    var pad = 3;
    return {
      min: min.map(function (v) { return Math.max(0, v - pad); }),
      max: max.map(function (v, d) { return Math.min(dim[d] - 1, v + pad); })
    };
  }

  // Grey ramp that starts at the stage colour, so empty field of view is the
  // stage itself and slice rectangles never show against it.
  function underlayColorMap(api, range, ground) {
    var spec = GROUNDS[ground] || GROUNDS.dark;
    var channel = function (hex, shift) { return ((hex >> shift) & 255) / 255; };
    var lo = [channel(spec.rampLow, 16), channel(spec.rampLow, 8), channel(spec.rampLow, 0)];
    var hi = [channel(spec.rampHigh, 16), channel(spec.rampHigh, 8), channel(spec.rampHigh, 0)];
    var colors = [];
    for (var i = 0; i < 256; i += 1) {
      var t = i / 255;
      colors.push([lo[0] + (hi[0] - lo[0]) * t, lo[1] + (hi[1] - lo[1]) * t, lo[2] + (hi[2] - lo[2]) * t]);
    }
    return new api.ColorMap(colors, { range: range, name: "Custom" });
  }

  // The underlay as the static figures draw it: the head is everything not
  // connected to the border through voxels at or below the floor (inner CSF
  // kept, outside set to `outside`); rim voxels take a blurred copy so the
  // 2 mm contour is smooth like the static 1 mm rim.
  function prepareUnderlay(volume, floor, outside, inside) {
    var data = typeof volume.getData === "function" ? volume.getData() : null;
    var dim = volume.space && volume.space.dim;
    if (!data || !dim || dim.length < 3 || data.length !== dim[0] * dim[1] * dim[2]) return volume;
    var nx = dim[0], ny = dim[1], nz = dim[2], n = data.length, plane = nx * ny;
    var low = new Uint8Array(n);
    for (var a = 0; a < n; a += 1) low[a] = !(Number(data[a]) > floor) ? 1 : 0;
    // Flood the outside from the border (6-connected).
    var out = new Uint8Array(n);
    var stack = [];
    var seed = function (index) { if (low[index] && !out[index]) { out[index] = 1; stack.push(index); } };
    for (var z0 = 0; z0 < nz; z0 += 1) {
      for (var y0 = 0; y0 < ny; y0 += 1) {
        for (var x0 = 0; x0 < nx; x0 += 1) {
          if (x0 === 0 || y0 === 0 || z0 === 0 || x0 === nx - 1 || y0 === ny - 1 || z0 === nz - 1) {
            seed((z0 * ny + y0) * nx + x0);
          }
        }
      }
    }
    while (stack.length) {
      var at = stack.pop();
      var x = at % nx, y = Math.floor(at / nx) % ny, z = Math.floor(at / plane);
      if (x > 0) seed(at - 1);
      if (x < nx - 1) seed(at + 1);
      if (y > 0) seed(at - nx);
      if (y < ny - 1) seed(at + nx);
      if (z > 0) seed(at - plane);
      if (z < nz - 1) seed(at + plane);
    }
    var src = new Float32Array(n);
    for (var b = 0; b < n; b += 1) {
      var v = Number(data[b]);
      src[b] = out[b] ? outside : Math.max(Number.isFinite(v) ? v : inside, inside);
    }
    var kernel = [1 / 16, 4 / 16, 6 / 16, 4 / 16, 1 / 16];
    var pass = function (from, to, stride, count) {
      for (var idx = 0; idx < n; idx += 1) {
        var pos = Math.floor(idx / stride) % count;
        var sum = 0;
        for (var k = -2; k <= 2; k += 1) {
          var q = Math.min(count - 1, Math.max(0, pos + k));
          sum += kernel[k + 2] * from[idx + (q - pos) * stride];
        }
        to[idx] = sum;
      }
    };
    var tmp = new Float32Array(n), blur = new Float32Array(n);
    pass(src, tmp, 1, nx);
    pass(tmp, blur, nx, ny);
    pass(blur, tmp, plane, nz);
    var result = src.slice();
    for (var c = 0; c < n; c += 1) {
      var cx = c % nx, cy = Math.floor(c / nx) % ny, cz = Math.floor(c / plane);
      var rim = false;
      for (var dz = -1; dz <= 1 && !rim; dz += 1) {
        for (var dy = -1; dy <= 1 && !rim; dy += 1) {
          for (var dx = -1; dx <= 1; dx += 1) {
            var xx = cx + dx, yy = cy + dy, zz = cz + dz;
            if (xx < 0 || yy < 0 || zz < 0 || xx >= nx || yy >= ny || zz >= nz) continue;
            if (out[(zz * ny + yy) * nx + xx] !== out[c]) { rim = true; break; }
          }
        }
      }
      if (rim) result[c] = tmp[c];
    }
    return withData(volume, result);
  }

  // The ground-dependent part of the underlay display: its grey ramp and, on
  // the light ground, hiding voxels at or below the floor (outside the head)
  // so the brain sits on the figure ground as in the static montage.
  function underlayLook(api, underlay, ground) {
    var spec = GROUNDS[ground] || GROUNDS.dark;
    var range = underlay.range, floor = underlay.floor;
    return {
      colormap: underlayColorMap(api, range, ground),
      threshold: spec.hideOutside ?
        [Math.min(range[0], floor) - Math.abs(range[1] - floor) - 1, floor] : [0, 0]
    };
  }

  // The underlay as drawn (floor, display range, smoothed head contour and
  // head bounds), decoded and prepared once per page and shared by every
  // viewer: a 1 mm template is 7 M voxels.
  var underlays = new Map();

  function underlayFor(state, assetId) {
    if (underlays.has(assetId)) return underlays.get(assetId);
    var pending = decodeAsset(state, assetId, true).then(function (volume) {
      var range = volume.getRange().map(Number);
      if (range.length !== 2 || !range.every(Number.isFinite)) {
        fail("Background volume has no finite display range.");
      }
      if (range[0] === range[1]) range = [range[0] - 0.5, range[1] + 0.5];
      var floor = backgroundFloor(volume, range);
      var bounds = headBounds(volume);
      // Hold the brightest tissue below white so suprathreshold colour reads
      // against the underlay instead of competing with it.
      range = [floor, floor + (range[1] - floor) * UNDERLAY_HEADROOM];
      var span = Math.abs(range[1] - floor) || 1;
      return {
        volume: prepareUnderlay(volume, floor, floor - 0.25 * span, floor + 0.01 * span),
        range: range,
        floor: floor,
        bounds: bounds
      };
    });
    pending.catch(function () { underlays.delete(assetId); });
    underlays.set(assetId, pending);
    return pending;
  }

  function backgroundLayer(underlay, ground) {
    var api = root.neuroimjs;
    var look = underlayLook(api, underlay, ground);
    var layer = new api.VolLayer(
      BACKGROUND_LAYER_ID,
      underlay.volume,
      look.colormap,
      underlay.range,
      look.threshold,
      1
    );
    // Templates often end inside tissue (neck, lower cerebellum); fade the
    // field-of-view edge into the stage rather than stopping at a hard line.
    if (typeof layer.setEdgeFade === "function") layer.setEdgeFade(3);
    // Clamped bicubic resampling: sharper anatomy than GPU bilinear
    // magnification, without ringing at the brain edge.
    if (typeof layer.setInterpolation === "function") layer.setInterpolation("cubic");
    return layer;
  }

  // A volume sharing `volume`'s space with new voxel data.
  function withData(volume, data) {
    var Volume = volume.constructor;
    try {
      var made = new Volume(volume.space, data);
      if (made.getData() === data) return made;
    } catch (error) {
      // Fall through to a prototype clone.
    }
    var clone = Object.create(Object.getPrototypeOf(volume));
    Object.assign(clone, volume);
    clone.data = data;
    return clone;
  }

  // The map as displayed at `threshold`: clusters below the extent -> NaN.
  function extentVolume(raw, map, threshold) {
    var displayApi = root.NeuroMosaicVolumeDisplay;
    var data = raw.getData();
    var excluded = displayApi.extentExcluded(
      data, raw.space.dim, threshold,
      map.display.min_cluster_size, map.display.connectivity
    );
    if (!excluded) return raw;
    var shown = data.slice();
    var any = false;
    for (var i = 0; i < excluded.length; i += 1) {
      if (excluded[i]) { shown[i] = NaN; any = true; }
    }
    return any ? withData(raw, shown) : raw;
  }

  function thresholdKey(threshold) {
    return Number(threshold[0]) + ":" + Number(threshold[1]);
  }

  // Re-apply the extent rule after the reader commits a new threshold.
  function applyExtent(state) {
    state.extentTimer = null;
    var layer = overlayVolLayer(state);
    var map = currentMap(state);
    if (!layer || !map || !state.rawOverlay || !state.viewer) return;
    var threshold = layer.getThreshold().map(Number);
    var key = thresholdKey(threshold);
    if (state.extentKey === key && layer.volume === state.displayVolume) return;
    var shown = extentVolume(state.rawOverlay, map, threshold);
    state.extentKey = key;
    state.displayVolume = shown;
    if (layer.volume !== shown) {
      state.viewer.updateLayerVolume(OVERLAY_LAYER_ID, shown, {
        range: layer.getRange().slice(),
        threshold: threshold,
        alpha: layer.opacity,
        colormap: layer.colorMap
      });
    }
    syncReportPalette(state, true);
    updateLegend(state);
    updateReadout(state);
  }

  function scheduleExtent(state) {
    var layer = overlayVolLayer(state);
    if (!layer) return;
    if (thresholdKey(layer.getThreshold()) === state.extentKey) return;
    if (state.extentTimer) clearTimeout(state.extentTimer);
    state.extentTimer = setTimeout(function () { applyExtent(state); }, 120);
  }

  function overlayLayer(state, raw, map) {
    var api = root.neuroimjs;
    var display = mapDisplay(map, raw);
    var volume = extentVolume(raw, map, display.threshold);
    state.rawOverlay = raw;
    state.displayVolume = volume;
    state.extentKey = thresholdKey(display.threshold);
    var layer = new api.VolLayer(
      OVERLAY_LAYER_ID,
      volume,
      display.colormap,
      display.range,
      display.threshold,
      display.alpha
    );
    // Statistical maps interpolate values before thresholding, so cluster
    // outlines follow the statistic rather than 2 mm voxel steps; colour never
    // blends across the threshold (the readout still reports the raw voxel).
    if (typeof layer.setInterpolation === "function") layer.setInterpolation("smooth");
    // A dark rim on the threshold contour keeps pale near-threshold colour
    // legible against bright white matter.
    if (typeof layer.setOutline === "function") layer.setOutline(0.3);
    // Outline only clusters that meet the report's minimum extent, so
    // isolated specks revealed at a lower threshold are not contoured.
    if (typeof layer.setMinOutlineClusterSize === "function") {
      layer.setMinOutlineClusterSize(Math.max(1, Number(map.display.min_cluster_size) || 10));
    }
    return layer;
  }

  function visibleControls(state, map) {
    var requested = state.scene.view.controls || [];
    var visible = [];
    if (requested.indexOf("threshold") >= 0) {
      visible.push("range");
      if (map.display.mode === "thresholded") visible.push("threshold");
    }
    if (requested.indexOf("palette") >= 0) visible.push("colormap");
    if (requested.indexOf("opacity") >= 0) visible.push("opacity");
    return visible;
  }

  function signed(text) {
    return String(text).replace(/^-/, MINUS);
  }

  function formatPeak(value, record) {
    var v = record && Number.isFinite(Number(record.peak_value)) ? record.peak_value : value;
    return root.NeuroMosaicVolumeDisplay.peakText(v, record && record.peak_digits, formatValue);
  }

  function formatValue(value) {
    var v = Number(value);
    var a = Math.abs(v);
    if (a === 0) return "0";
    if (a >= 1000) return signed(v.toFixed(0));
    if (a >= 100) return signed(v.toFixed(1));
    if (a >= 1) return signed(v.toFixed(2));
    return signed(v.toPrecision(3));
  }

  function currentMap(state) {
    return state.analysis.maps.find(function (candidate) {
      return candidate.map_id === state.currentMapId;
    });
  }

  function el(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text !== undefined) node.textContent = text;
    return node;
  }

  function updateReadout(state) {
    var map = currentMap(state);
    var value = state.viewer.getValue(OVERLAY_LAYER_ID);
    var coord = state.viewer.getWorldCoord();
    var readout = state.readout;
    readout.replaceChildren();
    var title = el("span", "nm-volume-readout-title", map.legend_title || map.label);
    title.title = map.legend_title || map.label;
    readout.appendChild(title);
    var coords = el("span", "nm-volume-readout-coords");
    ["x", "y", "z"].forEach(function (axis, index) {
      var pair = el("span", "nm-volume-readout-axis");
      pair.appendChild(el("span", "nm-volume-readout-key", axis));
      pair.appendChild(document.createTextNode(" " + signed(Number(coord[index]).toFixed(1))));
      coords.appendChild(pair);
      coords.appendChild(document.createTextNode(" "));
    });
    coords.appendChild(el("span", "nm-volume-readout-unit", "mm"));
    var vox = voxelOf(state, coord);
    if (vox) {
      coords.appendChild(document.createTextNode(" "));
      coords.appendChild(el("span", "nm-volume-readout-voxel", "voxel " + vox.join(" ")));
    }
    var line = el("span", "nm-volume-readout-value");
    var layer = overlayVolLayer(state);
    var status = root.NeuroMosaicVolumeDisplay.readoutState(
      value, rawValueAt(state, vox),
      layer && layer.getThreshold ? layer.getThreshold().map(Number) : null,
      map.display && map.display.min_cluster_size
    );
    var shown = status.kind === "shown";
    line.dataset.state = status.kind;
    var number = el(
      "strong", "nm-volume-readout-number",
      shown ? formatValue(status.value) : status.reason
    );
    if (!shown) number.classList.add("is-muted");
    line.appendChild(number);
    if (map.units && shown) {
      line.appendChild(document.createTextNode(" "));
      line.appendChild(el("span", "nm-volume-readout-units", map.units));
    }
    if (!shown && status.value !== null) {
      line.appendChild(document.createTextNode(" "));
      line.appendChild(el("span", "nm-volume-readout-hidden", "(value " + formatValue(status.value) + ")"));
    }
    line.appendChild(document.createTextNode(" "));
    state.clipChip = el("span", "nm-volume-readout-clip");
    state.clipChip.hidden = true;
    line.appendChild(state.clipChip);
    readout.appendChild(document.createTextNode(" "));
    readout.appendChild(line);
    readout.appendChild(document.createTextNode(" "));
    readout.appendChild(coords);
    updateLegendMarker(state, status.kind === "outside" ? null : status.value);
    updateCluster(state, coord);
    updateViewCoords(state, coord);
  }

  // The map's value at a voxel (before the extent rule); null if unknown.
  function rawValueAt(state, voxel) {
    var raw = state.rawOverlay;
    var data = raw && typeof raw.getData === "function" ? raw.getData() : null;
    var dim = raw && raw.space && raw.space.dim;
    if (!data || !dim || !voxel) return null;
    var i = voxel[0], j = voxel[1], k = voxel[2];
    if (i < 0 || j < 0 || k < 0 || i >= dim[0] || j >= dim[1] || k >= dim[2]) return NaN;
    return Number(data[(k * dim[1] + j) * dim[0] + i]);
  }

  // The map voxel (overlay grid) at a world coordinate. The underlay may be
  // on a finer grid, so never use layer 0 here.
  function voxelOf(state, coord) {
    var layer = overlayVolLayer(state);
    if (!layer || !layer.space || typeof layer.space.coordToGrid !== "function") return null;
    var g = layer.space.coordToGrid(coord);
    if (!g || g.some(function (v) { return !Number.isFinite(v); })) return null;
    return g.map(function (v) { return Math.round(v); });
  }

  var VIEW_AXIS = { sagittal: 0, coronal: 1, axial: 2 };

  // Neighbour offsets for the report's cluster connectivity (6, 18 or 26).
  function neighbourOffsets(connectivity) {
    return root.NeuroMosaicVolumeDisplay.neighbourOffsets(connectivity);
  }

  // The suprathreshold cluster under the cursor: voxels of the same sign
  // beyond the current threshold, joined with the connectivity the report's
  // cluster table uses (18-connected unless the manifest says otherwise).
  function clusterAt(state, voxel) {
    var layer = overlayVolLayer(state);
    if (!layer || !voxel) return null;
    var vol = layer.volume;
    var data = typeof vol.getData === "function" ? vol.getData() : null;
    var dim = vol.space && vol.space.dim;
    if (!data || !dim) return null;
    var nx = dim[0], ny = dim[1], nz = dim[2];
    var i0 = voxel[0], j0 = voxel[1], k0 = voxel[2];
    if (i0 < 0 || j0 < 0 || k0 < 0 || i0 >= nx || j0 >= ny || k0 >= nz) return null;
    var thr = (layer.getThreshold ? layer.getThreshold() : [0, 0]).map(Number);
    var seed = (k0 * ny + j0) * nx + i0;
    var v0 = data[seed];
    var positive = v0 >= thr[1] && v0 > 0;
    var negative = v0 <= thr[0] && v0 < 0;
    if (!(thr[0] < thr[1]) || (!positive && !negative)) return null;
    var map = currentMap(state);
    var connectivity = (map && map.display && map.display.connectivity) || "18-connect";
    var offsets = neighbourOffsets(connectivity);
    var key = thr[0] + ":" + thr[1] + ":" + connectivity;
    var memo = state.clusterMemo;
    if (memo && memo.volume === vol && memo.key === key && memo.members[seed]) return memo.result;
    var inside = positive ?
      function (v) { return v >= thr[1] && v > 0; } :
      function (v) { return v <= thr[0] && v < 0; };
    var members = new Uint8Array(data.length);
    var stack = [seed];
    members[seed] = 1;
    var count = 0, peak = v0, peakIndex = seed;
    while (stack.length) {
      var idx = stack.pop();
      count += 1;
      var v = data[idx];
      if (Math.abs(v) > Math.abs(peak)) { peak = v; peakIndex = idx; }
      var i = idx % nx, j = Math.floor(idx / nx) % ny, k = Math.floor(idx / (nx * ny));
      for (var o = 0; o < offsets.length; o += 1) {
        var ii = i + offsets[o][0], jj = j + offsets[o][1], kk = k + offsets[o][2];
        if (ii < 0 || ii >= nx || jj < 0 || jj >= ny || kk < 0 || kk >= nz) continue;
        var n = (kk * ny + jj) * nx + ii;
        if (members[n] || !inside(data[n])) continue;
        members[n] = 1;
        stack.push(n);
      }
    }
    var sp = vol.space.spacing || [1, 1, 1];
    var pi = peakIndex % nx, pj = Math.floor(peakIndex / nx) % ny, pk = Math.floor(peakIndex / (nx * ny));
    var result = {
      voxels: count,
      mm3: count * Math.abs(sp[0] * sp[1] * sp[2]),
      peak: peak,
      peakWorld: vol.space.gridToCoord([pi, pj, pk])
    };
    state.clusterMemo = { volume: vol, key: key, members: members, result: result };
    return result;
  }

  // The report cluster whose peak is the peak of the cluster under the cursor
  // (at the report threshold the two clusters are identical).
  function clusterRecord(state, cluster) {
    var map = currentMap(state);
    var records = map && Array.isArray(map.clusters) ? map.clusters : [];
    var peak = voxelOf(state, cluster.peakWorld);
    if (!peak || !records.length) return null;
    return records.find(function (record) {
      return record.voxel[0] === peak[0] && record.voxel[1] === peak[1] &&
        record.voxel[2] === peak[2];
    }) || null;
  }

  function atReportThreshold(state) {
    var layer = overlayVolLayer(state);
    var defaults = state.reportDefaults && state.reportDefaults.get(state.currentMapId);
    if (!layer || !defaults) return true;
    return samePair(layer.getThreshold().map(Number), defaults.threshold.map(Number));
  }

  function updateCluster(state, coord) {
    var line = state.clusterLine;
    if (!line) return;
    line.replaceChildren();
    var c = clusterAt(state, voxelOf(state, coord));
    if (!c) {
      line.appendChild(el("span", "nm-volume-cluster-empty", "No suprathreshold cluster at the cursor"));
      return;
    }
    var fmtInt = function (n) { return Math.round(n).toLocaleString("en-US"); };
    line.appendChild(el("span", "nm-volume-cluster-key", "Cluster"));
    line.appendChild(document.createTextNode(" "));
    var record = clusterRecord(state, c);
    if (record) {
      var name = el("span", "nm-volume-cluster-name");
      if (!atReportThreshold(state)) name.appendChild(el("span", "nm-volume-cluster-contains", "contains "));
      name.appendChild(el("strong", "nm-volume-cluster-id", record.id));
      var region = record.region + (record.hemisphere ? " (" + record.hemisphere + ")" : "");
      name.appendChild(document.createTextNode(" \u00b7 " + region));
      name.title = record.id + " \u00b7 " + region;
      line.appendChild(name);
      line.appendChild(document.createTextNode(" "));
    }
    line.appendChild(el("span", "nm-volume-cluster-size",
      fmtInt(c.voxels) + " voxels \u00b7 " + fmtInt(c.mm3) + " mm\u00b3"));
    line.appendChild(document.createTextNode(" "));
    var atPeak = voxelOf(state, coord).join() === voxelOf(state, c.peakWorld).join();
    var peakText = formatPeak(c.peak, record);
    var where = c.peakWorld.map(function (v) { return signed(Number(v).toFixed(1)); }).join(", ");
    if (atPeak) {
      line.appendChild(el("span", "nm-volume-cluster-atpeak",
        "Peak " + peakText + " at (" + where + ")"));
      return;
    }
    var go = el("button", "nm-volume-cluster-peak", "Go to peak " + peakText);
    go.type = "button";
    go.title = "Peak at (" + where + ") mm";
    go.setAttribute("aria-label", "Go to cluster peak " + peakText + " at " + where + " mm");
    go.addEventListener("click", function () {
      state.viewer.setWorldCoord(c.peakWorld);
      updateReadout(state);
    });
    line.appendChild(go);
  }

  // One view at a time on narrow screens: a segmented control above the stage,
  // shown only while the viewer reports a stacked layout.
  function buildViewSwitch(state) {
    if (typeof state.viewer.setStackedView !== "function") return;
    var group = el("div", "nm-volume-views");
    group.setAttribute("role", "group");
    group.setAttribute("aria-label", "Slice view");
    ["coronal", "sagittal", "axial"].forEach(function (view, i) {
      var b = el("button", "nm-volume-view-button", view.charAt(0).toUpperCase() + view.slice(1));
      b.type = "button";
      b.dataset.nmView = view;
      b.setAttribute("aria-pressed", i === 0 ? "true" : "false");
      b.addEventListener("click", function () {
        group.querySelectorAll("button").forEach(function (o) {
          o.setAttribute("aria-pressed", String(o === b));
        });
        state.viewer.setStackedView(view);
      });
      group.appendChild(b);
    });
    state.shell.insertBefore(group, state.viewerElement);
    state.viewSwitch = group;
    var sync = function () { group.hidden = !state.viewer.isStacked(); };
    sync();
    if (typeof ResizeObserver !== "undefined") {
      state.viewSwitchObserver = new ResizeObserver(sync);
      state.viewSwitchObserver.observe(state.viewerElement);
    }
  }

  function buildViewCoords(state) {
    state.viewCoords = {};
    Object.keys(VIEW_AXIS).forEach(function (view) {
      var cell = state.viewerElement.querySelector('[data-nij-view="' + view + '"]');
      if (!cell) return;
      var tag = el("span", "nm-volume-view-coord");
      tag.setAttribute("aria-hidden", "true");
      tag.appendChild(el("span", "nm-volume-view-name", view.charAt(0).toUpperCase() + view.slice(1)));
      var value = el("span", "nm-volume-view-value");
      tag.appendChild(value);
      cell.appendChild(tag);
      state.viewCoords[view] = value;
    });
  }

  function updateViewCoords(state, coord) {
    if (!state.viewCoords) return;
    Object.keys(state.viewCoords).forEach(function (view) {
      var axis = VIEW_AXIS[view];
      var text = ["x", "y", "z"][axis] + " " + signed(Number(coord[axis]).toFixed(1));
      state.viewCoords[view].textContent = text;
      var cell = state.viewerElement.querySelector('[data-nij-view="' + view + '"]');
      if (cell) {
        cell.setAttribute("aria-label", view.charAt(0).toUpperCase() + view.slice(1) +
          " slice, " + text + " mm. Up and Down arrows step through the slice.");
      }
    });
  }

  // The reader's last key or press: a plain wheel steps a focused pane only
  // when the reader put focus there (Tab, a press or a key in the pane),
  // never when a script did (a peak jump).
  var lastIntent = { tab: false, target: null };
  ["keydown", "pointerdown"].forEach(function (type) {
    document.addEventListener(type, function (event) {
      lastIntent = { tab: event.key === "Tab", target: event.target };
    }, true);
  });

  // Keyboard stepping on a focused slice pane: Up/Right (and Page Up) move
  // the slice forward along that pane's through-plane axis, Down/Left back.
  // Bound in the capture phase so the runtime's hover-based handler, which
  // steps whichever view the pointer last entered, never sees these keys.
  function bindSliceKeys(state) {
    var keys = {
      ArrowUp: 1, ArrowRight: 1, PageUp: 5,
      ArrowDown: -1, ArrowLeft: -1, PageDown: -5
    };
    Object.keys(VIEW_AXIS).forEach(function (view) {
      var cell = state.viewerElement.querySelector('[data-nij-view="' + view + '"]');
      if (!cell) return;
      cell.setAttribute("role", "group");
      if (!cell.hasAttribute("tabindex")) cell.tabIndex = 0;
    });
    var step = function (cell, steps) {
      var axis = VIEW_AXIS[cell.dataset.nijView];
      if (axis === undefined) return false;
      // One map voxel per step, whatever the underlay's resolution.
      var layer = overlayVolLayer(state);
      var spacing = layer && layer.space && layer.space.spacing ?
        Math.abs(Number(layer.space.spacing[axis])) || 1 : 1;
      var coord = state.viewer.getWorldCoord().slice();
      coord[axis] = Number(coord[axis]) + steps * spacing;
      state.viewer.setWorldCoord(coord);
      updateReadout(state);
      return true;
    };
    state.onSliceKey = function (event) {
      var steps = keys[event.key];
      if (!steps || event.altKey || event.ctrlKey || event.metaKey) return;
      var cell = event.target && event.target.closest ?
        event.target.closest("[data-nij-view]") : null;
      if (!cell || !state.viewerElement.contains(cell)) return;
      if (VIEW_AXIS[cell.dataset.nijView] === undefined) return;
      event.preventDefault();
      event.stopPropagation();
      step(cell, steps);
    };
    // The wheel scrolls the page; it steps a pane the reader focused, or
    // with Alt/Shift.
    state.onSliceWheel = function (event) {
      var cell = event.target && event.target.closest ?
        event.target.closest("[data-nij-view]") : null;
      if (!cell || !state.viewerElement.contains(cell)) return;
      var action = root.NeuroMosaicVolumeDisplay.wheelAction({
        focused: !!(document.activeElement && cell.contains(document.activeElement)),
        byReader: lastIntent.tab || !!(lastIntent.target && cell.contains(lastIntent.target)),
        ctrlKey: event.ctrlKey, metaKey: event.metaKey,
        altKey: event.altKey, shiftKey: event.shiftKey,
        deltaX: event.deltaX, deltaY: event.deltaY
      });
      if (action === "runtime") return;
      // Keep it from the runtime's canvas listener (it would cancel it).
      event.stopPropagation();
      if (action === "page") return;
      event.preventDefault();
      step(cell, action);
    };
    state.viewerElement.addEventListener("wheel", state.onSliceWheel, { capture: true, passive: false });
    state.viewerElement.addEventListener("keydown", state.onSliceKey, true);
  }

  // The two-thumb slider's inputs are 0 px tall: give them the hit height and
  // every panel slider a focus ring.
  var SLIDER_CSS =
    ".range-slider input[type='range'] { height: var(--rs-hit); }" +
    ".range-slider input[type='range']::-webkit-slider-runnable-track { height: var(--rs-hit); }" +
    ".range-slider input[type='range']::-webkit-slider-thumb { margin-top: 0; }" +
    ".range-slider input[type='range']:focus-visible {" +
    " outline: 2px solid var(--rs-fill) !important; outline-offset: 1px !important; border-radius: 6px; }";
  var PANEL_CSS =
    "input[type='range']:focus-visible { outline: 2px solid var(--nm-vv-accent, #2f6f73) !important;" +
    " outline-offset: 2px !important; border-radius: 4px; }";
  function styleSlider(shadowRoot, panel) {
    if (!shadowRoot || shadowRoot.querySelector("style[data-nm-slider]")) return;
    var style = document.createElement("style");
    style.setAttribute("data-nm-slider", "");
    style.textContent = panel ? PANEL_CSS : SLIDER_CSS;
    shadowRoot.appendChild(style);
  }

  // Accessible names for the control panel's two-thumb sliders, which the
  // runtime renders without labels inside nested shadow roots.
  function labelSliders(state) {
    var panel = state.controls;
    var shadow = panel && panel.shadowRoot;
    if (!shadow) return;
    styleSlider(shadow, true);
    shadow.querySelectorAll("range-slider").forEach(function (slider) {
      var section = slider.closest(".section");
      var labelNode = section && section.querySelector(".label");
      var name = labelNode ? labelNode.textContent.trim() : "Range";
      var apply = function () {
        var inner = slider.shadowRoot;
        if (!inner) return;
        styleSlider(inner);
        var inputs = inner.querySelectorAll('input[type="range"]');
        // The panel's own state is authoritative: the inner inputs may not
        // have received their values yet when this runs.
        var pair = name === "Threshold" ? panel.threshold : (name === "Range" ? panel.range : null);
        inputs.forEach(function (input, index) {
          var which = index === 0 ? "low" : "high";
          var current = pair && Number.isFinite(Number(pair[index])) ? pair[index] : input.value;
          if (state.sliderStep && Number(slider.step) !== state.sliderStep) slider.step = state.sliderStep;
          input.setAttribute("aria-label", name + " " + which);
          input.setAttribute("aria-valuetext", name + " " + which + " " + niceTick(current));
          if (!input.__nmLabelled) {
            input.__nmLabelled = true;
            input.addEventListener("input", function () {
              input.setAttribute("aria-valuetext", name + " " + which + " " + niceTick(input.value));
            });
          }
        });
      };
      if (slider.updateComplete && typeof slider.updateComplete.then === "function") {
        slider.updateComplete.then(apply);
      } else {
        apply();
      }
    });
  }

  function overlayVolLayer(state) {
    var stack = state.stack;
    if (!stack) return null;
    if (typeof stack.getLayerById === "function") return stack.getLayerById(OVERLAY_LAYER_ID);
    for (var i = 0; i < stack.length; i += 1) {
      if (stack.getLayer(i).id === OVERLAY_LAYER_ID) return stack.getLayer(i);
    }
    return null;
  }

  // Scale values use the report's rounding (3 significant digits: 22.2539
  // reads 22.3 as in captions); cursor values keep two decimals.
  function niceTick(value) {
    return root.NeuroMosaicVolumeDisplay.reportNumber(value);
  }

  function buildLegend(state) {
    var cell = state.viewer.getLegendElement && state.viewer.getLegendElement();
    if (!cell) return;
    var legend = el("div", "nm-volume-legend");
    var head = el("div", "nm-volume-legend-head");
    state.sidebarAnchor = state.readout.nextSibling;
    if (state.resetPosition) {
      var reset = state.resetPosition;
      state.resetPositionMarkup = reset.innerHTML;
      reset.setAttribute("aria-label", "Reset to report position");
      reset.title = "Reset to report position";
      reset.innerHTML = '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" ' +
        'stroke-width="1.5" aria-hidden="true"><circle cx="8" cy="8" r="4.5"/>' +
        '<path d="M8 1v3M8 12v3M1 8h3M12 8h3"/></svg>' +
        '<span class="nm-volume-reset-label">Report position</span>';
      reset.classList.add("nm-volume-reset-position--stage");
      head.appendChild(reset);
    }
    var save = el("button", "nm-volume-save");
    save.type = "button";
    save.title = "Save the three slices and the colour key as a PNG";
    save.setAttribute("aria-label", "Save view as PNG");
    save.innerHTML = '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" ' +
      'stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
      '<path d="M8 2.5v7M5 7l3 3 3-3M3 13h10"/></svg><span class="nm-volume-save-label">Save PNG</span>';
    save.addEventListener("click", function () { exportPNG(state); });
    head.appendChild(save);
    state.saveButton = save;
    legend.appendChild(state.readout);
    state.clusterLine = el("p", "nm-volume-cluster");
    legend.appendChild(state.clusterLine);
    legend.appendChild(head);
    var scale = el("div", "nm-volume-scale");
    scale.setAttribute("aria-hidden", "true");
    state.legendHist = el("canvas", "nm-volume-scale-hist");
    scale.appendChild(state.legendHist);
    state.legendBar = el("div", "nm-volume-scale-bar");
    state.legendMarker = el("span", "nm-volume-scale-marker");
    state.legendBar.appendChild(state.legendMarker);
    state.legendBar.appendChild(el("span", "nm-volume-scale-cap nm-volume-scale-cap--low"));
    state.legendBar.appendChild(el("span", "nm-volume-scale-cap nm-volume-scale-cap--high"));
    state.legendTicks = el("div", "nm-volume-scale-ticks");
    state.legendNote = el("div", "nm-volume-scale-note");
    scale.appendChild(state.legendBar);
    scale.appendChild(state.legendTicks);
    scale.appendChild(state.legendNote);
    legend.appendChild(scale);
    cell.appendChild(legend);
    var coarse = typeof matchMedia === "function" && matchMedia("(pointer: coarse)").matches;
    var sidebar = state.host.querySelector(".nm-volume-sidebar");
    if (sidebar) buildGroundSwitch(state, sidebar);
    if (sidebar && !sidebar.querySelector(".nm-volume-hint")) {
      sidebar.appendChild(el("p", "nm-volume-hint", coarse ?
        "Tap or drag on a slice to move the cursor. L is left." :
        "Click or drag on a slice to move the cursor. With a slice focused, \u2191/\u2193 (or Page Up/Down for 5) and the scroll wheel step through it; elsewhere the wheel scrolls the page (Alt or Shift + wheel steps the slice under the pointer). L is left."));
    }
    state.legend = legend;
  }

  // "Ground: Light | Dark" in the sidebar (kept across rebuilds).
  function buildGroundSwitch(state, sidebar) {
    var group = sidebar.querySelector(".nm-volume-grounds");
    if (!group) {
      group = el("div", "nm-volume-grounds");
      group.setAttribute("role", "group");
      group.setAttribute("aria-label", "Slice ground");
      group.appendChild(el("span", "nm-volume-grounds-label", "Ground"));
      ["light", "dark"].forEach(function (ground) {
        var button = el("button", "nm-volume-ground", ground === "light" ? "Light" : "Dark");
        button.type = "button";
        button.dataset.ground = ground;
        button.title = ground === "light" ?
          "Slices on the figure ground, as in the static montage" :
          "Slices on a dark instrument stage";
        button.addEventListener("click", function () {
          if (button.getAttribute("aria-pressed") === "true") return;
          switchGround(state.host, ground);
        });
        group.appendChild(button);
      });
      var controls = sidebar.querySelector("[data-nm-volume-controls]");
      sidebar.insertBefore(group, controls ? controls.nextSibling : null);
    }
    group.querySelectorAll(".nm-volume-ground").forEach(function (button) {
      button.setAttribute("aria-pressed", button.dataset.ground === state.ground ? "true" : "false");
    });
  }

  function updateLegend(state) {
    if (!state.legend) return;
    var layer = overlayVolLayer(state);
    var map = currentMap(state);
    if (!layer || !map) return;
    var range = layer.getRange().map(Number);
    var threshold = (layer.getThreshold ? layer.getThreshold() : [0, 0]).map(Number);
    var lo = range[0], hi = range[1], span = hi - lo || 1;
    var pct = function (v) { return Math.max(0, Math.min(100, ((v - lo) / span) * 100)); };
    var stops = [];
    var n = 24;
    var values = [];
    for (var i = 0; i <= n; i += 1) values.push(lo + (span * i) / n);
    var colors = layer.colorMap.getColors(values);
    colors.forEach(function (c, i) {
      stops.push("rgb(" + Math.round(c[0] * 255) + " " + Math.round(c[1] * 255) + " " +
        Math.round(c[2] * 255) + ") " + ((i / n) * 100).toFixed(2) + "%");
    });
    var bar = state.legendBar;
    bar.style.setProperty("--nm-scale-gradient", "linear-gradient(to right, " + stops.join(", ") + ")");
    var extent = typeof layer.getVolumeRange === "function" ? layer.getVolumeRange().map(Number) : range;
    var rgb = function (c) {
      return "rgb(" + Math.round(c[0] * 255) + " " + Math.round(c[1] * 255) + " " + Math.round(c[2] * 255) + ")";
    };
    bar.dataset.capLow = extent[0] < lo ? "true" : "false";
    bar.dataset.capHigh = extent[1] > hi ? "true" : "false";
    bar.style.setProperty("--nm-scale-cap-low", rgb(colors[0]));
    bar.style.setProperty("--nm-scale-cap-high", rgb(colors[colors.length - 1]));
    var thresholded = threshold[0] < threshold[1];
    bar.dataset.thresholded = thresholded ? "true" : "false";
    if (thresholded) {
      bar.style.setProperty("--nm-scale-cut-lo", pct(threshold[0]) + "%");
      bar.style.setProperty("--nm-scale-cut-hi", pct(threshold[1]) + "%");
    }
    bar.querySelectorAll(".nm-volume-scale-cut").forEach(function (n) { n.remove(); });
    if (thresholded) {
      [threshold[0], threshold[1]].forEach(function (t) {
        var p = pct(t);
        if (p <= 0 || p >= 100) return;
        var mark = el("span", "nm-volume-scale-cut");
        mark.style.left = p + "%";
        bar.appendChild(mark);
      });
    }
    var ticks = [lo, hi];
    if (thresholded) {
      if (threshold[0] > lo) ticks.push(threshold[0]);
      if (threshold[1] < hi) ticks.push(threshold[1]);
    }
    ticks.sort(function (a, b) { return a - b; });
    state.legendTicks.replaceChildren();
    ticks.forEach(function (t) {
      var tick = el("span", "nm-volume-scale-tick", niceTick(t));
      var p = pct(t);
      tick.style.left = p + "%";
      tick.dataset.edge = p <= 0 ? "start" : p >= 100 ? "end" : "mid";
      state.legendTicks.appendChild(tick);
    });
    hideCrowdedTicks(state.legendTicks);
    state.legendHistState = { layer: layer, lo: lo, hi: hi, threshold: threshold, thresholded: thresholded, colors: colors };
    drawHistogram(state);
    state.legendBar.setAttribute("aria-label", thresholded ?
      "Values between " + niceTick(threshold[0]) + " and " + niceTick(threshold[1]) + " are not drawn." :
      "");
    state.legendRange = [lo, hi];
    updateLegendMarker(state, state.viewer.getValue(OVERLAY_LAYER_ID));
  }

  // Log-count histogram of the map's non-zero values over the display range,
  // on the colour bar's x-scale. Bins inside the hidden band are drawn dim.
  var HIST_BINS = 72;

  function histogramCounts(state, layer, lo, hi) {
    // Keyed on the volume object itself: during a map switch the layer holds
    // the new volume before currentMapId catches up.
    var key = lo + ":" + hi;
    if (state.histCache && state.histCache.volume === layer.volume && state.histCache.key === key) {
      return state.histCache.counts;
    }
    var counts = new Float64Array(HIST_BINS);
    var data = layer.volume && typeof layer.volume.getData === "function" ? layer.volume.getData() : null;
    if (data) {
      var span = (hi - lo) || 1;
      for (var i = 0; i < data.length; i += 1) {
        var v = data[i];
        // Out-of-range values are represented by the colour bar's end caps.
        if (v === 0 || !(v >= lo && v <= hi)) continue;
        var b = Math.floor(((v - lo) / span) * HIST_BINS);
        counts[b >= HIST_BINS ? HIST_BINS - 1 : b] += 1;
      }
    }
    state.histCache = { volume: layer.volume, key: key, counts: counts };
    return counts;
  }

  function drawHistogram(state) {
    var h = state.legendHistState;
    var canvas = state.legendHist;
    if (!h || !canvas) return;
    var cssW = canvas.clientWidth, cssH = canvas.clientHeight;
    if (!cssW || !cssH) return;
    var dpr = window.devicePixelRatio || 1;
    canvas.width = Math.round(cssW * dpr);
    canvas.height = Math.round(cssH * dpr);
    var ctx = canvas.getContext("2d");
    if (!ctx) return;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, cssW, cssH);
    var histInk = (GROUNDS[state.ground] || GROUNDS.dark).hist;
    var counts = histogramCounts(state, h.layer, h.lo, h.hi);
    var peak = 0;
    for (var i = 0; i < counts.length; i += 1) peak = Math.max(peak, Math.log1p(counts[i]));
    if (!peak) return;
    var bw = cssW / HIST_BINS;
    for (var b = 0; b < HIST_BINS; b += 1) {
      if (!counts[b]) continue;
      var centre = h.lo + ((b + 0.5) / HIST_BINS) * (h.hi - h.lo);
      var hidden = h.thresholded && centre > h.threshold[0] && centre < h.threshold[1];
      var c = h.colors[Math.round(((b + 0.5) / HIST_BINS) * (h.colors.length - 1))];
      ctx.fillStyle = hidden ? histInk.hidden :
        "rgba(" + Math.round(c[0] * 255) + "," + Math.round(c[1] * 255) + "," + Math.round(c[2] * 255) + ",0.85)";
      var bh = Math.max(1, (Math.log1p(counts[b]) / peak) * (cssH - 1));
      ctx.fillRect(b * bw + 0.5, cssH - bh, Math.max(1, bw - 1), bh);
    }
    ctx.fillStyle = histInk.base;
    ctx.fillRect(0, cssH - 1, cssW, 1);
    // Cursor value, on the same x-scale as the key (pinned to the end if clipped).
    var v = state.viewer.getValue(OVERLAY_LAYER_ID);
    // Cursor value on the scale: a guide line and a caret pointing at the key.
    // Off-scale values are marked by the key's outlined end cap instead.
    if (v !== null && Number.isFinite(Number(v)) && Number(v) >= h.lo && Number(v) <= h.hi) {
      var t = (Number(v) - h.lo) / ((h.hi - h.lo) || 1);
      var x = Math.round(t * (cssW - 1)) + 0.5;
      ctx.fillStyle = histInk.guide;
      ctx.fillRect(x - 0.5, 0, 1, cssH - 6);
      // Caret sits on the baseline, pointing down at the key.
      ctx.fillStyle = histInk.caret;
      ctx.beginPath();
      ctx.moveTo(x - 4.5, cssH - 6);
      ctx.lineTo(x + 4.5, cssH - 6);
      ctx.lineTo(x, cssH);
      ctx.closePath();
      ctx.fill();
    }
  }

  // Save view (PNG): the three slices as shown, with a title line and the
  // colour key on a light ground (whatever the page theme), for slides and
  // lab meetings. The slices keep the ground the reader chose.
  function paneImages(state) {
    var cells = Array.prototype.slice.call(state.viewerElement.querySelectorAll("[data-nij-view]"));
    var order = ["sagittal", "coronal", "axial"];
    cells.sort(function (a, b) { return order.indexOf(a.dataset.nijView) - order.indexOf(b.dataset.nijView); });
    return cells.map(function (cell) {
      var view = cell.dataset.nijView;
      var canvas = cell.querySelector("canvas");
      if (!canvas || !canvas.width || !canvas.height) return null;
      // Draw a fresh frame so the drawing buffer is current when copied.
      try {
        var app = state.viewer.viewer.getSliceViewer(view).view.app;
        app.renderer.render(app.stage);
      } catch (error) { /* fall back to the last presented frame */ }
      var copy = document.createElement("canvas");
      copy.width = canvas.width;
      copy.height = canvas.height;
      copy.getContext("2d").drawImage(canvas, 0, 0);
      var axis = VIEW_AXIS[view];
      var coord = state.viewer.getWorldCoord();
      return {
        image: copy,
        label: view.charAt(0).toUpperCase() + view.slice(1) + "  " +
          ["x", "y", "z"][axis] + " = " + signed(String(Math.round(Number(coord[axis])))) + " mm"
      };
    }).filter(Boolean);
  }

  // "story-gt-math_slices.png": the report's file stem for the map (the same
  // slug as its figure downloads); other maps of a multi-map analysis keep
  // their ids.
  function exportName(state) {
    var article = state.host.closest && state.host.closest("[data-nm-file-stem]");
    var stem = article && article.getAttribute("data-nm-file-stem");
    if (!stem || state.currentMapId !== state.analysis.primary_map_id) {
      stem = [state.analysis.analysis_id, state.currentMapId].filter(Boolean).join("_");
    }
    return (stem + "_slices").replace(/[^A-Za-z0-9_.-]+/g, "-") + ".png";
  }

  function exportPNG(state) {
    var map = currentMap(state);
    var h = state.legendHistState;
    if (!map || !h) return;
    var panes = paneImages(state);
    if (!panes.length) return;
    var S = 2, PANE = 300, GAP = 12, PAD = 24;
    var widths = panes.map(function (p) { return Math.round(PANE * p.image.width / p.image.height); });
    var width = PAD * 2 + widths.reduce(function (a, b) { return a + b; }, 0) + GAP * (panes.length - 1);
    var height = PAD + 26 + PANE + 26 + 70 + PAD;
    var out = document.createElement("canvas");
    out.width = width * S;
    out.height = height * S;
    var ctx = out.getContext("2d");
    ctx.scale(S, S);
    var font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif";
    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, width, height);
    ctx.fillStyle = "#16191b";
    ctx.font = "600 15px " + font;
    ctx.textBaseline = "alphabetic";
    var coord = state.viewer.getWorldCoord().map(function (v) { return signed(String(Math.round(Number(v)))); });
    ctx.fillText((map.label || map.selector_label || "") + "  \u00b7  cursor (" + coord.join(", ") + ") mm", PAD, PAD + 14);
    var x = PAD, top = PAD + 26;
    ctx.font = "12px " + font;
    panes.forEach(function (pane, i) {
      ctx.drawImage(pane.image, x, top, widths[i], PANE);
      ctx.fillStyle = "#4a5256";
      ctx.fillText(pane.label, x, top + PANE + 17);
      x += widths[i] + GAP;
    });
    // Colour key: the viewer's own scale, threshold band hatched as on the page.
    var keyTop = top + PANE + 40, keyH = 12, CAP = 9;
    var keyW = Math.min(360, width - 2 * PAD - 2 * CAP);
    var KX = PAD + CAP;
    ctx.fillStyle = "#16191b";
    ctx.font = "600 12px " + font;
    ctx.fillText(map.legend_title || map.units || "Value", PAD, keyTop - 6);
    var values = [];
    for (var q = 0; q < keyW; q += 1) values.push(h.lo + ((q + 0.5) / keyW) * (h.hi - h.lo));
    var keyColors = h.layer && h.layer.colorMap && typeof h.layer.colorMap.getColors === "function" ?
      h.layer.colorMap.getColors(values) : null;
    var n = h.colors.length;
    for (var k = 0; k < keyW; k += 1) {
      var value = values[k];
      var hidden = h.thresholded && value > h.threshold[0] && value < h.threshold[1];
      var c = keyColors ? keyColors[k] : h.colors[Math.min(n - 1, Math.floor((k / keyW) * n))];
      ctx.fillStyle = hidden ? (((k % 6) < 2) ? "#c4c8c5" : "#efefeb") :
        "rgb(" + Math.round(c[0] * 255) + "," + Math.round(c[1] * 255) + "," + Math.round(c[2] * 255) + ")";
      ctx.fillRect(KX + k, keyTop, 1, keyH);
    }
    ctx.strokeStyle = "#8a9296";
    ctx.lineWidth = 0.5;
    ctx.strokeRect(KX, keyTop, keyW, keyH);
    // Cap triangles, as on the page: data beyond the colour scale's ends
    // take the end colour.
    var extentRange = h.layer && typeof h.layer.getVolumeRange === "function" ?
      h.layer.getVolumeRange().map(Number) : null;
    var endColor = function (end) {
      var c = keyColors ? keyColors[end ? keyW - 1 : 0] : h.colors[end ? n - 1 : 0];
      return "rgb(" + Math.round(c[0] * 255) + "," + Math.round(c[1] * 255) + "," + Math.round(c[2] * 255) + ")";
    };
    [[0, extentRange && extentRange[0] < h.lo], [1, extentRange && extentRange[1] > h.hi]].forEach(function (cap) {
      if (!cap[1]) return;
      var edge = cap[0] ? KX + keyW : KX;
      var tip = cap[0] ? edge + CAP : edge - CAP;
      ctx.beginPath();
      ctx.moveTo(edge, keyTop);
      ctx.lineTo(tip, keyTop + keyH / 2);
      ctx.lineTo(edge, keyTop + keyH);
      ctx.closePath();
      ctx.fillStyle = endColor(cap[0]);
      ctx.fill();
      ctx.stroke();
    });
    state.lastExportCaps = [Boolean(extentRange && extentRange[0] < h.lo), Boolean(extentRange && extentRange[1] > h.hi)];
    ctx.fillStyle = "#4a5256";
    ctx.font = "11px " + font;
    var ticks = [h.lo, h.hi];
    if (h.thresholded) {
      if (h.threshold[0] > h.lo) ticks.push(h.threshold[0]);
      if (h.threshold[1] < h.hi) ticks.push(h.threshold[1]);
    }
    ticks.forEach(function (t) {
      var tx = KX + ((t - h.lo) / ((h.hi - h.lo) || 1)) * keyW;
      var label = niceTick(t);
      var w = ctx.measureText(label).width;
      ctx.fillText(label, Math.max(PAD, Math.min(KX + keyW - w, tx - w / 2)), keyTop + keyH + 14);
    });
    var sym = /^\s*z/i.test(map.legend_title || "") ? "z" : (map.units || "value");
    var extent = Number(map.display && map.display.min_cluster_size);
    var note = root.NeuroMosaicVolumeDisplay.exportNote({
      modified: state.host.dataset.nmVolumeModified === "true",
      threshold: h.thresholded ? h.threshold : null,
      extent: extent,
      symbol: sym,
      lo: h.lo,
      hi: h.hi
    });
    state.lastExportNote = note;
    // Wrap the note to the image width (at most two lines).
    var words = note.split(" "), lines = [""];
    words.forEach(function (word) {
      var next = lines[lines.length - 1] ? lines[lines.length - 1] + " " + word : word;
      if (ctx.measureText(next).width > width - 2 * PAD && lines[lines.length - 1]) lines.push(word);
      else lines[lines.length - 1] = next;
    });
    lines.slice(0, 2).forEach(function (line, index) {
      ctx.fillText(line, PAD, keyTop + keyH + 34 + index * 15);
    });
    var name = exportName(state);
    var finish = function (url) {
      var anchor = document.createElement("a");
      anchor.href = url;
      anchor.download = name;
      document.body.appendChild(anchor);
      anchor.click();
      anchor.remove();
      var live = state.host.querySelector("[data-nm-volume-live]");
      if (live) live.textContent = "Saved " + name + ".";
      if (state.saveButton) {
        state.saveButton.dataset.done = "true";
        setTimeout(function () { if (state.saveButton) delete state.saveButton.dataset.done; }, 1600);
      }
    };
    state.lastExport = { name: name, width: out.width, height: out.height };
    if (typeof out.toBlob === "function") {
      out.toBlob(function (blob) {
        if (!blob) { finish(out.toDataURL("image/png")); return; }
        var url = URL.createObjectURL(blob);
        finish(url);
        setTimeout(function () { URL.revokeObjectURL(url); }, 4000);
      }, "image/png");
    } else {
      finish(out.toDataURL("image/png"));
    }
  }

  // Drop tick labels that would touch a neighbour; range ends always win.
  function hideCrowdedTicks(container) {
    var ticks = Array.prototype.slice.call(container.children);
    ticks.forEach(function (t) { t.hidden = false; });
    var width = container.clientWidth;
    if (!width) return;
    var kept = [];
    var order = ticks.slice().sort(function (a, b) {
      return (a.dataset.edge === "mid") - (b.dataset.edge === "mid");
    });
    order.forEach(function (t) {
      var w = t.offsetWidth;
      var x = (parseFloat(t.style.left) / 100) * width;
      var left = t.dataset.edge === "start" ? x : t.dataset.edge === "end" ? x - w : x - w / 2;
      var box = [left - 4, left + w + 4];
      var clash = kept.some(function (k) { return box[0] < k[1] && k[0] < box[1]; });
      if (clash) t.hidden = true; else kept.push(box);
    });
  }

  function updateLegendMarker(state, value) {
    if (!state.legendMarker || !state.legendRange) return;
    var lo = state.legendRange[0], hi = state.legendRange[1];
    if (value === null || value === undefined || !Number.isFinite(Number(value))) {
      state.legendMarker.hidden = true;
      state.legendNote.textContent = "";
      return;
    }
    state.legendMarker.hidden = false;
    var v = Number(value);
    state.legendMarker.dataset.clip = v > hi ? "high" : v < lo ? "low" : "none";
    var p = Math.max(0, Math.min(100, ((v - lo) / ((hi - lo) || 1)) * 100));
    state.legendMarker.style.left = p + "%";
    drawHistogram(state);
    var clipped = v > hi || v < lo;
    state.legendMarker.hidden = clipped;
    var hs = state.legendHistState;
    state.legendMarker.dataset.hidden = hs && hs.thresholded && v > hs.threshold[0] && v < hs.threshold[1] ? "true" : "false";
    state.legendBar.dataset.clipped = v > hi ? "high" : v < lo ? "low" : "none";
    if (state.clipChip) {
      state.clipChip.hidden = !clipped;
      state.clipChip.textContent = clipped ?
        (v > hi ? "above display max " : "below display min ") + niceTick(v > hi ? hi : lo) : "";
    }
    state.legendNote.textContent = clipped ?
      "The cursor value " + formatValue(v) + " is " + (v > hi ? "above" : "below") +
        " the display range; the key's end colour is used." :
      "";
  }

  function setStatus(state, message, kind) {
    state.status.textContent = message;
    state.status.dataset.state = kind || "ready";
  }

  function updateLauncherTarget(host, map) {
    var target = host.querySelector("[data-nm-volume-target]");
    if (target && map) target.textContent = map.label || map.selector_label;
  }

  function readyMessage(map) {
    return map.status.state === "empty" ?
      map.label + " has no displayable voxels at the report defaults." :
      "Interactive view ready.";
  }

  function updateModifiedState(state, controlState) {
    var defaults = state.reportDefaults.get(state.currentMapId);
    var modified = !sameControlState(controlState, defaults);
    state.host.dataset.nmVolumeModified = modified ? "true" : "false";
    if (modified) {
      setStatus(
        state,
        "Display modified for " + ((currentMap(state) || {}).label || state.currentMapId) +
          " (exploratory only; static results are unchanged).",
        "modified"
      );
      return;
    }
    var map = state.analysis.maps.find(function (candidate) {
      return candidate.map_id === state.currentMapId;
    });
    setStatus(state, readyMessage(map), map.status.state);
  }

  function staticGroup(analysisId) {
    return Array.prototype.find.call(
      document.querySelectorAll("[data-nm-map-group]"),
      function (group) { return group.dataset.nmMapGroup === analysisId; }
    );
  }

  function requestStaticMap(state, mapId) {
    var group = staticGroup(state.analysis.analysis_id);
    if (!group) return;
    group.dispatchEvent(new CustomEvent("nm-volume-map-request", {
      detail: {
        analysisId: state.analysis.analysis_id,
        mapId: mapId
      },
      bubbles: true
    }));
  }

  async function selectMap(state, mapId) {
    var map = state.analysis.maps.find(function (candidate) {
      return candidate.map_id === mapId;
    });
    if (!map) fail("Unknown map in interactive analysis: " + mapId);
    if (state.currentMapId === mapId) return state;
    if (state.currentMapId && state.controls) {
      state.mapStates.set(
        state.currentMapId,
        copyControlState(state.controls.getState(OVERLAY_LAYER_ID))
      );
    }
    state.mapSelect.disabled = true;
    state.switchingMap = true;
    setStatus(state, "Loading " + map.label + "...", "loading");
    try {
      var raw = await decodeAsset(state, map.asset_id, false);
      // Maps of one analysis share a grid (the overlay layer swaps volumes in
      // place); the underlay may be on its own grid ('world' alignment).
      root.neuroimjs.assertSameVolumeGeometry(state.overlayGrid, raw);
      var display = mapDisplay(map, raw);
      var volume = extentVolume(raw, map, display.threshold);
      state.rawOverlay = raw;
      state.displayVolume = volume;
      state.extentKey = thresholdKey(display.threshold);
      state.menuMap = map;
      state.viewer.updateLayerVolume(OVERLAY_LAYER_ID, volume, {
        range: display.range,
        threshold: display.threshold,
        alpha: display.alpha,
        colormap: display.colormap
      });
      state.controls.visibleControls = visibleControls(state, map);
      state.controls.selectLayer(OVERLAY_LAYER_ID);
      await state.controls.updateComplete;
      state.currentMapId = mapId;
      var defaults = state.controls.setDefaultsFromCurrent(OVERLAY_LAYER_ID);
      state.reportDefaults.set(mapId, copyControlState(defaults));
      var saved = state.mapStates.get(mapId);
      if (saved) {
        state.controls.applyState(saved);
        applyExtent(state);
      }
      state.mapSelect.value = mapId;
      state.host.dataset.nmVolumeMap = mapId;
      updateLauncherTarget(state.host, map);
      updateLegend(state);
      updateReadout(state);
      labelSliders(state);
      updateModifiedState(
        state, state.controls.getState(OVERLAY_LAYER_ID)
      );
      return state;
    } finally {
      state.switchingMap = false;
      state.mapSelect.disabled = false;
    }
  }

  async function createViewer(host) {
    var scene = sceneFor(host);
    if (needsServer(scene)) fail(FILE_MESSAGE);
    var analysis = analysisFor(scene, host.dataset.nmVolumeAnalysis);
    var viewerElement = host.querySelector("[data-nm-volume-viewer]");
    var controlsElement = host.querySelector("[data-nm-volume-controls]");
    viewerElement.replaceChildren();
    controlsElement.replaceChildren();
    delete host.dataset.nmVolumeFailed;
    var state = {
      host: host,
      scene: scene,
      analysis: analysis,
      status: host.querySelector("[data-nm-volume-status]"),
      shell: host.querySelector("[data-nm-volume-shell]"),
      viewerElement: viewerElement,
      controlsElement: controlsElement,
      mapSelect: host.querySelector("[data-nm-volume-map]"),
      readout: host.querySelector("[data-nm-volume-readout]"),
      resetPosition: host.querySelector("[data-nm-volume-reset-position]"),
      overlayCache: new Map(),
      mapStates: new Map(),
      reportDefaults: new Map(),
      currentMapId: null,
      switchingMap: false
    };
    states.set(host, state);
    // A parked viewer resumes in place: its status line keeps its text so
    // nothing on the page moves while it rebuilds.
    if (!isParked(host)) setStatus(state, "Loading interactive view...", "loading");

    var backgroundAsset = assetFor(scene, scene.background.asset_id);
    var primary = analysis.maps.find(function (map) {
      return map.map_id === analysis.primary_map_id;
    });
    var initialMap = analysis.maps.find(function (map) {
      return map.map_id === host.dataset.nmVolumeMap;
    }) || primary;
    var volumes = await Promise.all([
      underlayFor(state, backgroundAsset.asset_id),
      decodeAsset(state, initialMap.asset_id, false)
    ]);
    // The underlay may sit on a finer grid than the maps: neuroimjs 0.5 slices
    // each layer on its own grid and draws it at its world position.
    state.overlayGrid = volumes[1];

    state.menuMap = initialMap;
    var ground = groundOf(host);
    var theme = groundTheme(ground);
    state.ground = ground;
    host.dataset.nmVolumeGround = ground;
    state.underlay = volumes[0];
    var underlayLayer = backgroundLayer(volumes[0], ground);
    var stack = new root.neuroimjs.VolStack(
      underlayLayer,
      overlayLayer(state, volumes[1], initialMap)
    );
    var restoreConsole = quietPixiDeprecations();
    state.viewer = await root.neuroimjs.SimpleOrthogonalViewer.create(
      state.viewerElement,
      stack,
      {
        layout: "ortho",
        showCrosshair: true,
        showSlider: false,
        showOrientationLabels: true,
        gapPx: 1,
        cellPaddingPx: 24,
        stackBelowPx: 520,
        stackedLegendHeightPx: 292,
        stackMode: "single",
        backgroundColor: theme.backgroundColor,
        focusOutline: null,
        crosshairOptions: Object.assign({
          crossThickness: 1,
          crosshairGap: 22
        }, theme.crosshair),
        orientationLabelOptions: Object.assign({
          fontSize: 11,
          fontWeight: "600",
          fontFamily: "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
          letterSpacing: 0.6,
          strokeWidth: 0,
          shadowBlur: 3,
          margin: 5,
          anchor: "image"
        }, theme.orientationLabels)
      }
    );
    state.stack = stack;
    // Orientation labels are built on the first frames; release the console
    // filter once those have rendered.
    requestAnimationFrame(function () { requestAnimationFrame(restoreConsole); });
    var bounds = volumes[0].bounds;
    if (bounds && typeof state.viewer.setFitBounds === "function") state.viewer.setFitBounds(bounds);
    Array.prototype.forEach.call(state.viewerElement.children, function (element, index) {
      if (element.dataset.nijLegend !== undefined) return;
      element.dataset.view = element.dataset.nijView || ["axial", "coronal", "sagittal"][index];
    });
    buildLegend(state);
    buildViewCoords(state);
    buildViewSwitch(state);
    bindSliceKeys(state);
    bindSliceDrag(state);
    if (typeof ResizeObserver !== "undefined" && state.legend) {
      state.legendObserver = new ResizeObserver(function () {
        if (state.legendTicks) hideCrowdedTicks(state.legendTicks);
        drawHistogram(state);
      });
      state.legendObserver.observe(state.legend);
    }
    state.viewer.setWorldCoord(analysis.initial_world_coord);

    state.controls = new root.neuroimjs.LayerControlPanel();
    customizeControls(state);
    state.controls.visibleControls = visibleControls(state, initialMap);
    state.controls.volStack = stack;
    state.controls.viewer = state.viewer;
    state.controlsElement.appendChild(state.controls);
    await state.controls.updateComplete;
    state.controls.selectLayer(OVERLAY_LAYER_ID);
    var initialDefaults = state.controls.setDefaultsFromCurrent(
      OVERLAY_LAYER_ID
    );
    labelSliders(state);

    analysis.maps.forEach(function (map) {
      var option = document.createElement("option");
      option.value = map.map_id;
      option.textContent = map.label || map.selector_label;
      state.mapSelect.appendChild(option);
    });
    state.mapSelect.value = initialMap.map_id;
    state.currentMapId = initialMap.map_id;
    updateLauncherTarget(host, initialMap);
    state.reportDefaults.set(
      initialMap.map_id, copyControlState(initialDefaults)
    );
    state.onControlChange = function (event) {
      if (state.switchingMap || !state.currentMapId) return;
      var snapshot = copyControlState(event.detail);
      state.mapStates.set(state.currentMapId, snapshot);
      updateModifiedState(state, snapshot);
      updateLegend(state);
      labelSliders(state);
      updateReadout(state);
      // A new colour range re-splits the palette now (threshold: applyExtent).
      var layer = overlayVolLayer(state);
      if (layer && thresholdKey(layer.getThreshold()) === state.extentKey &&
          state.paletteKey !== thresholdKey(layer.getThreshold()) + "|" + thresholdKey(layer.getRange())) {
        syncReportPalette(state, true);
        updateLegend(state);
      }
      scheduleExtent(state);
    };
    state.controls.addEventListener(
      "layer-control-change", state.onControlChange
    );
    state.onMapSelectChange = function () {
      selectMap(state, state.mapSelect.value).then(function () {
        if (state.currentMapId === state.mapSelect.value) {
          requestStaticMap(state, state.currentMapId);
        }
      }).catch(function (error) {
        state.mapSelect.value = state.currentMapId;
        setStatus(state, error.message, "error");
      });
    };
    state.mapSelect.addEventListener("change", state.onMapSelectChange);
    state.offCoord = state.viewer.onCoordChange(function () {
      updateReadout(state);
      announcePeak(state);
    });
    state.onResetPosition = function () {
      state.viewer.setWorldCoord(analysis.initial_world_coord);
      updateReadout(state);
    };
    if (state.resetPosition) {
      state.resetPosition.addEventListener("click", state.onResetPosition);
    }
    restoreMemo(state);
    updateLegend(state);
    updateReadout(state);
    updateModifiedState(state, state.controls.getState(OVERLAY_LAYER_ID));
    host.dataset.nmVolumeReady = "true";
    applyPendingCoord(state);
    return state;
  }

  // What a reader set up in a viewer that was released (to free WebGL
  // contexts) or hidden: cursor and per-map display changes come back on
  // the next open.
  function saveMemo(state) {
    if (!state || !state.viewer || !state.currentMapId) return;
    try {
      var mapStates = new Map(state.mapStates);
      if (state.controls) {
        mapStates.set(state.currentMapId, copyControlState(state.controls.getState(OVERLAY_LAYER_ID)));
      }
      state.host.__nmMemo = {
        mapId: state.currentMapId,
        coord: state.viewer.getWorldCoord().slice(),
        mapStates: mapStates
      };
    } catch (error) {
      state.host.__nmMemo = null;
    }
  }

  function restoreMemo(state) {
    var memo = state.host.__nmMemo;
    state.host.__nmMemo = null;
    if (!memo || memo.mapId !== state.currentMapId) return;
    state.mapStates = memo.mapStates;
    var defaults = state.reportDefaults.get(state.currentMapId);
    var saved = memo.mapStates.get(state.currentMapId);
    if (saved && !sameControlState(saved, defaults)) {
      try {
        state.switchingMap = true;
        state.controls.applyState(saved);
        applyExtent(state);
      } catch (error) {
        // A saved state the new viewer rejects falls back to the report display.
      } finally {
        state.switchingMap = false;
      }
    }
    if (!state.host.__nmPendingCoord && Array.isArray(memo.coord)) {
      state.viewer.setWorldCoord(memo.coord);
    }
  }

  // A requested cursor position (for example from a peak-table row) that
  // arrived before the viewer finished loading.
  function applyPendingCoord(state) {
    var pending = state.host.__nmPendingCoord;
    if (pending) {
      state.host.__nmPendingCoord = null;
      state.viewer.setWorldCoord(pending);
      updateReadout(state);
    }
    announcePeak(state);
  }

  // Peak-table jumps are announced once, by the report page; the viewer
  // stays quiet so screen readers do not hear the jump twice.
  function announcePeak(state) {
    if (state && state.host) state.host.__nmAnnounce = null;
  }

  // Reader-facing control panel policy: a curated colour-map menu with plain
  // names for the map's scale, and threshold entries clamped to the data,
  // mirrored while "Link symmetric" is pressed.
  function customizeControls(state) {
    var panel = state.controls;
    var displayApi = root.NeuroMosaicVolumeDisplay;
    panel.colormapNamesIncluding = function (name) {
      var map = state.menuMap || currentMap(state);
      return displayApi.colormapMenu(map && map.display.scale, name);
    };
    // The map's own values bound the threshold (a cut beyond every voxel hides
    // the map); the sliders' domain also reaches the colour range, so the
    // shared cap (which may exceed this map's data) stays representable.
    var dataRange = function () {
      var layer = overlayVolLayer(state);
      var range = layer && typeof layer.getVolumeRange === "function" ?
        layer.getVolumeRange() : (panel.volumeRange || [0, 1]);
      return range.map(Number);
    };
    var thresholdDomain = function () {
      var range = dataRange();
      var bound = Math.max(Math.abs(range[0]), Math.abs(range[1]));
      return range[0] < 0 && range[1] > 0 ? [-bound, bound] : range;
    };
    var clamp = function (v) {
      var domain = thresholdDomain();
      return Math.max(domain[0], Math.min(domain[1], v));
    };
    panel.clampToThresholdDomain = clamp;
    var widen = function () {
      var layer = overlayVolLayer(state);
      if (!layer) return;
      var data = dataRange();
      var range = layer.getRange().map(Number);
      // Rounded outward to the slider grid so arrow-key steps land on round
      // values (an input's step base is its minimum).
      var grid = displayApi.sliderGrid(Math.min(data[0], range[0]), Math.max(data[1], range[1]));
      var lo = grid.min;
      var hi = grid.max;
      state.sliderStep = grid.step;
      var current = panel.volumeRange || [];
      if (Number.isFinite(lo) && Number.isFinite(hi) && (current[0] !== lo || current[1] !== hi)) {
        panel.volumeRange = [lo, hi];
      }
    };
    state.widenControls = widen;
    // Unlinked thresholds keep a valid band: low never above high, and for
    // a two-sided map low <= 0 <= high (one cut per sign).
    var orderBand = function (pair, which) {
      var map = currentMap(state) || state.menuMap;
      var range = dataRange();
      var twoSided = Boolean(map && map.display && map.display.tail === "two_sided" &&
        range[0] < 0 && range[1] > 0);
      return displayApi.orderThresholdBand(pair, which, twoSided);
    };
    var thresholdInput = function (which) {
      return function (event) {
        var value = panel.readField(event);
        if (value === undefined) return;
        var bound = Math.max.apply(null, thresholdDomain().map(Math.abs));
        var current = panel.threshold.slice();
        if (panel.thresholdLinked) {
          var r = Math.min(Math.abs(value), bound);
          panel.threshold = [-r, r];
        } else {
          panel.threshold = orderBand(which === "low" ?
            [clamp(value), current[1]] : [current[0], clamp(value)], which);
        }
        panel.applyThreshold();
        // The field shows the clamped value even when the state is unchanged.
        if (event.target) {
          event.target.value = panel.fmt(panel.threshold[which === "low" ? 0 : 1]);
        }
      };
    };
    panel.onThresholdLowInput = thresholdInput("low");
    panel.onThresholdHighInput = thresholdInput("high");
    // A colour-range end typed past the other end is rejected (the field
    // returns to the current value) instead of inverting the colour scale.
    var rangeInput = function (which, original) {
      return function (event) {
        var value = panel.readField(event);
        if (value === undefined) return;
        var kept = displayApi.rejectCrossedRange(panel.range.map(Number), which, value);
        if (kept) {
          // Keep the field: event.target is null once dispatch is over.
          var field = event.target;
          if (field) {
            field.value = panel.fmt(kept[which === "low" ? 0 : 1]);
            field.setAttribute("aria-invalid", "true");
            setTimeout(function () { field.removeAttribute("aria-invalid"); }, 1600);
          }
          var live = state.host.querySelector("[data-nm-volume-live]");
          if (live) {
            live.textContent = "Range " + which + " must stay " + (which === "low" ? "below " : "above ") +
              niceTick(kept[which === "low" ? 1 : 0]) + "; kept " + niceTick(kept[which === "low" ? 0 : 1]) + ".";
          }
          return;
        }
        original.call(panel, event);
      };
    };
    panel.onRangeLowInput = rangeInput("low", panel.onRangeLowInput);
    panel.onRangeHighInput = rangeInput("high", panel.onRangeHighInput);
    var onThresholdUpdate = panel.onThresholdUpdate;
    panel.onThresholdUpdate = function (event) {
      var before = panel.threshold.slice();
      onThresholdUpdate.call(panel, event);
      if (panel.thresholdLinked) return;
      var which = panel.threshold[1] !== before[1] ? "high" : "low";
      var fixed = orderBand(panel.threshold, which);
      if (!samePair(fixed, panel.threshold.map(Number))) {
        panel.threshold = fixed;
        panel.applyThreshold();
      }
    };
    // "Reset to defaults" restores the report display, including the linked
    // (symmetric) threshold the report opened with.
    var resetToDefaults = panel.resetToDefaults;
    panel.resetToDefaults = function () {
      var out = resetToDefaults.apply(panel, arguments);
      if (typeof panel.syncThresholdLink === "function") panel.syncThresholdLink(true);
      widen();
      return out;
    };
    var selectLayer = panel.selectLayer;
    panel.selectLayer = function () {
      var out = selectLayer.apply(panel, arguments);
      widen();
      return out;
    };
    panel.fmt = function (value) { return niceTick(value); };
    // The panel reads the shared preset registry while handling input (reset,
    // colour-map choice); make it hold this viewer's colours first.
    state.onPanelIntent = function () { syncReportPalette(state, false); };
    ["pointerdown", "keydown"].forEach(function (type) {
      state.controlsElement.addEventListener(type, state.onPanelIntent, true);
    });
  }

  // Drop PixiJS's collapsed "Deprecation Warning" group (orientation labels)
  // while the viewer mounts; every other message passes through.
  function quietPixiDeprecations() {
    var original = {
      groupCollapsed: console.groupCollapsed,
      groupEnd: console.groupEnd,
      warn: console.warn
    };
    var inGroup = false, skipNext = 0;
    var isPixi = function (value) {
      return typeof value === "string" && value.indexOf("PixiJS Deprecation Warning") >= 0;
    };
    console.groupCollapsed = function (first) {
      if (isPixi(first)) { inGroup = true; return; }
      return original.groupCollapsed.apply(console, arguments);
    };
    console.groupEnd = function () {
      if (inGroup) { inGroup = false; return; }
      return original.groupEnd.apply(console, arguments);
    };
    console.warn = function (first) {
      if (inGroup) return;
      if (skipNext) { skipNext -= 1; return; }
      if (isPixi(first)) { skipNext = 1; return; }
      return original.warn.apply(console, arguments);
    };
    return function restore() {
      console.groupCollapsed = original.groupCollapsed;
      console.groupEnd = original.groupEnd;
      console.warn = original.warn;
    };
  }

  // Dragging on a slice moves the cursor: while the primary button is held,
  // each animation frame replays the latest pointer position to the pane's
  // own click handling.
  function bindSliceDrag(state) {
    var drag = null, frame = 0;
    state.onDragDown = function (event) {
      if (event.button !== 0) return;
      var canvas = event.target;
      if (!canvas || canvas.tagName !== "CANVAS" || !canvas.closest("[data-nij-view]")) return;
      // After the runtime has moved the cursor to the pointer, centre it on
      // the map voxel under the pointer.
      var through = VIEW_AXIS[canvas.closest("[data-nij-view]").dataset.nijView];
      setTimeout(function () { snapToMapVoxel(state, through); }, 0);
      if (event.__nmReplay) return;
      drag = { canvas: canvas, pointerId: event.pointerId, pointerType: event.pointerType };
    };
    state.onDragMove = function (event) {
      if (!drag || event.__nmReplay || event.pointerId !== drag.pointerId) return;
      if (!(event.buttons & 1)) { drag = null; return; }
      drag.x = event.clientX;
      drag.y = event.clientY;
      if (frame) return;
      frame = requestAnimationFrame(function () {
        frame = 0;
        if (!drag || drag.x === undefined) return;
        var box = drag.canvas.getBoundingClientRect();
        if (drag.x < box.left || drag.x > box.right || drag.y < box.top || drag.y > box.bottom) return;
        var replay = new PointerEvent("pointerdown", {
          bubbles: true, cancelable: true, clientX: drag.x, clientY: drag.y,
          button: 0, buttons: 1, pointerId: drag.pointerId,
          pointerType: drag.pointerType, isPrimary: true
        });
        replay.__nmReplay = true;
        drag.canvas.dispatchEvent(replay);
      });
    };
    state.onDragEnd = function () { drag = null; };
    state.viewerElement.addEventListener("pointerdown", state.onDragDown, true);
    document.addEventListener("pointermove", state.onDragMove, true);
    document.addEventListener("pointerup", state.onDragEnd, true);
    document.addEventListener("pointercancel", state.onDragEnd, true);
  }

  // A click or drag selects the map voxel under the pointer: the cursor moves
  // to that voxel's centre in the pane's plane (the through-plane coordinate
  // is left alone). With an underlay on a finer grid the runtime would
  // otherwise leave it at the underlay texel centre, which can lie on the
  // boundary between two map voxels; the crosshair, coordinates and value then
  // all name one map voxel.
  function snapToMapVoxel(state, through) {
    var layer = overlayVolLayer(state);
    var space = layer && layer.space;
    if (!state.viewer || !space || typeof space.coordToGrid !== "function" ||
        typeof space.gridToCoord !== "function") return;
    var coord = state.viewer.getWorldCoord().slice();
    var grid = space.coordToGrid(coord);
    var dim = space.dim || [];
    var voxel = grid.map(function (v) { return Math.round(v); });
    if (voxel.some(function (v, axis) { return !Number.isFinite(v) || v < 0 || v >= dim[axis]; })) return;
    var centre = space.gridToCoord(voxel);
    var target = coord.map(function (v, axis) { return axis === through ? v : Number(centre[axis]); });
    var moved = target.some(function (v, axis) { return Math.abs(v - coord[axis]) > 1e-6; });
    if (!moved) return;
    state.viewer.setWorldCoord(target);
    updateReadout(state);
  }

  // Displayed: the reader chose Slices for this map (it may be off screen).
  function isShown(host) {
    var shell = host.querySelector("[data-nm-volume-shell]");
    if (host.hidden || !host.getClientRects().length) return false;
    return isParked(host) || (Boolean(shell) && !shell.hidden);
  }

  function isParked(host) {
    return host.dataset.nmVolumePaused === "true";
  }

  function glKey(host) {
    return "volume:" + (host.dataset.nmVolumeAnalysis || "");
  }

  var GL_COST = 3;

  // Register (or refresh) this viewer in the page's WebGL budget.
  function touchBudget(host) {
    GL.touch({
      key: glKey(host),
      cost: 3,
      element: host,
      shown: function () { return isShown(host); },
      release: function () { release(host); }
    });
  }

  // Free the viewer's contexts, keeping what the reader set up for later. A
  // viewer that is still the chosen view keeps its place on the page (no
  // layout shift) and resumes when it is next on screen.
  function release(host) {
    var state = states.get(host);
    var keepPlace = Boolean(state && state.viewer && state.shell && !state.shell.hidden && isShown(host));
    var height = keepPlace ? state.shell.getBoundingClientRect().height : 0;
    if (state) saveMemo(state);
    dispose(host, { keepMemo: true });
    if (keepPlace) park(host, height, false);
  }

  // The viewer's stage, crosshair and orientation labels on a ground.
  function groundTheme(ground) {
    var look = GROUNDS[ground] || GROUNDS.dark;
    return {
      backgroundColor: look.stage,
      crosshair: {
        crossColor: look.cross, crossAlpha: look.crossAlpha,
        haloColor: look.halo, haloAlpha: look.haloAlpha
      },
      orientationLabels: {
        color: look.label, alpha: look.labelAlpha, shadowAlpha: look.labelShadow
      }
    };
  }

  // Restyle the viewer for the other ground in place (neuroimjs setTheme()),
  // keeping the cursor, the map and any display change.
  function switchGround(host, ground) {
    host.dataset.nmVolumeGroundChoice = ground;
    try { if (root.localStorage) root.localStorage.setItem(GROUND_KEY, ground); } catch (error) { /* per-view only */ }
    var state = states.get(host);
    if (!state || !state.viewer) return;
    var api = root.neuroimjs;
    state.viewer.setTheme(groundTheme(ground));
    if (state.underlay) {
      var look = underlayLook(api, state.underlay, ground);
      state.viewer.updateLayer(BACKGROUND_LAYER_ID, {
        colormap: look.colormap,
        threshold: look.threshold
      });
    }
    state.ground = ground;
    host.dataset.nmVolumeGround = ground;
    var sidebar = host.querySelector(".nm-volume-sidebar");
    if (sidebar) buildGroundSwitch(state, sidebar);
    drawHistogram(state);
    var live = host.querySelector("[data-nm-volume-live]");
    if (live) live.textContent = "Slices shown on a " + ground + " ground.";
  }

  var PAUSED_TEXT = "Paused to free graphics memory; it resumes when it is back on screen.";
  var BLOCKED_TEXT = "Paused: too many interactive views are open on this page. Return another map to Static, or resume this one now.";
  var LOST_TEXT = "Paused: the browser reclaimed this view's graphics memory several times. Resume when ready.";

  // blocked: false (off screen), "budget" or "lost" (repeated context loss).
  function pausedText(blocked) {
    if (blocked === "lost") return LOST_TEXT;
    return blocked ? BLOCKED_TEXT : PAUSED_TEXT;
  }

  // A resumed view takes focus on its first slice pane (reading order).
  function focusFirstPane(host) {
    var cells = Array.prototype.slice.call(host.querySelectorAll("[data-nij-view]"))
      .filter(function (c) { return c.getClientRects().length; });
    cells.sort(function (a, b) {
      var ra = a.getBoundingClientRect(), rb = b.getBoundingClientRect();
      return (ra.top - rb.top) || (ra.left - rb.left);
    });
    var target = cells[0] || host.querySelector("[data-nm-volume-toggle]");
    if (target && typeof target.focus === "function") target.focus({ preventScroll: true });
  }

  function park(host, height, blocked) {
    host.dataset.nmVolumePaused = "true";
    var holder = host.querySelector("[data-nm-volume-paused]");
    if (!holder) {
      holder = el("div", "nm-volume-paused");
      holder.setAttribute("data-nm-volume-paused", "");
      var text = el("p", "nm-volume-paused-text");
      var button = el("button", "nm-volume-paused-resume", "Resume interactive view");
      button.type = "button";
      button.addEventListener("click", function () { resume(host, true); });
      holder.append(text, button);
      var shell = host.querySelector("[data-nm-volume-shell]");
      shell.parentNode.insertBefore(holder, shell);
    }
    if (height > 0) holder.style.height = Math.round(height) + "px";
    holder.querySelector(".nm-volume-paused-text").textContent = pausedText(blocked);
    holder.hidden = false;
    // The toggle keeps claiming the open view (its label must not reflow).
    var toggle = host.querySelector("[data-nm-volume-toggle]");
    if (toggle) {
      toggle.setAttribute("aria-expanded", "true");
      toggle.textContent = "Hide interactive view";
    }
    if (blocked) GL.unpark(glKey(host));
    else GL.park({ key: glKey(host), element: host, resume: function () { resume(host, false); } });
  }

  function unpark(host) {
    GL.unpark(glKey(host));
    if (!isParked(host)) return;
    delete host.dataset.nmVolumePaused;
    var holder = host.querySelector("[data-nm-volume-paused]");
    if (holder) holder.hidden = true;
  }

  // Rebuild a parked viewer: only while it is on screen, and only when its
  // contexts fit the budget (a reader's click may go up to the hard limit).
  async function resume(host, byReader) {
    if (!isParked(host) || host.__nmActivating) return;
    if (host.hidden || !host.getClientRects().length) { unpark(host); collapse(host); return; }
    if (!byReader && !GL.onScreen(host)) return;
    if (!GL.reserve(glKey(host), GL_COST) && !byReader) {
      park(host, 0, "budget");
      return;
    }
    GL.unpark(glKey(host));
    var holder = host.querySelector("[data-nm-volume-paused]");
    var hadFocus = !!(byReader && holder && holder.contains(document.activeElement));
    if (holder) holder.querySelector(".nm-volume-paused-text").textContent = "Resuming the interactive view\u2026";
    if (byReader) host.__nmLosses = [];
    try {
      await activate(host);
      if (byReader && (hadFocus || document.activeElement === document.body || !document.activeElement)) {
        focusFirstPane(host);
      }
    } catch (error) {
      unpark(host);
    }
  }

  async function activate(host) {
    var state = states.get(host);
    if (state && state.viewer && GL.isLost(state.viewerElement)) {
      // The browser reclaimed this viewer's contexts; rebuild it.
      dispose(host, { keepMemo: false });
      state = null;
    }
    if (state && state.viewer) {
      state.shell.hidden = false;
      unpark(host);
      if (typeof state.viewer.handleResize === "function") state.viewer.handleResize();
      touchBudget(host);
      applyPendingCoord(state);
      return state;
    }
    if (host.__nmActivating) return host.__nmActivating;
    host.__nmActivating = (async function () {
      try {
        if (!root.neuroimjs) {
          var status = host.querySelector("[data-nm-volume-status]");
          status.textContent = "Loading the interactive viewer…";
          status.dataset.state = "loading";
        }
        if (needsServer(sceneFor(host))) fail(FILE_MESSAGE);
        await ensureRuntime();
        // Free room first, so creating these contexts never makes the
        // browser drop someone else's.
        GL.reserve(glKey(host), GL_COST);
        var created = await createViewer(host);
        unpark(host);
        created.shell.hidden = false;
        touchBudget(host);
        return created;
      } catch (error) {
        var status = host.querySelector("[data-nm-volume-status]");
        unpark(host);
        dispose(host);
        var message = readerMessage(error);
        status.textContent = message;
        status.dataset.state = "error";
        host.dataset.nmVolumeFailed = "true";
        // The view router returns to the static figures and says why.
        host.dispatchEvent(new CustomEvent("nm-viewer-failed", {
          detail: { analysisId: host.dataset.nmVolumeAnalysis, view: "slices", message: message },
          bubbles: true
        }));
        throw error;
      } finally {
        host.__nmActivating = null;
      }
    }());
    return host.__nmActivating;
  }

  // The browser took this viewer's contexts (it never does for a viewer we
  // released ourselves). Rebuild only a viewer the reader can see, only when
  // it fits the budget, and at most twice in 15 s; anything else waits,
  // parked, for the reader to come back to it.
  document.addEventListener("webglcontextlost", function (event) {
    var canvas = event.target;
    if (!canvas || canvas.__nmReleased || !canvas.closest) return;
    var host = canvas.closest("[data-nm-volume-host]");
    if (!host || !states.get(host)) return;
    if (host.__nmRemountTimer) return;
    host.__nmRemountTimer = setTimeout(function () {
      host.__nmRemountTimer = null;
      var state = states.get(host);
      if (!state || !state.viewer) return;
      if (!isShown(host)) {
        // A hidden viewer is rebuilt by activate() on its next open.
        release(host);
        return;
      }
      var now = Date.now();
      host.__nmLosses = (host.__nmLosses || []).filter(function (t) { return now - t < 15000; });
      host.__nmLosses.push(now);
      var height = state.shell.getBoundingClientRect().height;
      release(host);
      if (!GL.onScreen(host)) return;
      if (host.__nmLosses.length > 2) {
        park(host, height, "lost");
        return;
      }
      if (!GL.reserve(glKey(host), GL_COST)) {
        park(host, height, "budget");
        return;
      }
      resume(host, false);
    }, 0);
  }, true);

  function bindHost(host) {
    if (host.dataset.nmVolumeBound === "true") return;
    host.dataset.nmVolumeBound = "true";
    var button = host.querySelector("[data-nm-volume-toggle]");
    var shell = host.querySelector("[data-nm-volume-shell]");
    if (!host.querySelector("[data-nm-volume-live]")) {
      var live = el("p", "nm-volume-live");
      live.setAttribute("data-nm-volume-live", "");
      live.setAttribute("aria-live", "polite");
      live.setAttribute("role", "status");
      host.appendChild(live);
    }
    // When the view router hides this panel (3D surface or Static), collapse
    // it so the toggle never claims an open view. The viewer keeps its state
    // and reappears instantly on the next open.
    if (typeof MutationObserver !== "undefined") {
      new MutationObserver(function () {
        if (!host.hidden) return;
        if (isParked(host)) { unpark(host); collapse(host); return; }
        if (!shell.hidden) collapse(host);
      }).observe(host, { attributes: true, attributeFilter: ["hidden"] });
    }
    button.addEventListener("click", async function () {
      if (!shell.hidden || isParked(host)) {
        unpark(host);
        dispose(host);
        // Tell the view router (surface bridge) so its radios stay in sync.
        host.dispatchEvent(new CustomEvent("nm-volume-hidden", {
          detail: { analysisId: host.dataset.nmVolumeAnalysis },
          bubbles: true
        }));
        return;
      }
      button.disabled = true;
      try {
        await activate(host);
        shell.hidden = false;
        button.setAttribute("aria-expanded", "true");
        button.textContent = "Hide interactive view";
      } catch (error) {
        // The visible status already explains the static fallback.
      } finally {
        button.disabled = false;
      }
    });
  }

  function collapse(host) {
    var shell = host.querySelector("[data-nm-volume-shell]");
    var button = host.querySelector("[data-nm-volume-toggle]");
    if (shell) shell.hidden = true;
    if (button) {
      button.setAttribute("aria-expanded", "false");
      button.textContent = "Explore volume interactively";
    }
  }

  function initAll(container) {
    (container || document).querySelectorAll("[data-nm-volume-host]").forEach(bindHost);
  }

  // Return the readout and reset button (moved onto the stage legend) to the
  // sidebar so a later activation finds them again.
  function restoreSidebarNodes(state) {
    var sidebar = state.host.querySelector(".nm-volume-sidebar");
    if (!sidebar || !state.legend) return;
    var anchor = state.sidebarAnchor && state.sidebarAnchor.parentNode === sidebar ?
      state.sidebarAnchor : sidebar.querySelector("[data-nm-volume-controls]");
    if (state.readout) {
      state.readout.replaceChildren();
      sidebar.insertBefore(state.readout, anchor);
    }
    if (state.resetPosition) {
      var reset = state.resetPosition;
      reset.classList.remove("nm-volume-reset-position--stage");
      reset.removeAttribute("aria-label");
      reset.removeAttribute("title");
      if (state.resetPositionMarkup !== undefined) reset.innerHTML = state.resetPositionMarkup;
      sidebar.insertBefore(reset, anchor);
    }
  }

  function dispose(host, options) {
    var state = states.get(host);
    if (!(options && options.keepMemo)) {
      if (state) saveMemo(state);
    }
    GL.drop(glKey(host));
    if (state) {
      if (state.legendObserver) state.legendObserver.disconnect();
      if (state.viewSwitchObserver) state.viewSwitchObserver.disconnect();
      if (state.viewSwitch) state.viewSwitch.remove();
      restoreSidebarNodes(state);
      if (typeof state.offCoord === "function") state.offCoord();
      if (state.extentTimer) clearTimeout(state.extentTimer);
      if (state.onPanelIntent && state.controlsElement) {
        ["pointerdown", "keydown"].forEach(function (type) {
          state.controlsElement.removeEventListener(type, state.onPanelIntent, true);
        });
      }
      if (state.onDragDown && state.viewerElement) {
        state.viewerElement.removeEventListener("pointerdown", state.onDragDown, true);
        document.removeEventListener("pointermove", state.onDragMove, true);
        document.removeEventListener("pointerup", state.onDragEnd, true);
        document.removeEventListener("pointercancel", state.onDragEnd, true);
      }
      if (state.onSliceKey && state.viewerElement) {
        state.viewerElement.removeEventListener("keydown", state.onSliceKey, true);
      }
      if (state.onSliceWheel && state.viewerElement) {
        state.viewerElement.removeEventListener("wheel", state.onSliceWheel, { capture: true });
      }
      if (state.controls && state.onControlChange) {
        state.controls.removeEventListener(
          "layer-control-change", state.onControlChange
        );
      }
      if (state.mapSelect && state.onMapSelectChange) {
        state.mapSelect.removeEventListener("change", state.onMapSelectChange);
      }
      if (state.resetPosition && state.onResetPosition) {
        state.resetPosition.removeEventListener("click", state.onResetPosition);
      }
      if (state.viewer) {
        var canvases = state.viewerElement ?
          Array.prototype.slice.call(state.viewerElement.querySelectorAll("canvas")) : [];
        try { state.viewer.dispose(); } catch (error) { /* lost context */ }
        GL.loseContexts(canvases);
      }
      if (state.overlayCache) state.overlayCache.clear();
      state.rawOverlay = null;
      state.displayVolume = null;
      states.delete(host);
    }
    var shell = host.querySelector("[data-nm-volume-shell]");
    var button = host.querySelector("[data-nm-volume-toggle]");
    var viewerElement = host.querySelector("[data-nm-volume-viewer]");
    var controlsElement = host.querySelector("[data-nm-volume-controls]");
    if (shell) shell.hidden = true;
    if (viewerElement) viewerElement.replaceChildren();
    if (controlsElement) controlsElement.replaceChildren();
    if (button) {
      button.setAttribute("aria-expanded", "false");
      button.textContent = "Explore volume interactively";
    }
    delete host.dataset.nmVolumeReady;
  }

  document.addEventListener("nm-map-change", function (event) {
    var detail = event.detail || {};
    if (!detail.analysisId || !detail.mapId) return;
    document.querySelectorAll("[data-nm-volume-host]").forEach(function (host) {
      if (host.dataset.nmVolumeAnalysis !== detail.analysisId) return;
      host.dataset.nmVolumeMap = detail.mapId;
      var state = states.get(host);
      var analysis;
      try {
        analysis = state ? state.analysis : analysisFor(sceneFor(host), detail.analysisId);
        updateLauncherTarget(host, analysis.maps.find(function (map) {
          return map.map_id === detail.mapId;
        }));
      } catch (error) {
        return;
      }
      if (!state || !state.viewer || state.currentMapId === detail.mapId) return;
      selectMap(state, detail.mapId).catch(function (error) {
        state.mapSelect.value = state.currentMapId;
        setStatus(state, error.message, "error");
      });
    });
  });

  // Move the cursor of an analysis's viewer, e.g. from a peak-table row:
  // detail { analysisId, world: [x, y, z] }. Applied now when the viewer is
  // open, otherwise as soon as it next opens.
  document.addEventListener("nm-volume-goto", function (event) {
    var detail = event.detail || {};
    var world = Array.isArray(detail.world) ? detail.world.map(Number) : null;
    if (!detail.analysisId || !world || world.length !== 3 ||
        !world.every(Number.isFinite)) return;
    document.querySelectorAll("[data-nm-volume-host]").forEach(function (host) {
      if (host.dataset.nmVolumeAnalysis !== detail.analysisId) return;
      host.__nmPendingCoord = world;
      var state = states.get(host);
      if (state && state.viewer && host.dataset.nmVolumeReady === "true") {
        applyPendingCoord(state);
      }
    });
  });

  root.NeuroMosaicVolumeReport = {
    version: ADAPTER_VERSION,
    runtimeVersion: RUNTIME_VERSION,
    initAll: initAll,
    activate: activate,
    selectMap: function (host, mapId) {
      var state = states.get(host);
      if (!state || !state.viewer) fail("Interactive viewer is not active.");
      return selectMap(state, mapId);
    },
    getState: function (host) { return states.get(host); },
    exportPNG: function (host) {
      var state = states.get(host);
      if (state && state.viewer) exportPNG(state);
    },
    ensureRuntime: ensureRuntime,
    dispose: dispose
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { initAll(document); });
  } else {
    initAll(document);
  }
  window.addEventListener("pagehide", function () {
    document.querySelectorAll("[data-nm-volume-host]").forEach(dispose);
  });
})(typeof globalThis !== "undefined" ? globalThis : window);
