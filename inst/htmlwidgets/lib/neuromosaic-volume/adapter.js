(function (root) {
  "use strict";

  var ADAPTER_VERSION = "1.2.0";
  var RUNTIME_VERSION = "0.4.0";
  var OVERLAY_LAYER_ID = "neuromosaic-overlay";
  var UNDERLAY_HEADROOM = 1.3;
  var STAGE_COLOR = 0x111619;
  var MINUS = "\u2212";
  var states = new WeakMap();

  function fail(message) {
    throw new Error(message);
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
      if (window.location.protocol === "file:") {
        fail(
          "Companion volume assets cannot be fetched from file://. " +
          "Serve this report over HTTP or render with montage_interactive(assets = 'embed')."
        );
      }
      throw error;
    }
  }

  async function decodeAsset(state, assetId, isBackground) {
    if (isBackground && state.backgroundVolume) return state.backgroundVolume;
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

    if (isBackground) {
      state.backgroundVolume = volume;
      return volume;
    }
    state.overlayCache.set(assetId, volume);
    while (state.overlayCache.size > state.scene.view.cache_maps) {
      state.overlayCache.delete(state.overlayCache.keys().next().value);
    }
    return volume;
  }

  function mapDisplay(map, volume) {
    var api = root.neuroimjs;
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

  // Intensity of the template's empty field of view. Otsu's split of a strided
  // sample separates background from tissue; the floor sits well below that
  // split so the dark background (and its faint halo) maps to the stage colour
  // while CSF and grey matter keep their contrast.
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
  function underlayColorMap(api, range) {
    var r0 = ((STAGE_COLOR >> 16) & 255) / 255;
    var g0 = ((STAGE_COLOR >> 8) & 255) / 255;
    var b0 = (STAGE_COLOR & 255) / 255;
    var colors = [];
    for (var i = 0; i < 256; i += 1) {
      var t = i / 255;
      colors.push([r0 + (1 - r0) * t, g0 + (1 - g0) * t, b0 + (1 - b0) * t]);
    }
    return new api.ColorMap(colors, { range: range, name: "Custom" });
  }

  function backgroundLayer(volume) {
    var api = root.neuroimjs;
    var range = volume.getRange().map(Number);
    if (range.length !== 2 || !range.every(Number.isFinite)) {
      fail("Background volume has no finite display range.");
    }
    if (range[0] === range[1]) range = [range[0] - 0.5, range[1] + 0.5];
    var floor = backgroundFloor(volume, range);
    // Hold the brightest tissue below white so suprathreshold colour reads
    // against the underlay instead of competing with it.
    range = [floor, floor + (range[1] - floor) * UNDERLAY_HEADROOM];
    var layer = new api.VolLayer(
      "neuromosaic-background",
      volume,
      underlayColorMap(api, range),
      range,
      [0, 0],
      1
    );
    // Templates often end inside tissue (neck, lower cerebellum); fade the
    // field-of-view edge into the stage rather than stopping at a hard line.
    if (typeof layer.setEdgeFade === "function") layer.setEdgeFade(3);
    return layer;
  }

  function overlayLayer(volume, map) {
    var api = root.neuroimjs;
    var display = mapDisplay(map, volume);
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
    line.appendChild(el(
      "strong", "nm-volume-readout-number",
      value === null ? "outside map" : formatValue(value)
    ));
    if (map.units && value !== null) line.appendChild(el("span", "nm-volume-readout-units", map.units));
    state.clipChip = el("span", "nm-volume-readout-clip");
    state.clipChip.hidden = true;
    line.appendChild(state.clipChip);
    readout.appendChild(document.createTextNode(" "));
    readout.appendChild(line);
    readout.appendChild(document.createTextNode(" "));
    readout.appendChild(coords);
    updateLegendMarker(state, value);
    updateCluster(state, coord);
    updateViewCoords(state, coord);
  }

  function voxelOf(state, coord) {
    var layer = state.stack && state.stack.getLayer(0);
    if (!layer || !layer.space || typeof layer.space.coordToGrid !== "function") return null;
    var g = layer.space.coordToGrid(coord);
    if (!g || g.some(function (v) { return !Number.isFinite(v); })) return null;
    return g.map(function (v) { return Math.round(v); });
  }

  var VIEW_AXIS = { sagittal: 0, coronal: 1, axial: 2 };

  // The suprathreshold cluster under the cursor: 26-connected voxels of the
  // same sign beyond the current threshold (FSL cluster's default connectivity).
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
    var key = thr[0] + ":" + thr[1];
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
      for (var dk = -1; dk <= 1; dk += 1) {
        var kk = k + dk; if (kk < 0 || kk >= nz) continue;
        for (var dj = -1; dj <= 1; dj += 1) {
          var jj = j + dj; if (jj < 0 || jj >= ny) continue;
          for (var di = -1; di <= 1; di += 1) {
            var ii = i + di; if (ii < 0 || ii >= nx) continue;
            var n = (kk * ny + jj) * nx + ii;
            if (members[n] || !inside(data[n])) continue;
            members[n] = 1;
            stack.push(n);
          }
        }
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
    line.appendChild(el("span", "nm-volume-cluster-size",
      fmtInt(c.voxels) + " voxels \u00b7 " + fmtInt(c.mm3) + " mm\u00b3"));
    line.appendChild(document.createTextNode(" "));
    var atPeak = voxelOf(state, coord).join() === voxelOf(state, c.peakWorld).join();
    var where = c.peakWorld.map(function (v) { return signed(Number(v).toFixed(1)); }).join(", ");
    if (atPeak) {
      line.appendChild(el("span", "nm-volume-cluster-atpeak",
        "Peak " + formatValue(c.peak) + " at (" + where + ")"));
      return;
    }
    var go = el("button", "nm-volume-cluster-peak", "Go to peak " + formatValue(c.peak));
    go.type = "button";
    go.title = "Peak at (" + where + ") mm";
    go.setAttribute("aria-label", "Go to cluster peak " + formatValue(c.peak) + " at " + where + " mm");
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
      state.viewCoords[view].textContent =
        ["x", "y", "z"][axis] + " " + signed(Number(coord[axis]).toFixed(1));
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

  // Same precision as the panel's numeric fields, so legend and controls agree.
  function niceTick(value) {
    var a = Math.abs(value);
    return signed(a >= 100 ? value.toFixed(0) : value.toFixed(2));
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
    if (sidebar && !sidebar.querySelector(".nm-volume-hint")) {
      sidebar.appendChild(el("p", "nm-volume-hint", coarse ?
        "Tap or drag on a slice to move the cursor. L is left." :
        "Click or drag on a slice to move the cursor; arrow keys step through slices. L is left."));
    }
    state.legend = legend;
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
      ctx.fillStyle = hidden ? "rgba(255, 255, 255, 0.10)" :
        "rgba(" + Math.round(c[0] * 255) + "," + Math.round(c[1] * 255) + "," + Math.round(c[2] * 255) + ",0.85)";
      var bh = Math.max(1, (Math.log1p(counts[b]) / peak) * (cssH - 1));
      ctx.fillRect(b * bw + 0.5, cssH - bh, Math.max(1, bw - 1), bh);
    }
    ctx.fillStyle = "rgba(255, 255, 255, 0.18)";
    ctx.fillRect(0, cssH - 1, cssW, 1);
    // Cursor value, on the same x-scale as the key (pinned to the end if clipped).
    var v = state.viewer.getValue(OVERLAY_LAYER_ID);
    // Cursor value on the scale: a guide line and a caret pointing at the key.
    // Off-scale values are marked by the key's outlined end cap instead.
    if (v !== null && Number.isFinite(Number(v)) && Number(v) >= h.lo && Number(v) <= h.hi) {
      var t = (Number(v) - h.lo) / ((h.hi - h.lo) || 1);
      var x = Math.round(t * (cssW - 1)) + 0.5;
      ctx.fillStyle = "rgba(255, 255, 255, 0.55)";
      ctx.fillRect(x - 0.5, 0, 1, cssH - 6);
      // Caret sits on the baseline, pointing down at the key.
      ctx.fillStyle = "rgba(255, 255, 255, 0.95)";
      ctx.beginPath();
      ctx.moveTo(x - 4.5, cssH - 6);
      ctx.lineTo(x + 4.5, cssH - 6);
      ctx.lineTo(x, cssH);
      ctx.closePath();
      ctx.fill();
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
        "Display modified for " + state.currentMapId +
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
      var volume = await decodeAsset(state, map.asset_id, false);
      root.neuroimjs.assertSameVolumeGeometry(state.backgroundVolume, volume);
      var display = mapDisplay(map, volume);
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
      if (saved) state.controls.applyState(saved);
      state.mapSelect.value = mapId;
      updateLauncherTarget(state.host, map);
      updateLegend(state);
      updateReadout(state);
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
    setStatus(state, "Loading interactive view...", "loading");

    var backgroundAsset = assetFor(scene, scene.background.asset_id);
    var primary = analysis.maps.find(function (map) {
      return map.map_id === analysis.primary_map_id;
    });
    var initialMap = analysis.maps.find(function (map) {
      return map.map_id === host.dataset.nmVolumeMap;
    }) || primary;
    var volumes = await Promise.all([
      decodeAsset(state, backgroundAsset.asset_id, true),
      decodeAsset(state, initialMap.asset_id, false)
    ]);
    root.neuroimjs.assertSameVolumeGeometry(volumes[0], volumes[1]);

    var stack = new root.neuroimjs.VolStack(
      backgroundLayer(volumes[0]),
      overlayLayer(volumes[1], initialMap)
    );
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
        backgroundColor: STAGE_COLOR,
        focusOutline: null,
        crosshairOptions: {
          crossColor: 0xffffff,
          crossAlpha: 0.58,
          crossThickness: 1,
          crosshairGap: 22,
          haloColor: 0x000000,
          haloAlpha: 0.35
        },
        orientationLabelOptions: {
          fontSize: 11,
          fontWeight: "600",
          fontFamily: "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
          letterSpacing: 0.6,
          color: 0xe6eaec,
          alpha: 0.6,
          strokeWidth: 0,
          shadowAlpha: 0.9,
          shadowBlur: 3,
          margin: 5,
          anchor: "image"
        }
      }
    );
    state.stack = stack;
    var bounds = headBounds(volumes[0]);
    if (bounds && typeof state.viewer.setFitBounds === "function") state.viewer.setFitBounds(bounds);
    Array.prototype.forEach.call(state.viewerElement.children, function (element, index) {
      if (element.dataset.nijLegend !== undefined) return;
      element.dataset.view = element.dataset.nijView || ["axial", "coronal", "sagittal"][index];
    });
    buildLegend(state);
    buildViewCoords(state);
    buildViewSwitch(state);
    if (typeof ResizeObserver !== "undefined" && state.legend) {
      state.legendObserver = new ResizeObserver(function () {
        if (state.legendTicks) hideCrowdedTicks(state.legendTicks);
        drawHistogram(state);
      });
      state.legendObserver.observe(state.legend);
    }
    state.viewer.setWorldCoord(analysis.initial_world_coord);

    state.controls = new root.neuroimjs.LayerControlPanel();
    state.controls.visibleControls = visibleControls(state, initialMap);
    state.controls.volStack = stack;
    state.controls.viewer = state.viewer;
    state.controlsElement.appendChild(state.controls);
    await state.controls.updateComplete;
    state.controls.selectLayer(OVERLAY_LAYER_ID);
    var initialDefaults = state.controls.setDefaultsFromCurrent(
      OVERLAY_LAYER_ID
    );

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
    });
    state.onResetPosition = function () {
      state.viewer.setWorldCoord(analysis.initial_world_coord);
      updateReadout(state);
    };
    if (state.resetPosition) {
      state.resetPosition.addEventListener("click", state.onResetPosition);
    }
    updateLegend(state);
    updateReadout(state);
    updateModifiedState(state, initialDefaults);
    host.dataset.nmVolumeReady = "true";
    return state;
  }

  async function activate(host) {
    var state = states.get(host);
    if (state && state.viewer) {
      state.shell.hidden = false;
      return state;
    }
    try {
      state = await createViewer(host);
      state.shell.hidden = false;
      return state;
    } catch (error) {
      var status = host.querySelector("[data-nm-volume-status]");
      dispose(host);
      status.textContent = "Interactive view unavailable: " + error.message +
        " The static panel remains authoritative.";
      status.dataset.state = "error";
      host.dataset.nmVolumeFailed = "true";
      throw error;
    }
  }

  function bindHost(host) {
    if (host.dataset.nmVolumeBound === "true") return;
    host.dataset.nmVolumeBound = "true";
    var button = host.querySelector("[data-nm-volume-toggle]");
    var shell = host.querySelector("[data-nm-volume-shell]");
    button.addEventListener("click", async function () {
      if (!shell.hidden) {
        dispose(host);
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

  function dispose(host) {
    var state = states.get(host);
    if (state) {
      if (state.legendObserver) state.legendObserver.disconnect();
      if (state.viewSwitchObserver) state.viewSwitchObserver.disconnect();
      if (state.viewSwitch) state.viewSwitch.remove();
      restoreSidebarNodes(state);
      if (typeof state.offCoord === "function") state.offCoord();
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
      if (state.viewer) state.viewer.dispose();
      if (state.overlayCache) state.overlayCache.clear();
      state.backgroundVolume = null;
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
