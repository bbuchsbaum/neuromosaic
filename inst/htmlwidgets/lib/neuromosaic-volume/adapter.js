(function (root) {
  "use strict";

  var ADAPTER_VERSION = "1.1.0";
  var RUNTIME_VERSION = "0.3.0";
  var OVERLAY_LAYER_ID = "neuromosaic-overlay";
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

  function backgroundLayer(volume) {
    var api = root.neuroimjs;
    var range = volume.getRange().map(Number);
    if (range.length !== 2 || !range.every(Number.isFinite)) {
      fail("Background volume has no finite display range.");
    }
    if (range[0] === range[1]) range = [range[0] - 0.5, range[1] + 0.5];
    return new api.VolLayer(
      "neuromosaic-background",
      volume,
      api.ColorMapFactory.createGrayscale({ range: range }),
      range,
      [0, 0],
      1
    );
  }

  function overlayLayer(volume, map) {
    var api = root.neuroimjs;
    var display = mapDisplay(map, volume);
    return new api.VolLayer(
      OVERLAY_LAYER_ID,
      volume,
      display.colormap,
      display.range,
      display.threshold,
      display.alpha
    );
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

  function updateReadout(state) {
    var map = state.analysis.maps.find(function (candidate) {
      return candidate.map_id === state.currentMapId;
    });
    var value = state.viewer.getValue(OVERLAY_LAYER_ID);
    var valueText = value === null ? "outside map" : Number(value).toPrecision(5);
    var coord = state.viewer.getWorldCoord().map(function (value) {
      return Number(value).toFixed(1);
    });
    state.readout.textContent = "x " + coord[0] + ", y " + coord[1] +
      ", z " + coord[2] + " mm | " + (map.legend_title || map.label) + ": " + valueText +
      (map.units ? " " + map.units : "");
  }

  function setStatus(state, message, kind) {
    state.status.textContent = message;
    state.status.dataset.state = kind || "ready";
  }

  function updateLauncherTarget(host, map) {
    var target = host.querySelector("[data-nm-volume-target]");
    if (target && map) target.textContent = map.selector_label || map.label;
  }

  function readyMessage(map) {
    return map.status.state === "empty" ?
      map.label + " has no displayable voxels at the report defaults." :
      "Interactive view ready. Display changes are exploratory only.";
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
        layout: "top-bottom",
        showCrosshair: true,
        showSlider: false,
        showOrientationLabels: true,
        gapPx: 8
      }
    );
    Array.prototype.forEach.call(state.viewerElement.children, function (element, index) {
      element.dataset.view = ["axial", "coronal", "sagittal"][index];
    });
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
      option.textContent = map.selector_label || map.label;
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

  function dispose(host) {
    var state = states.get(host);
    if (state) {
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
