(function () {
  "use strict";

  // neuromosaic interactive surface bridge: mounts the surfview report scene
  // into each [data-nm-surface-host], owns the floating controls, and keeps
  // the browser map selection in step with the static report selectors.
  var display = window.NeuroMosaicSurfaceDisplay;

  var BRAIN_VIEWS = [
    { id: "oblique", label: "Overview", title: "Overview: the angle that shows the most of this map" },
    { sep: true },
    { id: "left", label: "Lateral L", title: "Left hemisphere, lateral surface" },
    { id: "right", label: "Lateral R", title: "Right hemisphere, lateral surface" },
    { id: "left-medial", label: "Medial L", title: "Left hemisphere, medial surface" },
    { id: "right-medial", label: "Medial R", title: "Right hemisphere, medial surface" },
    { sep: true },
    { id: "dorsal", label: "Dorsal", title: "From above" },
    { id: "ventral", label: "Ventral", title: "From below" },
    { id: "anterior", label: "Anterior", title: "From the front" },
    { id: "posterior", label: "Posterior", title: "From behind" }
  ];
  var SPLIT_VIEWS = [
    { id: "lateral", label: "Lateral", title: "Lateral surfaces" },
    { id: "medial", label: "Medial", title: "Medial surfaces" },
    { id: "dorsal", label: "Dorsal", title: "From above" },
    { id: "ventral", label: "Ventral", title: "From below" },
    { id: "anterior", label: "Anterior", title: "From the front" },
    { id: "posterior", label: "Posterior", title: "From behind" }
  ];

  function parse(host, name) {
    try {
      return JSON.parse(host.getAttribute(name) || "{}");
    } catch (error) {
      return {};
    }
  }

  function part(host, name) {
    return host.querySelector("[data-nm-surface-" + name + "]");
  }

  function fmt(value) {
    return display.formatNumber(value, { minus: true });
  }

  function isAnatomical(host) {
    return host.getAttribute("data-nm-surface-layout") === "anatomical";
  }

  function legendFor(host) {
    return parse(host, "data-nm-surface-legends")[host.getAttribute("data-nm-surface-map")] || {};
  }

  // A short unit such as "z" or "t" doubles as the variable name.
  function symbolFor(legend) {
    return legend.units && legend.units.length <= 3 ? legend.units : "value";
  }

  function reportGroup(analysisId) {
    var groups = document.querySelectorAll("[data-nm-map-group]");
    for (var i = 0; i < groups.length; i += 1) {
      if (groups[i].getAttribute("data-nm-map-group") === analysisId) return groups[i];
    }
    return null;
  }

  // Status ------------------------------------------------------------------
  function setStatus(host, kind, text) {
    var node = part(host, "status");
    if (!node) return;
    if (!kind) {
      node.hidden = true;
      node.textContent = "";
      node.removeAttribute("data-kind");
      return;
    }
    node.hidden = false;
    node.setAttribute("data-kind", kind);
    node.textContent = text;
  }

  function flashError(host, text) {
    setStatus(host, "error", text);
    window.clearTimeout(host.__nmSurfaceStatusTimer);
    host.__nmSurfaceStatusTimer = window.setTimeout(function () { setStatus(host, null); }, 3500);
  }

  function showFallback(host, error) {
    var fallback = part(host, "author-fallback");
    var widget = host.querySelector(".nm-surface-widget");
    if (widget) widget.setAttribute("data-nm-surface-failed", "true");
    if (fallback) {
      fallback.hidden = false;
      if (error) fallback.textContent = fallback.textContent + " " + error.message;
      setStatus(host, null);
    } else {
      setStatus(host, "error", "Interactive surface unavailable; use the static montage above." +
        (error ? " " + error.message : ""));
    }
  }

  // Maps --------------------------------------------------------------------
  function setTarget(host, mapId) {
    host.setAttribute("data-nm-surface-map", mapId);
    var labels = parse(host, "data-nm-surface-labels");
    var title = part(host, "target");
    if (title) title.textContent = labels[mapId] || mapId;
    var select = part(host, "map-select");
    if (select && select.value !== mapId) select.value = mapId;
  }

  function requestReportMap(host, mapId) {
    var analysisId = host.getAttribute("data-nm-surface-analysis");
    var group = reportGroup(analysisId);
    if (group) {
      group.dispatchEvent(new CustomEvent("nm-map-request", {
        detail: { analysisId: analysisId, mapId: mapId },
        bubbles: true
      }));
    }
  }

  function selectMap(host, mapId) {
    var mapping = parse(host, "data-nm-surface-map-to-layer");
    var layerId = mapping[mapId];
    if (!layerId) return;
    setTarget(host, mapId);
    var handle = host.__nmSurfaceHandle;
    if (!handle) return;
    try {
      handle.selectLayer(layerId);
    } catch (error) {
      flashError(host, "The interactive surface could not show this map: " + error.message);
    }
  }

  function currentLayerId(host) {
    return display.layerIdForMap(
      parse(host, "data-nm-surface-map-to-layer"),
      host.getAttribute("data-nm-surface-map")
    );
  }

  // Embedded assets ---------------------------------------------------------
  function base64Bytes(text) {
    var binary = window.atob(text.replace(/\s/g, ""));
    var bytes = new Uint8Array(binary.length);
    for (var i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
    return bytes;
  }

  function decompress(bytes, compression) {
    if (compression === "none") return Promise.resolve(bytes);
    if (compression !== "gzip") {
      return Promise.reject(new Error("Unsupported surface asset compression: " + compression));
    }
    if (typeof window.DecompressionStream !== "function") {
      return Promise.reject(new Error("This browser cannot decompress embedded surface data."));
    }
    var stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream("gzip"));
    return new Response(stream).arrayBuffer().then(function (buffer) {
      return new Uint8Array(buffer);
    });
  }

  function surfaceFetch(input, init) {
    var url = typeof input === "string" ? input : input.url;
    var prefix = "nm-surface-asset:";
    var binary = { status: 200, headers: { "content-type": "application/octet-stream" } };
    if (url.indexOf(prefix) === 0) {
      var node = document.getElementById(decodeURIComponent(url.slice(prefix.length)));
      if (!node) {
        return Promise.resolve(new Response("", { status: 404, statusText: "Missing embedded surface asset" }));
      }
      var compression = node.getAttribute("data-compression") || "none";
      return decompress(base64Bytes(node.textContent || ""), compression).then(function (bytes) {
        return new Response(bytes, binary);
      });
    }
    return window.fetch(input, init).then(function (response) {
      if (!response.ok || !/\.gz(?:$|[?#])/.test(url)) return response;
      return response.arrayBuffer().then(function (buffer) {
        return decompress(new Uint8Array(buffer), "gzip");
      }).then(function (bytes) {
        return new Response(bytes, binary);
      });
    });
  }

  // Snapshot helpers --------------------------------------------------------
  function layerFor(snapshot, layerId) {
    if (!snapshot) return null;
    for (var i = 0; i < snapshot.surfaces.length; i += 1) {
      var layers = snapshot.surfaces[i].layers;
      for (var j = 0; j < layers.length; j += 1) {
        if (layers[j].id === layerId) return layers[j];
      }
    }
    return null;
  }

  function addresses(snapshot, layerId) {
    var out = [];
    (snapshot.surfaces || []).forEach(function (surface) {
      if (surface.layers.some(function (layer) { return layer.id === layerId; })) {
        out.push({ surfaceId: surface.id, layerId: layerId });
      }
    });
    return out;
  }

  // Colour bar --------------------------------------------------------------
  function colorStops(colorMapId) {
    var api = window.surfview && window.surfview.ColorMap;
    if (!api || typeof api.generatePreset !== "function") return null;
    try {
      return api.generatePreset(colorMapId, 256);
    } catch (error) {
      return null;
    }
  }

  function channel(value) {
    var v = Number(value);
    return Math.round(Math.max(0, Math.min(1, v > 1 ? v / 255 : v)) * 255);
  }

  function drawLegend(host, layer) {
    var box = part(host, "legend");
    if (!box) return;
    var scalar = layer && layer.scalarMapping;
    var stops = scalar && colorStops(scalar.colorMap.id);
    var range = scalar && display.pair(scalar.displayRange.value);
    if (!stops || !stops.length || !range || !(range[1] > range[0])) {
      box.hidden = true;
      host.__nmSurfaceLegendRange = null;
      return;
    }
    box.hidden = false;
    host.__nmSurfaceLegendRange = range;
    var mask = display.pair(scalar.maskInterval.value);
    var canvas = box.querySelector("canvas");
    var width = 256;
    var height = 8;
    canvas.width = width;
    canvas.height = height;
    var ctx = canvas.getContext("2d");
    var image = ctx.createImageData(width, height);
    for (var x = 0; x < width; x += 1) {
      var value = range[0] + (x + 0.5) / width * (range[1] - range[0]);
      var hidden = mask && mask[1] > mask[0] && value >= mask[0] && value <= mask[1];
      var color = stops[Math.min(stops.length - 1, Math.floor(x / width * stops.length))];
      for (var y = 0; y < height; y += 1) {
        var o = (y * width + x) * 4;
        if (hidden) {
          // Hatched band: "not shown", distinct from any data colour.
          var stripe = ((x + y * 3) % 12) < 3;
          var g = stripe ? 196 : 232;
          image.data[o] = g; image.data[o + 1] = g; image.data[o + 2] = g;
        } else {
          image.data[o] = channel(color[0]);
          image.data[o + 1] = channel(color[1]);
          image.data[o + 2] = channel(color[2]);
        }
        image.data[o + 3] = 255;
      }
    }
    ctx.putImageData(image, 0, 0);

    var legend = legendFor(host);
    var title = legend.title || layer.label || "";
    var units = legend.units || "";
    box.querySelector("[data-nm-legend-title]").textContent = title;
    // "Z-statistic" already names its unit "z"; repeat units only when they add information.
    box.querySelector("[data-nm-legend-units]").textContent =
      units && title.toLowerCase().indexOf(units.toLowerCase()) === 0 ? "" : units;
    var ticks = box.querySelector("[data-nm-legend-ticks]");
    // End labels hang inward from the bar ends, so keep about 48 CSS px
    // between tick positions at any legend width.
    var gap = Math.min(0.45, 48 / Math.max(1, ticks.clientWidth || 160));
    ticks.replaceChildren.apply(ticks, display.legendTicks(range, mask, gap).map(function (tick) {
      var node = document.createElement("span");
      var position = (tick - range[0]) / (range[1] - range[0]);
      node.style.left = (position * 100) + "%";
      if (position < 0.02) node.style.transform = "none";
      if (position > 0.98) node.style.transform = "translateX(-100%)";
      node.textContent = fmt(tick);
      return node;
    }));
    var note = box.querySelector("[data-nm-legend-note]");
    var symmetricMask = mask && Math.abs(mask[0] + mask[1]) < 1e-9 * Math.max(1, Math.abs(mask[1]));
    // A symmetric mask is already stated in the header ("|z| > 3.1") and drawn
    // as the hatched band; only an asymmetric one needs words here.
    if (mask && mask[1] > mask[0] && !symmetricMask) {
      note.hidden = false;
      note.textContent = mask[0] <= range[0]
        ? "Values below " + fmt(mask[1]) + " hidden"
        : mask[1] >= range[1]
          ? "Values above " + fmt(mask[0]) + " hidden"
          : "Hidden between " + fmt(mask[0]) + " and " + fmt(mask[1]);
    } else {
      note.hidden = true;
    }
  }

  function markLegend(host, value) {
    var marker = host.querySelector(".nm-sv-marker");
    var range = host.__nmSurfaceLegendRange;
    if (!marker) return;
    if (value === null || !range || !Number.isFinite(value)) {
      marker.hidden = true;
      return;
    }
    var position = Math.max(0, Math.min(1, (value - range[0]) / (range[1] - range[0])));
    marker.style.left = (position * 100) + "%";
    marker.hidden = false;
  }

  // Display popover ---------------------------------------------------------
  function setNumber(host, name, value) {
    var input = part(host, name);
    if (input && Number.isFinite(value) && document.activeElement !== input) {
      input.value = display.formatNumber(value);
    }
  }

  function isSymmetric(mask) {
    return mask && Math.abs(mask[0] + mask[1]) < 1e-9 * Math.max(1, Math.abs(mask[1]));
  }

  function renderControls(host, snapshot, layerId) {
    var layer = layerFor(snapshot, layerId);
    if (!layer) return;
    host.__nmSurfaceLayer = layer;
    drawLegend(host, layer);
    var defaults = host.__nmSurfaceDisplayStore.capture(layer);
    var scalar = layer.scalarMapping;
    var palette = part(host, "palette");
    if (palette && scalar) {
      var options = (scalar.availableColorMaps || []).filter(function (option) {
        return option.availability && option.availability.enabled;
      });
      var signature = options.map(function (option) { return option.id; }).join("\u0001");
      if (palette.getAttribute("data-options") !== signature) {
        palette.setAttribute("data-options", signature);
        palette.replaceChildren.apply(palette, options.map(function (option) {
          var node = document.createElement("option");
          node.value = option.id;
          node.textContent = option.label;
          return node;
        }));
      }
      palette.value = scalar.colorMap.id;
    }
    if (scalar) {
      var mask = display.pair(scalar.maskInterval.value);
      setNumber(host, "range-low", scalar.displayRange.value[0]);
      setNumber(host, "range-high", scalar.displayRange.value[1]);
      setNumber(host, "threshold-low", mask[0]);
      setNumber(host, "threshold-high", mask[1]);
      setNumber(host, "threshold-abs", Math.max(Math.abs(mask[0]), Math.abs(mask[1])));
      var fieldset = host.querySelector(".nm-sv-threshold");
      if (fieldset && !fieldset.hasAttribute("data-user-mode")) {
        setThresholdMode(host, isSymmetric(mask) ? "symmetric" : "asymmetric");
      }
      var symbol = part(host, "threshold-symbol");
      if (symbol) symbol.textContent = "|" + symbolFor(legendFor(host)) + "| above";
    }
    var opacity = part(host, "opacity");
    if (opacity) opacity.value = String(layer.opacity);
    var opacityValue = part(host, "opacity-value");
    if (opacityValue) opacityValue.value = Math.round(Number(layer.opacity) * 100) + "%";
    var modified = display.isModified(layer, defaults);
    host.setAttribute("data-nm-surface-modified", modified ? "true" : "false");
    var chip = part(host, "modified");
    if (chip) chip.hidden = !modified;
    renderMeta(host, layer);
  }

  function renderMeta(host, layer) {
    var meta = part(host, "meta");
    if (!meta || !layer || !layer.scalarMapping) return;
    var mask = display.pair(layer.scalarMapping.maskInterval.value);
    var legend = legendFor(host);
    var unit = legend.units ? " " + legend.units : "";
    var symbol = symbolFor(legend);
    var suffix = symbol === "value" ? unit : "";
    if (mask && mask[1] > mask[0]) {
      meta.textContent = isSymmetric(mask)
        ? "Shown where |" + symbol + "| > " + fmt(mask[1]) + suffix
        : "Hidden from " + fmt(mask[0]) + " to " + fmt(mask[1]) + unit;
    } else {
      meta.textContent = "All values shown";
    }
  }

  function reportResult(host, result) {
    if (result && result.ok) return true;
    flashError(host, "That display change was rejected: " +
      ((result && result.message) || "unknown error"));
    return false;
  }

  function withTarget(host, callback) {
    var handle = host.__nmSurfaceHandle;
    var target = handle && handle.controlTarget;
    if (!target) return false;
    var snapshot = target.getSnapshot();
    return addresses(snapshot, currentLayerId(host)).every(function (address) {
      return reportResult(host, callback(target, address));
    });
  }

  function applyScalar(host, update) {
    return withTarget(host, function (target, address) {
      return target.updateScalarMapping(address, update);
    });
  }

  function applyOpacity(host, value) {
    return withTarget(host, function (target, address) {
      return target.setLayerOpacity(address, value);
    });
  }

  function resetDisplay(host) {
    var defaults = host.__nmSurfaceDisplayStore.get(currentLayerId(host));
    if (!defaults) return;
    var fieldset = host.querySelector(".nm-sv-threshold");
    if (fieldset) fieldset.removeAttribute("data-user-mode");
    applyScalar(host, {
      colorMapId: defaults.colorMapId,
      displayRange: defaults.displayRange,
      maskInterval: defaults.maskInterval
    });
    applyOpacity(host, defaults.opacity);
  }

  function setThresholdMode(host, mode) {
    var fieldset = host.querySelector(".nm-sv-threshold");
    if (!fieldset) return;
    fieldset.setAttribute("data-nm-surface-threshold-mode", mode);
    var toggle = part(host, "threshold-toggle");
    if (toggle) {
      toggle.textContent = mode === "symmetric"
        ? "Set each side separately"
        : "Use one magnitude for both signs";
    }
  }

  // Inputs apply as the reader types (debounced) and once more on commit.
  function debounced(host, key, callback) {
    host.__nmSurfaceTimers = host.__nmSurfaceTimers || {};
    return function () {
      window.clearTimeout(host.__nmSurfaceTimers[key]);
      host.__nmSurfaceTimers[key] = window.setTimeout(callback, 220);
    };
  }

  function readPair(host, low, high) {
    return display.orderedFinitePair(part(host, low).value, part(host, high).value);
  }

  // Keyboard opening moves focus to the first field; pointer opening focuses
  // the panel itself so no heavy focus ring lands on a control.
  function setPopover(host, open, restoreFocus, viaKeyboard) {
    var panel = part(host, "display-controls");
    var toggle = part(host, "display-toggle");
    if (!panel || !toggle) return;
    if (!open && panel.hidden) return;
    panel.hidden = !open;
    toggle.setAttribute("aria-expanded", open ? "true" : "false");
    if (open) {
      var first = viaKeyboard ? panel.querySelector("select, input, button:not(.nm-sv-close)") : null;
      (first || panel).focus({ preventScroll: true });
    } else if (restoreFocus) {
      toggle.focus({ preventScroll: true });
    }
  }

  function bindDisplayControls(host) {
    var palette = part(host, "palette");
    if (palette) {
      palette.addEventListener("change", function () {
        applyScalar(host, { colorMapId: palette.value });
      });
    }
    var applyRange = function (commit) {
      var value = readPair(host, "range-low", "range-high");
      if (value) applyScalar(host, { displayRange: value });
      else if (commit) reportResult(host, { ok: false, message: "enter two finite values, low first" });
    };
    ["range-low", "range-high"].forEach(function (name) {
      var input = part(host, name);
      if (!input) return;
      input.addEventListener("input", debounced(host, "range", function () { applyRange(false); }));
      input.addEventListener("change", function () { applyRange(true); });
    });
    var applyAbs = function () {
      var value = Math.abs(Number(part(host, "threshold-abs").value));
      if (Number.isFinite(value)) applyScalar(host, { maskInterval: [-value, value] });
    };
    var abs = part(host, "threshold-abs");
    if (abs) {
      abs.addEventListener("input", debounced(host, "abs", applyAbs));
      abs.addEventListener("change", applyAbs);
    }
    var applyMask = function (commit) {
      var value = readPair(host, "threshold-low", "threshold-high");
      if (value) applyScalar(host, { maskInterval: value });
      else if (commit) reportResult(host, { ok: false, message: "enter two finite values, low first" });
    };
    ["threshold-low", "threshold-high"].forEach(function (name) {
      var input = part(host, name);
      if (!input) return;
      input.addEventListener("input", debounced(host, "mask", function () { applyMask(false); }));
      input.addEventListener("change", function () { applyMask(true); });
    });
    var modeToggle = part(host, "threshold-toggle");
    if (modeToggle) {
      modeToggle.addEventListener("click", function () {
        var fieldset = host.querySelector(".nm-sv-threshold");
        var next = fieldset.getAttribute("data-nm-surface-threshold-mode") === "symmetric"
          ? "asymmetric" : "symmetric";
        fieldset.setAttribute("data-user-mode", next);
        setThresholdMode(host, next);
        if (next === "symmetric") applyAbs();
      });
    }
    var opacity = part(host, "opacity");
    if (opacity) {
      opacity.addEventListener("input", function () {
        var value = Number(opacity.value);
        if (Number.isFinite(value)) applyOpacity(host, value);
      });
    }
    host.querySelectorAll("[data-nm-surface-reset-display]").forEach(function (button) {
      button.addEventListener("click", function () { resetDisplay(host); });
    });
    var toggle = part(host, "display-toggle");
    if (toggle) {
      toggle.addEventListener("click", function (event) {
        var keyboard = event.detail === 0;
        setPopover(host, toggle.getAttribute("aria-expanded") !== "true", keyboard, keyboard);
      });
    }
    var close = part(host, "display-close");
    // Focus returns to the toggle only for keyboard closes, so a pointer
    // close leaves no stray focus ring.
    if (close) close.addEventListener("click", function (event) { setPopover(host, false, event.detail === 0); });
    var panel = part(host, "display-controls");
    if (panel) {
      // Tabbing out of the panel closes it rather than leaving it over the brain.
      panel.addEventListener("focusout", function (event) {
        var next = event.relatedTarget;
        if (!next || panel.contains(next) || (toggle && toggle.contains(next))) return;
        setPopover(host, false, false);
      });
    }
    var select = part(host, "map-select");
    if (select) {
      select.addEventListener("change", function () {
        selectMap(host, select.value);
        requestReportMap(host, select.value);
      });
    }
  }

  // One document listener closes whichever popover the pointer left.
  function closePopoversOutside(event) {
    document.querySelectorAll("[data-nm-surface-host]").forEach(function (host) {
      var panel = part(host, "display-controls");
      var toggle = part(host, "display-toggle");
      if (!panel || panel.hidden) return;
      if (panel.contains(event.target) || (toggle && toggle.contains(event.target))) return;
      setPopover(host, false, false);
    });
  }

  // Views -------------------------------------------------------------------
  function viewSpecs(host) {
    return isAnatomical(host) ? BRAIN_VIEWS : SPLIT_VIEWS;
  }

  function buildViews(host) {
    var bar = part(host, "views");
    if (!bar || bar.childElementCount) return;
    var index = 0;
    viewSpecs(host).forEach(function (spec) {
      if (spec.sep) {
        var sep = document.createElement("span");
        sep.className = "nm-sv-view-sep";
        sep.setAttribute("aria-hidden", "true");
        bar.appendChild(sep);
        return;
      }
      index += 1;
      var button = document.createElement("button");
      button.type = "button";
      button.className = "nm-sv-view";
      button.textContent = spec.label;
      button.title = spec.title + " (" + index + ")";
      button.setAttribute("data-view", spec.id);
      button.setAttribute("aria-pressed", "false");
      button.tabIndex = index === 1 ? 0 : -1;
      button.addEventListener("click", function () { setView(host, spec.id); });
      bar.appendChild(button);
    });
    // One tab stop for the whole bar; arrow keys move between views.
    bar.addEventListener("keydown", function (event) {
      var buttons = Array.prototype.slice.call(bar.querySelectorAll(".nm-sv-view"));
      var index = buttons.indexOf(document.activeElement);
      if (index < 0) return;
      var next = event.key === "ArrowRight" ? index + 1
        : event.key === "ArrowLeft" ? index - 1
          : event.key === "Home" ? 0
            : event.key === "End" ? buttons.length - 1 : null;
      if (next === null) return;
      event.preventDefault();
      next = (next + buttons.length) % buttons.length;
      buttons[index].tabIndex = -1;
      buttons[next].tabIndex = 0;
      buttons[next].focus();
    });
  }

  function announce(host, text) {
    var live = part(host, "live");
    if (live) live.textContent = text;
  }

  function markView(host, view) {
    var buttons = host.querySelectorAll(".nm-sv-view");
    var active = null;
    buttons.forEach(function (button) {
      var pressed = button.getAttribute("data-view") === view;
      button.setAttribute("aria-pressed", pressed ? "true" : "false");
      if (pressed) active = button;
    });
    if (active && !host.querySelector(".nm-sv-views").contains(document.activeElement)) {
      buttons.forEach(function (button) { button.tabIndex = button === active ? 0 : -1; });
    }
  }

  function initialView(host) {
    return isAnatomical(host) ? host.getAttribute("data-nm-surface-view") || "oblique" : "lateral";
  }

  function setView(host, view) {
    var handle = host.__nmSurfaceHandle;
    if (!handle) return;
    try {
      if (isAnatomical(host)) handle.setBrainView(view);
      else handle.setView(view);
      host.__nmSurfaceView = view;
      markView(host, view);
      var spec = viewSpecs(host).filter(function (item) { return item.id === view; })[0];
      if (spec) announce(host, "View: " + spec.title);
    } catch (error) {
      flashError(host, "That view is unavailable: " + error.message);
    }
  }

  function resetView(host) {
    var handle = host.__nmSurfaceHandle;
    if (!handle) return;
    handle.resetView();
    host.__nmSurfaceView = initialView(host);
    markView(host, host.__nmSurfaceView);
  }

  function exportPNG(host) {
    var handle = host.__nmSurfaceHandle;
    var target = handle && handle.controlTarget;
    if (!target || typeof target.exportFigure !== "function") return;
    target.exportFigure().then(function (result) {
      if (!reportResult(host, result)) return;
      var name = [
        host.getAttribute("data-nm-surface-analysis"),
        host.getAttribute("data-nm-surface-map"),
        host.__nmSurfaceView || "custom"
      ].filter(Boolean).join("_").replace(/[^A-Za-z0-9_.-]+/g, "-");
      var anchor = document.createElement("a");
      anchor.href = result.value.dataUrl;
      anchor.download = name + ".png";
      document.body.appendChild(anchor);
      anchor.click();
      anchor.remove();
      announce(host, "Saved " + name + ".png");
      var button = part(host, "export");
      if (button) {
        button.setAttribute("data-done", "true");
        button.title = "Saved " + name + ".png";
        window.setTimeout(function () {
          button.removeAttribute("data-done");
          button.title = "Save PNG";
        }, 1600);
      }
    }).catch(function (error) {
      reportResult(host, { ok: false, message: error.message });
    });
  }

  // Camera nudges for the keyboard: orbit about the target and dolly.
  function orbit(host, yawDegrees, pitchDegrees) {
    var viewer = host.__nmSurfaceHandle && host.__nmSurfaceHandle.viewer;
    if (!viewer || !viewer.camera || !viewer.cameraControls) return;
    var camera = viewer.camera;
    var target = viewer.cameraControls.target;
    var offset = camera.position.clone().sub(target);
    var up = camera.up.clone().normalize();
    if (yawDegrees) offset.applyAxisAngle(up, yawDegrees * Math.PI / 180);
    if (pitchDegrees) {
      var right = up.clone().cross(offset).normalize();
      offset.applyAxisAngle(right, pitchDegrees * Math.PI / 180);
      camera.up.applyAxisAngle(right, pitchDegrees * Math.PI / 180);
    }
    camera.position.copy(target).add(offset);
    camera.lookAt(target);
    viewer.cameraControls.update();
    viewer.requestRender();
    host.__nmSurfaceView = null;
    markView(host, null);
  }

  function dolly(host, factor) {
    var viewer = host.__nmSurfaceHandle && host.__nmSurfaceHandle.viewer;
    if (!viewer || !viewer.camera || !viewer.cameraControls) return;
    var target = viewer.cameraControls.target;
    var offset = viewer.camera.position.clone().sub(target).multiplyScalar(factor);
    viewer.camera.position.copy(target).add(offset);
    viewer.cameraControls.update();
    viewer.requestRender();
  }

  function bindKeys(host) {
    var stage = host.querySelector(".nm-sv-stage");
    if (!stage) return;
    stage.addEventListener("keydown", function (event) {
      if (event.target !== stage || event.altKey || event.ctrlKey || event.metaKey) return;
      var specs = viewSpecs(host).filter(function (spec) { return !spec.sep; });
      var number = Number(event.key);
      var handled = true;
      if (Number.isInteger(number) && number >= 1 && number <= specs.length) {
        setView(host, specs[number - 1].id);
      } else if (event.key === "0") {
        resetView(host);
      } else if (event.key === "ArrowLeft") {
        orbit(host, -15, 0);
      } else if (event.key === "ArrowRight") {
        orbit(host, 15, 0);
      } else if (event.key === "ArrowUp") {
        orbit(host, 0, -15);
      } else if (event.key === "ArrowDown") {
        orbit(host, 0, 15);
      } else if (event.key === "+" || event.key === "=") {
        dolly(host, 0.85);
      } else if (event.key === "-" || event.key === "_") {
        dolly(host, 1 / 0.85);
      } else if (event.key === "s" || event.key === "S") {
        exportPNG(host);
      } else {
        handled = false;
      }
      if (handled) {
        event.preventDefault();
        dismissHint(host);
      }
    });
    host.addEventListener("keydown", function (event) {
      var panel = part(host, "display-controls");
      if (event.key === "Escape" && panel && !panel.hidden) setPopover(host, false, true);
    });
  }

  // Hover readout -------------------------------------------------------------
  function hideReadout(host) {
    var readout = part(host, "readout");
    if (readout) readout.hidden = true;
    markLegend(host, null);
  }

  function showReadout(host, clientX, clientY) {
    var handle = host.__nmSurfaceHandle;
    var viewer = handle && handle.viewer;
    var readout = part(host, "readout");
    if (!viewer || !readout || typeof viewer.pick !== "function") return;
    var hit = viewer.pick({ x: clientX, y: clientY, useGPU: false });
    if (!hit || hit.surfaceId === null || hit.vertexIndex === null) {
      hideReadout(host);
      return;
    }
    var inspection = viewer.inspectVertex(hit.surfaceId, hit.vertexIndex);
    var layerId = currentLayerId(host);
    var entry = inspection && inspection.values
      ? inspection.values.filter(function (item) { return item.layerId === layerId; })[0]
      : null;
    var value = entry && typeof entry.value === "number" ? entry.value : null;
    var legend = legendFor(host);
    var symbol = symbolFor(legend);
    var layer = host.__nmSurfaceLayer;
    var mask = layer && layer.scalarMapping && display.pair(layer.scalarMapping.maskInterval.value);
    var shown = value !== null && !(mask && mask[1] > mask[0] && value >= mask[0] && value <= mask[1]);
    var hemisphere = /^l/i.test(hit.surfaceId) ? "Left hemisphere" : /^r/i.test(hit.surfaceId) ? "Right hemisphere" : hit.surfaceId;
    readout.replaceChildren();
    var line = document.createElement("b");
    line.textContent = value === null
      ? "No data"
      : (symbol === "value" ? "" : symbol + " = ") + fmt(value) + (symbol === "value" && legend.units ? " " + legend.units : "");
    var sub = document.createElement("span");
    sub.textContent = hemisphere + (value !== null && !shown ? " · below threshold" : "");
    readout.append(line, sub);
    var box = host.querySelector(".nm-sv-stage").getBoundingClientRect();
    var x = clientX - box.left + 14;
    var y = clientY - box.top + 14;
    readout.hidden = false;
    var width = readout.offsetWidth;
    var height = readout.offsetHeight;
    if (x + width > box.width - 8) x = clientX - box.left - width - 14;
    if (y + height > box.height - 8) y = clientY - box.top - height - 14;
    readout.style.transform = "translate(" + Math.round(x) + "px," + Math.round(y) + "px)";
    markLegend(host, value);
  }

  function dismissHint(host) {
    var hint = part(host, "hint");
    if (hint) hint.setAttribute("data-dismissed", "true");
  }

  function bindPointer(host) {
    var stage = host.querySelector(".nm-sv-stage");
    if (!stage) return;
    var frame = 0;
    var last = null;
    stage.addEventListener("pointermove", function (event) {
      if (event.pointerType === "touch" || host.__nmSurfaceDragging) return;
      if (event.target.tagName !== "CANVAS") {
        hideReadout(host);
        return;
      }
      last = event;
      if (frame) return;
      frame = window.requestAnimationFrame(function () {
        frame = 0;
        if (last) showReadout(host, last.clientX, last.clientY);
      });
    });
    stage.addEventListener("pointerleave", function () { hideReadout(host); });
    stage.addEventListener("pointerdown", function (event) {
      if (event.target.tagName !== "CANVAS") return;
      host.__nmSurfaceDragging = true;
      host.__nmSurfaceDragStart = [event.clientX, event.clientY];
      hideReadout(host);
      dismissHint(host);
    });
    window.addEventListener("pointerup", function (event) {
      if (!host.__nmSurfaceDragging) return;
      host.__nmSurfaceDragging = false;
      var start = host.__nmSurfaceDragStart || [event.clientX, event.clientY];
      var moved = Math.abs(event.clientX - start[0]) + Math.abs(event.clientY - start[1]) > 3;
      if (moved && isAnatomical(host)) {
        host.__nmSurfaceView = null;
        markView(host, null);
      } else if (!moved && event.pointerType === "touch") {
        showReadout(host, event.clientX, event.clientY);
      }
    });
    stage.addEventListener("dblclick", function (event) {
      if (event.target.tagName === "CANVAS") resetView(host);
    });
  }

  // Touch: a page-scrolling thumb must not become a rotation. The canvas is
  // inert until the reader taps "explore"; "Done" (or tapping outside) returns
  // the stage to page scrolling.
  function bindTouchGate(host) {
    var stage = host.querySelector(".nm-sv-stage");
    var gate = part(host, "touch-gate");
    var done = part(host, "touch-done");
    if (!stage || !gate || !done || !window.matchMedia("(pointer: coarse)").matches) return;
    function lock(locked) {
      stage.setAttribute("data-locked", locked ? "true" : "false");
      gate.hidden = !locked;
      done.hidden = locked;
      if (locked) hideReadout(host);
    }
    gate.addEventListener("click", function () { lock(false); });
    done.addEventListener("click", function () { lock(true); });
    document.addEventListener("pointerdown", function (event) {
      if (stage.getAttribute("data-locked") === "false" && !stage.contains(event.target)) lock(true);
    });
    lock(true);
  }

  // Mounting ----------------------------------------------------------------
  function connectHandle(host, handle) {
    host.__nmSurfaceHandle = handle;
    var widget = host.querySelector(".surfwidget");
    if (widget) widget.__surfviewHandle = handle;
    return Promise.resolve(handle.ready).then(function () {
      var ground = host.getAttribute("data-nm-surface-ground");
      if (ground && handle.viewer && typeof handle.viewer.setFigureBackground === "function") {
        handle.viewer.setFigureBackground(parseInt(ground.replace("#", ""), 16));
      }
      selectMap(host, host.getAttribute("data-nm-surface-map"));
      buildViews(host);
      host.__nmSurfaceView = initialView(host);
      markView(host, host.__nmSurfaceView);
      var target = handle.controlTarget;
      if (target && typeof target.subscribe === "function") {
        var reverse = parse(host, "data-nm-surface-layer-to-map");
        host.__nmSurfaceSubscription = target.subscribe(function (snapshot) {
          var exclusive = snapshot && snapshot.capabilities && snapshot.capabilities.exclusiveMap;
          var layerId = exclusive && exclusive.displayedLayerId;
          var mapId = layerId && reverse[layerId];
          if (mapId && mapId !== host.getAttribute("data-nm-surface-map")) {
            setTarget(host, mapId);
            requestReportMap(host, mapId);
          }
          if (layerId) renderControls(host, snapshot, layerId);
        });
        renderControls(host, target.getSnapshot(), currentLayerId(host));
      }
      var stage = host.querySelector(".nm-sv-stage");
      if (stage && typeof ResizeObserver === "function" && typeof handle.setFitInsets === "function") {
        var width = 0;
        new ResizeObserver(function (entries) {
          var next = Math.round(entries[0].contentRect.width);
          if (!next || next === width) return;
          width = next;
          window.requestAnimationFrame(function () { handle.setFitInsets(fitInsets(host)); });
        }).observe(stage);
      }
      setStatus(host, null);
      host.setAttribute("data-nm-surface-ready", "true");
    });
  }

  // "auto" (data-driven) or "azimuth,elevation" in degrees.
  function parseOblique(value) {
    if (!value || value === "auto") return "auto";
    var parts = value.split(",").map(Number);
    return parts.length === 2 && parts.every(Number.isFinite)
      ? { azimuth: parts[0], elevation: parts[1] }
      : "auto";
  }

  // Keep the fitted brain clear of the controls floating over the canvas: the
  // title block and actions at the top, and on narrow stages the colour bar.
  function fitInsets(host) {
    var stage = host.querySelector(".nm-sv-stage");
    if (!stage) return { top: 0, bottom: 0 };
    var box = stage.getBoundingClientRect();
    var top = 0;
    [".nm-sv-head", ".nm-sv-actions"].forEach(function (selector) {
      var node = host.querySelector(selector);
      if (node) top = Math.max(top, node.getBoundingClientRect().bottom - box.top);
    });
    var bottom = 0;
    // A colour bar that spans much of a narrow stage would sit on the brain;
    // reserve its height too. On wide stages it only occupies a corner.
    var legend = part(host, "legend");
    if (legend) {
      var wasHidden = legend.hidden;
      legend.hidden = false;
      var rect = legend.getBoundingClientRect();
      legend.hidden = wasHidden;
      if (rect.width > 0.35 * box.width) bottom = Math.max(bottom, box.bottom - rect.top + 4);
    }
    return { top: Math.max(0, Math.round(top)), bottom: Math.max(0, Math.round(bottom)) };
  }

  function mount(host) {
    if (host.__nmSurfaceMount) return host.__nmSurfaceMount;
    setStatus(host, "loading", "Loading surface");
    host.__nmSurfaceMount = Promise.resolve().then(function () {
      if (!window.surfview || typeof window.surfview.mountSurfView !== "function") {
        throw new Error("The surfview runtime did not load.");
      }
      var manifestNode = document.getElementById(host.getAttribute("data-nm-surface-manifest"));
      if (!manifestNode) throw new Error("The surface scene manifest is missing.");
      var manifest = JSON.parse(manifestNode.textContent || "{}");
      manifest.selectedLayer = currentLayerId(host) || manifest.selectedLayer;
      var widget = host.querySelector(".surfwidget");
      var layout = host.getAttribute("data-nm-surface-layout") || "split";
      var options = {
        lazy: false,
        preset: host.getAttribute("data-nm-surface-preset") || "report",
        layout: layout,
        controls: false,
        mode: "report",
        baseUrl: document.baseURI,
        fetcher: surfaceFetch,
        fitInsets: fitInsets(host),
        onError: function (error) { showFallback(host, error); }
      };
      var bilateral = parse(host, "data-nm-surface-bilateral");
      if (bilateral && bilateral.id) options.bilateralGroup = bilateral;
      if (layout === "anatomical") {
        options.initialBrainView = host.getAttribute("data-nm-surface-view") || "oblique";
        options.oblique = parseOblique(host.getAttribute("data-nm-surface-oblique"));
        options.fov = 22;
      }
      return connectHandle(host, window.surfview.mountSurfView(widget, manifest, options));
    }).catch(function (error) {
      showFallback(host, error);
      throw error;
    });
    return host.__nmSurfaceMount;
  }

  function isShown(host) {
    return !host.hidden && host.getClientRects().length > 0;
  }

  function installRouter(host) {
    var analysisId = host.getAttribute("data-nm-surface-analysis");
    if (document.querySelector('[data-nm-view-router="' + CSS.escape(analysisId) + '"]')) return;
    var volume = document.querySelector('[data-nm-volume-analysis="' + CSS.escape(analysisId) + '"]');
    var router = document.createElement("fieldset");
    router.className = "nm-view-router";
    router.setAttribute("data-nm-view-router", analysisId);
    var legend = document.createElement("legend");
    legend.textContent = "View";
    router.appendChild(legend);
    var name = "nm-view-" + analysisId.replace(/[^A-Za-z0-9_-]/g, "-");
    function add(value, label) {
      var wrapper = document.createElement("label");
      var input = document.createElement("input");
      input.type = "radio";
      input.name = name;
      input.value = value;
      input.checked = value === "static";
      wrapper.append(input, document.createTextNode(label));
      router.appendChild(wrapper);
      return input;
    }
    var choices = [add("static", "Static")];
    if (volume) choices.push(add("slices", "Slices"));
    choices.push(add("surface", "3D surface"));
    function choose(view) {
      host.hidden = view !== "surface";
      if (volume) volume.hidden = view !== "slices";
      if (view === "surface") {
        mount(host).then(function () {
          if (host.__nmSurfaceHandle) {
            window.requestAnimationFrame(function () { host.__nmSurfaceHandle.resize(); });
          }
        }).catch(function () {});
      }
      if (view === "slices" && volume) {
        var toggle = volume.querySelector("[data-nm-volume-toggle]");
        if (toggle && toggle.getAttribute("aria-expanded") !== "true") toggle.click();
      }
    }
    choices.forEach(function (choice) {
      choice.addEventListener("change", function () {
        if (choice.checked) choose(choice.value);
      });
    });
    var anchor = volume && volume.compareDocumentPosition(host) & Node.DOCUMENT_POSITION_FOLLOWING
      ? volume
      : host;
    anchor.parentNode.insertBefore(router, anchor);
    choose("static");
  }

  function bind(host) {
    if (host.__nmSurfaceBound) return;
    host.__nmSurfaceBound = true;
    if (!display) {
      showFallback(host, new Error("The surface display helper did not load."));
      return;
    }
    host.__nmSurfaceDisplayStore = display.createDefaultStore();
    bindDisplayControls(host);
    bindKeys(host);
    bindPointer(host);
    bindTouchGate(host);
    var reset = part(host, "reset-view");
    if (reset) reset.addEventListener("click", function () { resetView(host); });
    var png = part(host, "export");
    if (png) png.addEventListener("click", function () { exportPNG(host); });
    var bar = host.querySelector(".nm-sv-legend canvas");
    if (bar && !bar.parentNode.classList.contains("nm-sv-barwrap")) {
      var wrap = document.createElement("div");
      wrap.className = "nm-sv-barwrap";
      bar.parentNode.insertBefore(wrap, bar);
      wrap.appendChild(bar);
      var marker = document.createElement("span");
      marker.className = "nm-sv-marker";
      marker.hidden = true;
      wrap.appendChild(marker);
    }
    // Build the view bar before mounting so the fit insets can measure it.
    buildViews(host);
    installRouter(host);
    if (isShown(host)) mount(host).catch(function () {});
  }

  function initialize() {
    var hosts = document.querySelectorAll("[data-nm-surface-host]");
    hosts.forEach(bind);
    document.addEventListener("pointerdown", closePopoversOutside);
    document.addEventListener("nm-map-change", function (event) {
      var detail = event.detail || {};
      hosts.forEach(function (host) {
        if (host.getAttribute("data-nm-surface-analysis") === detail.analysisId) {
          selectMap(host, detail.mapId);
        }
      });
    });
    window.addEventListener("pagehide", function () {
      hosts.forEach(function (host) {
        var subscription = host.__nmSurfaceSubscription;
        if (subscription && typeof subscription.unsubscribe === "function") subscription.unsubscribe();
        var handle = host.__nmSurfaceHandle;
        if (handle && typeof handle.dispose === "function") handle.dispose();
      });
    }, { once: true });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }
}());
