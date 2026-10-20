(function () {
  "use strict";

  // neuromosaic interactive surface bridge: mounts the surfview report scene
  // into each [data-nm-surface-host], owns the floating controls, and keeps
  // the browser map selection in step with the static report selectors.
  var display = window.NeuroMosaicSurfaceDisplay;

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

  var FILE_MESSAGE = "Interactive views need the report served over HTTP \u2014 open the shared link, or ask the author for the single-file version.";

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
    if (legend.units && legend.units.length <= 3) return legend.units;
    // "Z-statistic", "z", "t-statistic": name the statistic, not "value".
    var m = /^\s*([zZtTfF])(?:[- ]?stat(?:istic)?)?\s*$/.exec(legend.title || "");
    return m ? m[1].toLowerCase() : "value";
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

  // A failed surface hands the map back to its static figures; the router
  // says why in one line. Without a router the in-viewer note explains.
  function showFallback(host, error) {
    var widget = host.querySelector(".nm-surface-widget");
    if (widget) widget.setAttribute("data-nm-surface-failed", "true");
    setStatus(host, null);
    var analysisId = host.getAttribute("data-nm-surface-analysis");
    var router = document.querySelector('[data-nm-view-router="' + CSS.escape(analysisId) + '"]');
    if (router && typeof router.__nmFallBack === "function") {
      router.__nmFallBack("surface", surfaceMessage(error));
      return;
    }
    var fallback = part(host, "author-fallback");
    if (fallback) {
      if (!fallback.__nmBase) fallback.__nmBase = fallback.textContent;
      fallback.textContent = fallback.__nmBase + " " + surfaceMessage(error);
      fallback.hidden = false;
    } else {
      setStatus(host, "error", surfaceMessage(error));
    }
  }

  // Maps --------------------------------------------------------------------
  function setTarget(host, mapId) {
    if (host.getAttribute("data-nm-surface-map") !== mapId) clearPendingEdits(host);
    host.setAttribute("data-nm-surface-map", mapId);
    var labels = parse(host, "data-nm-surface-labels");
    var label = labels[mapId] || mapId;
    host.setAttribute("aria-label", "Interactive cortical surface: " + label);
    var stage = host.querySelector(".nm-sv-stage");
    if (stage) stage.setAttribute("aria-label", "3D surface viewer: " + label);
    var title = part(host, "target");
    if (title) title.textContent = label;
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
      // A corrupt stream rejects with a generic "Failed to fetch"; say what
      // failed, so a report opened from disk is not told to use HTTP.
      return decompress(base64Bytes(node.textContent || ""), compression).then(function (bytes) {
        return new Response(bytes, binary);
      }, function () {
        throw new Error("embedded surface data could not be decoded");
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

  // Atlas parcels ---------------------------------------------------------------
  // One lookup per page: per-hemisphere uint16 parcel ids (gzip, base64) plus
  // a label table. Decoded lazily on first use and shared by every host.
  var parcelLookup = null;

  function loadParcels() {
    if (parcelLookup) return parcelLookup;
    var tableNode = document.getElementById("nm-surface-parcel-table");
    if (!tableNode) {
      parcelLookup = Promise.resolve(null);
      return parcelLookup;
    }
    var descriptor;
    try {
      descriptor = JSON.parse(tableNode.textContent || "{}");
    } catch (error) {
      parcelLookup = Promise.resolve(null);
      return parcelLookup;
    }
    var sides = Object.keys(descriptor.payloads || {});
    parcelLookup = Promise.all(sides.map(function (side) {
      var node = document.getElementById(descriptor.payloads[side]);
      if (!node) return Promise.resolve(null);
      return decompress(base64Bytes(node.textContent || ""), node.getAttribute("data-compression") || "none")
        .then(function (bytes) {
          var view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
          var ids = new Uint16Array(bytes.byteLength / 2);
          for (var i = 0; i < ids.length; i += 1) ids[i] = view.getUint16(i * 2, true);
          return ids;
        });
    })).then(function (arrays) {
      var ids = {};
      sides.forEach(function (side, index) { if (arrays[index]) ids[side] = arrays[index]; });
      return { atlas: descriptor.atlas || "", ids: ids, parcels: descriptor.parcels || {} };
    }).catch(function () { return null; });
    return parcelLookup;
  }

  function regionAt(host, surfaceId, vertex) {
    var lookup = host.__nmSurfaceParcels;
    var ids = lookup && lookup.ids[surfaceId];
    if (!ids || vertex < 0 || vertex >= ids.length) return null;
    var id = ids[vertex];
    return display.describeParcel(id, lookup.parcels[String(id)], lookup.atlas);
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
    // Report palettes are drawn from the report's own colours, so the key
    // matches the static figure's key exactly.
    if (reportPalettes[colorMapId]) {
      return reportPalettes[colorMapId].map(function (hex) { return display.hexRgb(hex) || [0, 0, 0]; });
    }
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
          // Hatched band: "not shown", in the static key's #EFEFEB / #C4C8C5.
          var stripe = ((x + y * 3) % 12) < 3;
          image.data[o] = stripe ? 196 : 239;
          image.data[o + 1] = stripe ? 200 : 239;
          image.data[o + 2] = stripe ? 197 : 235;
        } else {
          image.data[o] = channel(color[0]);
          image.data[o + 1] = channel(color[1]);
          image.data[o + 2] = channel(color[2]);
        }
        image.data[o + 3] = 255;
      }
    }
    ctx.putImageData(image, 0, 0);
    drawCaps(host, box, stops, range);

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
    ticks.replaceChildren.apply(ticks, display.legendLabels(range, mask, gap).map(function (tick) {
      var node = document.createElement("span");
      var position = (tick.value - range[0]) / (range[1] - range[0]);
      node.style.left = (position * 100) + "%";
      if (position < 0.02) node.style.transform = "none";
      if (position > 0.98) node.style.transform = "translateX(-100%)";
      node.textContent = tick.label;
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

  // Triangles at the bar ends mark data beyond the colour range, painted in
  // the end colour, as on the static key.
  function drawCaps(host, box, stops, range) {
    var data = dataRange(host, currentLayerId(host));
    var wrap = box.querySelector(".nm-sv-barwrap");
    [["low", 0], ["high", stops.length - 1]].forEach(function (end) {
      var cap = wrap.querySelector(".nm-sv-cap-" + end[0]);
      if (!cap) {
        cap = document.createElement("span");
        cap.className = "nm-sv-cap nm-sv-cap-" + end[0];
        cap.setAttribute("aria-hidden", "true");
        wrap.appendChild(cap);
      }
      var beyond = data && (end[0] === "low" ? data[0] < range[0] : data[1] > range[1]);
      cap.hidden = !beyond;
      var c = stops[end[1]];
      cap.style.background = "rgb(" + channel(c[0]) + "," + channel(c[1]) + "," + channel(c[2]) + ")";
    });
    box.setAttribute("data-caps",
      (data && data[0] < range[0] ? "low " : "") + (data && data[1] > range[1] ? "high" : ""));
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

  // Colour maps ---------------------------------------------------------------
  var REPORT_PALETTE_PREFIX = "nm-signed-";
  // Report palettes by colour-map id, as hex colours low -> high.
  var reportPalettes = {};

  function manifestLayers(host) {
    if (!host.__nmSurfaceLayers) {
      var node = document.getElementById(host.getAttribute("data-nm-surface-manifest"));
      var layers = {};
      try { layers = JSON.parse((node && node.textContent) || "{}").layers || {}; } catch (error) { layers = {}; }
      var list = Array.isArray(layers) ? layers : Object.keys(layers).map(function (key) {
        return layers[key];
      });
      host.__nmSurfaceLayers = {};
      list.forEach(function (layer) { if (layer && layer.id) host.__nmSurfaceLayers[layer.id] = layer; });
    }
    return host.__nmSurfaceLayers;
  }

  function layerMetadata(host, layerId) {
    var layer = manifestLayers(host)[layerId];
    return (layer && layer.metadata) || {};
  }

  // The finite data range of a layer, when the report recorded it.
  function dataRange(host, layerId) {
    return display.pair(layerMetadata(host, layerId).data_range);
  }

  // Register each layer's report palette (threshold-saturated, so specific
  // to its threshold and cap) before any viewer reads the preset list.
  // surfview hands colour-map values to WebGL as linear light and the
  // renderer encodes its output as sRGB, so the report's sRGB colours are
  // decoded to linear here; otherwise every overlay renders lighter (dark
  // red #B40426 would show as pink).
  function hexToLinearRgb(hex) {
    var h = String(hex).replace("#", "");
    return [0, 2, 4].map(function (i) {
      var c = parseInt(h.substr(i, 2), 16) / 255;
      return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4);
    });
  }

  function registerReportPalettes() {
    var api = window.surfview && window.surfview.ColorMap;
    var presets = api && typeof api.getPresetMaps === "function" ? api.getPresetMaps() : null;
    document.querySelectorAll("[data-nm-surface-host]").forEach(function (host) {
      var layers = manifestLayers(host);
      Object.keys(layers).forEach(function (id) {
        var layer = layers[id];
        var colors = layer.metadata && layer.metadata.palette_colors;
        if (!Array.isArray(colors) || colors.length < 2 ||
            typeof layer.colorMap !== "string" ||
            layer.colorMap.indexOf(REPORT_PALETTE_PREFIX) !== 0) return;
        reportPalettes[layer.colorMap] = colors;
        if (presets) presets[layer.colorMap] = display.bleedGap(colors).map(hexToLinearRgb);
      });
    });
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
      var range0 = defaults.displayRange;
      var signed = Boolean(range0 && range0[0] < 0 && range0[1] > 0);
      var options = display.curateColorMaps(scalar.availableColorMaps || [], scalar.colorMap.id, signed);
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
      var data = dataRange(host, layerId);
      var absInput = part(host, "threshold-abs");
      if (absInput && data) absInput.max = display.formatNumber(Math.max(Math.abs(data[0]), Math.abs(data[1])));
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
        ? "Shown where |" + symbol + "| \u2265 " + fmt(mask[1]) + suffix
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
    clearPendingEdits(host);
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
  function clearPendingEdits(host) {
    Object.keys(host.__nmSurfaceTimers || {}).forEach(function (key) {
      window.clearTimeout(host.__nmSurfaceTimers[key]);
    });
  }

  function debounced(host, key, callback) {
    host.__nmSurfaceTimers = host.__nmSurfaceTimers || {};
    return function () {
      window.clearTimeout(host.__nmSurfaceTimers[key]);
      host.__nmSurfaceTimers[key] = window.setTimeout(callback, 220);
    };
  }

  function committed(host, key, callback) {
    return function () {
      window.clearTimeout((host.__nmSurfaceTimers || {})[key]);
      callback(true);
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
      input.addEventListener("change", committed(host, "range", applyRange));
    });
    // Thresholds are clamped into the data range: a threshold above the
    // largest |value| would claim a cut no vertex can pass.
    var applyAbs = function (commit) {
      var input = part(host, "threshold-abs");
      var value = display.clampMagnitude(input.value, dataRange(host, currentLayerId(host)));
      if (value === null) return;
      if (commit === true && Number(input.value) !== value) input.value = display.formatNumber(value);
      applyScalar(host, { maskInterval: [-value, value] });
    };
    var abs = part(host, "threshold-abs");
    if (abs) {
      abs.addEventListener("input", debounced(host, "abs", applyAbs));
      abs.addEventListener("change", committed(host, "abs", applyAbs));
    }
    var applyMask = function (commit) {
      var low = part(host, "threshold-low");
      var high = part(host, "threshold-high");
      var value = display.clampInterval(low.value, high.value, dataRange(host, currentLayerId(host)));
      if (value) {
        if (commit) {
          if (Number(low.value) !== value[0]) low.value = display.formatNumber(value[0]);
          if (Number(high.value) !== value[1]) high.value = display.formatNumber(value[1]);
        }
        applyScalar(host, { maskInterval: value });
      } else if (commit) {
        reportResult(host, { ok: false, message: "enter two finite values, low first" });
      }
    };
    ["threshold-low", "threshold-high"].forEach(function (name) {
      var input = part(host, name);
      if (!input) return;
      input.addEventListener("input", debounced(host, "mask", function () { applyMask(false); }));
      input.addEventListener("change", committed(host, "mask", applyMask));
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

  // Save PNG: the current 3D view on the light report ground whatever the
  // page theme, with a title line and the report's own colour key (not the
  // engine's generic bar), so it matches the static figures on a slide.
  function composeSurfacePNG(host, shot, width, height) {
    var layer = host.__nmSurfaceLayer;
    var scalar = layer && layer.scalarMapping;
    var stops = scalar && colorStops(scalar.colorMap.id);
    var range = scalar && display.pair(scalar.displayRange.value);
    var mask = scalar && display.pair(scalar.maskInterval.value);
    var legend = legendFor(host);
    var S = 2, PAD = 24, KEY = stops && range ? 84 : 0;
    var cssW = Math.round(width / S), cssH = Math.round(height / S);
    var out = document.createElement("canvas");
    out.width = (cssW + 2 * PAD) * S;
    out.height = (PAD + 26 + cssH + KEY + PAD) * S;
    var ctx = out.getContext("2d");
    ctx.scale(S, S);
    var font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif";
    ctx.fillStyle = "#fbfbf8";
    ctx.fillRect(0, 0, out.width, out.height);
    ctx.fillStyle = "#16191b";
    ctx.font = "600 15px " + font;
    var labels = parse(host, "data-nm-surface-labels");
    var spec = viewSpecs(host).filter(function (item) { return item.id === host.__nmSurfaceView; })[0];
    ctx.fillText((labels[host.getAttribute("data-nm-surface-map")] || "") +
      (spec ? "  \u00b7  " + spec.title : ""), PAD, PAD + 14);
    ctx.drawImage(shot, PAD, PAD + 26, cssW, cssH);
    if (KEY) {
      var CAP = 9, top = PAD + 26 + cssH + 24, keyW = Math.min(360, cssW - 2 * CAP), keyH = 12;
      var KX = PAD + CAP;
      ctx.font = "600 12px " + font;
      ctx.fillText(legend.title || "Value", PAD, top - 6);
      for (var k = 0; k < keyW; k += 1) {
        var value = range[0] + ((k + 0.5) / keyW) * (range[1] - range[0]);
        var hidden = mask && mask[1] > mask[0] && value >= mask[0] && value <= mask[1];
        var c = stops[Math.min(stops.length - 1, Math.floor((k / keyW) * stops.length))];
        ctx.fillStyle = hidden ? (((k % 6) < 2) ? "#c4c8c5" : "#efefeb") :
          "rgb(" + channel(c[0]) + "," + channel(c[1]) + "," + channel(c[2]) + ")";
        ctx.fillRect(KX + k, top, 1, keyH);
      }
      ctx.strokeStyle = "#8a9296";
      ctx.lineWidth = 0.5;
      ctx.strokeRect(KX, top, keyW, keyH);
      // Cap triangles, as on the page key: drawn values beyond the colour
      // scale take its end colour.
      var data = dataRange(host, currentLayerId(host));
      var caps = [Boolean(data && data[0] < range[0]), Boolean(data && data[1] > range[1])];
      caps.forEach(function (on, end) {
        if (!on) return;
        var c = stops[end ? stops.length - 1 : 0];
        var edge = end ? KX + keyW : KX;
        ctx.beginPath();
        ctx.moveTo(edge, top);
        ctx.lineTo(end ? edge + CAP : edge - CAP, top + keyH / 2);
        ctx.lineTo(edge, top + keyH);
        ctx.closePath();
        ctx.fillStyle = "rgb(" + channel(c[0]) + "," + channel(c[1]) + "," + channel(c[2]) + ")";
        ctx.fill();
        ctx.stroke();
      });
      host.__nmLastExportCaps = caps;
      ctx.fillStyle = "#4a5256";
      ctx.font = "11px " + font;
      display.legendLabels(range, mask, 0.12).forEach(function (tick) {
        var x = KX + ((tick.value - range[0]) / (range[1] - range[0])) * keyW;
        var w = ctx.measureText(tick.label).width;
        ctx.fillText(tick.label, Math.max(PAD, Math.min(KX + keyW - w, x - w / 2)), top + keyH + 14);
      });
      var shown = mask && mask[1] > mask[0] ?
        (Math.abs(mask[0] + mask[1]) < 1e-9 ?
          "shown where |" + symbolFor(legend) + "| \u2265 " + display.formatNumber(mask[1]) :
          "hidden between " + display.formatNumber(mask[0]) + " and " + display.formatNumber(mask[1])) :
        "no threshold";
      var sourceLayer = manifestLayers(host)[currentLayerId(host)];
      var mapping = display.mappingNote(sourceLayer && sourceLayer.provenance);
      var note = host.getAttribute("data-nm-surface-modified") === "true" ?
        mapping + "; display modified in the browser (exploratory): " + shown + ". The Static view is the figure of record." :
        mapping + "; " + shown + "; report display settings.";
      host.__nmLastExportNote = note;
      ctx.fillText(note, PAD, top + keyH + 34);
    }
    return out;
  }

  // "story-gt-math_surface-left-lateral.png": the report's file stem for the
  // map (the same slug as its figure downloads) plus the view shown.
  var VIEW_SLUGS = {
    left: "left-lateral", right: "right-lateral",
    "left-medial": "left-medial", "right-medial": "right-medial", oblique: "overview"
  };
  function exportName(host) {
    var view = host.__nmSurfaceView || "custom";
    var article = host.closest && host.closest("[data-nm-file-stem]");
    var stem = article && article.getAttribute("data-nm-file-stem");
    var mapId = host.getAttribute("data-nm-surface-map");
    var primary = host.getAttribute("data-nm-surface-primary-map");
    if (!stem || (primary && mapId && mapId !== primary)) {
      stem = [host.getAttribute("data-nm-surface-analysis"), mapId].filter(Boolean).join("_");
    }
    return (stem + "_surface-" + (VIEW_SLUGS[view] || view)).replace(/[^A-Za-z0-9_.-]+/g, "-") + ".png";
  }

  function exportPNG(host) {
    var handle = host.__nmSurfaceHandle;
    var viewer = handle && handle.viewer;
    var target = handle && handle.controlTarget;
    if (!viewer || typeof viewer.exportPNG !== "function") return;
    var name = exportName(host);
    var light = cssColorInt(host.getAttribute("data-nm-surface-ground")) || 0xfbfbf8;
    var stage = host.querySelector(".nm-sv-stage").getBoundingClientRect();
    var width = 1600;
    var height = Math.max(400, Math.round(width * stage.height / Math.max(1, stage.width)));
    new Promise(function (resolve, reject) {
      var shot = new Image();
      shot.onload = function () { resolve(shot); };
      shot.onerror = function () { reject(new Error("PNG export failed.")); };
      shot.src = viewer.exportPNG({
        width: width, height: height, backgroundColor: light, transparent: false,
        colorbar: false, scaleBar: false
      });
    }).then(function (shot) {
      var canvas = composeSurfacePNG(host, shot, width, height);
      var url = canvas.toDataURL("image/png");
      host.__nmLastExport = { name: name, width: canvas.width, height: canvas.height };
      var anchor = document.createElement("a");
      anchor.href = url;
      anchor.download = name;
      document.body.appendChild(anchor);
      anchor.click();
      anchor.remove();
      announce(host, "Saved " + name);
      var button = part(host, "export");
      if (button) {
        button.setAttribute("data-done", "true");
        button.title = "Saved " + name;
        window.setTimeout(function () {
          button.removeAttribute("data-done");
          button.title = "Save PNG";
        }, 1600);
      }
    }).catch(function (error) {
      reportResult(host, { ok: false, message: error.message });
    });
    return target;
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

  // surfview's CPU pick ray-casts every surface, including a hemisphere the
  // current view hides: in a medial view the other hemisphere sits between
  // the camera and the one on screen, so the readout named its parcels.
  // Pick only surfaces that are drawn.
  // Hover values carry two decimals, as in the peak tables and the volume
  // readout ("z = -10.04", never "-10" next to "0.359").
  function fixed2(value) {
    return display.formatFixed(value);
  }

  function showReadout(host, clientX, clientY) {
    var handle = host.__nmSurfaceHandle;
    var viewer = handle && handle.viewer;
    var readout = part(host, "readout");
    if (!viewer || !readout || typeof viewer.pick !== "function") return;
    // surfview picks the nearest drawn surface (hidden hemispheres excluded).
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
      : (symbol === "value" ? "" : symbol + " = ") + fixed2(value) + (symbol === "value" && legend.units ? " " + legend.units : "");
    if (value !== null && !shown) {
      var below = document.createElement("em");
      below.textContent = " below threshold";
      line.appendChild(below);
    }
    var region = regionAt(host, hit.surfaceId, hit.vertexIndex);
    var sub = document.createElement("span");
    sub.className = "nm-sv-readout-region";
    // Parcel names carry their own side ("Control · PFCl 4 (L)").
    sub.textContent = region && /\((L|R)\)$/.test(region.name)
      ? region.name
      : hemisphere + (region ? " \u00b7 " + region.name : "");
    readout.append(line, sub);
    if (region && region.detail) {
      var detail = document.createElement("span");
      detail.className = "nm-sv-readout-detail";
      detail.textContent = region.detail;
      readout.appendChild(detail);
    }
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

  // Runtime -----------------------------------------------------------------
  // The 1.4 MB surfview runtime is not part of the first page load. The report
  // carries its URL as an htmltools attachment link (a data URI in a
  // self-contained page), and the first mount injects it once.
  var runtimeLoad = null;

  function runtimeReady() {
    return Boolean(window.surfview && typeof window.surfview.mountSurfView === "function");
  }

  function loadRuntime() {
    if (runtimeLoad) return runtimeLoad;
    if (runtimeReady()) {
      registerReportPalettes();
      runtimeLoad = Promise.resolve(window.surfview);
      return runtimeLoad;
    }
    var link = document.getElementById("surfview-runtime-attachment");
    var url = link && link.getAttribute("href");
    if (!url) return Promise.reject(new Error("The surfview runtime is not part of this report."));
    runtimeLoad = new Promise(function (resolve, reject) {
      var script = document.createElement("script");
      script.src = url;
      script.async = true;
      script.setAttribute("data-nm-surface-runtime", "");
      script.onload = function () {
        if (!runtimeReady()) {
          reject(new Error("The surfview runtime did not load."));
          return;
        }
        // Report palettes must exist before any viewer reads the preset list.
        registerReportPalettes();
        resolve(window.surfview);
      };
      script.onerror = function () {
        // A later attempt may succeed (e.g. a transient network failure).
        runtimeLoad = null;
        script.remove();
        reject(new Error("The surfview runtime could not be loaded."));
      };
      document.head.appendChild(script);
    });
    return runtimeLoad;
  }

  // Report lighting: the overlay keeps its key colours. With the palette in
  // linear light (above), an overlay that is 90% emissive renders within a
  // few CIE76 units of the key while the remaining 10% and the curvature
  // underlay keep the stock key, ambient and fill lights for shape.
  var REPORT_LIGHTING = {
    overlayEmission: 0.9,
    silhouetteDarkening: 0.15
  };
  // A near-black specular: white highlights desaturate the overlay.
  var REPORT_SPECULAR = 0x0a0a0a;

  function surfacesOf(viewer) {
    var list = viewer && viewer.surfaces;
    if (!list) return [];
    if (typeof list.values === "function" && !Array.isArray(list)) return Array.from(list.values());
    return Array.prototype.slice.call(list);
  }

  function tuneMaterials(host) {
    var viewer = host.__nmSurfaceHandle && host.__nmSurfaceHandle.viewer;
    if (!viewer || !viewer.surfaces || host.getAttribute("data-nm-surface-preset") !== "report") return;
    surfacesOf(viewer).forEach(function (surface) {
      var material = surface && surface.mesh && surface.mesh.material;
      if (material && material.specular && typeof material.specular.setHex === "function") {
        material.specular.setHex(REPORT_SPECULAR);
      }
    });
    if (typeof viewer.requestRender === "function") viewer.requestRender();
  }

  // Sulcal shading as in the static strip, whose cortex runs from grey 0.72
  // (sulci) to 0.90 (gyri) in sRGB. surfview treats curvature greys as linear
  // light, so the same greys are 0.48-0.79 here (brightness 0.635 +/- 0.155);
  // the stock preset (0.57-0.89 linear) renders nearly flat once encoded.
  var REPORT_CURVATURE = { brightness: 0.635, contrast: 0.31, smoothness: 0.2 };

  function reportStyle(name) {
    if (name !== "report" || typeof window.surfview.resolveStylePreset !== "function") return null;
    try {
      var style = window.surfview.resolveStylePreset("report");
      Object.keys(REPORT_LIGHTING).forEach(function (key) { style.lighting[key] = REPORT_LIGHTING[key]; });
      if (style.curvature) {
        Object.keys(REPORT_CURVATURE).forEach(function (key) { style.curvature[key] = REPORT_CURVATURE[key]; });
      }
      return style;
    } catch (error) {
      return null;
    }
  }

  // The canvas ground follows the stage's CSS ground, so the viewer sits on
  // the page in both light and dark themes.
  function cssColorInt(text) {
    var m = /rgba?\(\s*(\d+)[,\s]+(\d+)[,\s]+(\d+)/.exec(text || "");
    if (m) return (Number(m[1]) << 16) + (Number(m[2]) << 8) + Number(m[3]);
    var hex = display.hexRgb(text);
    return hex ? (hex[0] << 16) + (hex[1] << 8) + hex[2] : null;
  }

  function applyGround(host) {
    var handle = host.__nmSurfaceHandle;
    var viewer = handle && handle.viewer;
    if (!viewer || typeof viewer.setFigureBackground !== "function") return;
    var stage = host.querySelector(".nm-sv-stage");
    var ground = stage ? cssColorInt(window.getComputedStyle(stage).backgroundColor) : null;
    if (ground === null) ground = cssColorInt(host.getAttribute("data-nm-surface-ground"));
    if (ground === null || ground === host.__nmSurfaceGround) return;
    host.__nmSurfaceGround = ground;
    viewer.setFigureBackground(ground);
    if (typeof viewer.requestRender === "function") viewer.requestRender();
  }

  function watchTheme(hosts) {
    var update = function () {
      hosts.forEach(function (host) { if (host.__nmSurfaceHandle) applyGround(host); });
    };
    if (window.matchMedia) {
      var query = window.matchMedia("(prefers-color-scheme: dark)");
      if (typeof query.addEventListener === "function") query.addEventListener("change", update);
    }
    if (typeof MutationObserver === "function") {
      new MutationObserver(update).observe(document.documentElement, {
        attributes: true, attributeFilter: ["data-theme", "class"]
      });
    }
  }

  // Mounting ----------------------------------------------------------------
  function connectHandle(host, handle, token) {
    host.__nmSurfaceHandle = handle;
    var widget = host.querySelector(".surfwidget");
    if (widget) widget.__surfviewHandle = handle;
    return Promise.resolve(handle.ready).then(function () {
      if (token.cancelled || host.__nmSurfaceHandle !== handle) return null;
      applyGround(host);
      tuneMaterials(host);
      selectMap(host, host.getAttribute("data-nm-surface-map"));
      loadParcels().then(function (lookup) {
        if (!token.cancelled) host.__nmSurfaceParcels = lookup;
      });
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
        host.__nmSurfaceResize = new ResizeObserver(function (entries) {
          var next = Math.round(entries[0].contentRect.width);
          if (!next || next === width) return;
          width = next;
          window.requestAnimationFrame(function () { handle.setFitInsets(fitInsets(host)); });
        });
        host.__nmSurfaceResize.observe(stage);
      }
      restoreSurfaceMemo(host);
      setStatus(host, null);
      host.setAttribute("data-nm-surface-ready", "true");
      touchSurfaceBudget(host);
      return handle;
    });
  }

  // WebGL budget ---------------------------------------------------------------
  function surfaceKey(host) {
    return "surface:" + host.getAttribute("data-nm-surface-analysis");
  }

  function touchSurfaceBudget(host) {
    GL.touch({
      key: surfaceKey(host),
      cost: 1,
      element: host,
      shown: function () { return isShown(host); },
      release: function () {
        // Still the chosen view (scrolled away): wait, parked, until the
        // reader is back. The stage keeps its fixed height.
        var keep = isShown(host);
        releaseSurface(host);
        if (keep) parkSurface(host, false);
      }
    });
  }

  function parkSurface(host, blocked) {
    host.setAttribute("data-nm-surface-paused", "true");
    // blocked: false (off screen), true (too many live views) or "lost" (the
    // browser took the context repeatedly).
    setStatus(host, "paused", blocked === "lost" ?
      "Paused: the browser reclaimed this view's graphics memory several times. Return to Static, then choose 3D surface again." :
      blocked ?
      "Paused: too many interactive views are open on this page. Return another map to Static, then choose 3D surface again." :
      "Paused to free graphics memory; it resumes when it is back on screen.");
    if (blocked) GL.unpark(surfaceKey(host));
    else GL.park({ key: surfaceKey(host), element: host, resume: function () { resumeSurface(host); } });
  }

  function unparkSurface(host) {
    GL.unpark(surfaceKey(host));
    if (host.getAttribute("data-nm-surface-paused") !== "true") return;
    host.removeAttribute("data-nm-surface-paused");
    setStatus(host, null);
  }

  // Rebuild a parked surface only on screen and within the budget.
  function resumeSurface(host) {
    if (host.getAttribute("data-nm-surface-paused") !== "true") return;
    if (!isShown(host)) { unparkSurface(host); return; }
    if (!GL.onScreen(host)) return;
    if (!GL.reserve(surfaceKey(host), 1)) { parkSurface(host, true); return; }
    unparkSurface(host);
    mount(host).catch(function () {});
  }

  // Free the surface viewer's context, remembering the view and any display
  // change so the next mount looks the same.
  function releaseSurface(host) {
    if (host.__nmSurfaceActivation) host.__nmSurfaceActivation.cancelled = true;
    var handle = host.__nmSurfaceHandle;
    GL.drop(surfaceKey(host));
    if (handle) {
      var memo = { view: host.__nmSurfaceView || null };
      try {
        var target = handle.controlTarget;
        var layer = target && layerFor(target.getSnapshot(), currentLayerId(host));
        if (layer && layer.scalarMapping && display.isModified(layer, host.__nmSurfaceDisplayStore.capture(layer))) {
          memo.scalar = {
            colorMapId: layer.scalarMapping.colorMap.id,
            displayRange: display.pair(layer.scalarMapping.displayRange.value),
            maskInterval: display.pair(layer.scalarMapping.maskInterval.value)
          };
          memo.opacity = Number(layer.opacity);
        }
      } catch (error) { /* keep the view only */ }
      host.__nmSurfaceMemo = memo;
      var subscription = host.__nmSurfaceSubscription;
      if (subscription && typeof subscription.unsubscribe === "function") subscription.unsubscribe();
      host.__nmSurfaceSubscription = null;
      if (host.__nmSurfaceResize) host.__nmSurfaceResize.disconnect();
      host.__nmSurfaceResize = null;
      var widget = host.querySelector(".surfwidget");
      var canvases = widget ? Array.prototype.slice.call(widget.querySelectorAll("canvas")) : [];
      try { if (typeof handle.dispose === "function") handle.dispose(); } catch (error) { /* lost */ }
      GL.loseContexts(canvases);
      if (widget) {
        widget.replaceChildren();
        widget.__surfviewHandle = null;
      }
    }
    host.__nmSurfaceHandle = null;
    host.__nmSurfaceMount = null;
    host.__nmSurfaceGround = null;
    host.removeAttribute("data-nm-surface-ready");
  }

  function restoreSurfaceMemo(host) {
    var memo = host.__nmSurfaceMemo;
    host.__nmSurfaceMemo = null;
    if (!memo) return;
    if (memo.scalar) applyScalar(host, memo.scalar);
    if (Number.isFinite(memo.opacity)) applyOpacity(host, memo.opacity);
    if (memo.view && memo.view !== host.__nmSurfaceView) setView(host, memo.view);
  }

  function surfaceLost(host) {
    var widget = host.querySelector(".surfwidget");
    return Boolean(host.__nmSurfaceHandle) && GL.isLost(widget);
  }

  document.addEventListener("webglcontextlost", function (event) {
    var canvas = event.target;
    if (!canvas || canvas.__nmReleased || !canvas.closest) return;
    var host = canvas.closest("[data-nm-surface-host]");
    if (!host || !host.__nmSurfaceHandle || host.__nmSurfaceRemount) return;
    // The browser took this context (never one we released). Rebuild only
    // a surface the reader can see, within the budget, at most twice in 15 s.
    host.__nmSurfaceRemount = window.setTimeout(function () {
      host.__nmSurfaceRemount = null;
      var shown = isShown(host);
      releaseSurface(host);
      if (!shown) return;
      var now = Date.now();
      host.__nmSurfaceLosses = (host.__nmSurfaceLosses || []).filter(function (t) { return now - t < 15000; });
      host.__nmSurfaceLosses.push(now);
      if (host.__nmSurfaceLosses.length > 2) { parkSurface(host, "lost"); return; }
      parkSurface(host, false);
      resumeSurface(host);
    }, 0);
  }, true);

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
    var token = { cancelled: false };
    host.__nmSurfaceActivation = token;
    setStatus(host, "loading", "Loading surface");
    host.__nmSurfaceMount = loadRuntime().then(function () {
      if (token.cancelled) return null;
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
        onError: function (error) { if (!token.cancelled) showFallback(host, error); }
      };
      var style = reportStyle(options.preset);
      if (style) options.preset = style;
      var bilateral = parse(host, "data-nm-surface-bilateral");
      if (bilateral && bilateral.id) options.bilateralGroup = bilateral;
      if (layout === "anatomical") {
        var memo = host.__nmSurfaceMemo;
        options.initialBrainView = (memo && memo.view) || host.getAttribute("data-nm-surface-view") || "oblique";
        options.oblique = parseOblique(host.getAttribute("data-nm-surface-oblique"));
        options.fov = 22;
      }
      return connectHandle(host, window.surfview.mountSurfView(widget, manifest, options), token);
    }).catch(function (error) {
      if (token.cancelled) return null;
      // Let a later choice retry (e.g. after a transient network failure).
      host.__nmSurfaceMount = null;
      showFallback(host, error);
      throw error;
    });
    return host.__nmSurfaceMount;
  }

  function isShown(host) {
    return !host.hidden && host.getClientRects().length > 0;
  }

  function volumeNeedsServer(volume) {
    if (window.location.protocol !== "file:") return false;
    if (document.querySelector("script[data-nm-volume-runtime-src]")) return true;
    var node = document.getElementById(volume.getAttribute("data-nm-volume-scene"));
    try {
      var scene = JSON.parse((node && node.textContent) || "{}");
      return (scene.assets || []).some(function (asset) {
        return asset.location && asset.location.kind !== "embedded";
      });
    } catch (error) {
      return false;
    }
  }

  // Bundle-mode surface data are companion files; embedded data use the
  // "nm-surface-asset:" scheme (or data: URIs) and work from disk.
  function surfaceNeedsServer(host) {
    var node = document.getElementById(host.getAttribute("data-nm-surface-manifest"));
    return display.manifestNeedsServer(window.location.protocol, (node && node.textContent) || "");
  }

  // From disk, HTTP is named only when a file could not be fetched; any other
  // failure (corrupt embedded data, WebGL) states its own reason.
  function surfaceMessage(error) {
    var reason = (error && error.message) || "unknown error";
    if (reason === FILE_MESSAGE || (window.location.protocol === "file:" &&
        /Failed to fetch|NetworkError|Load failed/.test(reason))) return FILE_MESSAGE;
    return "The 3D surface could not load (" + reason + "). The static figures are unaffected.";
  }

  function installRouter(host) {
    var analysisId = host.getAttribute("data-nm-surface-analysis");
    if (document.querySelector('[data-nm-view-router="' + CSS.escape(analysisId) + '"]')) return;
    var volume = document.querySelector('[data-nm-volume-analysis="' + CSS.escape(analysisId) + '"]');
    var slot = document.querySelector('[data-nm-view-slot="' + CSS.escape(analysisId) + '"]');
    var router = document.createElement("fieldset");
    router.className = "nm-view-router";
    router.setAttribute("data-nm-view-router", analysisId);
    var legend = document.createElement("legend");
    var viewLabel = slot && slot.getAttribute("data-nm-view-label");
    legend.textContent = viewLabel ? "View for " + viewLabel : "View";
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
    // A short reader-facing note under the router: why an interactive view
    // is unavailable, or why it closed. The static figures stay on screen.
    function note(text) {
      var node = router.__nmNote;
      if (!text) {
        if (node) node.hidden = true;
        return;
      }
      if (!node) {
        node = document.createElement("p");
        node.className = "nm-view-note";
        node.setAttribute("role", "status");
        node.setAttribute("data-nm-view-note", analysisId);
        router.__nmNote = node;
      }
      if (node.previousElementSibling !== router) router.after(node);
      node.textContent = text;
      node.hidden = false;
    }
    function disable(value, reason) {
      choices.forEach(function (choice) {
        if (choice.value !== value) return;
        choice.disabled = true;
        choice.setAttribute("aria-disabled", "true");
        choice.parentNode.setAttribute("data-disabled", "true");
        choice.parentNode.title = reason;
      });
    }
    // Opened from disk, a bundled report cannot fetch its companion data.
    var offline = [];
    if (volume && volumeNeedsServer(volume)) offline.push("slices");
    if (surfaceNeedsServer(host)) offline.push("surface");
    offline.forEach(function (value) { disable(value, FILE_MESSAGE); });
    if (offline.length) router.setAttribute("data-nm-view-offline", offline.join(" "));
    // One event per view change, from any source, for the report page.
    function announceView(view) {
      if (router.getAttribute("data-nm-view-current") === view) return;
      router.setAttribute("data-nm-view-current", view);
      router.dispatchEvent(new CustomEvent("nm-view-change", {
        bubbles: true,
        detail: { analysisId: analysisId, view: view }
      }));
    }
    // Any viewer failure returns the map to its static figures, with a note.
    function fallBack(view, message) {
      if (router.getAttribute("data-nm-view-current") !== view) return;
      choices[0].checked = true;
      choose("static");
      note(message);
    }
    function choose(view) {
      if (offline.indexOf(view) >= 0) view = "static";
      host.hidden = view !== "surface";
      if (volume) volume.hidden = view !== "slices";
      if (!offline.length) note(null);
      if (view !== "surface") unparkSurface(host);
      if (view === "surface") {
        unparkSurface(host);
        if (surfaceLost(host)) releaseSurface(host);
        if (!host.__nmSurfaceHandle) GL.reserve(surfaceKey(host), 1);
        mount(host).then(function (handle) {
          if (!handle || host.__nmSurfaceHandle !== handle) return;
          touchSurfaceBudget(host);
          if (host.__nmSurfaceHandle) {
            window.requestAnimationFrame(function () {
              if (host.__nmSurfaceHandle) host.__nmSurfaceHandle.resize();
            });
          }
        }).catch(function (error) {
          fallBack("surface", surfaceMessage(error));
        });
      }
      if (view === "slices" && volume) {
        var toggle = volume.querySelector("[data-nm-volume-toggle]");
        if (toggle && toggle.getAttribute("aria-expanded") !== "true") toggle.click();
      }
      announceView(view);
    }
    router.__nmFallBack = fallBack;
    if (volume) {
      volume.addEventListener("nm-viewer-failed", function (event) {
        fallBack("slices", (event.detail && event.detail.message) || FILE_MESSAGE);
      });
    }
    choices.forEach(function (choice) {
      choice.addEventListener("change", function () {
        if (choice.checked) choose(choice.value);
      });
    });
    // "Hide interactive view" inside the volume panel returns the router to
    // Static, so the radios never claim a view that is no longer shown.
    if (volume) {
      volume.addEventListener("nm-volume-hidden", function () {
        choices[0].checked = true;
        choose("static");
      });
    }
    if (slot) {
      // The report places a slot under each map's heading for the router.
      slot.appendChild(router);
    } else {
      var anchor = volume && volume.compareDocumentPosition(host) & Node.DOCUMENT_POSITION_FOLLOWING
        ? volume
        : host;
      anchor.parentNode.insertBefore(router, anchor);
    }
    choose("static");
    if (offline.length) note(FILE_MESSAGE);
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
    // The colour bar canvas is created here rather than in the report markup,
    // so a page read without JavaScript carries no canvas at all.
    var legendBox = part(host, "legend");
    if (legendBox && !legendBox.querySelector(".nm-sv-barwrap")) {
      var wrap = document.createElement("div");
      wrap.className = "nm-sv-barwrap";
      var bar = document.createElement("canvas");
      bar.setAttribute("aria-hidden", "true");
      wrap.appendChild(bar);
      legendBox.insertBefore(wrap, legendBox.querySelector("[data-nm-legend-ticks]"));
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
    registerReportPalettes();
    var hosts = document.querySelectorAll("[data-nm-surface-host]");
    hosts.forEach(bind);
    watchTheme(hosts);
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

  // Hooks for the report's browser tests.
  window.NeuroMosaicSurfaceBridge = {
    pick: function (host, x, y) {
      var viewer = host.__nmSurfaceHandle && host.__nmSurfaceHandle.viewer;
      return viewer ? viewer.pick({ x: x, y: y, useGPU: false }) : null;
    },
    release: releaseSurface,
    exportPNG: exportPNG
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }
}());
