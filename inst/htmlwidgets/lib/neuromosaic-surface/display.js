(function (root, factory) {
  "use strict";

  var api = factory();
  if (typeof module === "object" && module.exports) {
    module.exports = api;
  } else {
    root.NeuroMosaicSurfaceDisplay = api;
  }
}(typeof globalThis !== "undefined" ? globalThis : this, function () {
  "use strict";

  function pair(value) {
    return value && value.length === 2
      ? [Number(value[0]), Number(value[1])]
      : null;
  }

  function samePair(left, right, tolerance) {
    var tol = tolerance === undefined ? 1e-10 : Number(tolerance);
    return Boolean(left) && Boolean(right) &&
      Math.abs(left[0] - right[0]) <= tol &&
      Math.abs(left[1] - right[1]) <= tol;
  }

  function layerDefaults(layer) {
    if (!layer || !layer.id || !layer.scalarMapping) {
      throw new TypeError("A scalar surface layer is required.");
    }
    var scalar = layer.scalarMapping;
    return Object.freeze({
      colorMapId: scalar.colorMap.id,
      displayRange: Object.freeze(pair(scalar.displayRange.value)),
      maskInterval: Object.freeze(pair(scalar.maskInterval.value)),
      opacity: Number(layer.opacity)
    });
  }

  function isModified(layer, defaults) {
    if (!layer || !defaults || !layer.scalarMapping) return false;
    var scalar = layer.scalarMapping;
    return scalar.colorMap.id !== defaults.colorMapId ||
      !samePair(pair(scalar.displayRange.value), defaults.displayRange) ||
      !samePair(pair(scalar.maskInterval.value), defaults.maskInterval) ||
      Math.abs(Number(layer.opacity) - defaults.opacity) > 1e-10;
  }

  function orderedFinitePair(low, high) {
    var result = [Number(low), Number(high)];
    if (!result.every(Number.isFinite) || result[0] > result[1]) return null;
    return result;
  }

  function layerIdForMap(mapping, mapId) {
    if (!mapping || typeof mapId !== "string") return null;
    return typeof mapping[mapId] === "string" ? mapping[mapId] : null;
  }

  function createDefaultStore() {
    var defaults = Object.create(null);
    return Object.freeze({
      capture: function (layer) {
        if (!defaults[layer.id]) defaults[layer.id] = layerDefaults(layer);
        return defaults[layer.id];
      },
      get: function (layerId) {
        return defaults[layerId] || null;
      },
      resetUpdate: function (layerId) {
        var value = defaults[layerId];
        if (!value) return null;
        return Object.freeze({
          scalar: Object.freeze({
            colorMapId: value.colorMapId,
            displayRange: value.displayRange,
            maskInterval: value.maskInterval
          }),
          opacity: value.opacity
        });
      }
    });
  }

  // Three significant figures, trailing zeros dropped: 7.2243 -> "7.22",
  // 3.1 -> "3.1", 0.85 -> "0.85", 1234.5 -> "1235". Integers of any size stay
  // whole. `minus` swaps the ASCII hyphen for a typographic minus sign.
  function formatNumber(value, options) {
    var x = Number(value);
    if (!Number.isFinite(x)) return "";
    var digits = options && options.digits ? options.digits : 3;
    var text;
    if (x === 0) {
      text = "0";
    } else if (Math.abs(x) >= Math.pow(10, digits) || Number.isInteger(x)) {
      text = String(Math.round(x));
    } else {
      text = String(Number(x.toPrecision(digits)));
    }
    return options && options.minus ? text.replace(/^-/, "−") : text;
  }

  // Colour-bar tick values. The display-range ends always stay (without them
  // the colours have no scale); the mask edges (the threshold) come next, then
  // zero. A tick closer than `minGap` (fraction of the bar) to an accepted
  // tick is dropped, so labels never collide.
  function legendTicks(range, mask, minGap) {
    var r = pair(range);
    if (!r || !(r[1] > r[0])) return [];
    var gap = minGap === undefined ? 0.16 : Number(minGap);
    var span = r[1] - r[0];
    var candidates = [r[0], r[1]];
    var m = pair(mask);
    var masked = Boolean(m && m[1] > m[0]);
    if (masked) {
      if (m[0] > r[0] && m[0] < r[1]) candidates.push(m[0]);
      if (m[1] > r[0] && m[1] < r[1]) candidates.push(m[1]);
    }
    if (!masked && r[0] < 0 && r[1] > 0) candidates.push(0);
    var kept = [];
    candidates.forEach(function (value) {
      var clashes = kept.some(function (other) {
        return Math.abs(other - value) / span < gap;
      });
      if (!clashes) kept.push(value);
    });
    return kept.sort(function (a, b) { return a - b; });
  }

  // Labelled colour-bar ticks. When a symmetric threshold has room for only
  // one of its two edges, a single centred "±5" replaces the lone "5", so
  // the key never reads as if one sign had no threshold.
  function legendLabels(range, mask, minGap) {
    var r = pair(range);
    var ticks = legendTicks(range, mask, minGap);
    var m = pair(mask);
    var label = function (value) { return { value: value, label: formatNumber(value, { minus: true }) }; };
    var symmetric = m && m[1] > 0 && Math.abs(m[0] + m[1]) < 1e-9 * Math.max(1, Math.abs(m[1]));
    if (!symmetric || !r || !(r[0] < m[0] && m[1] < r[1])) return ticks.map(label);
    var hasLow = ticks.indexOf(m[0]) >= 0;
    var hasHigh = ticks.indexOf(m[1]) >= 0;
    if (hasLow === hasHigh) return ticks.map(label);
    var gap = minGap === undefined ? 0.16 : Number(minGap);
    var rest = ticks.filter(function (value) { return value !== m[0] && value !== m[1]; });
    var fits = rest.every(function (value) { return Math.abs(value) / (r[1] - r[0]) >= gap; });
    var out = rest.map(label);
    if (fits) out.push({ value: 0, label: "\u00b1" + formatNumber(m[1], { minus: true }) });
    return out.sort(function (a, b) { return a.value - b.value; });
  }

  // Yeo 7/17 network codes as used by Schaefer parcellations. The letter
  // suffix of the 17-network codes (ContA, DefaultB) is appended as is.
  var NETWORKS = {
    Vis: "Visual", VisCent: "Visual (central)", VisPeri: "Visual (peripheral)",
    SomMot: "Somatomotor", DorsAttn: "Dorsal attention",
    SalVentAttn: "Salience / ventral attention", Limbic: "Limbic",
    Cont: "Control", Default: "Default", TempPar: "Temporoparietal"
  };

  function networkName(code) {
    if (NETWORKS[code]) return NETWORKS[code];
    var m = /^([A-Za-z]+?)([A-C])$/.exec(String(code || ""));
    return m && NETWORKS[m[1]] ? NETWORKS[m[1]] + " " + m[2] : null;
  }

  // "Schaefer-200-7networks" -> "Schaefer-200, 7 networks"; others unchanged.
  function atlasName(atlas) {
    var text = String(atlas || "");
    var m = /^(.*?)-(\d+)networks$/i.exec(text);
    return m ? m[1] + ", " + m[2] + " networks" : text;
  }

  // Readable region text for a parcel id and its atlas table entry, in the
  // peak table's form: { name: "Control · PFCl 4 (L)",
  // detail: "Schaefer-200, 7 networks" }. Parcel 0 (or a missing entry) is
  // the medial wall / unlabelled cortex. Unrecognised labels stay verbatim.
  function describeParcel(id, entry, atlas) {
    var parcel = Number(id);
    var atlasText = atlasName(atlas);
    if (!parcel || !entry) return { name: "Medial wall", detail: atlasText };
    var full = String(entry.full || entry.label || parcel);
    var side = /^(LH|RH)_/.exec(full);
    var base = full.replace(/^(LH|RH)_/, "");
    var code = entry.network || base.split("_")[0];
    var network = networkName(code);
    var area = null;
    if (network && base.indexOf(code + "_") === 0) {
      area = base.slice(code.length + 1);
    } else if (network && entry.label && String(entry.label).indexOf(code + "_") !== 0) {
      area = String(entry.label);
    }
    if (!network || area === null) {
      return {
        name: String(entry.label || full),
        detail: full + (atlas ? " · " + atlasText : "")
      };
    }
    var name = network + (area ? " · " + area.split("_").join(" ") : "") +
      (side ? " (" + side[1].charAt(0) + ")" : "");
    return { name: name, detail: atlasText };
  }

  // Colour maps ---------------------------------------------------------------
  // Signed maps are offered diverging palettes only: a sequential or
  // single-hue map would paint one sign as "low". Unsigned maps get the
  // perceptually ordered sequential set. Labels are reader-facing names.
  var COLOR_MAP_LABELS = {
    "surface-heat": "Heat (two-sided)",
    "surface-heat-positive": "Heat",
    RdBu: "Red–blue",
    bluered: "Blue–red",
    coolwarm: "Cool–warm",
    viridis: "Viridis", inferno: "Inferno", magma: "Magma", plasma: "Plasma",
    hot: "Hot", greys: "Greys"
  };
  var SIGNED_COLOR_MAPS = ["coolwarm", "surface-heat", "bluered"];
  var UNSIGNED_COLOR_MAPS = [
    "surface-heat-positive", "viridis", "inferno", "magma", "plasma", "greys"
  ];
  var REPORT_PALETTE_PREFIX = "nm-signed-";

  function curateColorMaps(options, currentId, signed) {
    var enabled = (options || []).filter(function (option) {
      return option.availability && option.availability.enabled;
    });
    var byId = function (id) {
      return enabled.filter(function (option) { return option.id === id; })[0];
    };
    var out = [];
    var current = byId(currentId);
    if (current && currentId.indexOf(REPORT_PALETTE_PREFIX) === 0) {
      out.push({ id: current.id, label: "Report (nm-signed)" });
    }
    (signed ? SIGNED_COLOR_MAPS : UNSIGNED_COLOR_MAPS).forEach(function (id) {
      var option = byId(id);
      if (option) out.push({ id: id, label: COLOR_MAP_LABELS[id] || option.label || id });
    });
    // A current map outside the curated set stays selectable.
    if (current && !out.some(function (option) { return option.id === currentId; })) {
      out.unshift({ id: current.id, label: COLOR_MAP_LABELS[current.id] || current.label || current.id });
    }
    return out;
  }

  // A threshold magnitude in [0, max |data|]; null for non-numeric input.
  function clampMagnitude(value, dataRange) {
    var x = Math.abs(Number(value));
    if (!Number.isFinite(x)) return null;
    var r = pair(dataRange);
    if (r && r.every(Number.isFinite)) x = Math.min(x, Math.max(Math.abs(r[0]), Math.abs(r[1])));
    return x;
  }

  // An ordered mask interval with both ends clamped into the data range
  // (widened to include zero); null for non-numeric or reversed input.
  function clampInterval(low, high, dataRange) {
    var p = orderedFinitePair(low, high);
    if (!p) return null;
    var r = pair(dataRange);
    if (!r || !r.every(Number.isFinite)) return p;
    var lo = Math.min(r[0], 0);
    var hi = Math.max(r[1], 0);
    return [Math.min(Math.max(p[0], lo), hi), Math.min(Math.max(p[1], lo), hi)];
  }

  // The never-shown band of a threshold palette (a run of one neutral colour
  // around the centre) recoloured with the threshold colours on either side.
  // Hidden vertices still feed colour interpolation across a cluster's edge
  // triangles; a grey band there paints a pale fringe on every cluster.
  function bleedGap(colors) {
    var out = (colors || []).slice();
    var n = out.length;
    if (n < 3) return out;
    var mid = Math.floor((n - 1) / 2);
    var gap = String(out[mid]).toUpperCase();
    var lo = mid;
    var hi = mid;
    while (lo > 0 && String(out[lo - 1]).toUpperCase() === gap) lo -= 1;
    while (hi < n - 1 && String(out[hi + 1]).toUpperCase() === gap) hi += 1;
    if (hi - lo < 1 || lo === 0 || hi === n - 1) return out;
    for (var i = lo; i <= hi; i += 1) out[i] = i <= mid ? out[lo - 1] : out[hi + 1];
    return out;
  }

  // "#B40426" -> [180, 4, 38]; null when malformed.
  function hexRgb(hex) {
    var m = /^#?([0-9a-f]{6})$/i.exec(String(hex || ""));
    if (!m) return null;
    var n = parseInt(m[1], 16);
    return [n >> 16 & 255, n >> 8 & 255, n & 255];
  }

  // Hover values: two decimals with a true minus, as in the peak tables.
  function formatFixed(value) {
    var x = Number(value);
    if (!Number.isFinite(x)) return "";
    var text = x.toFixed(2);
    return /^-0\.00$/.test(text) ? "0.00" : text.replace(/^-/, "\u2212");
  }

  // A bundle-mode surface scene (companion .bin.gz files) cannot be read from
  // a report opened from disk; embedded payloads ("nm-surface-asset:", data:
  // or blob: URLs) can.
  function manifestNeedsServer(protocol, manifestText) {
    if (protocol !== "file:") return false;
    var found = String(manifestText || "").match(/"(?:url|uri|href|src)"\s*:\s*"[^"]*"/g) || [];
    return found.some(function (entry) {
      var url = entry.replace(/^[^:]*:\s*"/, "").replace(/"$/, "");
      return !/^(nm-surface-asset:|data:|blob:)/.test(url);
    });
  }

  return Object.freeze({
    formatFixed: formatFixed,
    manifestNeedsServer: manifestNeedsServer,
    atlasName: atlasName,
    bleedGap: bleedGap,
    clampInterval: clampInterval,
    clampMagnitude: clampMagnitude,
    curateColorMaps: curateColorMaps,
    describeParcel: describeParcel,
    hexRgb: hexRgb,
    formatNumber: formatNumber,
    legendLabels: legendLabels,
    legendTicks: legendTicks,
    pair: pair,
    samePair: samePair,
    layerDefaults: layerDefaults,
    isModified: isModified,
    orderedFinitePair: orderedFinitePair,
    layerIdForMap: layerIdForMap,
    createDefaultStore: createDefaultStore
  });
}));
