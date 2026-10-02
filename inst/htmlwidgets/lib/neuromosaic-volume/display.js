(function (root, factory) {
  "use strict";
  var api = factory();
  if (typeof module === "object" && module.exports) module.exports = api;
  root.NeuroMosaicVolumeDisplay = api;
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  "use strict";

  var DIVERGING = [
    "Report (nm-signed)", "Spectral", "RdYlGn", "RdBu", "PiYG", "PRGn", "RdYlBu", "BrBG",
    "RdGy", "PuOr", "BlueRed"
  ];
  var SEQUENTIAL = [
    "OrRd", "PuBu", "BuPu", "Oranges", "BuGn", "YlOrBr", "YlGn",
    "Reds", "RdPu", "Greens", "YlGnBu", "Purples", "GnBu", "Greys",
    "YlOrRd", "PuRd", "Blues", "PuBuGn", "Viridis", "Inferno",
    "Grayscale"
  ];
  var ALIASES = {
    "nm-signed": "Report (nm-signed)",
    "blue-red": "BlueRed",
    "diverging": "BlueRed",
    "rdbu": "RdBu",
    "vik": "BlueRed",
    "inferno": "Inferno",
    "sequential": "Inferno",
    "viridis": "Viridis",
    "greyscale": "Grayscale",
    "grayscale": "Grayscale"
  };

  function fail(message) {
    throw new Error(message);
  }

  function finitePair(values, label, allowEqual) {
    var pair = Array.isArray(values) ? values.map(Number) : [];
    if (pair.length !== 2 || !pair.every(Number.isFinite) ||
        pair[0] > pair[1] || (!allowEqual && pair[0] === pair[1])) {
      fail(label + " must contain two " +
        (allowEqual ? "ordered" : "increasing") + " finite numbers.");
    }
    return pair;
  }

  function outside(value, direction) {
    var margin = Math.max(1, Math.abs(value)) * 1e-12;
    var result = value + direction * margin;
    if (!Number.isFinite(result) || result === value) {
      fail("Volume range is too extreme for one-sided threshold translation.");
    }
    return result;
  }

  function thresholdPair(display, volumeRange) {
    if (display.mode === "continuous") return [0, 0];
    if (display.mode !== "thresholded") {
      fail("Display mode must be 'thresholded' or 'continuous'.");
    }
    var threshold = Number(display.threshold);
    if (!Number.isFinite(threshold) || threshold <= 0) {
      fail("Thresholded maps require a positive finite threshold.");
    }
    var range = finitePair(volumeRange, "Volume range", true);
    if (display.tail === "positive") {
      return [outside(Math.min(range[0], threshold), -1), threshold];
    }
    if (display.tail === "negative") {
      return [-threshold, outside(Math.max(range[1], -threshold), 1)];
    }
    if (display.tail === "two_sided") return [-threshold, threshold];
    fail("Threshold tail must be 'two_sided', 'positive', or 'negative'.");
  }

  function paletteName(display, availableMaps) {
    var requested = String(display.palette || "").toLowerCase();
    var canonical = ALIASES[requested];
    if (!canonical) {
      canonical = (availableMaps || []).find(function (name) {
        return name.toLowerCase() === requested;
      });
    }
    if (!canonical) {
      fail("Palette '" + display.palette + "' has no neuroimjs translation.");
    }
    var family = DIVERGING.indexOf(canonical) >= 0 ? "diverging" :
      (SEQUENTIAL.indexOf(canonical) >= 0 ? "sequential" : null);
    if (!family || family !== display.scale) {
      fail(
        "Palette '" + canonical + "' is incompatible with " +
        display.scale + " display semantics."
      );
    }
    return canonical;
  }

  function resolve(display, volumeRange, availableMaps) {
    if (!display || typeof display !== "object") fail("Display spec is missing.");
    if (display.scale !== "diverging" && display.scale !== "sequential") {
      fail("Display scale must be 'diverging' or 'sequential'.");
    }
    var alpha = Number(display.alpha);
    if (!Number.isFinite(alpha) || alpha < 0 || alpha > 1) {
      fail("Display alpha must be finite and between zero and one.");
    }
    return {
      range: finitePair(display.limits, "Display limits"),
      threshold: thresholdPair(display, volumeRange),
      alpha: alpha,
      colormapName: paletteName(display, availableMaps)
    };
  }

  // Neighbour offsets for 6-, 18- (faces + edges) or 26-connectivity, the
  // definitions neuroim2::conn_comp_3D uses for the report's cluster table.
  function neighbourOffsets(connectivity) {
    var limit = connectivity === "6-connect" ? 1 :
      (connectivity === "26-connect" ? 3 : 2);
    var out = [];
    for (var dk = -1; dk <= 1; dk += 1) {
      for (var dj = -1; dj <= 1; dj += 1) {
        for (var di = -1; di <= 1; di += 1) {
          var order = Math.abs(di) + Math.abs(dj) + Math.abs(dk);
          if (order === 0 || order > limit) continue;
          out.push([di, dj, dk]);
        }
      }
    }
    return out;
  }

  // Voxels of clusters (>= high or <= low, per sign) smaller than
  // `minClusterSize`: R's .montage_extent_excluded at any threshold.
  // Uint8Array (1 = excluded), or null when nothing can be excluded.
  function extentExcluded(data, dims, threshold, minClusterSize, connectivity) {
    var k = Math.floor(Number(minClusterSize));
    var nx = dims[0], ny = dims[1], nz = dims[2];
    var n = nx * ny * nz;
    var low = Number(threshold[0]), high = Number(threshold[1]);
    if (!(k > 1) || !(low < high) || !data || data.length !== n) return null;
    var offsets = neighbourOffsets(connectivity);
    var signOf = function (v) {
      return v >= high && v > 0 ? 1 : (v <= low && v < 0 ? -1 : 0);
    };
    var seen = new Uint8Array(n);
    var excluded = new Uint8Array(n);
    var stack = new Int32Array(n);
    var members = new Int32Array(n);
    for (var seed = 0; seed < n; seed += 1) {
      if (seen[seed]) continue;
      var sign = signOf(data[seed]);
      if (!sign) continue;
      var top = 0, count = 0;
      seen[seed] = 1;
      stack[top++] = seed;
      while (top) {
        var idx = stack[--top];
        members[count++] = idx;
        var i = idx % nx, j = Math.floor(idx / nx) % ny, kk = Math.floor(idx / (nx * ny));
        for (var o = 0; o < offsets.length; o += 1) {
          var ii = i + offsets[o][0], jj = j + offsets[o][1], k2 = kk + offsets[o][2];
          if (ii < 0 || ii >= nx || jj < 0 || jj >= ny || k2 < 0 || k2 >= nz) continue;
          var nb = (k2 * ny + jj) * nx + ii;
          if (seen[nb] || signOf(data[nb]) !== sign) continue;
          seen[nb] = 1;
          stack[top++] = nb;
        }
      }
      if (count < k) {
        for (var m = 0; m < count; m += 1) excluded[members[m]] = 1;
      }
    }
    return excluded;
  }

  // The report palette (`colors`, sampled over `limits`) rebuilt for the
  // layer's threshold and colour `range` (default `limits`), split at 0:
  // negative ramp over [low, -thr], positive over [thr, high]. A shown value
  // never takes the gap (hidden) colour, whatever the range.
  function resaturate(colors, limits, reportThreshold, threshold, range) {
    var n = Array.isArray(colors) ? colors.length : 0;
    var lo = Number(limits[0]), hi = Number(limits[1]);
    var t0 = Math.abs(Number(reportThreshold));
    if (n < 2 || !(hi > lo) || !(t0 > 0)) return colors;
    var rlo = range ? Number(range[0]) : lo, rhi = range ? Number(range[1]) : hi;
    if (!(rhi > rlo) || !Number.isFinite(rlo) || !Number.isFinite(rhi)) { rlo = lo; rhi = hi; }
    var span = hi - lo, rspan = rhi - rlo;
    var at = function (value) {
      var idx = Math.round(((value - lo) / span) * (n - 1));
      return colors[Math.max(0, Math.min(n - 1, idx))];
    };
    var gap = lo < 0 && hi > 0 ? at(0) : null;
    // A bin within one step of the threshold may hold drawn values: it takes
    // the threshold colour, never the gap.
    var half = rspan / (n - 1) / 2;
    var ramp = function (value) {
      var idx = Math.max(0, Math.min(n - 1, Math.round(((value - lo) / span) * (n - 1))));
      var step = value >= 0 ? 1 : -1;
      while (gap !== null && colors[idx] === gap && idx + step >= 0 && idx + step < n) idx += step;
      return colors[idx];
    };
    var out = new Array(n);
    for (var e = 0; e < n; e += 1) {
      var v = rlo + (rspan * e) / (n - 1);
      var positive = v >= 0;
      var sign = positive ? 1 : -1;
      var tn = positive ? Number(threshold[1]) : -Number(threshold[0]);
      if (!(tn > 0)) tn = 0;
      var capSrc = positive ? hi : -lo;
      var cap = positive ? rhi : -rlo;
      var a = Math.abs(v);
      if (!(capSrc > t0)) { out[e] = at(v); continue; }
      // A range end inside the threshold: its end bin holds every shown
      // value of that sign (clamped there), so it takes the cap colour.
      if (!(cap > tn) && e === (positive ? n - 1 : 0)) { out[e] = ramp(sign * capSrc); continue; }
      if (a + 2 * half < tn) { out[e] = gap === null ? at(v) : gap; continue; }
      if (!(cap > tn)) { out[e] = ramp(sign * capSrc); continue; }
      var mapped = t0 + (Math.max(0, a - tn) * (capSrc - t0)) / (cap - tn);
      out[e] = ramp(sign * Math.min(capSrc, mapped));
    }
    return out;
  }

  // Readout state at a voxel from the displayed value (NaN where the extent
  // rule removed it), the map's own value and the live threshold [lo, hi]:
  // outside (NaN or exactly 0: outside the analysis support), below, hidden
  // (cluster < k) or shown.
  function readoutState(shownValue, rawValue, threshold, minClusterSize) {
    var shown = shownValue === null || shownValue === undefined ? NaN : Number(shownValue);
    var raw = rawValue === null || rawValue === undefined ? NaN : Number(rawValue);
    var v = Number.isFinite(raw) ? raw : shown;
    if (!Number.isFinite(v) || v === 0) return { kind: "outside", value: null, reason: "outside map" };
    var t = Array.isArray(threshold) ? threshold.map(Number) : null;
    if (t && t[0] < t[1] && v > t[0] && v < t[1]) {
      return { kind: "below", value: v, reason: "below threshold" };
    }
    if (!Number.isFinite(shown)) {
      var k = Number(minClusterSize);
      return {
        kind: "hidden", value: v,
        reason: k > 1 ? "hidden (cluster < " + k + " voxels)" : "not displayed"
      };
    }
    return { kind: "shown", value: shown, reason: null };
  }

  // Wheel over a slice pane: "page" scrolls (default), "runtime" (a pane the
  // reader focused, Ctrl/Cmd zoom) or a +1/-1 step (Alt or Shift + wheel). A
  // pane focused by script (byReader false, e.g. a peak jump) scrolls.
  function wheelAction(e) {
    if ((e.focused && e.byReader) || e.ctrlKey || e.metaKey) return "runtime";
    if (!(e.altKey || e.shiftKey)) return "page";
    var delta = Number(e.deltaY) || Number(e.deltaX) || 0;
    if (!delta) return "page";
    return delta > 0 ? 1 : -1;
  }

  // May the runtime's document-level arrow keys act? Only from the page or a
  // slice pane, never a control. origin: { tag, editable, isPage, inPane }
  function runtimeKeyAllowed(origin) {
    if (!origin || origin.isPage) return true;
    if (origin.editable) return false;
    if (/^(INPUT|SELECT|TEXTAREA|BUTTON|A|SUMMARY)$/.test(String(origin.tag || "").toUpperCase())) return false;
    return !!origin.inPane;
  }

  // A peak in the table's precision: 2 dp, or `digits` (3-4) for ties.
  function peakText(value, digits, fallback) {
    var d = Number(digits);
    var v = Number(value);
    if (d > 2 && d <= 6 && Number.isFinite(v) && Math.abs(v) >= 1 && Math.abs(v) < 1000) {
      return v.toFixed(d).replace(/^-/, "\u2212");
    }
    return fallback(value);
  }

  // Colour maps offered per scale; diverging ones keep negative = cool.
  var MENU = {
    diverging: ["Report (nm-signed)", "BlueRed", "Purple–orange"],
    sequential: ["Inferno", "Viridis", "Grayscale"]
  };
  var MENU_ALIASES = { "Purple–orange": "PuOr" };

  function colormapMenu(scale, current) {
    var list = (MENU[scale] || MENU.diverging).slice();
    if (current && list.indexOf(current) < 0) list.unshift(current);
    return list;
  }

  // Scale numbers (cap, threshold, range ends) in the report's rounding:
  // three significant digits, so the shared cap 22.2539 reads 22.3 in the
  // viewer as in every caption and key.
  function reportNumber(value) {
    var v = Number(value);
    if (!Number.isFinite(v)) return "";
    if (v === 0) return "0";
    var text = String(Math.abs(v) >= 1000 || Number.isInteger(v) ? Math.round(v) : Number(v.toPrecision(3)));
    return text.replace(/^-/, "\u2212");
  }

  // An unlinked threshold band stays valid: low never above high (the edited
  // end yields), and a two-sided map keeps low <= 0 <= high.
  function orderThresholdBand(pair, which, twoSided) {
    var t = [Number(pair[0]), Number(pair[1])];
    if (twoSided) {
      t[0] = Math.min(t[0], 0);
      t[1] = Math.max(t[1], 0);
    }
    if (t[0] > t[1]) {
      if (which === "high") t[1] = t[0]; else t[0] = t[1];
    }
    return t;
  }

  // Export key note: the threshold, extent and colour scale shown, flagged
  // when modified. opts: { modified, threshold, extent, symbol, lo, hi }
  function exportNote(opts) {
    var t = opts.threshold;
    var shown = "";
    if (t && Number(t[0]) < Number(t[1])) {
      shown = Math.abs(Number(t[0]) + Number(t[1])) < 1e-9 ?
        "shown where |" + opts.symbol + "| \u2265 " + reportNumber(t[1]) :
        "hidden between " + reportNumber(t[0]) + " and " + reportNumber(t[1]);
      if (Number(opts.extent) > 1) shown += ", clusters \u2265 " + Number(opts.extent) + " voxels";
    } else {
      shown = "no threshold";
    }
    var lo = Number(opts.lo), hi = Number(opts.hi);
    var scale = Math.abs(lo + hi) < 1e-9 ?
      "colour scale \u00b1" + reportNumber(hi) :
      "colour scale " + reportNumber(lo) + " to " + reportNumber(hi);
    var text = shown.charAt(0).toUpperCase() + shown.slice(1) + "; " + scale;
    return opts.modified ?
      "Display modified in the browser (exploratory): " + shown + "; " + scale +
        ". The Static view is the figure of record." :
      text + " (report settings).";
  }

  // An edited colour-range end that would cross the other end is rejected:
  // the previous pair is kept (null when the edit is accepted as is).
  function rejectCrossedRange(pair, which, value) {
    var v = Number(value);
    if (!Number.isFinite(v)) return pair.slice();
    if (which === "low" ? v >= Number(pair[1]) : v <= Number(pair[0])) return pair.slice();
    return null;
  }

  // Slider step for a value domain: whole report units (0.1 for spans over
  // 10, 0.01 below), with the domain rounded outward to that grid so every
  // arrow-key step lands on a round value (the step base is the minimum).
  function sliderGrid(lo, hi) {
    var a = Number(lo), b = Number(hi);
    var span = Math.abs(b - a);
    var step = span > 10 ? 0.1 : span > 1 ? 0.01 : Math.pow(10, Math.floor(Math.log10(span || 1)) - 2);
    var round = function (v) { return Number(v.toFixed(10)); };
    return {
      step: step,
      min: round(Math.floor(round(a / step)) * step),
      max: round(Math.ceil(round(b / step)) * step)
    };
  }

  // A report opened from disk (file://) cannot fetch companion files: the
  // lazily loaded runtime or any asset not embedded in the page.
  function needsServer(protocol, scene, hasRuntimeSrc) {
    if (protocol !== "file:") return false;
    if (hasRuntimeSrc) return true;
    return Boolean(scene && Array.isArray(scene.assets) && scene.assets.some(function (asset) {
      return asset && asset.location && asset.location.kind !== "embedded";
    }));
  }

  return {
    reportNumber: reportNumber,
    readoutState: readoutState,
    wheelAction: wheelAction,
    runtimeKeyAllowed: runtimeKeyAllowed,
    peakText: peakText,
    exportNote: exportNote,
    rejectCrossedRange: rejectCrossedRange,
    sliderGrid: sliderGrid,
    orderThresholdBand: orderThresholdBand,
    needsServer: needsServer,
    resolve: resolve,
    thresholdPair: thresholdPair,
    paletteName: paletteName,
    neighbourOffsets: neighbourOffsets,
    extentExcluded: extentExcluded,
    colormapMenu: colormapMenu,
    resaturate: resaturate,
    menuAliases: MENU_ALIASES
  };
});
