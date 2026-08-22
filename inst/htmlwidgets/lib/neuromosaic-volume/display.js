(function (root, factory) {
  "use strict";
  var api = factory();
  if (typeof module === "object" && module.exports) module.exports = api;
  root.NeuroMosaicVolumeDisplay = api;
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  "use strict";

  var DIVERGING = [
    "Spectral", "RdYlGn", "RdBu", "PiYG", "PRGn", "RdYlBu", "BrBG",
    "RdGy", "PuOr", "BlueRed"
  ];
  var SEQUENTIAL = [
    "OrRd", "PuBu", "BuPu", "Oranges", "BuGn", "YlOrBr", "YlGn",
    "Reds", "RdPu", "Greens", "YlGnBu", "Purples", "GnBu", "Greys",
    "YlOrRd", "PuRd", "Blues", "PuBuGn", "Viridis", "Inferno",
    "Grayscale"
  ];
  var ALIASES = {
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

  return {
    resolve: resolve,
    thresholdPair: thresholdPair,
    paletteName: paletteName
  };
});
