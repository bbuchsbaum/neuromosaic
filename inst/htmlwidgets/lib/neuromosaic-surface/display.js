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

  return Object.freeze({
    pair: pair,
    samePair: samePair,
    layerDefaults: layerDefaults,
    isModified: isModified,
    orderedFinitePair: orderedFinitePair,
    layerIdForMap: layerIdForMap,
    createDefaultStore: createDefaultStore
  });
}));
