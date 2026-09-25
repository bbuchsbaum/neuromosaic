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

  // Yeo 7/17 network codes as used by Schaefer parcellations.
  var NETWORKS = {
    Vis: "Visual", VisCent: "Visual (central)", VisPeri: "Visual (peripheral)",
    SomMot: "Somatomotor", SomMotA: "Somatomotor A", SomMotB: "Somatomotor B",
    DorsAttn: "Dorsal attention", DorsAttnA: "Dorsal attention A", DorsAttnB: "Dorsal attention B",
    SalVentAttn: "Salience / ventral attention", SalVentAttnA: "Salience / ventral attention A",
    SalVentAttnB: "Salience / ventral attention B",
    Limbic: "Limbic", LimbicA: "Limbic A", LimbicB: "Limbic B",
    Cont: "Control", ContA: "Control A", ContB: "Control B", ContC: "Control C",
    Default: "Default", DefaultA: "Default A", DefaultB: "Default B", DefaultC: "Default C",
    TempPar: "Temporal-parietal"
  };
  var REGIONS = {
    PFC: "prefrontal", PFCl: "lateral prefrontal", PFCm: "medial prefrontal",
    PFCd: "dorsal prefrontal", PFCv: "ventral prefrontal", PFCmp: "medial posterior prefrontal",
    Par: "parietal", ParOper: "parietal operculum", Temp: "temporal", TempPole: "temporal pole",
    TempOcc: "temporal-occipital", Med: "medial", FrOper: "frontal operculum",
    FrOperIns: "frontal operculum / insula", Ins: "insula", PrC: "precentral", PrCv: "ventral precentral",
    PrCd: "dorsal precentral", PostC: "postcentral", pCun: "precuneus", pCunPCC: "precuneus / PCC",
    PCC: "posterior cingulate", Cingm: "mid-cingulate", ACC: "anterior cingulate",
    OFC: "orbitofrontal", IPL: "inferior parietal", IPS: "intraparietal sulcus",
    SPL: "superior parietal", FEF: "frontal eye field", ExStr: "extrastriate",
    ExStrInf: "inferior extrastriate", ExStrSup: "superior extrastriate", Striate: "striate",
    StriCal: "striate / calcarine", Aud: "auditory", Cent: "central", S2: "secondary somatosensory",
    Rsp: "retrosplenial", PHC: "parahippocampal", Vent: "ventral", Post: "posterior",
    Cinga: "anterior cingulate", ParMed: "medial parietal", Precuneus: "precuneus"
  };

  // Readable region text for a parcel id and its atlas table entry:
  // { name: "Visual network, extrastriate", detail: "LH_Vis_3 · Schaefer ..." }.
  // Parcel 0 (or a missing entry) is the medial wall / unlabelled cortex.
  function describeParcel(id, entry, atlas) {
    var parcel = Number(id);
    if (!parcel || !entry) return { name: "Medial wall", detail: atlas || "" };
    var code = entry.full || entry.label || String(parcel);
    var parts = String(entry.label || code).split("_");
    var network = entry.network || parts[0];
    var name = NETWORKS[network] ? NETWORKS[network] + " network" : network;
    var region = parts.slice(1).filter(function (token) {
      return !/^\d+$/.test(token) && token !== network;
    }).map(function (token) { return REGIONS[token] || token; });
    if (region.length) name += ", " + region.join(" ");
    return { name: name, detail: code + (atlas ? " \u00b7 " + atlas : "") };
  }

  return Object.freeze({
    describeParcel: describeParcel,
    formatNumber: formatNumber,
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
