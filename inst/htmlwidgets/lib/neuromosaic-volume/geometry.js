(function (root, factory) {
  var api = factory();
  if (typeof module === "object" && module.exports) module.exports = api;
  if (root) root.NeuroMosaicVolumeGeometry = api;
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  "use strict";

  var DEFAULT_TOLERANCE = 1e-6;

  function numericArray(value, name, length) {
    var out = Array.prototype.slice.call(value || []).map(Number);
    if (out.length !== length || out.some(function (x) { return !Number.isFinite(x); })) {
      throw new Error(name + " must contain " + length + " finite numbers");
    }
    return out;
  }

  function flattenAffine(value) {
    if (Array.isArray(value) && value.length === 4 && Array.isArray(value[0])) {
      var flat = [];
      for (var column = 0; column < 4; column += 1) {
        for (var row = 0; row < 4; row += 1) flat.push(Number(value[row][column]));
      }
      return numericArray(flat, "decoded affine", 16);
    }
    return numericArray(value, "decoded affine", 16);
  }

  function closeEnough(x, y, tolerance) {
    return Math.abs(x - y) <= tolerance * Math.max(1, Math.abs(x), Math.abs(y));
  }

  function equalNumbers(left, right, tolerance) {
    return left.length === right.length && left.every(function (x, index) {
      return closeEnough(x, right[index], tolerance);
    });
  }

  function affineSpacing(affine) {
    return [0, 1, 2].map(function (column) {
      var offset = column * 4;
      return Math.hypot(affine[offset], affine[offset + 1], affine[offset + 2]);
    });
  }

  function affineOrigin(affine) {
    return [affine[12], affine[13], affine[14]];
  }

  function affineOrientation(affine) {
    var positive = ["R", "A", "S"];
    var negative = ["L", "P", "I"];
    return [0, 1, 2].map(function (column) {
      var offset = column * 4;
      var vector = [affine[offset], affine[offset + 1], affine[offset + 2]];
      var axis = vector.reduce(function (best, value, index) {
        return Math.abs(value) > Math.abs(vector[best]) ? index : best;
      }, 0);
      return vector[axis] >= 0 ? positive[axis] : negative[axis];
    }).join("");
  }

  function normalizeDecodedGeometry(decoded) {
    if (!decoded || typeof decoded !== "object") {
      throw new Error("decoded volume geometry is missing");
    }
    var dimensions = decoded.dimensions || decoded.dims || decoded.shape;
    var affine = flattenAffine(decoded.affine || decoded.transform || decoded.matRAS);
    var normalized = {
      dimensions: numericArray(dimensions, "decoded dimensions", 3).map(function (x) {
        return Math.trunc(x);
      }),
      spacing: decoded.spacing ? numericArray(decoded.spacing, "decoded spacing", 3) : affineSpacing(affine),
      origin: decoded.origin ? numericArray(decoded.origin, "decoded origin", 3) : affineOrigin(affine),
      orientation: decoded.orientation || affineOrientation(affine),
      affine: affine,
      fingerprint: decoded.fingerprint || null
    };
    return normalized;
  }

  function geometryMismatches(expected, decoded, tolerance) {
    tolerance = tolerance == null ? DEFAULT_TOLERANCE : Number(tolerance);
    var actual = normalizeDecodedGeometry(decoded);
    var mismatches = [];
    var expectedDimensions = numericArray(expected.dimensions, "scene dimensions", 3);
    var expectedSpacing = numericArray(expected.spacing, "scene spacing", 3);
    var expectedOrigin = numericArray(expected.origin, "scene origin", 3);
    var expectedAffine = numericArray(expected.affine, "scene affine", 16);

    if (!expectedDimensions.every(function (x, i) { return x === actual.dimensions[i]; })) {
      mismatches.push("dimensions");
    }
    if (!equalNumbers(expectedSpacing, actual.spacing, tolerance)) mismatches.push("spacing");
    if (!equalNumbers(expectedOrigin, actual.origin, tolerance)) mismatches.push("origin");
    if (String(expected.orientation) !== String(actual.orientation)) mismatches.push("orientation");
    if (!equalNumbers(expectedAffine, actual.affine, tolerance)) mismatches.push("affine");
    if (actual.fingerprint && actual.fingerprint !== expected.fingerprint) {
      mismatches.push("fingerprint");
    }
    return mismatches;
  }

  function assertGeometryMatch(expected, decoded, assetId, tolerance) {
    var mismatches = geometryMismatches(expected, decoded, tolerance);
    if (mismatches.length) {
      throw new Error(
        "Decoded volume asset '" + assetId + "' does not match the VolumeScene geometry: " +
        mismatches.join(", ")
      );
    }
    return normalizeDecodedGeometry(decoded);
  }

  return {
    DEFAULT_TOLERANCE: DEFAULT_TOLERANCE,
    normalizeDecodedGeometry: normalizeDecodedGeometry,
    geometryMismatches: geometryMismatches,
    assertGeometryMatch: assertGeometryMatch
  };
});
