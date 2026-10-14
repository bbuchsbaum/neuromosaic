const test = require('node:test');
const assert = require('node:assert/strict');
const { decodeNifti } = require('../../inst/report/montage-space.js');

function nifti(values, dims = [values.length, 1, 1]) {
  const buffer = new ArrayBuffer(352 + values.length * 8);
  const view = new DataView(buffer);
  view.setInt32(0, 348, true);
  view.setInt16(40, 3, true);
  dims.forEach((n, i) => view.setInt16(42 + 2 * i, n, true));
  view.setInt16(70, 64, true);
  view.setFloat32(108, 352, true);
  values.forEach((n, i) => view.setFloat64(352 + 8 * i, n, true));
  return buffer;
}
const asset = { datatype: 'float64' };
test('Space conversion preserves zero, missingness and x-fastest order', () => {
  const result = decodeNifti(nifti([0, 1, NaN, -2, 0, 3], [3, 2, 1]), asset, 6, 'map');
  assert.ok(result instanceof Float32Array);
  assert.deepEqual(Array.from(result), [0, 1, NaN, -2, 0, 3]);
});
test('Space conversion fails on overflow, invalid template/mask, and truncated assets', () => {
  const max32 = (2 - 2 ** -23) * 2 ** 127;
  assert.deepEqual(Array.from(decodeNifti(nifti([-max32, max32]), asset, 2, 'map')), [-max32, max32]);
  assert.throws(() => decodeNifti(nifti([1e100]), asset, 1, 'map'), /overflows/);
  assert.throws(() => decodeNifti(nifti([NaN]), asset, 1, 'template'), /missing/);
  assert.throws(() => decodeNifti(nifti([.5]), asset, 1, 'mask'), /binary/);
  assert.throws(() => decodeNifti(nifti([1]).slice(0, 355), asset, 1, 'map'), /byte length/);
  assert.throws(() => decodeNifti(nifti([Infinity]), asset, 1, 'map'), /infinite/);
});
