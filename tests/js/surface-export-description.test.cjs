const { test } = require('node:test');
const assert = require('node:assert/strict');
const display = require('../../inst/htmlwidgets/lib/neuromosaic-surface/display.js');

test('surface export mapping follows the selected layer provenance', () => {
  assert.equal(display.mappingNote({ source: 'parcel_values' }),
    'Parcel values mapped directly to the cortical surface');
  assert.equal(display.mappingNote({ source: 'volume_projection', aggregate: 'max' }),
    'Volume values projected to the cortical surface');
  assert.equal(display.mappingNote({ source: 'precomputed' }),
    'Precomputed vertex values mapped to the cortical surface');
  assert.equal(display.mappingNote(null), 'Surface mapping details not recorded');
});
