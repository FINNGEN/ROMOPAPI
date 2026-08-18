// Test script to reproduce the trailing zero decoding issue
import { loadProto, HLL_COUNT, toBase64 } from './mergeHll.js';

await loadProto();

// Problematic sketch from the user
const problematicSketch = 'CHAQBRgCIAuCBxIQBRgKIA8yCrxB4UqoEPZIyREA';

console.log('Testing problematic sketch:', problematicSketch);
console.log('');

// Decode the base64 to see the bytes
const bytes = Buffer.from(problematicSketch, 'base64');
console.log('Bytes length:', bytes.length);
console.log('Bytes as array:', Array.from(bytes));
console.log('Last byte:', bytes[bytes.length - 1]);
console.log('');

// Try to decode and extract
try {
  console.log('Attempting HLL_COUNT.EXTRACT...');
  const result = HLL_COUNT.EXTRACT(problematicSketch);
  console.log('✓ SUCCESS! Estimated count:', result);
} catch (error) {
  console.log('✗ ERROR:', error.message);
  console.log('Full error:', error);
  console.log('');
  
  // Try removing the trailing zero
  console.log('Attempting with trailing zero removed...');
  const bytesWithoutTrailingZero = bytes.slice(0, -1);
  const sketchWithoutTrailingZero = bytesWithoutTrailingZero.toString('base64');
  console.log('New sketch:', sketchWithoutTrailingZero);
  console.log('New bytes:', Array.from(bytesWithoutTrailingZero));
  
  try {
    const result = HLL_COUNT.EXTRACT(sketchWithoutTrailingZero);
    console.log('✓ SUCCESS after removing trailing zero! Estimated count:', result);
  } catch (error2) {
    console.log('✗ Still failed:', error2.message);
  }
}

// Also test merging with this sketch
console.log('');
console.log('Testing MERGE_PARTIAL with problematic sketch...');
try {
  const merged = HLL_COUNT.MERGE_PARTIAL([problematicSketch]);
  console.log('✓ MERGE_PARTIAL succeeded');
  const count = HLL_COUNT.EXTRACT(merged);
  console.log('Estimated count from merged:', count);
} catch (error) {
  console.log('✗ MERGE_PARTIAL ERROR:', error.message);
}
