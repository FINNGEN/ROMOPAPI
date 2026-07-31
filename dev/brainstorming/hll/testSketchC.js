// Test sketch C in isolation
import { loadProto, HLL_COUNT } from './mergeHll.js';

await loadProto();

const sketchC = 'CAGQBxIM0AEQBRgKIA8yCgoYAAA=';
const bytes = Buffer.from(sketchC, 'base64');

console.log('Sketch C:', sketchC);
console.log('Bytes:', Array.from(bytes));
console.log('Length:', bytes.length);
console.log('');

console.log('Test 1: EXTRACT sketch C');
try {
  const count = HLL_COUNT.EXTRACT(sketchC);
  console.log('  ✓ Count:', count);
} catch (error) {
  console.log('  ✗ Error:', error.message);
  console.log('  Full error:', error);
}
console.log('');

console.log('Test 2: MERGE_PARTIAL([sketchC])');
try {
  const merged = HLL_COUNT.MERGE_PARTIAL([sketchC]);
  console.log('  ✓ Merged successfully');
  const count = HLL_COUNT.EXTRACT(merged);
  console.log('  Count:', count);
} catch (error) {
  console.log('  ✗ Error:', error.message);
  console.log('  Full error:', error);
}
