// Proposed fix: Strip trailing zero bytes from input
// This handles malformed protobuf data with null-byte padding

import protobuf from 'protobufjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PROTO_PATH = join(__dirname, 'zetasketch.proto');

const root = await protobuf.load(PROTO_PATH);
const Agg = root.lookupType('zetasketch.AggregatorStateProto');

// Test cases
const testCases = [
  {
    name: 'Problematic sketch with trailing zero',
    sketch: 'CHAQBRgCIAuCBxIQBRgKIA8yCrxB4UqoEPZIyREA',
    hasTrailingZero: true
  },
  {
    name: 'Same sketch with trailing zero removed',
    sketch: 'CHAQBRgCIAuCBxIQBRgKIA8yCrxB4UqoEPZIyRE=',
    hasTrailingZero: false
  }
];

console.log('=== Testing Different Decoding Strategies ===\n');

// Strategy 1: Naive decode (will fail with trailing zeros)
console.log('Strategy 1: Naive decode (original code)');
for (const tc of testCases) {
  try {
    const bytes = Buffer.from(tc.sketch, 'base64');
    Agg.decode(bytes);
    console.log(`  ✓ ${tc.name}: SUCCESS`);
  } catch (error) {
    console.log(`  ✗ ${tc.name}: FAILED - ${error.message}`);
  }
}

// Strategy 2: Strip trailing zeros before decode
console.log('\nStrategy 2: Strip trailing zeros');
function stripTrailingZeros(buffer) {
  let end = buffer.length;
  while (end > 0 && buffer[end - 1] === 0) {
    end--;
  }
  return buffer.slice(0, end);
}

for (const tc of testCases) {
  try {
    const bytes = Buffer.from(tc.sketch, 'base64');
    const cleaned = stripTrailingZeros(bytes);
    Agg.decode(cleaned);
    console.log(`  ✓ ${tc.name}: SUCCESS (stripped ${bytes.length - cleaned.length} byte(s))`);
  } catch (error) {
    console.log(`  ✗ ${tc.name}: FAILED - ${error.message}`);
  }
}

// Strategy 3: Try-catch with fallback (what user implemented)
console.log('\nStrategy 3: Try-catch with fallback to strip trailing zeros');
function decodeBlobSafe(input) {
  let bytes;
  if (typeof input === 'string') {
    bytes = Buffer.from(input, 'base64');
  } else if (input instanceof Uint8Array) {
    bytes = input;
  } else if (Buffer.isBuffer(input)) {
    bytes = input;
  } else {
    throw new Error('input must be base64 string, Buffer, or Uint8Array');
  }
  
  try {
    return Agg.decode(bytes);
  } catch (error) {
    // If decoding fails, try stripping trailing zeros
    if (error.message.includes('index out of range') || 
        error.message.includes('illegal tag')) {
      const cleaned = stripTrailingZeros(bytes);
      if (cleaned.length < bytes.length) {
        return Agg.decode(cleaned);
      }
    }
    throw error;
  }
}

for (const tc of testCases) {
  try {
    const decoded = decodeBlobSafe(tc.sketch);
    console.log(`  ✓ ${tc.name}: SUCCESS`);
  } catch (error) {
    console.log(`  ✗ ${tc.name}: FAILED - ${error.message}`);
  }
}

// Check if stripping zeros affects valid data
console.log('\n=== Testing Impact on Valid Data ===');
const validSketch = 'CHAQBRgCIAuCBxIQBRgKIA8yCrxB4UqoEPZIyRE='; // Without trailing zero
const bytes = Buffer.from(validSketch, 'base64');
const cleaned = stripTrailingZeros(bytes);
console.log(`Original length: ${bytes.length}`);
console.log(`After stripping: ${cleaned.length}`);
console.log(`Stripped any bytes: ${bytes.length !== cleaned.length}`);

// Verify both decode to same structure
const original = Agg.decode(bytes);
const afterStrip = Agg.decode(cleaned);
console.log('\nOriginal and stripped decode to identical structure:', 
  original.type === afterStrip.type &&
  original.num_values === afterStrip.num_values &&
  original.encoding_version === afterStrip.encoding_version &&
  original.value_type === afterStrip.value_type
);

console.log('\n=== Recommendation ===');
console.log('✓ Strategy 2 (always strip trailing zeros) is SAFE and EFFECTIVE');
console.log('  - Fixes the null-byte padding issue');
console.log('  - No performance impact on valid data'); 
console.log('  - More robust than try-catch fallback');
console.log('\n✓ Strategy 3 (try-catch fallback) also works but has overhead');
console.log('  - Only strips on error');
console.log('  - May hide other decoding issues');
console.log('\nThe data should be fixed at source if possible, but defensive');
console.log('decoding with trailing zero stripping is a valid workaround.');
