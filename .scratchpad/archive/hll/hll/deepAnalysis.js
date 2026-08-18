// Deep analysis of both sketches to understand the trailing zero pattern
import protobuf from 'protobufjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PROTO_PATH = join(__dirname, 'zetasketch.proto');

const root = await protobuf.load(PROTO_PATH);
const Agg = root.lookupType('zetasketch.AggregatorStateProto');

console.log('=== Analyzing Sketch Structure ===\n');

// Sketch A - works after removing 1 trailing zero
const sketchA = 'CHAQBRgCIAuCBxIQBRgKIA8yCrxB4UqoEPZIyREA';
const bytesA = Buffer.from(sketchA, 'base64');
console.log('Sketch A (30 bytes, 1 trailing zero):');
console.log(Array.from(bytesA).map((b, i) => `[${i}]:${b}`).join(' '));

// Try decoding with and without last byte
console.log('\nWith trailing zero (30 bytes):');
try {
  const decoded = Agg.decode(bytesA);
  console.log('  ✓ Decoded (should not happen!)');
} catch (e) {
  console.log('  ✗ Error:', e.message);
}

console.log('\nWithout trailing zero (29 bytes):');
try {
  const decoded = Agg.decode(bytesA.slice(0, -1));
  console.log('  ✓ Decoded successfully');
  console.log('  Fields:', {
    type: decoded.type,
    num_values: decoded.num_values?.toString(),
    encoding_version: decoded.encoding_version,
    value_type: decoded.value_type,
    hll_ext_length: decoded.hll_ext?.length
  });
} catch (e) {
  console.log('  ✗ Error:', e.message);
}

// Sketch C - has 2 trailing zeros
console.log('\n\nSketch C (20 bytes, 2 trailing zeros):');
const sketchC = 'CAGQBxIM0AEQBRgKIA8yCgoYAAA=';
const bytesC = Buffer.from(sketchC, 'base64');
console.log(Array.from(bytesC).map((b, i) => `[${i}]:${b}`).join(' '));

// Try different truncations
for (let removeCount = 0; removeCount <= 3; removeCount++) {
  const truncated = bytesC.slice(0, bytesC.length - removeCount);
  console.log(`\nWith ${bytesC.length - removeCount} bytes (removed ${removeCount}):`);
  try {
    const decoded = Agg.decode(truncated);
    console.log(`  ✓ Decoded successfully`);
    console.log('  Fields:', {
      type: decoded.type,
      num_values: decoded.num_values?.toString(),
      encoding_version: decoded.encoding_version,
      value_type: decoded.value_type,
      hll_ext_length: decoded.hll_ext?.length
    });
    
    // Try to decode the hll_ext too
    if (decoded.hll_ext) {
      const Hll = root.lookupType('zetasketch.HyperLogLogPlusUniqueStateProto');
      try {
        const hll = Hll.decode(decoded.hll_ext);
        console.log('  HLL fields:', {
          sparse_size: hll.sparse_size,
          precision: hll.precision,
          sparse_precision: hll.sparse_precision,
          data_length: hll.data?.length,
          sparse_data_length: hll.sparse_data?.length
        });
        
        // Check if sparse_data has trailing zeros
        if (hll.sparse_data) {
          const sd = hll.sparse_data;
          console.log('  sparse_data bytes:', Array.from(sd));
          let trailing = 0;
          for (let i = sd.length - 1; i >= 0; i--) {
            if (sd[i] === 0) trailing++;
            else break;
          }
          console.log('  sparse_data trailing zeros:', trailing);
        }
      } catch (e) {
        console.log('  ✗ HLL decode error:', e.message);
      }
    }
  } catch (e) {
    console.log(`  ✗ Error: ${e.message}`);
  }
}
