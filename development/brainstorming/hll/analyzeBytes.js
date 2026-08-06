// Detailed protobuf structure analysis
import protobuf from 'protobufjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const PROTO_PATH = join(__dirname, 'zetasketch.proto');

const root = await protobuf.load(PROTO_PATH);
const Agg = root.lookupType('zetasketch.AggregatorStateProto');
const Hll = root.lookupType('zetasketch.HyperLogLogPlusUniqueStateProto');

const problematicSketch = 'CHAQBRgCIAuCBxIQBRgKIA8yCrxB4UqoEPZIyREA';
const bytes = Buffer.from(problematicSketch, 'base64');

console.log('=== Protobuf Byte Analysis ===\n');
console.log('Total length:', bytes.length);
console.log('Bytes:', Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join(' '));
console.log('Last byte is:', bytes[bytes.length - 1], '(0x' + bytes[bytes.length - 1].toString(16) + ')');
console.log('');

// Manual protobuf wire format parsing
console.log('=== Manual Wire Format Parse ===');
let pos = 0;
function readVarint(buf, offset) {
  let value = 0;
  let shift = 0;
  let b;
  do {
    b = buf[offset++];
    value |= (b & 0x7f) << shift;
    shift += 7;
  } while (b & 0x80);
  return [value, offset];
}

while (pos < bytes.length) {
  const tagByte = bytes[pos];
  console.log(`\nByte ${pos}: 0x${tagByte.toString(16).padStart(2, '0')} (${tagByte})`);
  
  if (tagByte === 0) {
    console.log('  ⚠️  TAG = 0 → INVALID! (field number must be ≥ 1 in protobuf)');
    console.log('  This is what causes "illegal tag: field number 0" error');
    break;
  }
  
  const fieldNumber = tagByte >> 3;
  const wireType = tagByte & 0x7;
  console.log(`  Field number: ${fieldNumber}`);
  console.log(`  Wire type: ${wireType} (${wireType === 0 ? 'varint' : wireType === 2 ? 'length-delimited' : 'other'})`);
  
  pos++;
  
  if (wireType === 0) { // varint
    const [value, newPos] = readVarint(bytes, pos);
    console.log(`  Value: ${value}`);
    pos = newPos;
  } else if (wireType === 2) { // length-delimited
    const [length, newPos] = readVarint(bytes, pos);
    console.log(`  Length: ${length} bytes`);
    const data = bytes.slice(newPos, newPos + length);
    console.log(`  Data: [${Array.from(data).map(b => '0x' + b.toString(16).padStart(2, '0')).join(', ')}]`);
    pos = newPos + length;
  } else {
    console.log('  (skipping other wire type)');
    break;
  }
}

console.log('\n=== Attempting Decode (with trailing zero) ===');
try {
  const agg = Agg.decode(bytes);
  console.log('✓ Decoded successfully');
  console.log(JSON.stringify(agg, null, 2));
} catch (error) {
  console.log('✗ Decode failed:', error.message);
}

console.log('\n=== Attempting Decode (without trailing zero) ===');
const bytesNoTrailing = bytes.slice(0, -1);
try {
  const agg = Agg.decode(bytesNoTrailing);
  console.log('✓ Decoded successfully');
  console.log(JSON.stringify({
    type: agg.type,
    num_values: agg.num_values?.toString(),
    encoding_version: agg.encoding_version,
    value_type: agg.value_type,
    hll_ext_length: agg.hll_ext?.length
  }, null, 2));
  
  if (agg.hll_ext) {
    console.log('\n=== HLL Extension ===');
    const hll = Hll.decode(agg.hll_ext);
    console.log(JSON.stringify({
      sparse_size: hll.sparse_size,
      precision: hll.precision,
      sparse_precision: hll.sparse_precision,
      data_length: hll.data?.length,
      sparse_data_length: hll.sparse_data?.length
    }, null, 2));
  }
} catch (error) {
  console.log('✗ Decode failed:', error.message);
}

// Check if this is a pattern in BigQuery output
console.log('\n=== Analysis ===');
console.log('The trailing 0x00 byte appears to be extra padding or corruption.');
console.log('Valid protobuf messages should NOT have trailing zero bytes unless');
console.log('they encode a field. A standalone 0x00 is an invalid tag.');
console.log('\nThis could be:');
console.log('1. BigQuery bug in HLL_COUNT.INIT output');
console.log('2. Data corruption during transmission/storage');
console.log('3. String truncation/padding issue in the database');
