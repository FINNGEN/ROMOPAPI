// Check sketch C for trailing zeros
const sketchC = 'CAGQBxIM0AEQBRgKIA8yCgoYAAA=';
const bytes = Buffer.from(sketchC, 'base64');

console.log('Sketch C bytes:', Array.from(bytes));
console.log('Length:', bytes.length);
console.log('Last 5 bytes:', Array.from(bytes.slice(-5)));
console.log('');

// Count trailing zeros
let trailingZeros = 0;
for (let i = bytes.length - 1; i >= 0; i--) {
  if (bytes[i] === 0) {
    trailingZeros++;
  } else {
    break;
  }
}

console.log('Trailing zeros:', trailingZeros);
console.log('');

// Strip and try again
const stripped = bytes.slice(0, bytes.length - trailingZeros);
console.log('After stripping:', Array.from(stripped));
console.log('New length:', stripped.length);
console.log('New base64:', stripped.toString('base64'));
