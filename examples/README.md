# Krypton Examples

This directory contains example programs written in Krypton.

## Running Examples

To run an example, use the Krypton CLI:

```bash
# From the project root
cargo run --bin krypton-cli -- --file examples/variables.kr
```

## Example: variables.kr

This example demonstrates variable usage in Krypton:

1. Variable declaration and initialization
2. Basic arithmetic with variables
3. Variable reassignment
4. String variables and concatenation

Expected output:
```
52
-10
420
4
Hello World
```

The output shows:
- Sum of x and y (42 + 10 = 52)
- Difference between x and y (42 - 10 = -10)
- Product of x and y (42 * 10 = 420)
- Quotient of x and y (42 / 10 = 4)
- Concatenated greeting ("Hello World") 