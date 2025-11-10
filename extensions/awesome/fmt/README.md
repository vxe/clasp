# fmt Bindings for Clasp

Auto-generated clbind bindings for [fmt](https://github.com/fmtlib/fmt.git).

## Description

A modern formatting library

## License

MIT

## Generation

These bindings were auto-generated using `tools/bindgen/generate_bindings.py`.

To regenerate:

```bash
cd clasp
python3 tools/bindgen/generate_bindings.py --repo fmt
```

## Building

```bash
mkdir build && cd build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make -j$(nproc)
```

## Testing

```bash
clasp --load tests/test-fmt.lisp
```

## Metadata

- Generator version: 0.1.0
- Source commit: e69e5f977d458f2650bb346dadf2ad30c5320281
- Generated: 2025-11-05
