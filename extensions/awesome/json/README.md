# json Bindings for Clasp

Auto-generated clbind bindings for [json](https://github.com/nlohmann/json.git).

## Description

JSON for Modern C++

## License

MIT

## Generation

These bindings were auto-generated using `tools/bindgen/generate_bindings.py`.

To regenerate:

```bash
cd clasp
python3 tools/bindgen/generate_bindings.py --repo json
```

## Building

```bash
mkdir build && cd build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make -j$(nproc)
```

## Testing

```bash
clasp --load tests/test-json.lisp
```

## Metadata

- Generator version: 0.1.0
- Source commit: 9cca280a4d0ccf0c08f47a99aa71d1b0e52f8d03
- Generated: 2025-11-05
