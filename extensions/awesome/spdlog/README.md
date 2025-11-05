# spdlog Bindings for Clasp

Auto-generated clbind bindings for [spdlog](https://github.com/gabime/spdlog.git).

## Description

Fast C++ logging library

## License

MIT

## Generation

These bindings were auto-generated using `tools/bindgen/generate_bindings.py`.

To regenerate:

```bash
cd clasp
python3 tools/bindgen/generate_bindings.py --repo spdlog
```

## Building

```bash
mkdir build && cd build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make -j$(nproc)
```

## Testing

```bash
clasp --load tests/test-spdlog.lisp
```

## Metadata

- Generator version: 0.1.0
- Source commit: 7c02e204c92545f869e2f04edaab1f19fe8b19fd
- Generated: 2025-11-05
