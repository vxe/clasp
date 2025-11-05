# fmt Bindings for Clasp

Auto-generated clbind bindings for [fmt](https://github.com/fmtlib/fmt) - A modern formatting library.

## Description

This extension provides Clasp bindings for the fmt C++ formatting library, allowing you to use fmt's powerful formatting capabilities from Common Lisp.

## Status

This is an **example/template** structure. The actual bindings can be generated using:

```bash
cd /home/user/clasp
python3 tools/bindgen/generate_bindings.py --repo fmt --verbose
```

## Example Usage (once full bindings are generated)

```lisp
(require :awesome-fmt)

;; Format a string
(fmt:format "Hello, {}!" "World")
;; => "Hello, World!"

;; Format with multiple arguments
(fmt:format "{} + {} = {}" 1 2 3)
;; => "1 + 2 = 3"
```

## Building

Once generated, build with:

```bash
mkdir -p extensions/awesome/fmt/build
cd extensions/awesome/fmt/build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make -j$(nproc)
```

## Testing

```bash
clasp --load extensions/awesome/fmt/tests/test-fmt.lisp
```

## License

MIT (same as fmt library)

## Metadata

- Generator version: 0.1.0
- Source: https://github.com/fmtlib/fmt.git
- Example created: 2025-11-05
