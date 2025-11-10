# Clbind Bindings for Awesome-C++ Libraries

This project provides auto-generated **clbind** bindings for popular C++ libraries from the [awesome-cpp](https://github.com/fffaraz/awesome-cpp) collection, making them accessible from **Common Lisp** via **Clasp**.

## Overview

Clasp is a Common Lisp implementation that seamlessly interoperates with C++. This project extends Clasp by providing ready-to-use bindings for high-quality C++ libraries, enabling Lisp developers to leverage:

- **Modern formatting** with [fmt](https://github.com/fmtlib/fmt)
- **JSON processing** with [nlohmann/json](https://github.com/nlohmann/json)
- **Fast logging** with [spdlog](https://github.com/gabime/spdlog)
- And more...

## Features

- **Automated generation**: Bindings are auto-generated using libclang AST parsing
- **Reproducible**: Docker-based environment ensures consistent builds
- **CI/CD integrated**: GitHub Actions runs generation + tests on every commit
- **Well-structured**: Each library gets its own extension package with tests and docs
- **Upstream-friendly**: Designed to be PR'd back to clasp mainline

## Quick Start

### Prerequisites

```bash
# Ubuntu/Debian
sudo apt-get install -y python3 python3-pip cmake build-essential clang-15 libclang-15-dev

# Install Python dependencies
pip3 install -r tools/bindgen/requirements.txt
```

### Generate Bindings

```bash
# Generate all bindings
python3 tools/bindgen/generate_bindings.py --all --verbose

# Generate bindings for a specific library
python3 tools/bindgen/generate_bindings.py --repo fmt --verbose

# Or use the helper script
./tools/bindgen/update.sh --all
```

### Structure

After generation, extensions are organized as:

```
extensions/awesome/
  fmt/
    src/            # C++ clbind wrapper code
    lisp/           # Lisp package and loader
    tests/          # Test files
    README.md       # Library-specific documentation
    METADATA        # Generation metadata
    CMakeLists.txt  # Build configuration
  json/
    ...
  spdlog/
    ...
```

## Architecture

### Components

1. **`tools/bindgen/generate_bindings.py`**: Main generator script
   - Uses libclang to parse C++ headers
   - Discovers classes, functions, enums
   - Generates clbind wrapper code
   - Emits Lisp loader and test stubs

2. **`tools/bindgen/repo-mapping.yaml`**: Configuration
   - Lists target libraries
   - Specifies what to expose/exclude
   - Defines build options

3. **`ci/`**: CI/CD infrastructure
   - Docker images for reproducible builds
   - GitHub Actions workflows
   - Build and test scripts

### Generation Process

```
┌──────────────────┐
│  repo-mapping    │
│    .yaml         │
└────────┬─────────┘
         │
         v
┌──────────────────┐
│  Clone repo      │
│  (git)           │
└────────┬─────────┘
         │
         v
┌──────────────────┐
│  Build library   │
│  (cmake)         │
└────────┬─────────┘
         │
         v
┌──────────────────┐
│  Parse headers   │
│  (libclang)      │
└────────┬─────────┘
         │
         v
┌──────────────────┐
│  Generate        │
│  clbind code     │
└────────┬─────────┘
         │
         v
┌──────────────────┐
│  Write extension │
│  (C++ + Lisp)    │
└──────────────────┘
```

## Configuration

Edit `tools/bindgen/repo-mapping.yaml` to add or modify libraries:

```yaml
- name: my-lib
  repo: https://github.com/user/my-lib.git
  checkout: v1.0.0
  description: "My awesome library"
  license: MIT
  headers_path: include
  build_type: cmake
  cmake_options:
    - "-DBUILD_TESTS=OFF"
  expose:
    namespaces:
      - mylib
    classes:
      - mylib::MyClass
    functions:
      - mylib::my_function
  exclude:
    namespaces:
      - mylib::internal
  package_name: "MY-LIB"
  lisp_package: "my-lib"
```

## CI/CD

The GitHub Actions workflow (`.github/workflows/generate-and-test-bindings.yml`) automatically:

1. Generates bindings for all libraries
2. Checks for uncommitted changes (fails if out of sync)
3. Runs structure validation
4. Runs unit tests (when clasp is available)
5. Builds Docker image for reproducibility

### Running CI Locally

```bash
# Using Docker
docker build -t clasp-bindgen -f ci/docker/Dockerfile.bindgen .
docker run -v $(pwd):/workspace clasp-bindgen \
  python3 tools/bindgen/generate_bindings.py --all --verbose

# Or directly
./ci/build-and-test.sh --verbose
```

## Testing

Each extension includes tests in `tests/`:

```bash
# Run tests for a specific extension (requires clasp)
clasp --load extensions/awesome/fmt/tests/test-fmt.lisp

# Or run all tests
for test in extensions/awesome/*/tests/test-*.lisp; do
  clasp --load "$test"
done
```

## Development Workflow

### Adding a New Library

1. Add entry to `tools/bindgen/repo-mapping.yaml`
2. Run generator: `python3 tools/bindgen/generate_bindings.py --repo my-lib --verbose`
3. Review generated code in `extensions/awesome/my-lib/`
4. Adjust configuration if needed (expose/exclude patterns)
5. Build and test
6. Commit generated files

### Updating an Existing Library

```bash
# Regenerate bindings
./tools/bindgen/update.sh --repo fmt

# Review changes
git diff extensions/awesome/fmt/

# Commit if appropriate
git add extensions/awesome/fmt/
git commit -m "Update fmt bindings"
```

### Troubleshooting

**Problem**: libclang not found

```bash
# Install libclang Python bindings
pip3 install libclang

# Set LLVM path if needed
export LLVM_CONFIG=/usr/bin/llvm-config-15
```

**Problem**: Parser finds too many symbols

- Tighten `exclude` patterns in repo-mapping.yaml
- Use namespace filtering
- Manually specify symbols in `expose` section

**Problem**: Generated bindings don't compile

- Check C++ standard version (use `-std=c++17` in clang_args)
- Verify library is built correctly
- Check for template-heavy APIs (may need manual wrappers)

## Design Principles

1. **Minimal manual intervention**: Generator should produce working bindings automatically
2. **Upstream-friendly**: Structure and style match clasp conventions
3. **Reproducible**: Same inputs → same outputs (deterministic generation)
4. **Fail-fast**: CI fails if generated code is out of sync
5. **Transparent**: Metadata files track provenance (source commit, generator version)

## Limitations

Current limitations and future work:

- **Templates**: Limited support for complex template instantiations
  - Workaround: Manually specify common instantiations in config
- **Operator overloads**: Not automatically exposed
  - Workaround: Wrap in named functions
- **Complex ownership**: Shared_ptr, unique_ptr handling is basic
  - Future: Better smart pointer support
- **Callbacks**: C++ → Lisp callbacks need manual wrappers
  - Future: Automated callback generation

## Contributing

### To This Project

See `CONTRIBUTING-BINDINGS.md` for details on:
- Code style and conventions
- PR requirements
- Testing guidelines
- Review process

### Upstream to Clasp

To contribute these bindings to upstream clasp:

1. Start with small, stable libraries (fmt, json)
2. Ensure tests are comprehensive and stable
3. Include clear documentation
4. Open PR with 1-3 extensions at a time
5. Be responsive to maintainer feedback

## License

- **Generator code**: Same as clasp (LGPL 2.1+)
- **Generated bindings**: Inherit license of target library (see each extension's LICENSE file)
- **This is important**: Each extension includes attribution and license info from the upstream C++ library

## Resources

- [Clasp documentation](https://github.com/clasp-developers/clasp)
- [Clbind manual](https://clasp-developers.github.io/clbind-doc.html)
- [Awesome-cpp list](https://github.com/fffaraz/awesome-cpp)
- [libclang Python bindings](https://libclang.readthedocs.io/)

## Roadmap

- [x] Basic generator with libclang support
- [x] CI/CD pipeline
- [x] Starter libraries (fmt, json, spdlog)
- [ ] Expand to 10+ libraries
- [ ] Improve template handling
- [ ] Add smart pointer support
- [ ] Generate API documentation
- [ ] Submit upstream PR to clasp

## Contact

For questions or issues:
- Open an issue in this repository
- Discuss on clasp mailing list/Discord
- See `CONTRIBUTING-BINDINGS.md` for more

---

**Status**: Alpha - Core infrastructure complete, expanding library coverage

**Last Updated**: 2025-11-05
