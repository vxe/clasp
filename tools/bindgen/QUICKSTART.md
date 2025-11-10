# Quick Start Guide - Clasp Awesome-Cpp Bindings

Get up and running with the clbind binding generator in 5 minutes.

## Install Dependencies

### Ubuntu/Debian
```bash
sudo apt-get update
sudo apt-get install -y \
  python3 python3-pip \
  cmake build-essential \
  clang-15 llvm-15-dev libclang-15-dev \
  git

pip3 install -r tools/bindgen/requirements.txt
```

### macOS
```bash
brew install python cmake llvm@15
pip3 install -r tools/bindgen/requirements.txt
```

## Generate Your First Binding

### Option 1: Single Library
```bash
# Generate bindings for fmt
python3 tools/bindgen/generate_bindings.py --repo fmt --verbose

# Output will be in: extensions/awesome/fmt/
```

### Option 2: All Libraries
```bash
# Generate all configured bindings
python3 tools/bindgen/generate_bindings.py --all --verbose
```

### Option 3: Use Helper Script
```bash
# Single library
./tools/bindgen/update.sh --repo fmt

# All libraries
./tools/bindgen/update.sh --all
```

## What Gets Generated?

After running the generator, you'll find:

```
extensions/awesome/fmt/
├── src/
│   └── fmt_bindings.cc          # C++ clbind wrapper
├── lisp/
│   └── fmt.lisp                 # Lisp package
├── tests/
│   └── test-fmt.lisp            # Test suite
├── CMakeLists.txt               # Build config
├── README.md                    # Documentation
├── METADATA                     # Generation info
└── LICENSE.fmt                  # Upstream license
```

## Understanding the Output

### C++ Bindings (`src/*.cc`)
```cpp
void initialize_fmt_bindings() {
  package_ pkg("AWESOME-FMT");
  scope_& scope = pkg.scope();

  // Exposes C++ classes and functions to Lisp
  scope.def("format", &fmt::format);
}
```

### Lisp Package (`lisp/*.lisp`)
```lisp
(defpackage :awesome-fmt
  (:use :common-lisp)
  (:export #:format #:print))

;; Usage:
;; (awesome-fmt:format "Hello {}" "World")
```

### Tests (`tests/*.lisp`)
```lisp
(defun test-format ()
  (let ((result (awesome-fmt:format "x={}" 42)))
    (assert (string= result "x=42"))))
```

## Customizing Generation

Edit `tools/bindgen/repo-mapping.yaml`:

```yaml
- name: my-lib
  repo: https://github.com/user/my-lib.git
  checkout: v1.0.0

  # What to expose
  expose:
    namespaces:
      - mylib
    classes:
      - mylib::MyClass
    functions:
      - mylib::process

  # What to exclude
  exclude:
    namespaces:
      - mylib::internal
    patterns:
      - ".*_detail.*"
```

Then regenerate:
```bash
python3 tools/bindgen/generate_bindings.py --repo my-lib --verbose
```

## Common Tasks

### Add a New Library

1. Edit `tools/bindgen/repo-mapping.yaml`
2. Add your library entry
3. Run: `python3 tools/bindgen/generate_bindings.py --repo my-lib --verbose`
4. Review: `extensions/awesome/my-lib/`
5. Test: Adjust configuration and regenerate as needed

### Update an Existing Library

```bash
# Update to new version - edit repo-mapping.yaml checkout tag
# Then regenerate
./tools/bindgen/update.sh --repo fmt
```

### Check What Changed

```bash
git status extensions/awesome/
git diff extensions/awesome/fmt/
```

### Run Tests

```bash
# If you have clasp built
clasp --load extensions/awesome/fmt/tests/test-fmt.lisp

# Or verify structure
./ci/build-and-test.sh
```

## Troubleshooting

### "libclang not found"
```bash
pip3 install libclang
# Or set path
export LLVM_CONFIG=/usr/bin/llvm-config-15
```

### "Too many symbols generated"
Add exclusion patterns in repo-mapping.yaml:
```yaml
exclude:
  patterns:
    - ".*_internal.*"
    - ".*detail::.*"
```

### "Generated code doesn't compile"
- Check C++ standard version (config: `clang_args: ["-std=c++17"]`)
- Verify library builds standalone first
- Check for template-heavy APIs (may need manual wrappers)

### "Repository clone failed"
- Check internet connection
- Verify repo URL is correct
- Try: `git clone <repo>` manually to test

## Using Docker

Build and use the Docker environment:

```bash
# Build image
docker build -t clasp-bindgen -f ci/docker/Dockerfile.bindgen .

# Run generator in container
docker run --rm -v $(pwd):/workspace clasp-bindgen \
  python3 tools/bindgen/generate_bindings.py --all --verbose

# Interactive shell
docker run --rm -it -v $(pwd):/workspace clasp-bindgen /bin/bash
```

## Next Steps

1. **Read the full docs**: `README-clbind-awesome-cpp.md`
2. **Review examples**: Check `extensions/awesome/fmt/`
3. **Add your library**: Follow `CONTRIBUTING-BINDINGS.md`
4. **Join the community**: Clasp Discord/mailing list

## Quick Command Reference

```bash
# Generate
python3 tools/bindgen/generate_bindings.py --repo LIBRARY --verbose
python3 tools/bindgen/generate_bindings.py --all

# Helper
./tools/bindgen/update.sh --repo LIBRARY
./tools/bindgen/update.sh --all

# Verify
./ci/build-and-test.sh

# Test (requires clasp)
clasp --load extensions/awesome/LIBRARY/tests/test-LIBRARY.lisp
```

## File Locations

| What | Where |
|------|-------|
| Generator script | `tools/bindgen/generate_bindings.py` |
| Configuration | `tools/bindgen/repo-mapping.yaml` |
| Generated output | `extensions/awesome/LIBRARY/` |
| CI scripts | `ci/` |
| Main docs | `README-clbind-awesome-cpp.md` |
| Contributing | `CONTRIBUTING-BINDINGS.md` |

---

**Happy binding!** 🚀

For questions: Open an issue or check the full documentation.
