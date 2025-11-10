# Quick Command Reference

## Generate Bindings

```bash
# Generate all configured libraries
python3 tools/bindgen/generate_bindings.py --all --verbose

# Generate single library
python3 tools/bindgen/generate_bindings.py --repo fmt --verbose
python3 tools/bindgen/generate_bindings.py --repo json --verbose
python3 tools/bindgen/generate_bindings.py --repo spdlog --verbose

# Using helper script
./tools/bindgen/update.sh --all
./tools/bindgen/update.sh --repo fmt
```

## Validate Generated Code

```bash
# Run full validation
./ci/build-and-test.sh --verbose

# Check structure only
for ext in extensions/awesome/*/; do
  echo "Checking $(basename $ext)..."
  test -f "$ext/README.md" && echo "  ✓ README"
  test -f "$ext/METADATA" && echo "  ✓ METADATA"
  test -d "$ext/src" && echo "  ✓ src/"
  test -d "$ext/lisp" && echo "  ✓ lisp/"
done
```

## View Generated Code

```bash
# View C++ bindings
cat extensions/awesome/fmt/src/fmt_bindings.cc

# View Lisp package
cat extensions/awesome/fmt/lisp/fmt.lisp

# View metadata
cat extensions/awesome/fmt/METADATA

# View all generated files
find extensions/awesome -type f | sort
```

## Git Operations

```bash
# Check what changed
git status extensions/awesome/
git diff extensions/awesome/

# View generated code diffs
git diff extensions/awesome/fmt/src/

# See generation history
git log --oneline extensions/awesome/
```

## Configuration

```bash
# Edit configuration
vim tools/bindgen/repo-mapping.yaml

# Validate YAML syntax
python3 -c "import yaml; yaml.safe_load(open('tools/bindgen/repo-mapping.yaml'))"

# View current config
cat tools/bindgen/repo-mapping.yaml
```

## Docker

```bash
# Build Docker image
docker build -t clasp-bindgen -f ci/docker/Dockerfile.bindgen .

# Run generator in container
docker run --rm -v $(pwd):/workspace clasp-bindgen \
  python3 tools/bindgen/generate_bindings.py --all --verbose

# Interactive shell
docker run --rm -it -v $(pwd):/workspace clasp-bindgen bash
```

## Statistics

```bash
# Count extensions
ls -d extensions/awesome/*/ | wc -l

# Count generated files
find extensions/awesome -type f | wc -l

# Lines of code
find extensions/awesome -name "*.cc" -o -name "*.lisp" | xargs wc -l

# Show structure
ls -la extensions/awesome/*/
```

## Clean Up

```bash
# Remove temp build directories
rm -rf /tmp/clbind-gen/

# Remove all generated extensions (careful!)
# git clean -fdx extensions/awesome/
```

## Testing (requires clasp built)

```bash
# Load an extension test
clasp --load extensions/awesome/fmt/tests/test-fmt.lisp

# Build an extension
cd extensions/awesome/fmt
mkdir build && cd build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make -j$(nproc)
```

## Documentation

```bash
# Main docs
cat README-clbind-awesome-cpp.md

# Quick start
cat tools/bindgen/QUICKSTART.md

# Test results
cat tools/bindgen/TEST_RESULTS.md

# Contributing guide
cat CONTRIBUTING-BINDINGS.md
```

## CI/CD

```bash
# Check workflow syntax
cat .github/workflows/generate-and-test-bindings.yml

# Run locally
./ci/build-and-test.sh

# Check Docker build
docker build -t clasp-bindgen -f ci/docker/Dockerfile.bindgen . --no-cache
```

## Troubleshooting

```bash
# Check dependencies
python3 --version
cmake --version
git --version
python3 -c "import yaml; print('PyYAML OK')"
python3 -c "import clang.cindex; print('libclang OK')"

# Verify generator syntax
python3 -m py_compile tools/bindgen/generate_bindings.py

# Get help
python3 tools/bindgen/generate_bindings.py --help
```

## Common Workflows

### Add a New Library

1. Edit `tools/bindgen/repo-mapping.yaml`
2. Add library configuration
3. Generate: `python3 tools/bindgen/generate_bindings.py --repo NEWLIB --verbose`
4. Review: `cat extensions/awesome/NEWLIB/README.md`
5. Validate: `./ci/build-and-test.sh`
6. Commit: `git add extensions/awesome/NEWLIB/ && git commit -m "Add NEWLIB bindings"`

### Update Existing Library

1. Edit version in `tools/bindgen/repo-mapping.yaml`
2. Regenerate: `./tools/bindgen/update.sh --repo LIBRARY`
3. Review changes: `git diff extensions/awesome/LIBRARY/`
4. Commit if satisfied

### Full Regeneration

```bash
# Regenerate everything
python3 tools/bindgen/generate_bindings.py --all --verbose

# Check what changed
git status extensions/awesome/

# Review and commit
git diff extensions/awesome/
git add extensions/awesome/
git commit -m "Regenerate all bindings"
```
