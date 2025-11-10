# How to Add a New Library - Step-by-Step Guide

## Quick Overview (30 seconds)

1. Edit `tools/bindgen/repo-mapping.yaml` - add your library config
2. Run `python3 tools/bindgen/generate_bindings.py --repo LIBRARY --verbose`
3. Review generated code in `extensions/awesome/LIBRARY/`
4. Commit and push

**That's it!** The generator does the rest.

---

## Detailed Walkthrough (5 minutes)

### Step 1: Choose a Library

Pick a C++ library from [awesome-cpp](https://github.com/fffaraz/awesome-cpp) that:
- ✅ Has a stable API
- ✅ Uses compatible license (MIT/BSD/Apache preferred)
- ✅ Is actively maintained
- ✅ Has clear documentation

**Good starting choices:**
- Header-only libraries (easiest)
- CMake-based libraries
- Libraries with minimal dependencies

### Step 2: Add Configuration

Edit `tools/bindgen/repo-mapping.yaml` and add an entry:

```yaml
- name: example-lib                          # Short name (no spaces)
  repo: https://github.com/user/example.git  # Git repository URL
  checkout: v1.0.0                           # Git tag or branch
  description: "What this library does"      # Brief description
  license: MIT                               # License type
  headers_path: include                      # Where headers are installed
  build_type: cmake                          # Build system (currently only cmake)

  # CMake options (optional)
  cmake_options:
    - "-DBUILD_TESTS=OFF"
    - "-DBUILD_EXAMPLES=OFF"

  # What to expose to Lisp
  expose:
    namespaces:
      - examplelib              # Root namespace(s)
    classes:
      - examplelib::Widget      # Specific classes
      - examplelib::Container
    functions:
      - examplelib::process     # Specific functions
      - examplelib::initialize

  # What to exclude
  exclude:
    namespaces:
      - examplelib::internal    # Private namespaces
      - examplelib::detail
    patterns:
      - ".*_impl$"              # Regex patterns
      - ".*_internal.*"

  # Lisp package names
  package_name: "EXAMPLE-LIB"   # Lisp package (uppercase)
  lisp_package: "example-lib"   # Nickname (lowercase)
```

**Configuration Tips:**

| Field | Tips |
|-------|------|
| `name` | Short, lowercase, no spaces. Used for directory name. |
| `checkout` | Use stable release tags (v1.0.0) not 'main' or 'master' |
| `headers_path` | Usually `include` or `src` - check library docs |
| `expose.namespaces` | Start with root namespace, add more if needed |
| `expose.classes` | List key classes only. Start small! |
| `expose.functions` | List important functions. Can expand later. |
| `exclude.patterns` | Use regex to filter internals (`.*detail.*`, `.*_internal.*`) |

### Step 3: Generate Bindings

Run the generator:

```bash
cd /path/to/clasp

# Generate for your library
python3 tools/bindgen/generate_bindings.py --repo example-lib --verbose
```

**What happens:**
1. ⏬ Clones the repository
2. 🔧 Runs CMake to build/install headers
3. 🔍 Discovers symbols (classes, functions)
4. 📝 Generates C++ clbind wrapper code
5. 📦 Creates Lisp package definitions
6. ✅ Writes tests, docs, and build files

**Output location:** `extensions/awesome/example-lib/`

### Step 4: Review Generated Code

Check what was created:

```bash
# View directory structure
ls -la extensions/awesome/example-lib/

# Check C++ bindings
cat extensions/awesome/example-lib/src/example-lib_bindings.cc

# Check Lisp package
cat extensions/awesome/example-lib/lisp/example-lib.lisp

# Read auto-generated README
cat extensions/awesome/example-lib/README.md

# View metadata
cat extensions/awesome/example-lib/METADATA
```

**Look for:**
- ✅ C++ code looks reasonable
- ✅ Classes and functions you wanted are exposed
- ✅ No unexpected internals exposed
- ✅ Lisp package exports make sense
- ✅ README is accurate

### Step 5: Validate Structure

Run validation:

```bash
./ci/build-and-test.sh
```

Should show:
```
✓ example-lib structure OK
✓ Tests present
Status: SUCCESS ✓
```

### Step 6: Test & Refine (Optional)

If you have clasp built, test the bindings:

```bash
cd extensions/awesome/example-lib
mkdir build && cd build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make
cd ..
clasp --load tests/test-example-lib.lisp
```

**Common issues:**

| Issue | Solution |
|-------|----------|
| Too many symbols | Add more `exclude` patterns in config |
| Missing symbols | Add to `expose` lists in config |
| Build errors | Check library builds standalone first |
| Template errors | May need manual wrappers (advanced) |

If needed, adjust configuration and regenerate:

```bash
# Edit config
vim tools/bindgen/repo-mapping.yaml

# Regenerate
python3 tools/bindgen/generate_bindings.py --repo example-lib --verbose

# Check differences
git diff extensions/awesome/example-lib/
```

### Step 7: Commit

Once satisfied:

```bash
# Stage changes
git add tools/bindgen/repo-mapping.yaml
git add extensions/awesome/example-lib/

# Commit with descriptive message
git commit -m "Add bindings for example-lib v1.0.0

- Expose Widget and Container classes
- Expose process and initialize functions
- Include tests and documentation
"

# Push
git push origin your-branch-name
```

---

## Real-World Examples

### Example 1: Header-Only Library (like nlohmann/json)

```yaml
- name: json
  repo: https://github.com/nlohmann/json.git
  checkout: v3.11.3
  description: "JSON for Modern C++"
  license: MIT
  headers_path: include
  build_type: cmake
  header_only: true                    # ← Marks as header-only
  cmake_options:
    - "-DJSON_BuildTests=OFF"
    - "-DJSON_Install=ON"
  expose:
    namespaces:
      - nlohmann
    classes:
      - nlohmann::json
    functions:
      - nlohmann::json::parse
  exclude:
    namespaces:
      - nlohmann::detail
  package_name: "AWESOME-JSON"
  lisp_package: "awesome-json"
```

### Example 2: Library with Build (like fmt)

```yaml
- name: fmt
  repo: https://github.com/fmtlib/fmt.git
  checkout: "10.2.1"
  description: "A modern formatting library"
  license: MIT
  headers_path: include
  build_type: cmake
  cmake_options:
    - "-DFMT_DOC=OFF"
    - "-DFMT_TEST=OFF"
    - "-DFMT_INSTALL=ON"
  expose:
    namespaces:
      - fmt
    classes:
      - fmt::format_error
    functions:
      - fmt::format
      - fmt::print
  exclude:
    namespaces:
      - fmt::detail
    patterns:
      - ".*_internal.*"
  package_name: "AWESOME-FMT"
  lisp_package: "awesome-fmt"
```

### Example 3: Logging Library (like spdlog)

```yaml
- name: spdlog
  repo: https://github.com/gabime/spdlog.git
  checkout: v1.13.0
  description: "Fast C++ logging library"
  license: MIT
  headers_path: include
  build_type: cmake
  cmake_options:
    - "-DSPDLOG_BUILD_TESTS=OFF"
    - "-DSPDLOG_INSTALL=ON"
  expose:
    namespaces:
      - spdlog
    classes:
      - spdlog::logger
    functions:
      - spdlog::info
      - spdlog::warn
      - spdlog::error
  exclude:
    namespaces:
      - spdlog::details
    patterns:
      - ".*_mt$"    # Skip multithreaded variants
  package_name: "AWESOME-SPDLOG"
  lisp_package: "awesome-spdlog"
```

---

## Tips & Best Practices

### 🎯 Start Small
- Begin with 2-3 key classes/functions
- Expand in later iterations
- Easier to review and debug

### 🔍 Use Exclude Patterns
```yaml
exclude:
  patterns:
    - ".*detail.*"      # Anything with 'detail'
    - ".*internal.*"    # Anything with 'internal'
    - ".*_impl$"        # Ends with '_impl'
    - ".*_mt$"          # Ends with '_mt' (multithreaded variants)
```

### 📝 Namespace Strategy
```yaml
expose:
  namespaces:
    - mylib           # Root namespace

exclude:
  namespaces:
    - mylib::internal # Exclude subnamespace
    - mylib::detail
```

### 🚫 What Not to Do
- ❌ Don't expose everything - be selective
- ❌ Don't use 'main' or 'master' branches - use release tags
- ❌ Don't skip testing before committing
- ❌ Don't commit if generator shows errors

### ✅ What to Do
- ✅ Use stable release tags
- ✅ Start with minimal exposure
- ✅ Test generated code
- ✅ Review generated files before committing
- ✅ Document any issues in README

---

## Troubleshooting

### Generator Fails to Clone

**Problem:** `git clone` fails

**Solutions:**
- Check repo URL is correct
- Verify network connection
- Try cloning manually: `git clone REPO_URL`

### CMake Configuration Fails

**Problem:** CMake errors during configuration

**Solutions:**
- Check library builds standalone first
- Review library's build documentation
- Add necessary cmake_options
- Check for missing dependencies

### Too Many Symbols Generated

**Problem:** Hundreds of unwanted symbols

**Solutions:**
```yaml
# Add more exclusion patterns
exclude:
  namespaces:
    - mylib::internal
    - mylib::detail
    - mylib::impl
  patterns:
    - ".*_internal.*"
    - ".*detail::.*"
    - "operator.*"      # Skip operators if problematic
```

### Missing Symbols

**Problem:** Expected classes/functions not generated

**Solutions:**
1. Check symbol is in allowed namespace
2. Add explicitly to `expose` section
3. Remove from `exclude` patterns
4. Verify symbol is actually public in C++

### Compilation Errors (when building with clasp)

**Problem:** Generated C++ doesn't compile

**Common causes:**
- Template-heavy APIs → May need manual wrappers
- Operator overloads → Add to exclude patterns
- Complex type conversions → May need custom converters

**Solutions:**
- Simplify exposed symbols
- Check clbind documentation for advanced features
- Consider manual wrapper layer

---

## Quick Command Reference

```bash
# Add to config
vim tools/bindgen/repo-mapping.yaml

# Generate
python3 tools/bindgen/generate_bindings.py --repo LIBRARY --verbose

# Validate
./ci/build-and-test.sh

# Review
cat extensions/awesome/LIBRARY/README.md
git diff extensions/awesome/LIBRARY/

# Commit
git add tools/bindgen/repo-mapping.yaml extensions/awesome/LIBRARY/
git commit -m "Add LIBRARY bindings"
```

---

## Getting Help

- **Full docs**: See `README-clbind-awesome-cpp.md`
- **Contributing guide**: See `CONTRIBUTING-BINDINGS.md`
- **Command reference**: See `tools/bindgen/COMMANDS.md`
- **Test results**: See `tools/bindgen/TEST_RESULTS.md`
- **Quick start**: See `tools/bindgen/QUICKSTART.md`

---

## Success Checklist

Before committing, verify:

- [ ] Library clones successfully
- [ ] CMake builds/configures without errors
- [ ] Generator runs without errors
- [ ] Generated C++ looks reasonable
- [ ] Generated Lisp looks reasonable
- [ ] No unexpected symbols exposed
- [ ] All expected symbols present
- [ ] README.md is accurate
- [ ] METADATA file present
- [ ] LICENSE file included (if available)
- [ ] Tests present
- [ ] Validation script passes
- [ ] Git shows expected changes only

---

**You're ready to add libraries!** 🚀

Start with a simple header-only library for practice, then move to more complex ones.
