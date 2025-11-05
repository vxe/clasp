# Contributing to Clasp Awesome-Cpp Bindings

Thank you for your interest in contributing! This document explains how to contribute to the clbind bindings project.

## Getting Started

### Prerequisites

1. **Development environment**:
   ```bash
   sudo apt-get install -y python3 python3-pip cmake build-essential \
     clang-15 llvm-15-dev libclang-15-dev git
   ```

2. **Python dependencies**:
   ```bash
   pip3 install -r tools/bindgen/requirements.txt
   ```

3. **Familiarity with**:
   - C++ and Common Lisp basics
   - Git and GitHub workflow
   - CMake build system (helpful but not required)

### Repository Setup

```bash
# Clone (or your fork)
git clone https://github.com/clasp-developers/clasp.git
cd clasp

# Create a feature branch
git checkout -b feature/add-library-xyz

# Verify generator works
python3 tools/bindgen/generate_bindings.py --repo fmt --verbose
```

## Types of Contributions

### 1. Adding New Libraries

The most common contribution is adding bindings for a new C++ library.

#### Steps

1. **Choose a library**:
   - From [awesome-cpp](https://github.com/fffaraz/awesome-cpp)
   - Check license compatibility (prefer MIT, BSD, Apache)
   - Verify it has a stable API and active maintenance
   - Prefer header-only or easy-to-build libraries initially

2. **Add configuration**:
   Edit `tools/bindgen/repo-mapping.yaml`:
   ```yaml
   - name: my-lib
     repo: https://github.com/user/my-lib.git
     checkout: v1.2.3  # Use a stable tag
     description: "Brief description"
     license: MIT
     headers_path: include
     build_type: cmake
     cmake_options:
       - "-DBUILD_TESTS=OFF"
     expose:
       namespaces:
         - mylib
       # Start small - add a few key classes/functions
       classes:
         - mylib::CoreClass
       functions:
         - mylib::main_function
     exclude:
       namespaces:
         - mylib::internal
         - mylib::detail
       patterns:
         - ".*_impl$"
     package_name: "MY-LIB"
     lisp_package: "my-lib"
   ```

3. **Generate bindings**:
   ```bash
   python3 tools/bindgen/generate_bindings.py --repo my-lib --verbose
   ```

4. **Review generated code**:
   - Check `extensions/awesome/my-lib/src/*.cc` for correctness
   - Verify `extensions/awesome/my-lib/lisp/*.lisp` loads properly
   - Read `extensions/awesome/my-lib/README.md`

5. **Add tests**:
   Edit `extensions/awesome/my-lib/tests/test-my-lib.lisp` to exercise key functionality:
   ```lisp
   (defun test-basic-usage ()
     "Test basic my-lib functionality"
     (let ((obj (my-lib:make-core-class)))
       (assert (my-lib:some-method obj))
       (format t "~&[PASS] Basic usage test~%")))
   ```

6. **Build and test** (if clasp is available):
   ```bash
   mkdir -p extensions/awesome/my-lib/build
   cd extensions/awesome/my-lib/build
   cmake .. -DCLASP_DIR=/path/to/clasp/install
   make
   clasp --load ../tests/test-my-lib.lisp
   ```

7. **Document edge cases**:
   If you encountered issues during generation, document them in the library's README:
   - Template instantiations needed
   - Manual wrapper functions added
   - Known limitations

8. **Commit and PR**:
   ```bash
   git add tools/bindgen/repo-mapping.yaml extensions/awesome/my-lib/
   git commit -m "Add bindings for my-lib v1.2.3"
   git push origin feature/add-library-xyz
   ```

#### Checklist for New Libraries

- [ ] Library is from awesome-cpp or similarly reputable
- [ ] License is compatible (MIT/BSD/Apache preferred)
- [ ] Entry added to `repo-mapping.yaml` with meaningful expose/exclude rules
- [ ] Bindings generate without errors
- [ ] Generated C++ code compiles (if buildable)
- [ ] Lisp package loads (if testable)
- [ ] Tests included and pass
- [ ] README.md is complete
- [ ] METADATA file generated
- [ ] LICENSE file from upstream included
- [ ] No unrelated changes in commit

### 2. Improving the Generator

Contributions to `tools/bindgen/generate_bindings.py` are welcome!

#### Areas for Improvement

- **Better template handling**: Detect and handle common template patterns
- **Smart pointers**: Improved shared_ptr/unique_ptr wrapping
- **Operators**: Auto-generate bindings for operator overloads
- **Documentation**: Extract Doxygen comments and generate Lisp docstrings
- **Error handling**: Better diagnostics when parsing fails

#### Testing Generator Changes

```bash
# Test on existing libraries
python3 tools/bindgen/generate_bindings.py --repo fmt --verbose
python3 tools/bindgen/generate_bindings.py --repo json --verbose

# Verify no unexpected changes
git diff extensions/awesome/

# Test on a new library
python3 tools/bindgen/generate_bindings.py --repo my-test-lib --verbose
```

#### Code Style

- Follow PEP 8 for Python code
- Use type hints where appropriate
- Add docstrings to functions and classes
- Keep functions focused and modular

### 3. Improving CI/CD

Enhancements to `.github/workflows/` and `ci/` scripts are valuable:

- **Performance**: Speed up generation or build steps
- **Coverage**: Add more validation checks
- **Artifacts**: Better artifact management
- **Notifications**: Slack/Discord integration
- **Documentation**: Auto-generate docs from bindings

### 4. Documentation

Documentation improvements are always welcome:

- Fix typos and clarity issues
- Add examples and use cases
- Document common patterns and pitfalls
- Translate to other languages

### 5. Testing

Improving the test infrastructure:

- Add integration tests that call C++ from Lisp
- Create golden file tests (record expected outputs)
- Add performance benchmarks
- Test on multiple platforms (macOS, Linux, etc.)

## Coding Standards

### Python (Generator)

```python
# Use type hints
def generate_wrapper(lib: LibraryConfig, symbols: List[SymbolInfo]) -> str:
    """Generate C++ wrapper code for a library.

    Args:
        lib: Library configuration
        symbols: List of symbols to expose

    Returns:
        Generated C++ code as a string
    """
    pass

# Use descriptive names
def should_expose_symbol(lib: LibraryConfig, symbol: SymbolInfo) -> bool:
    # Implementation
    pass

# Keep functions focused
def _parse_class_methods(cursor) -> List[Method]:
    # Small, single-purpose helper
    pass
```

### C++ (Generated Bindings)

Generated C++ should follow clasp conventions:
- Use `clbind::` namespace explicitly
- Include proper headers
- Add comments with source location
- Handle exceptions appropriately

### Lisp (Package Definitions)

```lisp
;;;; Well-commented and structured

(defpackage :library-name
  (:use :common-lisp)
  (:documentation "Clear description")
  (:export
   ;; Exported symbols
   #:function-name
   #:class-name))

(in-package :library-name)

;; Implementation
```

### YAML (Configuration)

```yaml
# Use consistent indentation (2 spaces)
# Add comments for non-obvious settings
- name: library
  # This option disables tests because they require network
  cmake_options:
    - "-DBUILD_TESTS=OFF"
```

## Pull Request Process

### Before Submitting

1. **Test locally**:
   ```bash
   ./ci/build-and-test.sh --verbose
   ```

2. **Check for uncommitted files**:
   ```bash
   git status
   ```

3. **Verify CI will pass**:
   - All generated files are committed
   - Tests pass
   - No merge conflicts with main

4. **Write clear commit message**:
   ```
   Add bindings for fmt v10.2.1

   - Expose core formatting functions
   - Add basic tests for format() and print()
   - Include upstream license and attribution

   Resolves #123
   ```

### PR Checklist

- [ ] PR title clearly describes the change
- [ ] Description explains what and why
- [ ] All CI checks pass
- [ ] No unrelated changes included
- [ ] Documentation updated if needed
- [ ] Tests added/updated as appropriate
- [ ] Commit messages are clear

### Review Process

1. **Automated checks**: CI must pass
2. **Code review**: Maintainer will review within 1 week
3. **Feedback**: Address comments and push updates
4. **Approval**: Once approved, PR will be merged

### Common Review Comments

- "Please regenerate bindings" - You've manually edited generated files
- "Tests needed" - Add tests that exercise the bindings
- "License check" - Ensure upstream license is included
- "Too broad" - Start with smaller subset of exposed symbols

## Communication

- **Issues**: For bug reports, feature requests, or questions
- **Discussions**: For general topics and brainstorming
- **PR comments**: For specific code review feedback
- **Clasp channels**: Join clasp's community channels for broader discussions

## Style Guide Summary

### Naming Conventions

- **Python**: `snake_case` for functions/variables, `PascalCase` for classes
- **C++**: Follow clasp conventions (`snake_case` generally)
- **Lisp**: `kebab-case` (e.g., `my-function-name`)
- **Files**: `kebab-case.ext` (e.g., `generate-bindings.py`)

### File Organization

```
extensions/awesome/library-name/
  src/
    library-name_bindings.cc       # Main C++ wrapper
    library-name_manual.cc         # Manual additions (if needed)
  lisp/
    library-name.lisp              # Package and loader
    utils.lisp                     # Helper functions (if needed)
  tests/
    test-library-name.lisp         # Main tests
    test-advanced.lisp             # Additional test suites
  README.md                        # Library-specific docs
  METADATA                         # Generation metadata
  CMakeLists.txt                   # Build config
  LICENSE.library-name             # Upstream license
```

### Comments

```python
# Python: Use docstrings for functions/classes
def my_function():
    """Clear description of what this does.

    Include details about parameters and return values.
    """
    pass

# Inline comments for complex logic
if complex_condition:
    # Explain why this is necessary
    special_handling()
```

```cpp
// C++: Comment the "why", not the "what"
void initialize_bindings() {
  // Expose error types first so other bindings can reference them
  class_<FormatError>(scope, "format-error");

  // Main formatting API
  scope.def("format", &fmt::format);
}
```

## Getting Help

- **Generator not working?** Open an issue with:
  - Command you ran
  - Full error output
  - Your Python and clang versions
- **Library won't build?** Check:
  - Is the library version stable?
  - Are CMake options correct?
  - Does it build standalone?
- **Bindings don't compile?** Consider:
  - Template-heavy APIs may need manual wrappers
  - Some libraries require special flags
  - Check clasp compatibility

## Recognition

Contributors will be acknowledged in:
- The main README
- Release notes
- Generated bindings (via git history)

Significant contributions may warrant co-authorship credit.

## Code of Conduct

Be respectful, inclusive, and constructive. We're all here to make awesome tools!

---

Thank you for contributing to making C++ libraries accessible from Common Lisp! 🎉
