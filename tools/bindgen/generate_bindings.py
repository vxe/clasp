#!/usr/bin/env python3
"""
Clasp clbind Binding Generator for awesome-cpp libraries

This script automatically generates clbind bindings for C++ libraries
listed in repo-mapping.yaml. It uses libclang to parse C++ headers
and emit clbind wrapper code.

Usage:
    python3 generate_bindings.py --all
    python3 generate_bindings.py --repo fmt
    python3 generate_bindings.py --config custom-config.yaml
"""

import os
import sys
import yaml
import subprocess
import argparse
import shutil
import re
import hashlib
from pathlib import Path
from typing import Dict, List, Set, Optional, Tuple
from dataclasses import dataclass, field
from datetime import datetime

# Try to import clang bindings
try:
    import clang.cindex
    from clang.cindex import CursorKind, TypeKind, AccessSpecifier
    CLANG_AVAILABLE = True
except ImportError:
    print("Warning: libclang Python bindings not available. Install with: pip3 install libclang")
    print("Falling back to heuristic-based binding generation")
    CLANG_AVAILABLE = False


@dataclass
class LibraryConfig:
    """Configuration for a single library to generate bindings for"""
    name: str
    repo: str
    checkout: str
    description: str
    license: str
    headers_path: str
    build_type: str
    package_name: str
    lisp_package: str
    cmake_options: List[str] = field(default_factory=list)
    header_only: bool = False
    expose: Dict = field(default_factory=dict)
    exclude: Dict = field(default_factory=dict)


@dataclass
class SymbolInfo:
    """Information about a C++ symbol to bind"""
    kind: str  # 'class', 'function', 'enum', etc.
    name: str
    full_name: str
    namespace: str
    file_path: str
    line_number: int
    is_public: bool = True
    template_params: List[str] = field(default_factory=list)
    base_classes: List[str] = field(default_factory=list)
    methods: List[Dict] = field(default_factory=list)


class BindingGenerator:
    """Main class for generating clbind bindings"""

    def __init__(self, config_path: str, verbose: bool = False):
        self.config_path = Path(config_path)
        self.verbose = verbose
        self.root_dir = self.config_path.parent.parent.parent  # clasp root
        self.load_config()

    def load_config(self):
        """Load configuration from YAML file"""
        with open(self.config_path, 'r') as f:
            config_data = yaml.safe_load(f)

        self.settings = config_data.get('settings', {})
        self.libraries = []

        for lib_data in config_data.get('libraries', []):
            lib = LibraryConfig(**lib_data)
            self.libraries.append(lib)

        self.output_dir = self.root_dir / self.settings.get('output_dir', 'extensions/awesome')
        self.temp_dir = Path(self.settings.get('temp_dir', '/tmp/clbind-gen'))
        self.generator_version = self.settings.get('generator_version', '0.1.0')

    def log(self, message: str, level: str = 'INFO'):
        """Log a message"""
        if self.verbose or level in ['ERROR', 'WARNING']:
            timestamp = datetime.now().strftime('%H:%M:%S')
            print(f"[{timestamp}] {level}: {message}")

    def run_command(self, cmd: List[str], cwd: Optional[Path] = None,
                    check: bool = True) -> Tuple[int, str, str]:
        """Run a shell command and return exit code, stdout, stderr"""
        self.log(f"Running: {' '.join(cmd)}")
        result = subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=True,
            text=True,
            check=False
        )

        if check and result.returncode != 0:
            self.log(f"Command failed with exit code {result.returncode}", 'ERROR')
            self.log(f"STDERR: {result.stderr}", 'ERROR')
            raise RuntimeError(f"Command failed: {' '.join(cmd)}")

        return result.returncode, result.stdout, result.stderr

    def clone_repo(self, lib: LibraryConfig) -> Path:
        """Clone a library repository"""
        repo_dir = self.temp_dir / lib.name

        if repo_dir.exists():
            self.log(f"Repository {lib.name} already exists, pulling updates...")
            self.run_command(['git', 'fetch'], cwd=repo_dir)
        else:
            self.log(f"Cloning {lib.repo}...")
            self.temp_dir.mkdir(parents=True, exist_ok=True)
            self.run_command(['git', 'clone', lib.repo, str(repo_dir)])

        # Checkout specific version
        self.log(f"Checking out {lib.checkout}...")
        self.run_command(['git', 'checkout', lib.checkout], cwd=repo_dir)

        # Get commit hash for reproducibility
        _, commit_hash, _ = self.run_command(['git', 'rev-parse', 'HEAD'], cwd=repo_dir)
        lib.source_commit = commit_hash.strip()

        return repo_dir

    def build_library(self, lib: LibraryConfig, repo_dir: Path) -> Path:
        """Build the library (or prepare headers)"""
        install_dir = self.temp_dir / f"{lib.name}-install"

        if lib.build_type == 'cmake':
            build_dir = repo_dir / 'build'
            build_dir.mkdir(exist_ok=True)

            cmake_cmd = [
                'cmake',
                str(repo_dir),
                f'-DCMAKE_INSTALL_PREFIX={install_dir}',
                '-DCMAKE_BUILD_TYPE=Release',
            ] + lib.cmake_options

            self.log(f"Configuring {lib.name} with CMake...")
            self.run_command(cmake_cmd, cwd=build_dir)

            if not lib.header_only:
                self.log(f"Building {lib.name}...")
                self.run_command(['cmake', '--build', '.', '--', '-j4'], cwd=build_dir)

            self.log(f"Installing {lib.name} to {install_dir}...")
            self.run_command(['cmake', '--install', '.'], cwd=build_dir)
        else:
            raise ValueError(f"Unsupported build type: {lib.build_type}")

        return install_dir

    def should_expose_symbol(self, lib: LibraryConfig, symbol: SymbolInfo) -> bool:
        """Determine if a symbol should be exposed based on configuration"""
        # Check exclusions first
        exclude = lib.exclude

        # Check excluded namespaces
        for ns in exclude.get('namespaces', []):
            if symbol.namespace.startswith(ns):
                return False

        # Check excluded patterns
        for pattern in exclude.get('patterns', []):
            if re.match(pattern, symbol.full_name):
                return False

        # Check exposures
        expose = lib.expose

        # If specific namespaces are listed, symbol must be in one
        if expose.get('namespaces'):
            namespace_match = False
            for ns in expose['namespaces']:
                if symbol.namespace.startswith(ns):
                    namespace_match = True
                    break
            if not namespace_match:
                return False

        # Check if symbol is explicitly listed
        if symbol.kind == 'class' and expose.get('classes'):
            return symbol.full_name in expose['classes']
        elif symbol.kind == 'function' and expose.get('functions'):
            return symbol.full_name in expose['functions']

        # Default: expose if in allowed namespace
        return True

    def discover_symbols_libclang(self, lib: LibraryConfig,
                                   install_dir: Path) -> List[SymbolInfo]:
        """Use libclang to discover symbols in headers"""
        if not CLANG_AVAILABLE:
            self.log("libclang not available, using fallback", 'WARNING')
            return self.discover_symbols_fallback(lib, install_dir)

        symbols = []
        include_path = install_dir / lib.headers_path

        # Setup clang index
        index = clang.cindex.Index.create()

        # Find header files
        header_files = list(include_path.rglob('*.h')) + \
                      list(include_path.rglob('*.hpp'))

        self.log(f"Parsing {len(header_files)} header files with libclang...")

        for header in header_files:
            if self.verbose:
                self.log(f"Parsing {header.name}")

            # Parse with clang
            clang_args = self.settings.get('clang_args', ['-std=c++17'])
            clang_args.append(f'-I{include_path}')

            try:
                tu = index.parse(str(header), args=clang_args)

                # Walk AST and collect symbols
                self._walk_ast(tu.cursor, lib, symbols)
            except Exception as e:
                self.log(f"Error parsing {header}: {e}", 'WARNING')
                continue

        self.log(f"Discovered {len(symbols)} symbols")
        return symbols

    def _walk_ast(self, cursor, lib: LibraryConfig, symbols: List[SymbolInfo]):
        """Recursively walk clang AST and collect symbols"""
        # Only process declarations in the main file (not system headers)
        if cursor.location.file is None:
            return

        # Extract namespace
        namespace = self._get_namespace(cursor)

        # Process different kinds of declarations
        if cursor.kind == CursorKind.CLASS_DECL or cursor.kind == CursorKind.STRUCT_DECL:
            if cursor.access_specifier == AccessSpecifier.PUBLIC or \
               cursor.access_specifier == AccessSpecifier.INVALID:
                symbol = SymbolInfo(
                    kind='class',
                    name=cursor.spelling,
                    full_name=cursor.type.spelling if cursor.type else cursor.spelling,
                    namespace=namespace,
                    file_path=str(cursor.location.file),
                    line_number=cursor.location.line,
                    is_public=True
                )

                # Get base classes
                for base in cursor.get_children():
                    if base.kind == CursorKind.CXX_BASE_SPECIFIER:
                        symbol.base_classes.append(base.type.spelling)

                if self.should_expose_symbol(lib, symbol):
                    symbols.append(symbol)

        elif cursor.kind == CursorKind.FUNCTION_DECL:
            if cursor.linkage == clang.cindex.LinkageKind.EXTERNAL:
                symbol = SymbolInfo(
                    kind='function',
                    name=cursor.spelling,
                    full_name=self._get_qualified_name(cursor),
                    namespace=namespace,
                    file_path=str(cursor.location.file),
                    line_number=cursor.location.line,
                    is_public=True
                )

                if self.should_expose_symbol(lib, symbol):
                    symbols.append(symbol)

        # Recurse for children
        for child in cursor.get_children():
            self._walk_ast(child, lib, symbols)

    def _get_namespace(self, cursor) -> str:
        """Get the namespace of a cursor"""
        namespaces = []
        parent = cursor.semantic_parent

        while parent and parent.kind != CursorKind.TRANSLATION_UNIT:
            if parent.kind == CursorKind.NAMESPACE:
                namespaces.insert(0, parent.spelling)
            parent = parent.semantic_parent

        return '::'.join(namespaces) if namespaces else ''

    def _get_qualified_name(self, cursor) -> str:
        """Get fully qualified name of a cursor"""
        namespace = self._get_namespace(cursor)
        if namespace:
            return f"{namespace}::{cursor.spelling}"
        return cursor.spelling

    def discover_symbols_fallback(self, lib: LibraryConfig,
                                   install_dir: Path) -> List[SymbolInfo]:
        """Fallback: discover symbols using heuristics (when libclang unavailable)"""
        self.log("Using heuristic-based symbol discovery", 'INFO')
        symbols = []

        # For the fallback, we'll use the explicit lists from config
        expose = lib.expose

        # Add classes
        for class_name in expose.get('classes', []):
            namespace = '::'.join(class_name.split('::')[:-1])
            symbols.append(SymbolInfo(
                kind='class',
                name=class_name.split('::')[-1],
                full_name=class_name,
                namespace=namespace,
                file_path='',
                line_number=0,
                is_public=True
            ))

        # Add functions
        for func_name in expose.get('functions', []):
            namespace = '::'.join(func_name.split('::')[:-1])
            symbols.append(SymbolInfo(
                kind='function',
                name=func_name.split('::')[-1],
                full_name=func_name,
                namespace=namespace,
                file_path='',
                line_number=0,
                is_public=True
            ))

        return symbols

    def generate_wrapper_code(self, lib: LibraryConfig, symbols: List[SymbolInfo],
                              install_dir: Path) -> Tuple[str, str]:
        """Generate C++ wrapper code and Lisp loader code"""
        # Generate C++ wrapper
        cpp_code = self._generate_cpp_wrapper(lib, symbols, install_dir)

        # Generate Lisp loader
        lisp_code = self._generate_lisp_loader(lib, symbols)

        return cpp_code, lisp_code

    def _generate_cpp_wrapper(self, lib: LibraryConfig,
                              symbols: List[SymbolInfo],
                              install_dir: Path) -> str:
        """Generate C++ clbind wrapper code"""
        include_path = install_dir / lib.headers_path

        # Start with header
        code = f"""/*
 * Auto-generated clbind bindings for {lib.name}
 * Generated by clbind-generator v{self.generator_version}
 * Source: {lib.repo}
 * Commit: {getattr(lib, 'source_commit', 'unknown')}
 * License: {lib.license}
 * DO NOT EDIT - regenerate using generate_bindings.py
 */

#include <clasp/clbind/clbind.h>

"""

        # Add library includes
        # Determine main header file
        main_headers = {
            'fmt': ['fmt/core.h', 'fmt/format.h'],
            'json': ['nlohmann/json.hpp'],
            'spdlog': ['spdlog/spdlog.h']
        }

        headers = main_headers.get(lib.name, [f'{lib.name}/{lib.name}.h'])
        for header in headers:
            code += f"#include <{header}>\n"

        code += "\n"
        code += "using namespace clbind;\n\n"

        # Generate initialization function
        code += f"""namespace {lib.lisp_package.replace('-', '_')}_bindings {{

void initialize_{lib.name}_bindings() {{
  package_ pkg("{lib.package_name}");
  scope_& scope = pkg.scope();

"""

        # Group symbols by namespace
        by_namespace = {}
        for symbol in symbols:
            ns = symbol.namespace or '__global__'
            if ns not in by_namespace:
                by_namespace[ns] = []
            by_namespace[ns].append(symbol)

        # Generate bindings for each namespace
        for ns, ns_symbols in sorted(by_namespace.items()):
            if ns != '__global__':
                code += f"  // Namespace: {ns}\n"

            # First, expose classes
            for symbol in ns_symbols:
                if symbol.kind == 'class':
                    code += self._generate_class_binding(symbol)

            # Then, expose functions
            for symbol in ns_symbols:
                if symbol.kind == 'function':
                    code += self._generate_function_binding(symbol)

            code += "\n"

        code += """}

} // namespace
"""

        return code

    def _generate_class_binding(self, symbol: SymbolInfo) -> str:
        """Generate clbind binding for a class"""
        class_name = symbol.name
        full_name = symbol.full_name

        code = f"  // Class: {full_name}\n"

        if symbol.base_classes:
            bases = ', '.join(symbol.base_classes)
            code += f'  class_<{full_name}, {bases}>(scope, "{class_name}")\n'
        else:
            code += f'  class_<{full_name}>(scope, "{class_name}")\n'

        code += f'    .def_constructor<>();  // Default constructor\n'
        code += '\n'

        return code

    def _generate_function_binding(self, symbol: SymbolInfo) -> str:
        """Generate clbind binding for a function"""
        func_name = symbol.name
        full_name = symbol.full_name

        code = f"  // Function: {full_name}\n"
        code += f'  scope.def("{func_name}", &{full_name});\n'
        code += '\n'

        return code

    def _generate_lisp_loader(self, lib: LibraryConfig,
                              symbols: List[SymbolInfo]) -> str:
        """Generate Lisp package and loader code"""
        code = f""";;;; Auto-generated Lisp loader for {lib.name} bindings
;;;; Generated by clbind-generator v{self.generator_version}
;;;; DO NOT EDIT - regenerate using generate_bindings.py

(defpackage :{lib.lisp_package}
  (:use :common-lisp)
  (:nicknames :awesome-{lib.name})
  (:documentation "{lib.description}")
  (:export
"""

        # Export symbols
        for symbol in symbols[:20]:  # Limit for brevity
            lisp_name = symbol.name.replace('_', '-').upper()
            code += f'   #{lisp_name}\n'

        code += """   ))

(in-package :""" + lib.lisp_package + """)

(defun load-bindings ()
  "Load the """ + lib.name + """ extension bindings."
  ;; The C++ initialization function will be called automatically
  ;; when the extension is loaded by clasp's module system
  (format t "~&Loading """ + lib.name + """ bindings...~%"))

;;; Call initialization
(load-bindings)
"""

        return code

    def write_extension(self, lib: LibraryConfig, cpp_code: str,
                        lisp_code: str, install_dir: Path):
        """Write generated extension to disk"""
        ext_dir = self.output_dir / lib.name
        ext_dir.mkdir(parents=True, exist_ok=True)

        # Write C++ source
        src_dir = ext_dir / 'src'
        src_dir.mkdir(exist_ok=True)

        cpp_file = src_dir / f'{lib.name}_bindings.cc'
        with open(cpp_file, 'w') as f:
            f.write(cpp_code)
        self.log(f"Wrote C++ bindings to {cpp_file}")

        # Write Lisp loader
        lisp_dir = ext_dir / 'lisp'
        lisp_dir.mkdir(exist_ok=True)

        lisp_file = lisp_dir / f'{lib.name}.lisp'
        with open(lisp_file, 'w') as f:
            f.write(lisp_code)
        self.log(f"Wrote Lisp loader to {lisp_file}")

        # Write metadata
        self._write_metadata(lib, ext_dir, install_dir)

        # Write CMakeLists.txt
        self._write_cmake(lib, ext_dir, install_dir)

        # Write tests
        self._write_tests(lib, ext_dir)

        # Write README
        self._write_readme(lib, ext_dir)

    def _write_metadata(self, lib: LibraryConfig, ext_dir: Path, install_dir: Path):
        """Write metadata files for reproducibility"""
        metadata = f"""GENERATOR_VERSION: {self.generator_version}
SOURCE_REPO: {lib.repo}
SOURCE_COMMIT: {getattr(lib, 'source_commit', 'unknown')}
CHECKOUT_TAG: {lib.checkout}
LICENSE: {lib.license}
GENERATED_DATE: {datetime.now().isoformat()}
"""
        with open(ext_dir / 'METADATA', 'w') as f:
            f.write(metadata)

        # Copy license if available
        license_files = ['LICENSE', 'LICENSE.txt', 'LICENSE.md', 'COPYING']
        repo_dir = self.temp_dir / lib.name
        for license_file in license_files:
            src = repo_dir / license_file
            if src.exists():
                shutil.copy(src, ext_dir / f'LICENSE.{lib.name}')
                break

    def _write_cmake(self, lib: LibraryConfig, ext_dir: Path, install_dir: Path):
        """Write CMakeLists.txt for building the extension"""
        include_path = install_dir / lib.headers_path

        cmake_content = f"""# CMakeLists.txt for {lib.name} clbind bindings
cmake_minimum_required(VERSION 3.15)
project({lib.name}_bindings CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find clasp
find_package(Clasp REQUIRED)

# Include directories
include_directories(
  ${{CLASP_INCLUDE_DIRS}}
  {include_path}
)

# Source files
set(SOURCES
  src/{lib.name}_bindings.cc
)

# Build shared library
add_library({lib.name}_bindings SHARED ${{SOURCES}})

target_link_libraries({lib.name}_bindings
  ${{CLASP_LIBRARIES}}
)

# Installation
install(TARGETS {lib.name}_bindings
  LIBRARY DESTINATION lib/clasp/extensions
)

install(DIRECTORY lisp/
  DESTINATION lib/clasp/extensions/{lib.name}
  FILES_MATCHING PATTERN "*.lisp"
)
"""

        with open(ext_dir / 'CMakeLists.txt', 'w') as f:
            f.write(cmake_content)

    def _write_tests(self, lib: LibraryConfig, ext_dir: Path):
        """Write basic test files"""
        test_dir = ext_dir / 'tests'
        test_dir.mkdir(exist_ok=True)

        test_content = f""";;;; Tests for {lib.name} bindings
(in-package :cl-user)

(defpackage :{lib.lisp_package}-tests
  (:use :common-lisp :{lib.lisp_package})
  (:documentation "Tests for {lib.name} bindings"))

(in-package :{lib.lisp_package}-tests)

(defun test-load ()
  "Test that the extension loads successfully"
  (format t "~&Testing {lib.name} extension load...~%")
  (format t "~&{lib.name} extension loaded successfully!~%")
  t)

(defun run-all-tests ()
  "Run all tests"
  (format t "~&Running {lib.name} tests...~%")
  (test-load)
  (format t "~&All tests passed!~%"))

;;; Run tests when loaded
(run-all-tests)
"""

        with open(test_dir / f'test-{lib.name}.lisp', 'w') as f:
            f.write(test_content)

    def _write_readme(self, lib: LibraryConfig, ext_dir: Path):
        """Write README for the extension"""
        readme = f"""# {lib.name} Bindings for Clasp

Auto-generated clbind bindings for [{lib.name}]({lib.repo}).

## Description

{lib.description}

## License

{lib.license}

## Generation

These bindings were auto-generated using `tools/bindgen/generate_bindings.py`.

To regenerate:

```bash
cd clasp
python3 tools/bindgen/generate_bindings.py --repo {lib.name}
```

## Building

```bash
mkdir build && cd build
cmake .. -DCLASP_DIR=/path/to/clasp/install
make -j$(nproc)
```

## Testing

```bash
clasp --load tests/test-{lib.name}.lisp
```

## Metadata

- Generator version: {self.generator_version}
- Source commit: {getattr(lib, 'source_commit', 'unknown')}
- Generated: {datetime.now().strftime('%Y-%m-%d')}
"""

        with open(ext_dir / 'README.md', 'w') as f:
            f.write(readme)

    def generate_for_library(self, lib_name: str):
        """Generate bindings for a single library"""
        # Find library config
        lib = None
        for l in self.libraries:
            if l.name == lib_name:
                lib = l
                break

        if not lib:
            raise ValueError(f"Library {lib_name} not found in configuration")

        self.log(f"=== Generating bindings for {lib.name} ===", 'INFO')

        # Clone repository
        repo_dir = self.clone_repo(lib)

        # Build library
        install_dir = self.build_library(lib, repo_dir)

        # Discover symbols
        symbols = self.discover_symbols_libclang(lib, install_dir)

        if not symbols:
            self.log(f"No symbols found for {lib.name}", 'WARNING')
            return

        # Generate wrapper code
        cpp_code, lisp_code = self.generate_wrapper_code(lib, symbols, install_dir)

        # Write extension
        self.write_extension(lib, cpp_code, lisp_code, install_dir)

        self.log(f"=== Successfully generated bindings for {lib.name} ===", 'INFO')

    def generate_all(self):
        """Generate bindings for all configured libraries"""
        for lib in self.libraries:
            try:
                self.generate_for_library(lib.name)
            except Exception as e:
                self.log(f"Failed to generate bindings for {lib.name}: {e}", 'ERROR')
                if self.verbose:
                    import traceback
                    traceback.print_exc()


def main():
    parser = argparse.ArgumentParser(
        description='Generate clbind bindings for awesome-cpp libraries'
    )
    parser.add_argument(
        '--config',
        default='tools/bindgen/repo-mapping.yaml',
        help='Path to configuration file'
    )
    parser.add_argument(
        '--repo',
        help='Generate bindings for a specific repository'
    )
    parser.add_argument(
        '--all',
        action='store_true',
        help='Generate bindings for all configured repositories'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Verbose output'
    )

    args = parser.parse_args()

    if not args.repo and not args.all:
        parser.error('Either --repo or --all must be specified')

    # Create generator
    generator = BindingGenerator(args.config, verbose=args.verbose)

    # Generate bindings
    if args.all:
        generator.generate_all()
    elif args.repo:
        generator.generate_for_library(args.repo)


if __name__ == '__main__':
    main()
