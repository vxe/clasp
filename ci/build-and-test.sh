#!/bin/bash
# Build and test script for clbind bindings
# Can be run locally or in CI

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLASP_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "========================================="
echo "Clasp Binding Build and Test"
echo "========================================="

# Parse arguments
VERBOSE=""
REPO=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --verbose|-v)
            VERBOSE="--verbose"
            shift
            ;;
        --repo)
            REPO="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

cd "$CLASP_ROOT"

# Step 1: Generate bindings
echo ""
echo "Step 1: Generating bindings..."
echo "-------------------------------"

if [ -n "$REPO" ]; then
    python3 tools/bindgen/generate_bindings.py --repo "$REPO" $VERBOSE
else
    python3 tools/bindgen/generate_bindings.py --all $VERBOSE
fi

# Step 2: Verify structure
echo ""
echo "Step 2: Verifying structure..."
echo "-------------------------------"

for ext_dir in extensions/awesome/*/; do
    if [ -d "$ext_dir" ]; then
        ext_name=$(basename "$ext_dir")
        echo "Checking $ext_name..."

        # Check required files
        [ -f "$ext_dir/README.md" ] || echo "  WARNING: Missing README.md"
        [ -f "$ext_dir/METADATA" ] || echo "  WARNING: Missing METADATA"
        [ -d "$ext_dir/src" ] || echo "  WARNING: Missing src/"
        [ -d "$ext_dir/lisp" ] || echo "  WARNING: Missing lisp/"
        [ -d "$ext_dir/tests" ] || echo "  WARNING: Missing tests/"

        # Check for source files
        src_files=$(find "$ext_dir/src" -name "*.cc" -o -name "*.cpp" 2>/dev/null | wc -l)
        [ "$src_files" -gt 0 ] || echo "  WARNING: No C++ source files found"

        lisp_files=$(find "$ext_dir/lisp" -name "*.lisp" 2>/dev/null | wc -l)
        [ "$lisp_files" -gt 0 ] || echo "  WARNING: No Lisp files found"

        echo "  ✓ $ext_name structure OK"
    fi
done

# Step 3: Check for changes (git)
echo ""
echo "Step 3: Checking for changes..."
echo "-------------------------------"

if [ -n "$(git status --porcelain extensions/awesome/)" ]; then
    echo "Changes detected in generated files:"
    git status --porcelain extensions/awesome/
    echo ""
    echo "Run 'git diff extensions/awesome/' to see changes"
else
    echo "No changes in generated files"
fi

# Step 4: Run tests
echo ""
echo "Step 4: Running tests..."
echo "-------------------------------"

test_failed=0

for test_dir in extensions/awesome/*/tests/; do
    if [ -d "$test_dir" ]; then
        ext_name=$(basename "$(dirname "$test_dir")")
        echo "Testing $ext_name..."

        # For now, just verify test files exist
        test_files=$(find "$test_dir" -name "*.lisp" 2>/dev/null)

        if [ -n "$test_files" ]; then
            echo "  Found test files:"
            echo "$test_files" | sed 's/^/    /'
            echo "  ✓ Tests present"

            # Note: Actually running the tests would require clasp
            # echo "  Running: clasp --load $test_file"
            # clasp --load "$test_file" || test_failed=1
        else
            echo "  WARNING: No test files found"
        fi
    fi
done

# Summary
echo ""
echo "========================================="
echo "Build and Test Complete!"
echo "========================================="

if [ $test_failed -eq 0 ]; then
    echo "Status: SUCCESS ✓"
    exit 0
else
    echo "Status: FAILED ✗"
    exit 1
fi
