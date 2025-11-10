#!/bin/bash
# Update bindings for a specific library or all libraries

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLASP_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

usage() {
    echo "Usage: $0 [--all | --repo REPO_NAME]"
    echo ""
    echo "Update clbind bindings for awesome-cpp libraries"
    echo ""
    echo "Options:"
    echo "  --all          Update all configured libraries"
    echo "  --repo NAME    Update specific library"
    echo "  --verbose, -v  Verbose output"
    echo "  --help, -h     Show this help"
    exit 1
}

# Parse arguments
REPO=""
ALL=false
VERBOSE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --all)
            ALL=true
            shift
            ;;
        --repo)
            REPO="$2"
            shift 2
            ;;
        --verbose|-v)
            VERBOSE="--verbose"
            shift
            ;;
        --help|-h)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

if [ "$ALL" = false ] && [ -z "$REPO" ]; then
    echo "Error: Either --all or --repo must be specified"
    usage
fi

# Change to clasp root
cd "$CLASP_ROOT"

# Run generator
if [ "$ALL" = true ]; then
    echo "Updating all library bindings..."
    python3 tools/bindgen/generate_bindings.py --all $VERBOSE
else
    echo "Updating bindings for $REPO..."
    python3 tools/bindgen/generate_bindings.py --repo "$REPO" $VERBOSE
fi

echo ""
echo "Update complete! Generated extensions are in: extensions/awesome/"
