#!/bin/bash

set -a

# global variables for options

FANCY=false

# options for script

while getopts "hf" opt; do
  case ${opt} in
  h)
    clear
    echo "Usage: $0 [-Options]"
    echo
    echo "Description:"
    echo "This script builds IMITATOR. Run install-deps.sh first when dependencies are missing."
    echo
    echo "Options list:"
    echo "    -h    Display this help message"
    echo "    -f    Fancy the output"
    echo
    exit 0
    ;;
  f)
    FANCY=true
    ;;
  \?)
    echo "Invalid option: $OPTARG. Executing the script without options."
    ;;
  esac
done

# initialize printing functions with or without -f option

if [ $FANCY = "true" ]; then
  error() { echo -e "\033[31mERROR: \033[0m$1"; }
  information() { echo -e "\033[32mINFO: \033[0m$1"; }
  success() { echo -e "\033[92mSUCCESS: \033[0m$1"; }
else
  error() { echo -e "ERROR: $1"; }
  information() { echo -e "INFO: $1"; }
  success() { echo -e "SUCCESS: $1"; }
fi

# script folder
information "Setting up the build environment..."

if [ -z "${GITHUB_WORKSPACE}" ]; then
  SCRIPT_FOLDER=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
  ROOT_FOLDER="$(dirname $(dirname $SCRIPT_FOLDER))"
  cd "$ROOT_FOLDER"
fi

if ! command -v opam >/dev/null 2>&1; then
  error "opam is not installed. Please run .github/scripts/install-deps.sh first."
  exit 1
fi

if ! opam switch list --short | grep -q '^imitator$'; then
  error "opam switch 'imitator' is missing. Please run .github/scripts/install-deps.sh first."
  exit 1
fi

eval "$(opam env --switch=imitator)"

# Cleaning previous builds
information "Cleaning previous builds..."
dune clean

# Build IMITATOR
information "Building IMITATOR..."
dune build --stop-on-first-error || {
  error "An issue has occurred while building IMITATOR."
  exit 1
}

if [ ! -x "bin/imitator" ]; then
  error "Binary 'bin/imitator' does not exist after dune build."
  exit 1
fi

success "IMITATOR has been built successfully."

exit 0
