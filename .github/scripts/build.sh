#!/bin/bash

set -a

# initialize functions for printing messages

if [ "$1" = "-f" ] || [ "$1" = "--fancy" ]; then
  error() { echo -e "\033[31mERROR: \033[0m$1";}
  warning() { echo -e "\033[33mWARNING: \033[0m$1";}
  information() { echo -e "\033[32mINFO: \033[0m$1";}
  note() { echo -e "\033[34mNOTE: \033[0m$1";}
  success() { echo -e "\033[92mSUCCESS: \033[0m$1";}
  cmd() { echo -e "\033[35m$1\033[0m";}
else
  error() { echo -e "ERROR: $1";}
  warning() { echo -e "WARNING: $1";}
  information() { echo -e "INFO: $1";}
  note() { echo -e "NOTE: $1";}
  success() { echo -e "SUCCESS: $1";}
  cmd() { echo -e "$1";}
fi

# check OS
case $(uname) in
'Linux')
  RUNNER_OS='Linux'
  ;;
'Darwin')
  RUNNER_OS='macOS'
  ;;
*)
  error "This script only supports Linux or OSX."
  exit 1
  ;;
esac

# ignore sudo commands when the user is root
sudo() {
  [[ $EUID = 0 ]] || set -- command sudo "$@"
  "$@"
}

# script folder
information "Setting up the environment..."

if [ -z "${GITHUB_WORKSPACE}" ]; then
  SCRIPT_FOLDER=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
  PATCH_FOLDER="$(dirname $SCRIPT_FOLDER)/patches"
  ROOT_FOLDER="$(dirname $(dirname $SCRIPT_FOLDER))"
  cd "$ROOT_FOLDER"
else
  SCRIPT_FOLDER="${GITHUB_WORKSPACE}/.github/scripts"
  PATCH_FOLDER="${GITHUB_WORKSPACE}/.github/patches"
fi

# install dependencies
information "Installing dependencies for IMITATOR..."

if [[ "$RUNNER_OS" = "Linux" ]]; then
  DEBIAN_FRONTEND=noninteractive
  sudo apt-get update -qq
  sudo apt-get install -qq wget unzip curl build-essential libtinfo-dev g++ m4 opam python3 \
    libgmp-dev libmpfr-dev libppl-dev graphviz plotutils || {
      error "One of the depedencies had an issue installing itself. Please use the command $(cmd "apt-get update") or make sure that $(cmd "sudo") rights have been granted"
      exit 1
    }
elif [[ "$RUNNER_OS" = "macOS" ]]; then
  brew install opam gmp ppl graphviz plotutils || {
      error "One of the depedencies had an issue installing itself. Please make sure that $(cmd "brew") is installed or that $(cmd "sudo") rights have been granted"
      exit 1
    }
fi

# python fix
information "Fixing python symlink..."

[ ! -x "$(command -v python)" ] && sudo ln -s $(which python3) "/usr/bin/python"

# install opam and ocaml libraries
information "Initialising opam..."

[[ ${DOCKER_RUNNING} ]] && opam init -a --disable-sandboxing || opam init -a

information "Installing opam and OCaml libraries..."

opam install -y extlib fileutils oasis alcotest menhir || echo -e 
eval $(opam env)

# install mlgmp
information "Installing mlgmp..."

if [ ! -d "$(opam var lib)/gmp" ]; then
  bash "${SCRIPT_FOLDER}/install-mlgmp.sh" || {
    error "An issue has occured while installing mlgmp."
    exit 1
  }
else
  note "mmlgmp already installed. Skipping."
fi

# instal ppl
information "Installing ppl..."

if [ ! -d "$(opam var lib)/ppl" ]; then
  bash "${SCRIPT_FOLDER}/install-ppl.sh" || {
    error "An issue has occured while installing plp."
    exit 1
  }
else
  note "ppl already installed. Skipping."
fi

# patch oasis for OSX
if [[ "$RUNNER_OS" = "macOS" ]]; then
  information "Patching oasis for OSX..."
  patch -p0 <"${PATCH_FOLDER}/oasis-config.patch"
fi

# Cleaning previous builds
information "Cleaning previous builds..."
dune clean 2>&1 >/dev/null

# Build IMITATOR
information "Building IMITATOR..."
dune build 2>&1 >/dev/null || {
  error "An issue has occured while building IMITATOR. Please check the error log."
  exit 1
}

# rename artefact
if [ ! -z "${GITHUB_WORKSPACE}" ]; then
  information "Renaming artefact..."
  cd bin
  platform=$(echo "${RUNNER_OS}" | awk '{print tolower($1)}')
  tag="${GITHUB_REF_NAME##*/}"
  mv "imitator" "imitator-${tag}-${platform}-amd64"
fi

success "IMITATOR has been built successfully."

exit 0 