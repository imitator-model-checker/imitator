#!/bin/bash

set -a

# global variables for options

FANCY=false
err=./error.log

# options for script

while getopts "hf" opt; do
  case ${opt} in
  h)
    clear
    echo "Usage: $0 [-Options]"
    echo
    echo "Description:"
    echo "This script installs the dependencies needed to build IMITATOR."
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
  warning() { echo -e "\033[33mWARNING: \033[0m$1"; }
  information() { echo -e "\033[32mINFO: \033[0m$1"; }
  note() { echo -e "\033[34mNOTE: \033[0m$1"; }
  success() { echo -e "\033[92mSUCCESS: \033[0m$1"; }
  cmd() { echo -e "\033[35m$1\033[0m"; }
else
  error() { echo -e "ERROR: $1"; }
  warning() { echo -e "WARNING: $1"; }
  information() { echo -e "INFO: $1"; }
  note() { echo -e "NOTE: $1"; }
  success() { echo -e "SUCCESS: $1"; }
  cmd() { echo -e "$1"; }
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
    libgmp-dev libmpfr-dev libppl-dev graphviz plotutils &>$err || {
    error "One of the depedencies had an issue installing itself. Please use the command $(cmd "apt-get update") or make sure that $(cmd "sudo") rights have been granted"
    exit 1
  }
elif [[ "$RUNNER_OS" = "macOS" ]]; then
  brew install wget opam gmp mpfr ppl graphviz plotutils &>$err || {
    error "One of the depedencies had an issue installing itself. Please make sure that $(cmd "brew") is installed or that $(cmd "sudo") rights have been granted"
    exit 1
  }
fi

# python fix
information "Fixing python symlink..."

[ ! -x "$(command -v python)" ] && sudo ln -s $(which python3) "/usr/local/bin/python"

# initialise opam
information "Initialising opam..."

[[ ${DOCKER_RUNNING} ]] && opam init -a --disable-sandboxing &>$err || opam init -a &>$err || {
  error "An issue has occured while initialising opam. Please check the error log."
  exit 1
}

# switch to ocaml 4.14
information "Switching to OCaml 4.14.2..."

if ! opam switch list --short | grep -q '^imitator$'; then
  opam switch create imitator 4.14.2 &>$err || {
    error "An issue has occured while creating the switch 4.14.2. Please check the error log."
    exit 1
  }
fi

opam switch imitator
eval $(opam env)

# install opam and ocaml libraries

information "Installing opam and OCaml libraries..."

opam install -y extlib fileutils oasis alcotest menhir &>$err || {
  error "An issue has occured while installing opam and OCaml libraries. Please check the error log."
  exit 1
}

# install mlgmp
information "Installing mlgmp..."

gmp_lib_dir="$(opam var lib)/gmp"
if [ ! -d "$gmp_lib_dir" ] || ! find "$gmp_lib_dir" -maxdepth 1 -type f -name 'gmp*.cmx' | grep -q .; then
  bash "${SCRIPT_FOLDER}/install-mlgmp.sh" &>$err || {
    error "An issue has occured while installing mlgmp. Please check the error log."
    exit 1
  }
else
  note "mlgmp already installed. Skipping."
fi

# instal ppl
information "Installing PPL..."

ppl_lib_dir="$(opam var lib)/ppl"
if [ ! -d "$ppl_lib_dir" ] || ! find "$ppl_lib_dir" -maxdepth 1 -type f -name 'ppl_ocaml*.cmx' | grep -q .; then
  bash "${SCRIPT_FOLDER}/install-ppl.sh" &>$err || {
    error "An issue has occured while installing plp. Please check the error log."
    exit 1
  }
else
  note "PPL already installed. Skipping."
fi

rm -f $err
success "IMITATOR dependencies have been installed successfully."

exit 0
