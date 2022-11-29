#!/bin/bash

set -a

# check OS
case $(uname) in
'Linux')
    RUNNER_OS='Linux'
    ;;
'Darwin')
    RUNNER_OS='macOS'
    ;;
*)
    echo "This script only supports Linux or OSX"
    exit 1
    ;;
esac

# ignore sudo commands when the user is root
sudo() {
    [[ $EUID = 0 ]] || set -- command sudo "$@"
    "$@"
}

# script folder
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
if [[ "$RUNNER_OS" = "Linux" ]]; then
    DEBIAN_FRONTEND=noninteractive
    sudo apt-get update -qq
    sudo apt-get install -qq wget unzip curl build-essential g++ m4 opam python3 \
        libgmp-dev libmpfr-dev libppl-dev \
        graphviz plotutils
elif [[ "$RUNNER_OS" = "macOS" ]]; then
    brew install opam gmp ppl graphviz plotutils
fi

# python fix
[ ! -x "$(command -v python)" ] && ln -s $(which python3) "/usr/bin/python"

# install opam and ocaml libraries
docker_container=$(echo `[ ! -f /.dockerenv ]` $?)
[[ "${docker_container}" = '0' ]] &&  opam init -a || opam init -a --disable-sandboxing

opam install -y extlib fileutils oasis
eval $(opam env)

# install mlgmp
[ ! -d "$(opam var lib)/gmp" ] && bash "${SCRIPT_FOLDER}/install-mlgmp.sh"

# instal ppl
[ ! -d "$(opam var lib)/ppl" ] && bash "${SCRIPT_FOLDER}/install-ppl.sh"

# patch oasis for OSX
if [[ "$RUNNER_OS" = "macOS" ]]; then
    patch -p0 <"${PATCH_FOLDER}/oasis-config.patch"
fi

# Build IMITATOR
bash build.sh

# rename artefact
if [[ -v "${GITHUB_WORKSPACE}" ]]; then
    cd bin
    platform=$(echo "${RUNNER_OS}" | awk '{print tolower($1)}')
    tag="${GITHUB_REF_NAME##*/}"
    mv "imitator" "imitator-${tag}-${platform}-amd64"
fi
