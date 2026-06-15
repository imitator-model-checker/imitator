#!/bin/bash

set -euo pipefail

case "$(uname)" in
Linux)
  platform="linux"
  ;;
Darwin)
  platform="macos"
  ;;
*)
  echo "Unsupported platform: $(uname)" >&2
  exit 1
  ;;
esac

tag="${GITHUB_REF_NAME##*/}"
mv "bin/imitator" "bin/imitator-${tag}-${platform}-amd64"
