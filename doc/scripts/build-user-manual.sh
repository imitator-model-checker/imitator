#!/bin/sh
#
# Build the IMITATOR user guide (PDF).
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
"$script_dir/latex-compile.sh" user-guide IMITATOR-user-manual.tex
