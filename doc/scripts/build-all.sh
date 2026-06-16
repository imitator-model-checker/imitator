#!/bin/sh
#
# Build both manuals (user guide + developer manual).
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
"$script_dir/build-user-manual.sh"
"$script_dir/build-developer-manual.sh"
echo ""
echo "  Both manuals successfully built! :-)"
