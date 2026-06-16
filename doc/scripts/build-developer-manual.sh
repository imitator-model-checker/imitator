#!/bin/sh
#
# Build the IMITATOR developer manual (PDF), including the UML class
# diagrams generated from the .m4/.dot sources in developer-manual/diagrams.
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
doc_root=$(dirname -- "$script_dir")
diagrams="$doc_root/developer-manual/diagrams"

for tool in m4 dot; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "ERROR: '$tool' is required to build the class diagrams (m4 + graphviz)." >&2
    exit 1
  }
done

echo "Generating class diagrams ..."
# Run m4 from the diagrams dir so its include(`...') directives resolve.
(
  cd "$diagrams"
  for d in classDiagramSimplified classDiagramFull; do
    m4 "$d.m4" > "$d.dot"
    dot -Tpng -o "$d.png" "$d.dot"
  done
)

"$script_dir/latex-compile.sh" developer-manual IMITATOR-developer-manual.tex
