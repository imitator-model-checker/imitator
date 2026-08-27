#!/bin/sh
#
# ************************************************************
#                       IMITATOR
#
# Generic LaTeX build helper for the manuals.
#
# Usage: latex-compile.sh <manual-subdir> <main.tex>
#   e.g. latex-compile.sh user-guide IMITATOR-user-manual.tex
#
# Compiles doc/<manual-subdir>/<main.tex> into <main>.pdf next to it,
# making the shared assets in doc/shared reachable (commons.tex,
# title_and_toc.tex, bibliography.tex, biblio.bib, images, ...).
# ************************************************************

set -eu

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <manual-subdir> <main.tex>" >&2
  exit 1
fi

# Resolve doc/ root from this script's own location (doc/scripts/).
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
doc_root=$(dirname -- "$script_dir")
shared="$doc_root/shared"

manual_dir="$doc_root/$1"
main=$(basename -- "$2" .tex)

if [ ! -f "$manual_dir/$main.tex" ]; then
  echo "ERROR: $manual_dir/$main.tex not found." >&2
  exit 1
fi

for tool in pdflatex biber; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "ERROR: '$tool' is required but not installed." >&2
    exit 1
  }
done

# Shared LaTeX inputs and the .bib are kept in doc/shared. We expose them
# with absolute paths so resolution does not depend on the current directory.
# (Image paths inside commons.tex stay relative to the manual dir, which is
# why we cd into it below.)
TEXINPUTS="$shared:$shared/images:${TEXINPUTS:-}"
BIBINPUTS="$shared:${BIBINPUTS:-}"
export TEXINPUTS BIBINPUTS

cd "$manual_dir"

echo "Compiling $1/$main.tex ..."
pdflatex -interaction=nonstopmode -halt-on-error "$main.tex"
biber "$main"
pdflatex -interaction=nonstopmode -halt-on-error "$main.tex"
pdflatex -interaction=nonstopmode -halt-on-error "$main.tex"

# Tidy LaTeX intermediates (keep only the PDF). Specific extensions only,
# so nothing unexpected is removed.
for ext in aux bbl bcf blg log out toc lof lot run.xml; do
  rm -f "$main.$ext"
done

echo "Built $manual_dir/$main.pdf"
