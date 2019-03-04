#!/bin/bash -eux

if [[ "$TRAVIS_OS_NAME" = "linux" ]]; then
  # Install latex
  sudo apt-get install -qq texlive-latex-recommended texlive-latex-extra \
    texlive-fonts-recommended texlive-fonts-extra latexmk

  # Build documentation
  cd doc
  m4 classDiagramSimplified.m4 | dot -Tpng -o classDiagramSimplified.png
  m4 classDiagramFull.m4 | dot -Tpng -o classDiagramFull.png
  latexmk -pdf IMITATOR-not-developer-manual.tex
  latexmk -pdf IMITATOR-user-manual.tex
fi
