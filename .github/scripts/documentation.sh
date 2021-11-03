#!/bin/bash -eux

if [[ "$RUNNER_OS" = "Linux" ]]; then
  # Install latex
  sudo apt-get install -qq texlive-latex-recommended texlive-latex-extra \
    texlive-fonts-recommended texlive-fonts-extra texlive-bibtex-extra biber

  # Build documentation
  cd doc
  sh buildManuals.sh
fi
