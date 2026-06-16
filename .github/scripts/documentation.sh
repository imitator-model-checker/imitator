#!/bin/bash -eux

if [[ "$RUNNER_OS" = "Linux" ]]; then
  # Install latex
  echo -e "\033[32mINFO: \033[0mInstalling latex"
  sudo apt-get install -qq texlive-latex-recommended texlive-latex-extra \
    texlive-fonts-recommended texlive-fonts-extra texlive-bibtex-extra biber \
    graphviz m4 || {
      echo -e "\033[31mERROR: \033[0mFailed to install latex"
      exit 1
    }

  # Build documentation
  echo -e "\033[32mINFO: \033[0mBuilding documentation"
  sh doc/scripts/build-all.sh || {
      echo -e "\033[31mERROR: \033[0mFailed to build documentation"
      exit 1
  }
fi
