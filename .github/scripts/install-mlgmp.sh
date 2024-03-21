#!/bin/bash

# clone mlgmp from github
git clone https://github.com/monniaux/mlgmp.git && cd mlgmp

# apply patch for Ocaml > 4.05.0
git apply "${PATCH_FOLDER}/gmp.patch"

# compile and install
make && make install && cd ..
rm -rf mlgmp

# copy META file
cp METAS/META.gmp "$(opam var lib)/gmp/META"
