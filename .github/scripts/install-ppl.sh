#!/bin/bash

# PPL version
PPL_VERSION=1.2

# download PPL
wget -q --no-check-certificate https://www.bugseng.com/products/ppl/download/ftp/releases/${PPL_VERSION}/ppl-${PPL_VERSION}.zip
unzip -qq ppl-${PPL_VERSION}.zip

cd ppl-${PPL_VERSION}

# Patch clang for OSX
if [[ "$RUNNER_OS" = "macOS" ]]; then
    # patch clang
    patch -p0 <"${PATCH_FOLDER}/clang5.patch"
fi

# compile ppl
./configure --prefix=$(opam var prefix) --with-mlgmp=$(opam var lib)/gmp --disable-documentation --enable-interfaces=ocaml

# compile Ocaml interface
cd interfaces/OCaml && make -j 4 && make install && cd ../../..
rm -rf ppl-${PPL_VERSION}*

# copy META file
patch -p0 <"${PATCH_FOLDER}/META.ppl.patch"
cp METAS/META.ppl "$(opam var lib)/ppl/META"
