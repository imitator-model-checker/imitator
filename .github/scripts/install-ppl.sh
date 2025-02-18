#!/bin/bash

# PPL version
PPL_VERSION=1.2

# download PPL
information "Downloading PPL-${PPL_VERSION}"

wget -q --no-check-certificate https://www.bugseng.com/products/ppl/download/ftp/releases/${PPL_VERSION}/ppl-${PPL_VERSION}.zip || {
    error "Failed to download PPL-${PPL_VERSION}."
    exit 1
}
unzip -qq ppl-${PPL_VERSION}.zip

cd ppl-${PPL_VERSION}

# Patch PPL
information "Patching PPL-${PPL_VERSION}..."
patch -p0 <"${PATCH_FOLDER}/ppl_gc.patch" || {
    error "Failed to patch PPL-${PPL_VERSION}."
    cd ../
    rm -rf ppl-${PPL_VERSION}*
    exit 1
}

# Patch clang for OSX
if [[ "$RUNNER_OS" = "macOS" ]]; then
    information "Patching clang for OSX..."
    patch -p0 <"${PATCH_FOLDER}/clang5.patch" 
    EXTRA_ARGS="--with-gmp=$(brew --prefix)"
fi

# clean previous builds
information "Cleaning previous builds..."
make clean

# compile ppl
./configure -q --prefix=$(opam var prefix) --with-mlgmp=$(opam var lib)/gmp ${EXTRA_ARGS} --disable-documentation --enable-interfaces=ocaml || {
    error "Failed to configure PPL-${PPL_VERSION}."
    cd ../
    rm -rf ppl-${PPL_VERSION}*
    exit 1
}

# compile Ocaml interface

information "Compiling PPL-${PPL_VERSION} OCaml interface..."
make -j 4 || {
    error "An issue has occured while compiling PPL-${PPL_VERSION} OCaml interface."
    cd ../
    rm -rf ppl-${PPL_VERSION}*
    exit 1
}

information "Installing PPL-${PPL_VERSION} OCaml interface..."
make install || {
    error "An issue has occured while installing PPL-${PPL_VERSION} OCaml interface."
    cd ../
    rm -rf ppl-${PPL_VERSION}* $(opam var lib)/ppl
    exit 1
}

cd ../
rm -rf ppl-${PPL_VERSION}*

# copy META file
information "Copying META file..."
cp METAS/META.ppl "$(opam var lib)/ppl/META" || {
    error "An issue has occured while copying META file."
    rm -rf $(opam var lib)/ppl
    exit 1
}

exit 0