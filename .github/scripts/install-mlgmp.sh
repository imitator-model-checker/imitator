#!/bin/bash

# clone mlgmp from github
git clone https://github.com/monniaux/mlgmp.git || warning "mlgmp already exists, skipping cloning"
cd mlgmp

# apply patch for Ocaml > 4.05.0
information "Applying patch for Ocaml"
git apply "${PATCH_FOLDER}/gmp.patch"

# clean previous builds
information "Cleaning previous builds"
make clean

# compile and install
information "Compiling mlgmp"
make -s &>$err || {
    error "An issue has occured while compiling mlgmp."
    cd ..
    rm -rf mlgmp
    exit 1
}

information "Installing mlgmp"
make install -s &>$err || {
    error "An issue has occured while installing mlgmp."
    cd ..
    rm -rf mlgmp
    exit 1
}

information "Installing mlgmp OCaml native metadata..."
GMP_OCAML_LIB_DIR="$(opam var lib)/gmp"
GMP_OCAML_CMX_FILES=$(find . -type f -name 'gmp*.cmx')

if [ -z "${GMP_OCAML_CMX_FILES}" ]; then
    error "mlgmp OCaml native metadata files (*.cmx) were not found."
    cd ..
    rm -rf mlgmp
    exit 1
fi

find . -type f -name 'gmp*.cmx' -exec cp {} "${GMP_OCAML_LIB_DIR}" \; &>$err || {
    error "An issue has occured while installing mlgmp OCaml native metadata."
    cd ..
    rm -rf mlgmp
    exit 1
}

cd ..
rm -rf mlgmp

# copy META file
cp METAS/META.gmp "$(opam var lib)/gmp/META" &>$err || {
    error "An issue has occured while copying META file."
    exit 1
}

exit 0
