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

cd ..
rm -rf mlgmp

# copy META file
cp METAS/META.gmp "$(opam var lib)/gmp/META" &>$err || {
    error "An issue has occured while copying META file."
    exit 1
}

exit 0