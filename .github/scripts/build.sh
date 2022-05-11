# PPL version
PPL_VERSION=1.2

# Patches folder
PATCH_FOLDER="${GITHUB_WORKSPACE}/.github/patches"

if [[ "$RUNNER_OS" = "Linux" ]]; then
    sudo apt-get update -qq
    sudo apt-get install -qq wget unzip curl build-essential g++ m4 ocaml-native-compilers camlp4-extra ocaml oasis ocaml-findlib \
        libextlib-ocaml libextlib-ocaml-dev libfileutils-ocaml-dev \
        libgmp-dev libgmp-ocaml libgmp-ocaml-dev libmpfr-dev \
        libppl-dev \
        graphviz plotutils

    if [[ "$DISTRIBUTED" = "True" ]]; then
        sudo apt-get install -qq openmpi-bin openmpi-common libopenmpi-dev

        # installing version without Bytes package
        git clone https://github.com/xavierleroy/ocamlmpi
        (cd ocamlmpi; git reset --hard c6eaf91; make clean; make MPIINCDIR=/usr/lib/openmpi/include; make opt; sudo make install)
        rm -rf ocamlmpi

        # Installting Bytes Package
        git clone https://github.com/chambart/ocaml-bytes.git
        (cd ocaml-bytes; ./configure --prefix=/usr --libdir=/usr/lib/ocaml/; make; sudo make install)
        rm -rf ocaml-bytes
    fi
elif [[ "$RUNNER_OS" = "macOS" ]]; then
    brew install opam gmp plotutils ppl

    # install opam and ocaml libraries
    opam init -a
    opam install -y extlib fileutils oasis
    eval $(opam env)

    # install mlgmp
    git clone https://github.com/monniaux/mlgmp.git && cd mlgmp
    git apply "${PATCH_FOLDER}/gmp.patch"
    make && sudo make install && cd ..
    rm -rf mlgmp
    sudo cp METAS/META.gmp $(opam var lib)/gmp/META
fi

# installing PPL
wget -q --no-check-certificate https://www.bugseng.com/products/ppl/download/ftp/releases/${PPL_VERSION}/ppl-${PPL_VERSION}.zip
unzip -qq ppl-${PPL_VERSION}.zip

cd ppl-${PPL_VERSION}
if [[ "$RUNNER_OS" = "Linux" ]]; then
    ./configure --prefix=/usr
elif [[ "$RUNNER_OS" = "macOS" ]]; then
    # patch clang
    patch -p0 < "${PATCH_FOLDER}/clang5.patch"

    # compile ppl
    ./configure --prefix=$(opam var prefix) --with-mlgmp=$(opam var lib)/gmp --disable-documentation --enable-interfaces=ocaml
    make
fi
cd interfaces/OCaml && make -j 4 && sudo make install && cd ../../..
rm -rf ppl-${PPL_VERSION}*

# Build IMITATOR
if [[ "$RUNNER_OS" = "Linux" ]]; then
    sudo cp METAS/META.ppl /usr/lib/ocaml/METAS/
elif [[ "$RUNNER_OS" = "macOS" ]]; then
    # patch oasis
    patch -p0 < "${PATCH_FOLDER}/oasis-config.patch"

    # patch ppl META file
    patch -p0 < "${PATCH_FOLDER}/META.ppl.patch"

    echo $(ls -al)
    sudo cp METAS/META.ppl $(opam var lib)/ppl/META
fi

if [[ "$DISTRIBUTED" = "True" ]]; then
    sh build-patator.sh
else
    sh build.sh
fi
