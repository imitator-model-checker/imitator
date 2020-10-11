# PPL version
PPL_VERSION=1.2

if [[ "$TRAVIS_OS_NAME" = "linux" ]]; then
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
fi

# installing PPL
wget -q --no-check-certificate https://www.bugseng.com/products/ppl/download/ftp/releases/${PPL_VERSION}/ppl-${PPL_VERSION}.zip
unzip -qq ppl-${PPL_VERSION}.zip
(cd ppl-${PPL_VERSION}; ./configure --prefix=/usr; cd interfaces/OCaml; make -j 4; sudo make install)
rm -rf ppl-${PPL_VERSION}*

# Build IMITATOR
sudo cp METAS/META.ppl /usr/lib/ocaml/METAS/

if [[ "$DISTRIBUTED" = "False" ]]; then
    sh build.sh
else
    sh build-patator.sh
fi
