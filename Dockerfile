# Ubuntu Docker image
FROM ubuntu:latest
LABEL maintainer="Jaime Arias <arias@lipn.univ-paris13.fr>"

# PPL version to be installed
ENV PPL_VERSION=1.2

# Installing dependencies
RUN apt-get update -qq && \
  apt-get install --no-install-recommends -qq \
  build-essential g++ m4 \
  unzip wget \
  python \
  ocaml-native-compilers camlp4-extra ocaml ocaml-findlib oasis ocamlbuild \
  graphviz plotutils \
  libextlib-ocaml libextlib-ocaml-dev libfileutils-ocaml-dev \
  libgmp-dev libgmp-ocaml libgmp-ocaml-dev \
  libmpfr-dev \
  libppl-dev && \
  apt-get autoremove -y && \
  rm -rf /var/lib/apt/lists/* && \
  # compiling ppl
  wget -q --no-check-certificate https://www.bugseng.com/products/ppl/download/ftp/releases/${PPL_VERSION}/ppl-${PPL_VERSION}.zip && \
  unzip -qq ppl-${PPL_VERSION}.zip && \
  (cd ppl-${PPL_VERSION}; ./configure --prefix=/usr; cd interfaces/OCaml; make -j 4; make install) && \
  rm -rf ppl-${PPL_VERSION}*

# Copying files for build imitator
COPY . /imitator/

# Compiling imitator
RUN cd /imitator && \
  cp METAS/META.ppl /usr/lib/ocaml/METAS/ && \
  sh build.sh && \
  rm -rf _build IMITATOR.native _oasis _tags

# Change the working directory
WORKDIR /imitator/bin

# Default command
ENTRYPOINT [ "/imitator/bin/imitator" ]
