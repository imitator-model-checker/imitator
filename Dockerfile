# syntax=docker/dockerfile:1.7

####################################################################
# Stage builder: install the toolchain and compile IMITATOR.
####################################################################
FROM ubuntu:22.04 AS builder
LABEL maintainer="Jaime Arias <arias@lipn.univ-paris13.fr>"

# DOCKER_RUNNING tells install-deps.sh to init opam with --disable-sandboxing;
# DEBIAN_FRONTEND keeps apt non-interactive.
ENV DOCKER_RUNNING=true \
  DEBIAN_FRONTEND=noninteractive

WORKDIR /imitator

# Copy only the dependency installer inputs first. This keeps the expensive
# apt/opam/mlgmp/PPL setup cached when ordinary source files change.
COPY .github/scripts/install-deps.sh .github/scripts/install-deps.sh
COPY .github/scripts/install-mlgmp.sh .github/scripts/install-mlgmp.sh
COPY .github/scripts/install-ppl.sh .github/scripts/install-ppl.sh
COPY .github/patches .github/patches

# Install the system packages, opam switch, OCaml libraries, mlgmp and PPL.
# The apt cache mount persists package downloads across local builds without
# copying them into the image layer. The opam switch is kept in this layer so
# later source-only rebuilds can reuse it reliably from Docker's layer cache.
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
  bash .github/scripts/install-deps.sh

# Copy the whole repository only after dependency setup (see .dockerignore for
# what is excluded), then compile IMITATOR and promote the stripped binary.
COPY . .

ARG IMITATOR_GIT_HASH
ARG IMITATOR_GIT_BRANCH

RUN IMITATOR_GIT_HASH="$IMITATOR_GIT_HASH" \
  IMITATOR_GIT_BRANCH="$IMITATOR_GIT_BRANCH" \
  bash .github/scripts/build.sh

# Make the opam switch available by default in interactive dev shells.
RUN echo 'eval "$(opam env --switch=imitator)"' >> /root/.bashrc

####################################################################
# Stage runtime: minimal image with just the binary + its tools.
####################################################################
FROM ubuntu:22.04 AS runtime
LABEL maintainer="Jaime Arias <arias@lipn.univ-paris13.fr>"

ARG DEBIAN_FRONTEND=noninteractive

# Runtime dependencies. The apt cache mount mirrors the builder stage so local
# rebuilds of this layer reuse previously downloaded packages.
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
  apt-get update -qq \
  && apt-get install -y --no-install-recommends \
  graphviz \
  plotutils \
  && rm -rf /var/lib/apt/lists/*

# Copy only the compiled binary from the builder stage.
COPY --from=builder /imitator/bin/imitator /usr/local/bin/imitator

# Fail the build early if the binary cannot run in this slim image
# (e.g. a missing shared library).
RUN imitator -version

# Run as an unprivileged user; /workspace is its home and the mount point
# for the user's models and IMITATOR's output files.
RUN useradd --system --create-home --home-dir /workspace --shell /usr/sbin/nologin imitator
WORKDIR /workspace
USER imitator

ENTRYPOINT [ "/usr/local/bin/imitator" ]
