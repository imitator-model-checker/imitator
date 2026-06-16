# syntax=docker/dockerfile:1.7

####################################################################
# Stage builder: install the toolchain and compile IMITATOR.
####################################################################
FROM ubuntu:22.04 AS builder
LABEL maintainer="Jaime Arias <arias@lipn.univ-paris13.fr>"

# DOCKER_RUNNING tells build.sh to init opam with --disable-sandboxing;
# DEBIAN_FRONTEND keeps apt non-interactive.
ENV DOCKER_RUNNING=true \
  DEBIAN_FRONTEND=noninteractive

WORKDIR /imitator

# Copy the whole repository (see .dockerignore for what is excluded).
COPY . .

# Build IMITATOR. build.sh installs the system packages, opam, the OCaml
# switch, mlgmp and PPL, then runs `dune build`, which promotes a stripped,
# statically-linked binary to /imitator/bin/imitator.
#
# The cache mounts persist apt downloads and the opam root (switch +
# mlgmp/PPL bindings) across builds; build.sh already skips reinstalling
# them when present, so warm rebuilds avoid recompiling PPL from source.
# They live in the build cache only, never in a layer.
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
  --mount=type=cache,target=/root/.opam,sharing=locked \
  bash .github/scripts/build.sh

# Make the opam switch available by default in interactive dev shells.
RUN echo 'eval "$(opam env --switch=imitator)"' >> /root/.bashrc

####################################################################
# Stage runtime: minimal image with just the binary + its tools.
####################################################################
FROM ubuntu:22.04 AS runtime
LABEL maintainer="Jaime Arias <arias@lipn.univ-paris13.fr>"

ARG DEBIAN_FRONTEND=noninteractive

# Runtime dependencies:
RUN apt-get update -qq \
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
