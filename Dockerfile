# Ubuntu Docker image
FROM ubuntu:20.04
LABEL maintainer="Jaime Arias <arias@lipn.univ-paris13.fr>"

ENV DOCKER_RUNNING=true

# Copying files for build imitator
COPY . /imitator/

# Compiling imitator
RUN cd /imitator && bash .github/scripts/build.sh && rm -rf .github

# Change the working directory
WORKDIR /imitator/bin

# Default command
ENTRYPOINT [ "/imitator/bin/imitator" ]
