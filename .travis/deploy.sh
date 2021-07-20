#!/bin/bash -eux

cd bin

if [[ "$RUNNER_OS" = "Linux" ]]; then
    if [ -f "patator" ]; then
        mv "patator" "patator-$TAG-amd64"
    else
        mv "imitator" "imitator-$TAG-amd64"
    fi
fi
