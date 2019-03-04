#!/bin/bash -eux

cd bin

if [[ "$TRAVIS_OS_NAME" = "linux" ]]; then
    if [[ "$DISTRIBUTED" = "False" ]]; then
        mv "imitator" "imitator-$TRAVIS_TAG-amd64"
    else
        mv "patator" "patator-$TRAVIS_TAG-amd64"
    fi
fi
