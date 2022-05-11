#!/bin/bash -eux

cd bin

platform=`echo "${RUNNER_OS}" | sed 's/./\L&/g'`

if [ -f "patator" ]; then
    mv "patator" "patator-$TAG-${platform}-amd64"
else
    mv "imitator" "imitator-$TAG-${platform}-amd64"
fi