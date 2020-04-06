#!/usr/bin/env bash
set -eu
## This is a legacy Teamcity script until we work out how to avoid
## creating this artefact while keeping the OrderlyWeb build happy.
GIT_ID=$(git rev-parse --short=7 HEAD)
GIT_BRANCH=$(git symbolic-ref --short HEAD)
REGISTRY=docker.montagu.dide.ic.ac.uk:5000
NAME=orderly
ORDERLY_VERSION=$(grep '^Version: ' DESCRIPTION  | sed 's/Version: *//')

PRIVATE_DOCKER_COMMIT_TAG=$REGISTRY/$NAME:$GIT_ID

docker build \
       --build-arg GIT_ID=$GIT_ID \
       --build-arg GIT_BRANCH=$GIT_BRANCH \
       --build-arg ORDERLY_VERSION=$ORDERLY_VERSION \
       --tag $PRIVATE_DOCKER_COMMIT_TAG \
       -f docker/Dockerfile .

DEST=build
rm -rf $DEST
docker run --rm --entrypoint create_orderly_demo.sh \
       -u $(id -u teamcity) \
       -v "${PWD}":/orderly \
       -w /orderly \
       $PRIVATE_DOCKER_COMMIT_TAG \
       "$DEST"
