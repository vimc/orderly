#!/usr/bin/env bash
set -e

GIT_ID=$(git rev-parse --short=7 HEAD)
GIT_BRANCH=$(git symbolic-ref --short HEAD)
REGISTRY=docker.montagu.dide.ic.ac.uk:5000
NAME=orderly
PUBLIC_REGISTRY=vimc
ORDERLY_VERSION=$(grep '^Version: ' DESCRIPTION  | sed 's/Version: *//')

APP_DOCKER_TAG=$REGISTRY/$NAME
APP_DOCKER_COMMIT_TAG=$REGISTRY/$NAME:$GIT_ID
APP_DOCKER_BRANCH_TAG=$REGISTRY/$NAME:$GIT_BRANCH

docker build \
       --build-arg GIT_ID=$GIT_ID \
       --build-arg GIT_BRANCH=$GIT_BRANCH \
       --build-arg ORDERLY_VERSION=$ORDERLY_VERSION \
       --tag $APP_DOCKER_COMMIT_TAG \
       --tag $APP_DOCKER_BRANCH_TAG \
       -f docker/Dockerfile .

docker push $APP_DOCKER_BRANCH_TAG
docker push $APP_DOCKER_COMMIT_TAG

if [ $GIT_BRANCH = "master" ]; then
    docker tag $APP_DOCKER_BRANCH_TAG $PUBLIC_TAG
    docker push $PUBLIC_REGISTRY/$NAME:master
    # Also the versioned images
    docker push $REGISTRY/$NAME:$ORDERLY_VERSION
    docker push $PUBLIC_REGISTRY/$NAME:v$ORDERLY_VERSION
fi
