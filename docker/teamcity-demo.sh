#!/bin/sh
TAG=$(git rev-parse --short HEAD)
DEST=demo
docker run --rm --entrypoint create_orderly_demo.sh \
       -u `id -u teamcity` \
       -v "${PWD}":/orderly \
       -w /orderly \
       "docker.montagu.dide.ic.ac.uk:5000/orderly:${TAG}" \
       "$DEST"
zip -r $DEST.zip $DEST
rm -r $DEST
