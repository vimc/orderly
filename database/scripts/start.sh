#!/usr/bin/env bash
## Starts a container:
##
##   orderly_db
##
## On the network
##
##   orderly_db_nw
set -ex

if (( "$#" < 1 || "$#" > 2 )); then
    echo "Usage: start.sh <DB_VERSION> [<DB_PORT>]"
    echo "Starts the orderly database, using the specified image version."
    echo "If DB_PORT is provided, exposes the main database on the host"
    echo "machine at that port."
    exit 1
fi

set -ex
DB_VERSION=$1
DB_PORT=$2

PORT_MAPPING=
if [[ ! -z $DB_PORT ]]; then
    PORT_MAPPING="-p $DB_PORT:5432"
fi

REGISTRY=docker.montagu.dide.ic.ac.uk:5000

DB_IMAGE=$REGISTRY/orderly-db:$DB_VERSION
MIGRATE_IMAGE=$REGISTRY/orderly-db-migrate:$DB_VERSION

DB_CONTAINER=orderly_db
NETWORK=orderly_db_nw

function cleanup {
    set +e
    docker stop $DB_CONTAINER
    docker network rm $NETWORK
}
trap cleanup EXIT

docker network create $NETWORK

# Pull fresh images, but if it fails continue so as to facilitate
# situations with no registry access
docker pull $DB_IMAGE || true
docker pull $MIGRATE_IMAGE || true

# First the core database:
docker run --rm --network=$NETWORK -d \
    --name $DB_CONTAINER $PORT_MAPPING $DB_IMAGE $PG_CONFIG

# Wait for things to become responsive
docker exec $DB_CONTAINER postgres-wait

# Do the migrations
docker run --rm --network=$NETWORK $MIGRATE_IMAGE

trap - EXIT
