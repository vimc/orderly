#!/usr/bin/env bash
wait_for()
{
    echo "waiting $TIMEOUT seconds for postgres"
    start_ts=$(date +%s)
    while :
    do
        # Using pg_ready as:
        #
        #   pg_isready -U $POSTGRES_USER -d $POSTGRES_DB
        #
        # seems heaps nicer but does not actually work properly
        # because it pulls us up to soon.
        psql -U $POSTGRES_USER -d $POSTGRES_DB -c "select 1;" > /dev/null 2>&1
        result=$?
        if [[ $result -eq 0 ]]; then
            end_ts=$(date +%s)
            echo "postgres is available after $((end_ts - start_ts)) seconds"
            break
        fi
        sleep 1
        echo "...still waiting"
    done
    return $result
}

TIMEOUT=15
wait_for
RESULT=$?
