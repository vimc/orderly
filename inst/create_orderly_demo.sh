#!/bin/sh
set -ex
if [ "$#" -ne 1 ]; then
    echo "Expected one argument (path)"
    exit 1
fi
DEST=$1
Rscript -e "orderly:::create_orderly_demo(\"$DEST/demo\", git = TRUE)"
