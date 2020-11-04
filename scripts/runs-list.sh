#!/usr/bin/env bash

set -e

list_runs() {
        local depl=$1

        ssh bench find "$depl"/runs -name genesis.json | cut -d/ -f3 | sort -n
}

depl=${1:-bench-1}

list_runs "$depl"
