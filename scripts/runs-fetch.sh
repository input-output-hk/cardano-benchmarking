#!/usr/bin/env bash

set -e

fetch_run() {
        local depl=$1 run=$2
        local deplbase=bench:$depl/runs/$run/ rundir=./runs/$run
        shift 2

        mkdir -p "$rundir"/analysis
        scp ${@/#/$deplbase} "$rundir"
}

run=$1
depl=${2:-bench-1}

fetch_list=(
        #analysis.json
        genesis.json
        meta.json
        logs/logs-nodes.tar.xz
        logs/logs-explorer.tar.xz
)

fetch_run "$depl" "$run" "${fetch_list[@]}"
