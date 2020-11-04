#!/usr/bin/env bash

set -e

unpack_run() {
        local run=$1 rundir=./runs/$run

        mkdir -p "$rundir"/analysis
        (
                cd "$rundir"/analysis
                tar xaf ../logs-nodes.tar.xz logs-node-{0,1,3,10,20}
        )
}

fetch_run() {
        local depl=$1 run=$2 rundir=./runs/$run deplbase=bench:$depl/runs/$run/

        scp "$deplbase"/genesis.json "$deplbase"/logs/logs-nodes.tar.xz "$rundir"
}

run=$1
depl=${2:-bench-1}

fetch_run "$depl" "$run"
unpack_run "$run"
