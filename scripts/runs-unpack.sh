#!/usr/bin/env bash

set -e

unpack_run() {
        local run=$1
        local rundir=./runs/$run

        mkdir -p "$rundir"/analysis
        (
                cd "$rundir"/analysis
                tar xaf ../logs-nodes.tar.xz    logs-node-{1,10}
                tar xaf ../logs-explorer.tar.xz
        )
}

run=$1

unpack_run "$run"
