#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2046,SC2206,SC2207

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/common.sh

set -e

subcmd=${1:?Usage:  runspar SUBCOMMAND [RUN..]}
shift

subinterp="$basedir"/runs-$subcmd.sh
test -x "$subinterp" ||
        fail "no interpreter for $subcmd:  missing $subinterp"

if test -z "$*"
then runs=($(ls runs))
else runs=("$@")
fi

oprint "mapping $subinterp over runs:  ${runs[*]}"

runs_count=${#runs[*]}
oprint "jobs to run:  $runs_count total"

max_batch=$(grep -e '^processor' /proc/cpuinfo | wc -l)

if test $runs_count -gt $max_batch
then oprint "that's too much for a single run -- starting in batches of $max_batch"
     base=0
     while test $base -lt $runs_count
     do batch=(${runs[*]:$base:$max_batch})
        oprint "starting job batch:  ${batch[*]}"

        ## Use the first job as the time gauge:
        for x in ${batch[*]:1:$max_batch}
        do oprint "run:  $x"
           "$subinterp" "$x" &
        done
        ## ..by starting it late and blocking on it:
        sleep 5
        {  x=${batch[0]}
           oprint "run:  $x"
           "$subinterp" "$x"
        }

        oprint "completed batch of ${#batch[*]} jobs:  ${batch[*]}"
        base=$((base + max_batch))
     done
else oprint "that's doable in one go -- blasting ahead"
     for x in ${runs[*]}
     do oprint "run:  $x"
        "$subinterp" "$x" &
     done
fi
