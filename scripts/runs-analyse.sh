#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2046,SC2206,SC2207

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/common.sh

logdir_default=runs-last
logdir=${1:-$logdir_default}
if test "$logdir" = '-';   then logdir=$logdir_default; fi
if test -d runs/"$logdir"; then logdir=runs/"$logdir"; fi
if test $# -gt 0; then shift; fi
test -d "$logdir" ||
        fail "logdir absent: $logdir"

run_name=$(basename "$(realpath "$logdir")" |
           cut -d. -f3-)

genesis_default="$logdir"/genesis.json
genesis=${1:-$genesis_default}
if test "$genesis" = '-'; then genesis=$genesis_default; fi
if test $# -gt 0; then shift; fi
test -f "$genesis" -a -r "$genesis" ||
        fail "genesis absent: $genesis"

machines=($*)
if test ${#machines[*]} -eq 0
then machines=($(cd "$logdir"/analysis;
                 find . -type d -name 'logs-node-*' | sed 's_^\./logs-__';)); fi

oprint "run:       $run_name"
oprint "logdir:    $logdir"
oprint "genesis:   $genesis"
oprint "machines:  ${machines[*]}"

prebuild 'locli' || exit 1

set -eo pipefail

oprint "querying genesis params.."
jq '. + { staking: {}, initialFunds: {} }' "$genesis" > "$genesis". &&
        mv "$genesis". "$genesis"

machine_logfiles() {
        local logdir=$1 mach=$2
        ls "$logdir"/analysis/logs-"$mach"/node-*.json
}

locli_analyse_cmd_mach_args() {
        local logdir=$1 mach=$2

        echo --run-metafile
        echo "$logdir"/meta.json
        echo --genesis
        echo "$logdir"/genesis.json
        echo --dump-logobjects
        echo "$logdir"/analysis/logs-"$mach".logobjects.json
        echo --dump-leaderships
        echo "$logdir"/analysis/logs-"$mach".leaderships.json
        echo --analysis-output
        echo "$logdir"/analysis/logs-"$mach".analysis.json
        echo --pretty-timeline
        echo "$logdir"/analysis/logs-"$mach".analysis.txt
        echo --export-stats
        echo "$logdir"/analysis/logs-"$mach".stats.csv
        echo --export-timeline
        echo "$logdir"/analysis/logs-"$mach".timeline.csv
        echo --cpu-spans-histogram
        echo "$logdir"/analysis/logs-"$mach".cpu85-span-lens.png
}

analyse_mach() {
        local logdir=$1 mach=$2 logs mach_consolidated

        ## 1. enumerate
        logs=($(machine_logfiles "$logdir" "$mach")
              $(machine_logfiles "$logdir" 'explorer')
              $(ls "$logdir"/analysis/logs-explorer/generator-*.json))
        mach_consolidated="$logdir"/analysis/logs-"$mach".json

        ## 2. filter & join
        grep -hFf "$keyfile" "${logs[@]}" \
             > "$mach_consolidated"

        ## 3. analyse
        oprint "analysing logs of:  $mach  (lines: $(wc -l "$mach_consolidated"))"
        run locli 'analyse' 'leadership'            \
                $(locli_analyse_cmd_mach_args "$logdir" "$mach") \
                "$mach_consolidated"
}

## 0. compute filter subset
keyfile=$(mktemp -t XXXXXXXXXX.keys)
run locli analyse substring-keys > "$keyfile"

time for mach in ${machines[*]}
do analyse_mach     "$logdir" "$mach"
done

rm -f "$keyfile"

oprint "analyses at:  $logdir/analysis"
