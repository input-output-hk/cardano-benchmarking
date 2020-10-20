#!/usr/bin/env bash
# shellcheck disable=SC1090

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/../../scripts/common.sh
. "$basedir"/configuration/parameters

gendir=$basedir/$GENESISDIR_shelley
logdir="$basedir"/logs

prebuild 'locli' || exit 1

set -eo pipefail

machines=(node-1 node-2 node-3)

leadership_analysis_args=(
        analyse leadership
        --slot-length  "$(jq .slotLength  "$gendir"/genesis.json -r)"
        --epoch-slots  "$(jq .epochLength "$gendir"/genesis.json -r)"
        --system-start "$(jq .systemStart "$gendir"/genesis.json -r)"
)

keyfile=$(mktemp -t XXXXXXXXXX.keys)
run locli analyse substring-keys > "$keyfile"

mkdir -p "$logdir"/analysis
for mach in ${machines[*]}
do grep -hFf "$keyfile" "$logdir"/"$mach"*.json \
        > "$logdir"/analysis/logs-"$mach".json
   run locli ${leadership_analysis_args[*]} \
         --dump-leaderships     "$logdir"/analysis/logs-"$mach".leaderships.json \
         --dump-pretty-timeline "$logdir"/analysis/logs-"$mach".leaderships.pretty.json \
         > "$logdir"/analysis/logs-"$mach".leadership-analysis.json \
         "$logdir"/analysis/logs-"$mach".json
done

rm -f "$keyfile"

oprint "leadership analyses at:  $logdir/analysis"
