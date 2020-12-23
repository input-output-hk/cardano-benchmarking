#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2046,SC2206,SC2207

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/common.sh

set -e

logdir_default=runs-last
logdir=${1:-$logdir_default}
if test "$logdir" = '-';   then logdir=$logdir_default; fi
if test -d runs/"$logdir"; then logdir=runs/"$logdir"; fi
if test $# -gt 0; then shift; fi
test -d "$logdir" ||
        fail "logdir absent: $logdir"

run_name=$(basename "$(realpath "$logdir")" |
           cut -d. -f3-)

machines=($*)
if test ${#machines[*]} -eq 0
then machines=($(cd "$logdir"/analysis;
                 find . -type d -name 'logs-node-*' | sed 's_^\./logs-__';)); fi

spreadsheet_mach() {
        local logdir=$1 mach=$2

        oprint "generating sheets for $mach"
        ln -sf logs-"$mach".stats.csv    "$logdir"/analysis/"$run_name"."$mach".s.csv
        ln -sf logs-"$mach".timeline.csv "$logdir"/analysis/"$run_name"."$mach".t.csv

        ssconvert "$logdir"/analysis/"$run_name"."$mach".s.csv "$logdir"/analysis/stats-"$mach".ods
        # ssconvert "$logdir"/analysis/"$run_name"."$mach".t.csv "$logdir"/analysis/timeline-"$mach".ods
}

time for mach in ${machines[*]}
do spreadsheet_mach "$logdir" "$mach"
done
