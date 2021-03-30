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
run_batch=$(basename "$(realpath "$logdir")" |
            cut -d. -f3)

machines=($*)
if test ${#machines[*]} -eq 0
then machines=($(cd "$logdir"/analysis;
                 find . -type d -name 'logs-node-*' | sed 's_^\./logs-__';)); fi

spreadsheet_mach() {
        local logdir=$1 mach=$2

        oprint "generating sheets for $mach"
        local infix

        ln -sf logs-"$mach".stats.csv           "$logdir"/analysis/"$run_name".csv
        rm -f "$logdir"/analysis/stats-"$mach".ods
        ssconvert "$logdir"/analysis/"$run_name".csv "$logdir"/analysis/"$(cut -d. -f3 <<<$run_batch)".ods

        ## We ignore the derived vectors for now.
        # rm -f "$logdir"/analysis/drv-"$mach".*.ods
        # for d in 0 1
        # do ln -sf logs-"$mach".derived.$d.csv "$logdir"/analysis/"$run_name".d$d.csv
        #    ssconvert "$logdir"/analysis/"$run_name".d$d.csv "$logdir"/analysis/drv-"$mach".$d.ods
        # done

        ## We ignore the timeline for now.
        # ln -sf logs-"$mach".timeline.csv "$logdir"/analysis/"$run_name"."$mach".t.csv
        # ssconvert "$logdir"/analysis/"$run_name"."$mach".t.csv "$logdir"/analysis/timeline-"$mach".ods
}

# time for mach in ${machines[*]}
# do spreadsheet_mach "$logdir" "$mach"
# done

spreadsheet_mach "$logdir" 'node-1'
