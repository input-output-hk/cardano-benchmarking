#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2046,SC2206,SC2207

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/common.sh

set -e

mach=node-1

if test -z "$*"
then runs=($(ls runs))
else runs=("$@")
fi

spreadsheets_mach() {
        local mach=$1
        shift
        local runs=($*)

        for r in ${runs[*]}
        do if test ! -f runs/"$r"/analysis/stats-"$mach".ods
           then continue; fi
           echo runs/"$r"/analysis/stats-"$mach".ods
        done
}

pre_sheets_list=(
        runs/Pre-summary.ods
)

target=runs/stats-"$mach".ods
rm -f "$target"
ssconvert --merge-to "$target" \
          ${pre_sheets_list[*]} \
          $(spreadsheets_mach "$mach" ${runs[*]})
