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
        local kind=$1
        local mach=$2
        shift 2
        local runs=($*)

        for r in ${runs[*]}
        do if test ! -f runs/"$r"/analysis/"$kind"-"$mach".ods
           then continue; fi
           echo runs/"$r"/analysis/"$kind"-"$mach".ods
        done
}

pre_sheets_list=(
        runs/Pre-summary.ods
)

target=runs/stats-"$mach".ods
rm -f "$target"
ssconvert --merge-to "$target" \
          ${pre_sheets_list[*]} \
          $(spreadsheets_mach 'stats' "$mach" ${runs[*]})

target=runs/drv-"$mach".ods
rm -f "$target"
ssconvert --merge-to "$target" \
          ${pre_sheets_list[*]} \
          $(spreadsheets_mach 'drv' "$mach" ${runs[*]})
