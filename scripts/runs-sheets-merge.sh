#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2046,SC2206,SC2207

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/common.sh

set -e

mach=node-1

presheet=runs/$1; shift

test -f "$presheet" ||
    fail "Pre-sheet $presheet doesn't exit."

if test -z "$*"
then runs=($(ls runs))
else runs=("$@")
fi

spreadsheets() {
    batch_tag=$1

    pushd runs >/dev/null

    for r in ${runs[*]}
    do if test ! -f "$r"/analysis/*.ods
       then fail "bad run id: $r"; fi

       rtag=$(cut -d. -f3 <<<$r)
       ln -sf $(eval ls "$r"/analysis/*.ods) ${batch_tag}-${rtag}.ods
       echo runs/${batch_tag}-${rtag}.ods
    done

    popd >/dev/null
}

batch_tag=$(date +%Y-%m-%d)
target=runs/$batch_tag.ods
rm -f "$target"
ssconvert --merge-to "$target" \
          ${presheet} \
          $(spreadsheets $batch_tag)

# target=runs/drv-"$mach".ods
# rm -f "$target"
# ssconvert --merge-to "$target" \
#           ${pre_sheets_list[*]} \
#           $(spreadsheets_mach 'drv' "$mach" ${runs[*]})
