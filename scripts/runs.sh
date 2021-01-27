#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2046,SC2206,SC2207

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/common.sh

set -e

subcmd=${1:?Usage:  runs SUBCOMMAND [RUN..]}
shift

subinterp="$basedir"/runs-$subcmd.sh
test -x "$subinterp" ||
        fail "no interpreter for $subcmd:  missing $subinterp"

if test -z "$*"
then runs=($(ls runs |
             while read s; test -n "$s"; do if test -d "runs/$s"; then echo $s; fi; done))
else runs=("$@")
fi

oprint "mapping $subinterp over runs:  ${runs[*]}"
for x in ${runs[*]}
do if test ! -d "runs/$x"; then continue; fi
   oprint "run:  $x"
   "$subinterp" "$x"
done
