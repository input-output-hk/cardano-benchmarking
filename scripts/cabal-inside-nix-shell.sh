#!/usr/bin/env bash

default_localise_repo_list=(
        ouroboros-network
        iohk-monitoring-framework
        cardano-node
)

localise_repo_list=("$@")
if test -z "${localise_repo_list[*]}"
then localise_repo_list=(${default_localise_repo_list[*]})
fi

cabal_project="$(git rev-parse --show-toplevel)"/cabal.project

set -x
sed -ni '1,/--- 8< ---/ p'      "$cabal_project"
for repo in ${localise_repo_list[*]}
do sed -i 's_^    -- ../'$repo'/_    ../'$repo'/_' "$cabal_project"
done
