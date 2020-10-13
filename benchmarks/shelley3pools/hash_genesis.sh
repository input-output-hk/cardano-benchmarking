#!/usr/bin/env bash

set -e

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/../../scripts/common.sh
. "$basedir"/configuration/parameters

cd "$basedir"

gendir=$GENESISDIR_shelley
cli=${CLICMD:-'run cardano-cli'}

$cli shelley genesis hash --genesis "$gendir"/genesis.json |
        tr -d '"' > "$gendir"/GENHASH
