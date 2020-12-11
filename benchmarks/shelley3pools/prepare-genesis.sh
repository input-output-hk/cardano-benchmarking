#!/usr/bin/env bash
# shellcheck disable=SC1090

# preparation
BASEDIR=$(realpath "$(dirname "$0")")
. "${BASEDIR}"/../../scripts/common.sh

cd "$BASEDIR" || exit 1

rm -f 'configuration/start-time'
. "$BASEDIR"/configuration/parameters
./prepare_genesis_byron.sh
if test -z "$reuse_genesis"
then ./prepare_genesis_shelley_staked.sh
else sed -i 's/"systemStart": ".*"/"systemStart": "'"$(date \
       --iso-8601=seconds \
       --date=@$(cat "$BASEDIR"/configuration/start-time))"'"/
       ' "$GENESISDIR_shelley"/genesis.json
     ./hash_genesis.sh
fi
