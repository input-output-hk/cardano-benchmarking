#!/usr/bin/env bash

exit 0

# currently, we only update the genesis to be reused
GENESISDIR=configuration/genesis

CLICMD="stack --nix exec cardano-cli --"

MAGIC=4242

${CLICMD} shelley genesis create --genesis-dir ${GENESISDIR} --testnet-magic ${MAGIC} --supply 1000000000

