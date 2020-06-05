#!/usr/bin/env bash

# currently, we only update the genesis to be reused
GENESISDIR=configuration/genesis

CLICMD="stack --nix exec cardano-cli --"

${CLICMD} shelley genesis create --genesis-dir ${GENESISDIR} --supply 1000000000

