#!/usr/bin/env bash

# currently, we only update the genesis to be reused
GENESISDIR=configuration/genesis

CLICMD="stack --nix exec cardano-cli --"

MAGIC=42

# set parameters in template
sed -i ${GENESISDIR}/genesis.spec.json \
    -e 's/"slotLength": .*,/"slotLength": 0.2,/' \
    -e 's/"activeSlotsCoeff": .*,/"activeSlotsCoeff": 0.1,/' \
    -e 's/"securityParam": .*,/"securityParam": 10,/' \
    -e 's/"epochLength": .*,/"epochLength": 1500,/' \
    -e 's/"maxLovelaceSupply": .*,/"maxLovelaceSupply": 1000000000,/' \
    -e 's/"decentralisationParam": .*,/"decentralisationParam": 0.7,/'


${CLICMD} shelley genesis create --genesis-dir ${GENESISDIR} --testnet-magic ${MAGIC} --supply 1000000000

