
# create new genesis

## limitations

The CLI command will reset the networkMagic to "42" every time, so we stick to that.


## settings

(https://github.com/input-output-hk/cardano-node/blob/master/doc/shelley-genesis.md)

CLICMD="cabal --enable-nix exec cardano-cli --"

GENESISDIR=genesis01

if [ ! -d ${GENESISDIR} ]; then mkdir -p ${GENESISDIR}; fi

${CLICMD} shelley genesis create \
     --genesis-dir ${GENESISDIR} \
     --gen-genesis-keys 3 \
     --gen-utxo-keys 3 \
     --start-time "2020-06-09T06:00:01Z" \
     --testnet-magic 42 \
     --supply 1000000002

## manual genesis: rerun the command below to update the genesis from keys and the spec
${CLICMD} shelley genesis create --genesis-dir ${GENESISDIR} --testnet-magic 42


## make genesis keys (only if manual)

NKEYS=3
for N in $(seq 1 $NKEYS); do
  ${CLICMD} shelley genesis key-gen-genesis \
      --verification-key-file ${GENESISDIR}/genesis-keys/genesis${N}.vkey \
      --signing-key-file ${GENESISDIR}/genesis-keys/genesis${N}.skey
  ${CLICMD} shelley genesis key-gen-delegate \
      --verification-key-file ${GENESISDIR}/delegate-keys/delegate${N}.vkey \
      --signing-key-file ${GENESISDIR}/delegate-keys/delegate${N}.skey \
      --operational-certificate-issue-counter ${GENESISDIR}/delegate-keys/delegate-opcert${N}.counter
   ${CLICMD} shelley genesis key-gen-utxo \
      --verification-key-file ${GENESISDIR}/utxo-keys/utxo${N}.vkey \
      --signing-key-file ${GENESISDIR}/utxo-keys/utxo${N}.skey
done


## remake genesis from spec

every address will start with (10^9 / $NKEYS) Lovelaces

${CLICMD} shelley genesis create --genesis-dir ${GENESISDIR} --supply 1000000000

