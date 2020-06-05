
# So in order to run a Shelley node we will need to:

    generate an operator's offline key;
    generate a KES operational key;
    generate a VRF operational key; and
    issue an operational certificate

CLICMD="stack --nix exec cardano-cli --"

## make KES (Key Evolving Scheme) and VRF keys

NNODES=3
for N in $(seq 1 $NNODES); do
    mkdir -p ${GENESISDIR}/node${N}

    ${CLICMD} shelley node key-gen-KES \
      --verification-key-file ${GENESISDIR}/node${N}/kes.vkey \
      --signing-key-file ${GENESISDIR}/node${N}/kes.skey
    ${CLICMD} shelley node key-gen-VRF \
      --verification-key-file ${GENESISDIR}/node${N}/vrf.vkey \
      --signing-key-file ${GENESISDIR}/node${N}/vrf.skey

    # certificate (adapt kes-period for later certs)
    ${CLICMD} shelley node issue-op-cert \
      --hot-kes-verification-key-file ${GENESISDIR}/node${N}/kes.vkey \
      --cold-signing-key-file ${GENESISDIR}/delegate-keys/delegate${N}.skey \
      --operational-certificate-issue-counter ${GENESISDIR}/delegate-keys/delegate-opcert${N}.counter \
      --kes-period 0 \
      --out-file ${GENESISDIR}/node${N}/cert
done


## run nodes
N=0
stack exec cardano-node -- run \
    --config ${GENESISDIR}/node${N}/configuration.yaml \
    --topology ${GENESISDIR}/node${N}/topology.json \
    --database-path ${GENESISDIR}/node${N}/db \
    --socket-path ${GENESISDIR}/node${N}/node.sock \
    --shelley-kes-key ${GENESISDIR}/node${N}/kes.skey \
    --shelley-vrf-key ${GENESISDIR}/node${N}/vrf.skey \
    --shelley-operational-certificate ${GENESISDIR}/node${N}/cert \
    --port $((3000 + $N))

