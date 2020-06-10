
# So in order to run a Shelley node we will need to:

    generate an operator's offline key;
    generate a KES operational key;
    generate a VRF operational key; and
    issue an operational certificate

CLICMD="stack --nix exec cardano-cli --"

## make KES (Key Evolving Scheme) and VRF keys

NNODES=3
for N in $(seq 1 $NNODES); do
    mkdir -p ${GENESISDIR}/node${N}/cold

    ${CLICMD} shelley node key-gen-KES \
      --verification-key-file ${GENESISDIR}/node${N}/kes.vkey \
      --signing-key-file ${GENESISDIR}/node${N}/kes.skey
    ${CLICMD} shelley node key-gen-VRF \
      --verification-key-file ${GENESISDIR}/node${N}/vrf.vkey \
      --signing-key-file ${GENESISDIR}/node${N}/vrf.skey

    # cold keys (do not copy to production system)
    ${CLICMD} shelley node key-gen \
      --cold-verification-key-file ${GENESISDIR}/node${N}/cold/operator.vkey \
      --cold-signing-key-file ${GENESISDIR}/node${N}/cold/operator.skey \
      --operational-certificate-issue-counter-file ${GENESISDIR}/node${N}/cold/operator.counter

    # certificate (adapt kes-period for later certs)
    ${CLICMD} shelley node issue-op-cert \
      --hot-kes-verification-key-file ${GENESISDIR}/node${N}/kes.vkey \
      --cold-signing-key-file ${GENESISDIR}/node${N}/cold/operator.skey \
      --operational-certificate-issue-counter ${GENESISDIR}/node${N}/cold/operator.counter \
      --kes-period 0 \
      --out-file ${GENESISDIR}/node${N}/node.cert
done
