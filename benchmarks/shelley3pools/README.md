# Shelley3Pools mini benchmark

## preparations

1. create a new genesis following [docs/prepare-genesis.md](docs/prepare-genesis.md)

    * `export GENESISDIR=genesis42`
    * `mkdir ${GENESISDIR}`
    * and link `./configuration/genesis/` to the new genesis' directory
      `ln -sf ${GENESISDIR} configuration/genesis`


2. generate keys and certificates for the nodes following [docs/pools-keys-certs.md](docs/pools-keys-certs.md)

3. start the benchmark

    * with nix: `./start.sh --nix`
    * default, with cabal: `./start.sh`

4. observe the benchmarking: http://localhost:12799/


## analyse timeline of run

the nodes in _shelley3nodes_ are counted from 1, but the `reconstruct-timeline` utility expects them
to start from 0.

```
LOGPATH=logs
NNODES=3
OUTDIR=timeline-42
mkdir ${OUTDIR}
for N in $(seq 0 $((NNODES - 1))); do
  ../../scripts/nodeisleader.sh ${LOGPATH}/node$((N+1))-* | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/leader-${N}.csv
  ../../scripts/addedtocurrentchain.sh ${LOGPATH}/node$((N+1))-* | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/addtochain-${N}.csv
  ../../scripts/adoptedblock.sh ${LOGPATH}/node$((N+1))-* | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/adopted-${N}.csv
done
```

reconstruct timeline:
```
cabal --enable-nix run reconstruct-timeline -- ${NNODES} ${OUTDIR} | tee -a ${OUTDIR}/timeline.txt
cp timeline.csv ${OUTDIR}/
```

