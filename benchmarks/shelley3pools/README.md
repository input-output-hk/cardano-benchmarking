# Shelley3Pools mini benchmark

## preparations

1. create a new genesis following [docs/prepare-genesis.md](docs/prepare-genesis.md)

    * `export GENESISDIR=genesis42`
    * `mkdir ${GENESISDIR}`
    * and link `./configuration/genesis/` to the new genesis' directory
      `ln -sf ${GENESISDIR} configuration/genesis`


2. generate keys and certificates for the nodes following [docs/pools-keys-certs.md](docs/pools-keys-certs.md)

3. start the benchmark

    * with stack: `./start.sh --stack-nix`
    * with nix: `./start.sh --nix`
    * default, with cabal: `./start.sh `

4. observe the benchmarking: http://localhost:12799/

