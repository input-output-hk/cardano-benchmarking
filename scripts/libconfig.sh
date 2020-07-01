#!/usr/bin/env bash
# shellcheck disable=SC2034

## This shell module captures all things that are prone to change,
## due to 'cardano-node' / 'cardano-benchmarking' / 'haskell.nix' changes.

## Map cabal package name into a canonical package set name,
## as (supposed to be) made available uniformly across 'default.nix' files
## in all our repositories.
declare -A CABALPKG_TO_HASKELLNIX_PKGSET
CABALPKG_TO_HASKELLNIX_PKGSET=(
        [cardano-api]='cardanoNodeHaskellPackages'
        [cardano-cli]='cardanoNodeHaskellPackages'
        [cardano-config]='cardanoNodeHaskellPackages'
        [cardano-node]='cardanoNodeHaskellPackages'
        [cardano-db-sync]='cardanoDbSyncHaskellPackages'
        [cardano-rt-view]='cardanoBenchmarkingHaskellPackages'
        [cardano-tx-generator]='cardanoBenchmarkingHaskellPackages'
        [bmtimeline]='cardanoBenchmarkingHaskellPackages'
)

declare -A CABALEXE_TO_CABALPKG
CABALEXE_TO_CABALPKG=(
        [cardano-cli]='cardano-cli'
        [cardano-config]='cardano-node'
        [cardano-db-sync]='cardano-db-sync'
        [cardano-node]='cardano-node'
        [cardano-rt-view]='cardano-rt-view'
        [cardano-rt-view-service]='cardano-rt-view'
        [cardano-tx-generator]='cardano-tx-generator'
        [cardano-tx-generator-byron]='cardano-tx-generator'
        [bmtimeline]='bm-timeline'
)

declare -A CABALEXE_TO_LIBOPTS
CABALEXE_TO_LIBOPTS=(
)
