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
        [cardano-tx-generator]='cardanoBenchmarkingHaskellPackages'
        [locli]='cardanoBenchmarkingHaskellPackages'
)

declare -A CABALEXE_TO_CABALPKG
CABALEXE_TO_CABALPKG=(
        [cardano-cli]='cardano-cli'
        [cardano-config]='cardano-node'
        [cardano-node]='cardano-node'
        [cardano-tx-generator]='cardano-tx-generator'
        [locli]='locli'
)

declare -A CABALEXE_TO_LIBOPTS
CABALEXE_TO_LIBOPTS=(
)
