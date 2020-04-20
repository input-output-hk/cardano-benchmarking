[![Build status](https://badge.buildkite.com/19c55c10f4ea983dd84ec31abbd39c85f2396cb65364dd180b.svg)](https://buildkite.com/input-output-hk/cardano-benchmarking?branch=master)

# cardano-benchmarking

## preparation

After first checkout of the repository, update the submodules:
`git submodule update --init`


## building

### 1 using `stack`

`stack --nix build --copy-bins`
will compile the project and copy the binaries to the directory ./bin/

### 2 using `cabal` (version >= 3)

`cabal v2-build all`

`cabal v2-install ext/cardano-node.git/cardano-node:exe:cardano-node --install-method=copy --installdir=./bin`
