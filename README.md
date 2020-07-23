[![Build status](https://badge.buildkite.com/19c55c10f4ea983dd84ec31abbd39c85f2396cb65364dd180b.svg)](https://buildkite.com/input-output-hk/cardano-benchmarking?branch=master)

# cardano-benchmarking

This repository contains two important tools for `cardano-node` benchmarking:

1. [cardano-tx-generator](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-tx-generator)
2. [cardano-rt-view-service](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-rt-view)

The first one is a generator of transactions, both for Byron and Shelley. For more details please [see the project](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-tx-generator).

The second one is a service that allows to see the state of running `cardano-node` processes in the real time. For more details please [see the project](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-rt-view).

[![RTView screenshot](https://github.com/input-output-hk/cardano-benchmarking/blob/master/cardano-rt-view/screenshot-small.png)](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-rt-view)

## Preparation

After first checkout of the repository, update the submodules:

```
$ git submodule update --init
```

## Building on Linux / Mac OSX

The following commands will compile the project and copy the binaries to the directory `./bin/`.

### 1 using `stack`

```
$ stack --nix build --copy-bins
```

### 2 using `cabal` (version >= 3)

```
$ cabal build all
$ cabal install ext/cardano-node.git/cardano-node:exe:cardano-node --install-method=copy --installdir=./bin
```

## Building on Windows

Install _MinGW_ (64 bit) ![MinGW](https://raw.githubusercontent.com/wiki/input-output-hk/cardano-benchmarking/mingw64.png) environment and run `stack` in it to compile _cardano-benchmarking_ and all its dependencies.
(see our [wiki page](https://github.com/input-output-hk/cardano-benchmarking/wiki/BuildingOnWindows))
