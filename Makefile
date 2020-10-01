MODE ?= cabal
CABAL_OPTIONS ?= -j8 # --ghc-options="+RTS -qn8 -A32M -RTS"

all: help

###
###
###
help:
	@echo
	@echo "Available targets:"
	@echo
	@echo "  sync:     sync cabal.project to ../cardano-node"
	@echo "  shell:    enter the Nix shell"
	@echo "  setup:    post-nix-shell-entrance interactive development setup"
	@echo "  build:    build all local cluster dependencies"
	@echo "  cabal:    run the default local cluster benchmark in cabal mode"
	@echo "  nix:      run the default local cluster benchmark in Nix mode"
	@echo
	@echo "Clean:"
	@echo
	@echo "  clean clean-all clean-cabal clean-runtime"
	@echo

EXES=cardano-node cardano-cli cardano-tx-generator

###
###
###
nix:                      MODE=nix
cabal:                    MODE=cabal
cabal:                    EXTRA_OPTS=--no-path-exes
nix cabal: cluster

sync:
	@echo "Syncing 'cabal.project' to ../cardano-node"
	./scripts/sync-to.sh

strict-sync strict:
	@echo "Strictly syncing 'cabal.project' to ../cardano-node"
	./scripts/sync-to.sh --strict-coherence

shell: sync
	nix-shell --max-jobs 8 --cores 0

strict-shell: strict-sync
	nix-shell --max-jobs 8 --cores 0

setup:
	@echo "Modifying 'cabal.project' for interactive development"
	./scripts/cabal-inside-nix-shell.sh

build: setup
	cabal v2-build ${CABAL_OPTIONS} $(foreach exe,${EXES},${exe}:exe:${exe})

cluster: build
	cd ./benchmarks/shelley3pools; ./start.sh --${MODE} ${EXTRA_OPTS}

###
###
###
clean: clean-all

clean-all: clean-cabal clean-runtime

clean-cabal:
	cabal clean

clean-runtime:
	rm -rf db db-* logs logs-*

cls:
	echo -en "\ec"
