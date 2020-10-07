MODE ?= cabal
CABAL_OPTIONS ?= -j8 --ghc-options="+RTS -qn8 -A32M -RTS"

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

sync-shell: sync shell

build:
	cabal build ${CABAL_OPTIONS} ${EXES}

sync:
	@echo "Syncing 'cabal.project' to ../cardano-node"
	./scripts/sync-to.sh

shell:
	nix-shell --max-jobs 8 --cores 0

setup:
	@echo "Modifying 'cabal.project' for interactive development"
	./scripts/cabal-inside-nix-shell.sh

cluster:
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
