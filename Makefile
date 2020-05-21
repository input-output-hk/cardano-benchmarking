MODE ?= stack

all: help

###
###
###
help:
	@echo
	@echo "Available targets:"
	@echo
	@echo "Build:"
	@echo
	@echo "  build-stack"
	@echo
	@echo "Run:"
	@echo
	@echo "  cluster3nodes (+ aliases: nix cabal stack cabal-in-nix-shell)"
	@echo "  analyse"
	@echo
	@echo "Clean:"
	@echo
	@echo "  clean clean-all clean-cabal clean-stack clean-runtime"
	@echo

###
###
###
build: build-${MODE}

build-stack:
	stack --nix build --copy-bins

###
###
###
nix:                      MODE=nix
cabal:                    MODE=cabal
stack:                    MODE=stack
cabal-in-nix-shell cabsh: MODE=cabal
cabal-in-nix-shell cabsh: PRELUDE=sed -ni '1,/--- 8< ---/ p' "$$(git rev-parse --show-toplevel)"/cabal.project
nix cabal stack cabsh cabal-in-shell run-cluster3nodes run: cluster3nodes

cluster3nodes:
	$(or ${PRELUDE},true)
	./benchmarks/cluster3nodes/start.sh --${MODE}

analyse:
	cd benchmarks/cluster3nodes && ../../scripts/analyse.sh

###
###
###
clean: clean-all

clean-all: clean-cabal clean-stack clean-runtime

clean-cabal:
	cabal clean

clean-stack:
	stack clean

clean-runtime:
	rm -rf db db-* logs logs-*

cls:
	echo -en "\ec"
