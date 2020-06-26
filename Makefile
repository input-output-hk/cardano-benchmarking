MODE ?= cabal
ERA ?= shelley

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
	@echo "  byron shelley"
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
cabal-in-nix-shell cabsh byron shelley: MODE=cabal
byron:                    ERA=byron
shelley:                  ERA=shelley

cabal-in-nix-shell cabsh: PRELUDE=${NIX_CABAL_SETUP}
nix cabal stack cabsh cabal-in-shell byron shelley: cluster3nodes

cluster3nodes:
	$(or ${PRELUDE},true)
	./benchmarks/cluster3nodes/start.sh --${MODE} --${ERA}

NIX_CABAL_SETUP := sed -ni '1,/--- 8< ---/ p' "$$(git rev-parse --show-toplevel)"/cabal.project

cabal-nix-setup:
	${NIX_CABAL_SETUP}

cabal-restore:
	git checkout HEAD "$$(git rev-parse --show-toplevel)"/cabal.project

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
