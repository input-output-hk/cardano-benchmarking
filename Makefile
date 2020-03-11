MODE ?= stack

all: help

###
###
###
help:
	@echo
	@echo "Available targets:"
	@echo
	@echo "  build build-* run run-* clean clean-*"
	@echo
	@echo "Build targets:"
	@echo
	@echo "  build-stack"
	@echo
	@echo "Run targets:"
	@echo
	@echo "  run-cluster3nodes"
	@echo
	@echo "Clean targets:"
	@echo
	@echo "  clean-all"
	@echo "  clean-stack"
	@echo "  clean-runtime"
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
run: run-cluster3nodes

run-cluster3nodes:
	cd benchmarks/cluster3nodes && ./start.sh

###
###
###
clean: clean-all

clean-all: clean-stack clean-runtime

clean-stack:
	stack clean

clean-runtime:
	rm -rf db db-* logs
