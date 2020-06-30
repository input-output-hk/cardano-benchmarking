#!/usr/bin/env bash

BUILD=${1:-"--stack-nix"}

# Create topology files for NNODES
./topology-generator.sh "${BUILD}"
wait

# Create configuration files for NNODES
./configuration-generator.sh "${BUILD}"
wait

# Create Genesis based on parameters
./prepare_genesis-generic.sh "${BUILD}"
wait

# Run the nodes
./start-generic.sh "${BUILD}"
