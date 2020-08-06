#!/bin/sh

# Usage:
#
#   ./apply-stylish-haskell.sh
#
# All Haskell modules will be reformatted.

ALL_DIRS_WITH_HASKELL="bm-timeline cardano-rt-view cardano-tx-generator tx-generator-shelley"

for DIR in ${ALL_DIRS_WITH_HASKELL}; do
  find "${DIR}" -type f -name '*.hs' -exec stylish-haskell -i {} \;
done
