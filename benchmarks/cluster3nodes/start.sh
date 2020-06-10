#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e

# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

prebuild 'cardano-tx-generator-byron' || exit 1
prebuild 'cardano-rt-view-service' || exit 1
prebuild 'cardano-node' || exit 1
#prebuild 'cardano-db-sync' || exit 1
prebuild 'cardano-cli' || exit 1

cd "${BASEDIR}"
tmux new-s -E -s Cluster3Nodes -n Main "${BASEDIR}/benchmark.sh ${*@Q}"

