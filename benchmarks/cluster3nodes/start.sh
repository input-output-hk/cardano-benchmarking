#!/usr/bin/env bash

set -e

# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

BASEDIR="$(realpath "$(dirname "$0")")"

cd "${BASEDIR}"
tmux new-s -E -s Cluster3Nodes -n Main "${BASEDIR}/benchmark.sh ${*@Q}"

