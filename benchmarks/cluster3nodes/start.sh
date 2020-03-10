#!/usr/bin/env bash

# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

BASEPATH=$(realpath $(dirname $0))

tmux new-s -s Cluster3Nodes -n Main "${BASEPATH}/benchmark.sh"

