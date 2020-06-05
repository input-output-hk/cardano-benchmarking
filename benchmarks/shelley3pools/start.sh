#!/usr/bin/env bash

set -e

# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

tmux new-s -E -s Shelley3Pools -n Main "./benchmark.sh $*; $SHELL"

