#!/usr/bin/env bash

set -e

# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

if test -n "$(pgrep -fal tmux)"
then tmux kill-session -t Shelley3Pools
fi
tmux new-s -E -s Shelley3Pools -n Main "/usr/bin/env bash ./benchmark.sh $*; $SHELL"

