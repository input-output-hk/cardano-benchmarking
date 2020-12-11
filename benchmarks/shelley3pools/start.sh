#!/usr/bin/env bash

set -e

# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

if test -n "$(pgrep -fal tmux)"
then echo "ERROR:  can't co-exist with existing tmux sessions, sorry!" >&2
     exit 1
fi
tmux new-s -E -s Shelley3Pools -n Main "/usr/bin/env bash ./benchmark.sh $*; $SHELL"

