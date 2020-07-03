#!/usr/bin/env bash

set -e

# prepare environment
BASEDIR=$(realpath $(dirname "$0"))

WORKINGDIRECTORY=${BASEDIR}/temporary

if [ -d ${WORKINGDIRECTORY} ]; then
  echo "I'll remove and recreate the directory: ${WORKINGDIRECTORY}"
  read -e -i "Y" -p "OK? (Y/N)? " ANSW SOMTHLSE
  if [ "${ANSW}" = "Y" ]; then
    rm -rf ${WORKINGDIRECTORY}
  fi
fi

mkdir -p ${WORKINGDIRECTORY}
export WORKINGDIRECTORY

# create topologies
${BASEDIR}/topology-generator.sh

# create configurations
${BASEDIR}/configuration-generator.sh


# this benchmark runs in 'tmux' which provides several windows and panes
# to arrange the nodes and the transaction generator.

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

tmux new-s -E -s Shelley3Pools -n Main "./benchmark.sh $*; $SHELL"

