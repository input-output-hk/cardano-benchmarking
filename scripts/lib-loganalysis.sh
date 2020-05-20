#!/usr/bin/env bash
# shellcheck disable=2155

op_msgtype_timespan() {
        local type first last
        type=$(echo $1 | sed 's_^.*\([^\.]*\)$_\1_'); shift
        first=$(grep -Fhi "${type}" "$@" | sort | head -n1 | jq .at | sed 's_^.*T\(.*\)Z.*$_\1_')
        last=$(grep  -Fhi "${type}" "$@" | sort | tail -n1 | jq .at | sed 's_^.*T\(.*\)Z.*$_\1_')
        echo "${first} - ${last}"
}

op_analyse_losses() {
        local sfrom sto lfrom lto rfrom rto tys_explorer tys_generator
        sfrom=$(head -n1 analysis/stx_stime.2 | sed 's_^.*T\(.*\)Z.*$_\1_')
        sto=$(tail   -n1 analysis/stx_stime.2 | sed 's_^.*T\(.*\)Z.*$_\1_')
        lfrom=$(head -n1 analysis/rtx_stime-missing.2 | sed 's_^.*T\(.*\)Z.*$_\1_')
        lto=$(tail   -n1 analysis/rtx_stime-missing.2 | sed 's_^.*T\(.*\)Z.*$_\1_')
        rfrom=$(head -n1 analysis/rtx_rtime.2 | sed 's_^.*T\(.*\)Z.*$_\1_')
        rto=$(tail   -n1 analysis/rtx_rtime.2 | sed 's_^.*T\(.*\)Z.*$_\1_')

        tys_explorer=$(grep -F 'txid'  ./log-explorer.json |
                       ../tools/msgtypes.sh |
                       jq 'map (.kind) | join (" ")' --raw-output --slurp)
        tys_generator=$(grep -F 'txid' ./generator.json |
                       ../tools/msgtypes.sh |
                       jq 'map (.kind) | join (" ")' --raw-output --slurp)
        cat <<EOF
  sends:   ${sfrom} - ${sto}
  losses:  ${lfrom} - ${lto}
  recvs:   ${rfrom} - ${rto}

Message kinds mentioning 'txid':

  explorer node:  ${tys_explorer}
$(for ty in ${tys_explorer}
  do echo -e "    ${ty}:  $(op_msgtype_timespan ${ty} ./log-explorer.json)"; done)

  generator:      ${tys_generator}
$(for ty in ${tys_generator}
  do echo -e "    ${ty}:  $(op_msgtype_timespan ${ty} ./generator.json)"; done)
EOF
}
