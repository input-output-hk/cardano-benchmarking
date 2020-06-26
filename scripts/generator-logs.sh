#!/usr/bin/env bash

set -euo pipefail

usage() {
        cat <<EOF
USAGE:  $(basename "$0")  OP OPARGS..

Manipulate tx-generator logs.

  Valid ops are:

    log-tids [LOG..=stdin]        Node2node logfile's submission thread ids (TIDs).
    tid-trace [N=0] [LOG..=stdin] Node2node submission trace for N'th thread.
    tid-debug [N=0] [LOG..=stdin] Node2node submission debug trace for N'th thread.
    trace-report [TRACE..=stdin]  Neat CSV out of a submission trace.

    report-schema                 Libreoffice CSV import schema.
    log-sleeps [LOG..=stdin]      Generator sleeps.

EOF
}

main() {
        cmd="$1"; shift
        case "$cmd" in
        log-tids )          log_tids          "$@";;
        tid-trace )         log_tid_trace     "$@";;
        tid-debug )         log_tid_debug     "$@";;
        trace-report )      trace_report      "$@";;
        report-schema )     report_schema     "$@";;
        log-sleeps )        log_sleeps        "$@";;
        call )              "$@";;
        * ) echo "ERROR: bad subcommand: $1">&2; exit 1;; esac
}

msgs=(
        ReqIdsBlocking
        IdsListBlocking
        ReqIdsPrompt
        IdsListPrompt
        EndOfProtocol
        KThxBye
        ReqTxs
        TxList
)
msgs_debug=(
        TraceBenchTxSubServAnn
        TraceBenchTxSubServReq
        TraceBenchTxSubServAck
        TraceBenchTxSubServDrop
        TraceBenchTxSubServOuts
        TraceBenchTxSubDebug
        TraceBenchTxSubError
)

log_tids() {
        grep "\"\($(IFS='| '; echo "${msgs[*]}" | sed 's_|_\\|_g')\)\".*" "$@" |
        jq '.thread' --raw-output | sort -u
}

log_nth_tid() {
        local thread_no=$1; shift
        log_tids "$@" | xargs echo -n | cut -d' ' -f$((thread_no + 1))
}

log_tid_trace() {
        local thread_no=$1 thread_id; shift
        thread_id=$(log_nth_tid "${thread_no}" "$@")

        grep "\"\($(IFS='| '; echo "${msgs[*]}" | sed 's_|_\\|_g')\)\".*" "$@" |
        jq ' .
           | "'${thread_id}'" as $tid
           | if $tid != "" then select(.thread == $tid) else . end
           | { at: .at, thread: .thread } + .data
           ' --compact-output
}

log_tid_debug() {
        local thread_no=$1 thread_id; shift
        thread_id=$(log_nth_tid "${thread_no}" "$@")

        grep "\"\($(IFS='| '; echo "${msgs[*]}|${msgs_debug[*]}" | sed 's_|_\\|_g')\)\".*" "$@" |
        jq ' .
           | "'${thread_id}'" as $tid
           | if $tid != "" then select(.thread == $tid) else . end
           | { at: .at, thread: .thread } + .data
           ' --compact-output
}

trace_report() {
        sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/' "$@" |
        sed '$!N; s/^\(.*\)\n\(.*\)$/{ "req": \1, "resp": \2 }/' |
        jq '
def render_req($r):
  if      $r.kind == "ReqIdsBlocking" or $r.kind == "ReqIdsPrompt"
  then [$r.at, $r.kind, $r.ack, $r.req, null]
  else if $r.kind == "ReqTxs"
  then [$r.at, $r.kind, null,   null,   $r.req]
  else try error("invalid request kind: \($r)") catch empty end end;

def render_resp($r):
  if      $r.kind == "IdsListBlocking" or $r.kind == "IdsListPrompt"
  then [$r.at, $r.kind, $r.sent, null]
  else if $r.kind == "TxList"
  then [$r.at, $r.kind, null,    $r.sent]
  else try error("invalid response kind: \($r)") catch empty end end;

## This goes exponential.. Need to investigate jq JOIN.
# def go($s; $acc): .
# | if $s | has(1) | not then $acc
#   else go( $s[2:]
#          ; [ render_req($s[0]) + render_resp($s[1]) ]
#            + acc)
#   end;

[[ "req stamp", "req kind", "acked ids", "reqd ids", "reqd txs"
 , "rep stamp", "rep kind", "sent ids", "sent txs"]] +
 map
  ( render_req(.req) + render_resp(.resp)
  )
| .[]
| @csv' --slurp --raw-output
}

log_sleeps() {
        grep --no-filename -F '******* sleeping for' "$@" |
        jq ' map ({ at: .at[:18], sleep: (.data.msg[21:-1] | fromjson) })
           | group_by (.at)
           | map ({ at: .[0].at, total_slept: (map (.sleep) | add)})
           | .[]' --slurp --compact-output
}

main "$@"
# ReqIdsBlocking  ack req -> mkObject [ "kind" .= A.String "ReqIdsBlocking"
#                                     , "ack"  .= A.toJSON ack
#                                     , "req"  .= A.toJSON req ]
# IdsListBlocking sent    -> mkObject [ "kind" .= A.String "IdsListBlocking"
#                                     , "sent" .= A.toJSON sent ]
# ReqIdsPrompt    ack req -> mkObject [ "kind" .= A.String "ReqIdsPrompt"
#                                     , "ack"  .= A.toJSON ack
#                                     , "req"  .= A.toJSON req ]
# IdsListPrompt   sent    -> mkObject [ "kind" .= A.String "IdsListPrompt"
#                                     , "sent" .= A.toJSON sent ]
# EndOfProtocol           -> mkObject [ "kind" .= A.String "EndOfProtocol" ]
# KThxBye                 -> mkObject [ "kind" .= A.String "KThxBye" ]
# ReqTxs          req     -> mkObject [ "kind" .= A.String "ReqTxs"
#                                     , "req"  .= A.toJSON req ]
# TxList          sent    -> mkObject [ "kind" .= A.String "TxList"
#                                     , "sent" .= A.toJSON sent ]
