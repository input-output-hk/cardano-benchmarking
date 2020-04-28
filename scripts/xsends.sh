#!/bin/sh

# Given a JSON log from a Tx generator with configuration, where:
#
#   minSeverity: Debug
#   TracingVerbosity: MaximalVerbosity
#
# ..extract the list of submitted transactions,
# as soon as we see them being requested by the remote peer:
#
#   | TraceBenchTxSubServReq [txid]
#   -- ^ Request for @tx@ recieved from `TxSubmit.TxSubmission` protocol
#   --   peer.

jq '
  select (.data.kind == "TraceBenchTxSubServReq")
| .at as $at        # bind timestamp
| .data.txIds   # narrow to the txid list
| map ( .[11:]         # cut the "txid: txid: " prefix
      | "\(.);\($at)") # produce the resulting string
| .[]               # merge string lists over all messages
' $1 |
tr -d '"'
