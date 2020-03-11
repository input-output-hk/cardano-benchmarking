#!/bin/sh

# Given a JSON log from a node with configuration, where:
#
#   minSeverity: Debug
#   TracingVerbosity: MaximalVerbosity
#   TraceBlockFetchClient: True
#
# ..extract the list of incoming transactions,
# as soon as we see them coming in a via BlockFetch protocol.

jq '
  select (.data.msg.kind == "MsgBlock")
| .at as $at        # bind timestamp
| .data.msg.txids   # narrow to the txid list
| map ( .[11:]         # cut the "txid: txid: " prefix
      | "\(.);\($at)") # produce the resulting string
| .[]               # merge string lists over all messages
' $1 |
tr -d '"'
