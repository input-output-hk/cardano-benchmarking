#!/bin/sh

# {"at":"2020-05-08T18:14:13.00Z","env":"1.11.0:00000","ns":["cardano.node.Forge.metrics"],"data":{"kind":"LogValue","value":{"tag":"PureI","contents":889},"name":"nodeIsLeader"},"app":[],"msg":"","pid":"2898","loc":null,"host":"a","sev":"Critical","thread":"48"}
# {"at":"2020-05-15T19:38:20.00Z","env":"1.11.0:66f0e","ns":["cardano.node.Forge"],"data":{"kind":"TraceNodeIsLeader","slot":341},"app":[],"msg":"","pid":"27264","loc":null,"host":"nixos19","sev":"Info","thread":"42"}

#jq -r 'select(.ns[0]=="cardano.node.Forge.metrics" and .data.kind=="LogValue" and .data.name=="nodeIsLeader") | [ .data.value.contents, .at, .data.name ] | @csv' | \

grep -h '"TraceNodeIsLeader"' $* |
jq -r 'select(.ns[0]=="cardano.node.Forge" and .data.kind=="TraceNodeIsLeader") | [ .data.slot, .at, .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
