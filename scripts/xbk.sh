#!/bin/sh

# [2019-12-03 16:15:58.59 UTC]

#sed -ne '/ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock/{s/ \[[0-9]\+-[0-9]\+-[0-9]\+ [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\.[0-9][0-9] UTC\]/\n&/;s/Tx [^ ]\+ with inputs /\n&/g;p;}' $1  | sed -ne '
#  s/^ \[\([0-9]\+-[0-9]\+-[0-9]\+\) \([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\.[0-9][0-9]\) UTC\].*$/\1T\2Z/
# jq --slurp 'map (select (.data.event.kind == "TraceCopyToImmDBEvent.CopiedBlockToImmDB")) | .[0] | "\(.at);\(.data.tip | sub (".*@(?<slot>.*)$"; "\(.slot)"))"' $LOGFILE_JSON | xargs echo | sed 's/\([0-9]\)T\([0-9]\)/\1 \2/; s/\(20[0-9][0-9]\)/"\1/; s/Z/"/'

sed -ne '/ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock/{s/"at":"/\n&/;s/Tx [^ ]\+ with inputs /\n&/g;p;}' $1  | sed -ne ' 
  s/^"at":"\([^"]\+\).*$/\1/
  t keep 
  b cont 
  : keep 
  h; n
  : cont 
  s/^Tx \([a-f0-9]\+\) with inputs.*$/\1/
  t good 
  d
  :good
  G
  s/\n/;/g
  p
  '

