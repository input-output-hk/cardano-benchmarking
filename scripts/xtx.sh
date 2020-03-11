#!/bin/sh

sed -ne '/"cardano","cli","generate-txs","submit"],.*,"msg":"Send ConnectionId/{s/"at":"/\n&/;s/tx:/\n&/g;p;}' $*  | sed -ne ' 
  s/^"at":"\([^"]\+\).*$/\1/
  t keep 
  b cont 
  : keep 
  h; n
  : cont 
  s/^tx: Tx \([a-f0-9]\+\).*$/\1/
  t good 
  d
  :good
  G
  s/\n/;/g
  p
  '

