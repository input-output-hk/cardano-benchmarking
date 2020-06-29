#!/usr/bin/env bash

: <<'END_COMMENT'
Example 3-node topology file:

{
  "Producers": [
    {
      "addr": "127.0.0.1",
      "port": 3000,
      "valency": 1
    },
    {
      "addr": "127.0.0.1",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "127.0.0.1",
      "port": 3003,
      "valency": 1
    }
  ]
}
END_COMMENT

. ./configuration/parameters

NNODES=${NNODES:-3}
PORTBASE=${PORTBASE:-3}

for node in $(seq 0 $(( NNODES - 1 )))
do
    current_port=$(( node + PORTBASE ))
    echo $(echo '
{
    "Producers": ['

    i="0"
    while [ $i -le $(( NNODES - 1 )) ]
    do
        port=$(( PORTBASE + $i ))

        if [ $port -ne $current_port ]; then

        echo "
        {
            \"addr\": \"127.0.0.1\",
            \"port\": ${port},
            \"valency\": 1
        },"
        fi

        i=$(($i+1))
    done
    
    echo '      
    ]
}
') | sed -zr 's/,([^,]*$)/\1/' > ./configuration/topology-node-$(( node + 1 )).json
done
