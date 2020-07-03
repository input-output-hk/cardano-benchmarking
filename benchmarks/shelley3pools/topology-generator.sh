#!/usr/bin/env bash

if [ -z "${WORKINGDIRECTORY}" ]; then
  echo "missing \$WORKINGDIRECTORY."
  exit 1
fi

. ./configuration/parameters

if [ -z "$NNODES" ]; then
  echo "missing \$NNODES in configuration/parameters"; exit 1
fi
if [ -z "$PORTBASE" ]; then
  echo "missing \$PORTBASE in configuration/parameters"; exit 1
fi

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
') | sed -zr 's/,([^,]*$)/\1/' > ${WORKINGDIRECTORY}/topology-node-$(( node + 1 )).json
done
