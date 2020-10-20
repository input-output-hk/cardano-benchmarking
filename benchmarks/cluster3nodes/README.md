## run the benchmark

`./start.sh`


## analyse timeline of run
```
LOGPATH=logs
NNODES=3
OUTDIR=timeline-42
mkdir ${OUTDIR}
for N in $(seq 0 $((NNODES - 1))); do
  ../../scripts/nodeisleader.sh ${LOGPATH}/node-${N}-* | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/leader-${N}.csv
  ../../scripts/addedtocurrentchain.sh ${LOGPATH}/node-${N}-* | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/addtochain-${N}.csv
  ../../scripts/adoptedblock.sh ${LOGPATH}/node-${N}-* | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/adopted-${N}.csv
done
```

reconstruct timeline:
```
cabal --enable-nix run reconstruct-timeline -- ${NNODES} ${OUTDIR} | tee -a ${OUTDIR}/timeline.txt
cp timeline.csv ${OUTDIR}/
```

## preparation for the explorer

### database

* access authentication

get the credentials how to connect to a PostgreSQL instance (version 9.x or 10.x).

add them to the file `configuration/pgpass`.

example:
```
localhost:5432:cl3demo:cl3demo:verysecure
```

* database user

the database `cl3demo` will be created as the user `cl3demo`.

make sure this user exists in the database you are connecting to, and it has the right to create new databases.
(created using a `CREATE USER` command)

