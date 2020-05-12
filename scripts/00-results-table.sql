DROP TABLE IF EXISTS results;

SELECT block.slot_no, block.slot_leader, block.epoch_no,
        block.block_no, txq.txcount, txq.size, block."time",
        "time" - lag("time", 1) OVER (ORDER BY "time") AS delta
INTO results
FROM
  (SELECT tx.block, COUNT(*) as txcount, SUM(tx.size) as size
    FROM tx as tx
    GROUP BY tx.block) as txq
INNER JOIN block ON txq.block = block.id;

-- show result table
SELECT * FROM results;
