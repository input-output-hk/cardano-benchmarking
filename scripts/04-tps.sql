-- calculate TPS
SELECT resq.*, (resq."total_trxs" / extract(epoch from resq."delta_t")) AS TPS
FROM
  (SELECT MAX(subq.min) AS t0, MAX(subq.max) AS t1, MAX("delta_t") AS "delta_t", MAX("total_tx") AS "total_trxs"
   FROM
     (SELECT MIN("time"), MAX("time"), (MAX("time") - MIN("time")) AS "delta_t", NULL AS "total_tx"
     FROM results
     WHERE size > 0

     UNION ALL

     SELECT NULL, NULL, NULL, SUM(innerq.txcount) AS "total_tx"
     FROM
       (SELECT txcount
        FROM results
        WHERE size > 0
        ORDER BY "time"
        OFFSET 1) AS innerq  -- do not count tx in first block
       ) AS subq
  ) AS resq;
