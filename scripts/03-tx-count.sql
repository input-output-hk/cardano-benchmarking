SELECT COUNT(iq.tid) AS "tx_count"
FROM (SELECT id AS tid FROM tx WHERE size > 0) AS iq;
