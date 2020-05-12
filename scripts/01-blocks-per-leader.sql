-- output number of slots per node
SELECT slot_leader, COUNT(*) AS "block_count"
FROM results
WHERE size > 0
GROUP BY slot_leader;
