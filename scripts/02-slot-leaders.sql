-- output column counts
SELECT COUNT(iq.slot_leader) AS "count_slot_leaders", NULL AS "block_count"
FROM (SELECT DISTINCT slot_leader FROM results WHERE size > 0) AS iq
UNION ALL
SELECT NULL, COUNT(iq.block_no)
FROM (SELECT block_no FROM block WHERE tx_count > 0) AS iq;
