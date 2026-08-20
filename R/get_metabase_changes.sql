-- ============================================================================
-- Get metabase structural changes for a snapshot
--
-- Returns one row per affected dataset for the given snapshot, classifying
-- each as a whole-dataset event (dataset_added / dataset_removed) or a
-- within-dataset change (changed), with dimension changes and level (position)
-- counts. The `time` dimension is excluded throughout: every dataset gains a
-- time position every period, so it would swamp the output.
--
-- Detection keys off from_snapshot_id / to_snapshot_id (integers), never off
-- the valid_from / valid_to timestamps, to avoid microsecond-precision
-- mismatches.
--
-- new_ds   : datasets whose first-ever rows appeared in this snapshot
-- gone_ds  : datasets that had live rows before and none surviving after
-- excluded : union of the two, kept out of the per-dimension/level breakdown
--            so a whole-dataset event does not also emit per-dimension noise
-- ============================================================================
CREATE OR REPLACE FUNCTION eurostat.get_metabase_changes(p_snapshot_id BIGINT)
RETURNS TABLE (
    dataset       TEXT,
    event         TEXT,
    dim_changes   TEXT,
    level_added   BIGINT,
    level_removed BIGINT
)
AS $$
    WITH new_ds AS (
        SELECT DISTINCT m.dataset
        FROM eurostat.metabase m
        WHERE m.from_snapshot_id = p_snapshot_id
          AND NOT EXISTS (
              SELECT 1 FROM eurostat.metabase o
              WHERE o.dataset = m.dataset
                AND o.from_snapshot_id < p_snapshot_id)
    ),
    gone_ds AS (
        SELECT DISTINCT m.dataset
        FROM eurostat.metabase m
        WHERE m.to_snapshot_id = p_snapshot_id
          AND NOT EXISTS (
              SELECT 1 FROM eurostat.metabase o
              WHERE o.dataset = m.dataset
                AND o.to_snapshot_id IS NULL)
    ),
    excluded AS (
        SELECT dataset FROM new_ds
        UNION
        SELECT dataset FROM gone_ds
    ),
    dim_ev AS (
        SELECT m.dataset, m.dim, 'dim_added' AS change_type
        FROM eurostat.metabase m
        WHERE m.from_snapshot_id = p_snapshot_id
          AND m.dim <> 'time'
          AND m.dataset NOT IN (SELECT dataset FROM excluded)
          AND NOT EXISTS (
              SELECT 1 FROM eurostat.metabase o
              WHERE o.dataset = m.dataset AND o.dim = m.dim
                AND o.from_snapshot_id < m.from_snapshot_id
                AND (o.to_snapshot_id IS NULL OR o.to_snapshot_id >= m.from_snapshot_id))
        UNION ALL
        SELECT m.dataset, m.dim, 'dim_removed'
        FROM eurostat.metabase m
        WHERE m.to_snapshot_id = p_snapshot_id
          AND m.dim <> 'time'
          AND m.dataset NOT IN (SELECT dataset FROM excluded)
          AND NOT EXISTS (
              SELECT 1 FROM eurostat.metabase o
              WHERE o.dataset = m.dataset AND o.dim = m.dim
                AND o.to_snapshot_id IS NULL)
    ),
    pos_ev AS (
        SELECT x.dataset,
               count(*) FILTER (WHERE x.kind = 'added')   AS level_added,
               count(*) FILTER (WHERE x.kind = 'removed') AS level_removed
        FROM (
            SELECT m.dataset, 'added' AS kind
            FROM eurostat.metabase m
            WHERE m.from_snapshot_id = p_snapshot_id
              AND m.dim <> 'time'
              AND m.dataset NOT IN (SELECT dataset FROM excluded)
            UNION ALL
            SELECT m.dataset, 'removed'
            FROM eurostat.metabase m
            WHERE m.to_snapshot_id = p_snapshot_id
              AND m.dim <> 'time'
              AND m.dataset NOT IN (SELECT dataset FROM excluded)
        ) x
        GROUP BY x.dataset
    )
    SELECT coalesce(d.dataset, p.dataset)                     AS dataset,
           'changed'                                          AS event,
           string_agg(DISTINCT d.dim || ':' || d.change_type, ', ') AS dim_changes,
           max(p.level_added)                                 AS level_added,
           max(p.level_removed)                               AS level_removed
    FROM dim_ev d
    FULL JOIN pos_ev p ON p.dataset = d.dataset
    GROUP BY coalesce(d.dataset, p.dataset)

    UNION ALL
    SELECT nd.dataset, 'dataset_added', NULL, NULL, NULL FROM new_ds nd

    UNION ALL
    SELECT gd.dataset, 'dataset_removed', NULL, NULL, NULL FROM gone_ds gd

    ORDER BY event, dataset;
$$ LANGUAGE sql STABLE;
