-- ============================================================================
-- Get the specific levels removed for a set of datasets in a snapshot
--
-- Returns the actual position codes removed (not just counts) for the given
-- datasets at the given snapshot, so alert emails can list removed levels --
-- the actionable case, since a removed level breaks queries filtering on it.
-- The `time` dimension is excluded.
--
-- Detection keys off to_snapshot_id (integer), never the valid_to timestamp,
-- to avoid microsecond-precision mismatches. The dataset list is a native
-- text[] parameter, so no client-side quoting or IN-list building is needed.
-- ============================================================================
CREATE OR REPLACE FUNCTION eurostat.get_removed_levels(p_snapshot_id BIGINT,
                                                       p_datasets TEXT)
RETURNS TABLE (
    dataset TEXT,
    dim     TEXT,
    removed TEXT
)
AS $$
    SELECT m.dataset,
           m.dim,
           string_agg(m.pos, ', ' ORDER BY m.pos) AS removed
    FROM eurostat.metabase m
    WHERE m.to_snapshot_id = p_snapshot_id
      AND m.dim <> 'time'
      AND m.dataset = ANY(string_to_array(p_datasets, ','))
    GROUP BY m.dataset, m.dim
    ORDER BY m.dataset, m.dim;
$$ LANGUAGE sql STABLE;
