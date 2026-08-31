-- ============================================================================
-- Subscription schema change: add the periods axis
--
-- Structural alerts (scope) and new-period alerts (periods) are independent
-- axes. scope gains 'none' so a subscriber can opt OUT of structural alerts
-- entirely while still receiving period pings.
-- ============================================================================
ALTER TABLE eurostat.subscription
  DROP CONSTRAINT subscription_scope_check;              -- name may differ; check \d
ALTER TABLE eurostat.subscription
  ADD CONSTRAINT subscription_scope_check CHECK (scope IN ('none','breaking','all'));
ALTER TABLE eurostat.subscription
  ADD COLUMN periods boolean NOT NULL DEFAULT false;


-- ============================================================================
-- Get new time periods for a snapshot
--
-- Returns each dataset that gained a `time` position in the given snapshot,
-- with the new period code. A new time position appears when Eurostat opens a
-- new period in a table's structure -- which happens ONCE, when the period is
-- created, independent of which countries have yet filled in their data. So
-- this fires once per table per period, at the table's native frequency
-- (monthly M, quarterly Q, weekly W, daily D...), and is immune to the
-- country-by-country data dribble that plagues Eurostat's own "last updated"
-- alerts.
--
-- Additions only (from_snapshot_id): a period being removed is not a "new data"
-- event. Multiple new positions in one snapshot (rare) are aggregated per
-- dataset.
-- ============================================================================
CREATE OR REPLACE FUNCTION eurostat.get_new_periods(p_snapshot_id BIGINT)
RETURNS TABLE (
    dataset     TEXT,
    new_periods TEXT
)
AS $$
    SELECT m.dataset,
           string_agg(m.pos, ', ' ORDER BY m.pos) AS new_periods
    FROM eurostat.metabase m
    WHERE m.dim = 'time'
      AND m.from_snapshot_id = p_snapshot_id
    GROUP BY m.dataset
    ORDER BY m.dataset;
$$ LANGUAGE sql STABLE;
