-- ============================================================================
  -- Tests get_metabase_changes
--
  -- Sets up two snapshots (s1 = prior state, s2 = the snapshot under test) and
-- metabase rows arranged to exercise every event type the function emits:
  --
  --   ds_changed_lvl : a level added and a level removed (changed)
--   ds_changed_dim : a dimension added and a dimension removed (changed)
--   ds_new         : first appears at s2                       (dataset_added)
--   ds_gone        : all rows expired at s2, none survive      (dataset_removed)
--   ds_time_only   : only its time dimension changes           (no output)
--   ds_stable      : unchanged across s2                       (no output)
--
  -- Rows are keyed on from_snapshot_id / to_snapshot_id. valid_from / valid_to
-- are set too (they are NOT NULL / meaningful) but the function ignores them.
-- ============================================================================
  BEGIN;
DO $$
  DECLARE
s1 BIGINT;
s2 BIGINT;
v_dim_changes TEXT;
v_level_added BIGINT;
v_level_removed BIGINT;
v_event TEXT;
v_n INT;
BEGIN
RAISE NOTICE 'Starting get_metabase_changes tests...';

-- ---- snapshots ---------------------------------------------------------
  INSERT INTO eurostat.snapshot (file_sha256, n_rows)
VALUES ('test_sha_s1', 0) RETURNING snapshot_id INTO s1;
INSERT INTO eurostat.snapshot (file_sha256, n_rows)
VALUES ('test_sha_s2', 0) RETURNING snapshot_id INTO s2;

-- ---- ds_changed_lvl : geo gains ES, loses FR (level add + remove) -------
  -- surviving level (present before, still live)
INSERT INTO eurostat.metabase (dataset, dim, pos, ord, valid_from, valid_to,
                               from_snapshot_id, to_snapshot_id)
VALUES ('ds_changed_lvl', 'geo', 'DE', 1, now(), NULL, s1, NULL);
-- level removed at s2
INSERT INTO eurostat.metabase VALUES
('ds_changed_lvl', 'geo', 'FR', 2, now(), now(), s1, s2);
-- level added at s2
INSERT INTO eurostat.metabase VALUES
('ds_changed_lvl', 'geo', 'ES', 3, now(), NULL, s2, NULL);
-- a time position also changes; must be ignored
INSERT INTO eurostat.metabase VALUES
('ds_changed_lvl', 'time', '2026', 1, now(), NULL, s2, NULL);

-- ---- ds_changed_dim : dim 'unit' added, dim 'freq' removed -------------
  -- pre-existing dimension that persists (so the dataset is not "new")
INSERT INTO eurostat.metabase VALUES
('ds_changed_dim', 'geo', 'DE', 1, now(), NULL, s1, NULL);
-- whole dimension 'freq' removed at s2 (its only level expires, none live)
INSERT INTO eurostat.metabase VALUES
('ds_changed_dim', 'freq', 'A', 1, now(), now(), s1, s2);
-- whole dimension 'unit' added at s2
INSERT INTO eurostat.metabase VALUES
('ds_changed_dim', 'unit', 'PC', 1, now(), NULL, s2, NULL);

-- ---- ds_new : first-ever rows appear at s2 -----------------------------
  INSERT INTO eurostat.metabase VALUES
('ds_new', 'geo', 'DE', 1, now(), NULL, s2, NULL),
('ds_new', 'geo', 'FR', 2, now(), NULL, s2, NULL),
('ds_new', 'time', '2026', 1, now(), NULL, s2, NULL);

-- ---- ds_gone : had rows before, all expire at s2, none survive ---------
  INSERT INTO eurostat.metabase VALUES
('ds_gone', 'geo', 'DE', 1, now(), now(), s1, s2),
('ds_gone', 'geo', 'FR', 2, now(), now(), s1, s2),
('ds_gone', 'time', '2025', 1, now(), now(), s1, s2);

-- ---- ds_time_only : only a time position changes -----------------------
  INSERT INTO eurostat.metabase VALUES
('ds_time_only', 'geo', 'DE', 1, now(), NULL, s1, NULL);   -- stable
INSERT INTO eurostat.metabase VALUES
('ds_time_only', 'time', '2026', 1, now(), NULL, s2, NULL);-- new time only

-- ---- ds_stable : nothing changes at s2 ---------------------------------
  INSERT INTO eurostat.metabase VALUES
('ds_stable', 'geo', 'DE', 1, now(), NULL, s1, NULL);

-- =======================================================================
  -- Assertions
-- =======================================================================

  -- Test 1: ds_changed_lvl reported as 'changed', 1 level added, 1 removed,
--         no dim change (dim_changes NULL). time excluded from counts.
SELECT event, dim_changes, level_added, level_removed
INTO v_event, v_dim_changes, v_level_added, v_level_removed
FROM eurostat.get_metabase_changes(s2)
WHERE dataset = 'ds_changed_lvl';

ASSERT v_event = 'changed',
format('ds_changed_lvl event should be changed, got %s', v_event);
ASSERT v_dim_changes IS NULL,
format('ds_changed_lvl dim_changes should be NULL, got %s', v_dim_changes);
ASSERT v_level_added = 1,
format('ds_changed_lvl level_added should be 1, got %s', v_level_added);
ASSERT v_level_removed = 1,
format('ds_changed_lvl level_removed should be 1, got %s', v_level_removed);

-- Test 2: ds_changed_dim reports both a dim_added and a dim_removed
SELECT dim_changes INTO v_dim_changes
FROM eurostat.get_metabase_changes(s2)
WHERE dataset = 'ds_changed_dim';

ASSERT v_dim_changes LIKE '%unit:dim_added%',
format('ds_changed_dim should show unit:dim_added, got %s', v_dim_changes);
ASSERT v_dim_changes LIKE '%freq:dim_removed%',
format('ds_changed_dim should show freq:dim_removed, got %s', v_dim_changes);

-- Test 3: ds_new reported as dataset_added, no per-dim breakdown
SELECT event, dim_changes INTO v_event, v_dim_changes
FROM eurostat.get_metabase_changes(s2)
WHERE dataset = 'ds_new';

ASSERT v_event = 'dataset_added',
format('ds_new event should be dataset_added, got %s', v_event);
ASSERT v_dim_changes IS NULL,
'ds_new should have no dim_changes (whole-dataset event)';

-- Test 4: ds_gone reported as dataset_removed
SELECT event INTO v_event
FROM eurostat.get_metabase_changes(s2)
WHERE dataset = 'ds_gone';

ASSERT v_event = 'dataset_removed',
format('ds_gone event should be dataset_removed, got %s', v_event);

-- Test 5: ds_time_only produces NO row (time-only change is invisible)
ASSERT NOT EXISTS (
  SELECT 1 FROM eurostat.get_metabase_changes(s2)
  WHERE dataset = 'ds_time_only'),
'ds_time_only should produce no output (time changes are excluded)';

-- Test 6: ds_stable produces NO row
ASSERT NOT EXISTS (
  SELECT 1 FROM eurostat.get_metabase_changes(s2)
  WHERE dataset = 'ds_stable'),
'ds_stable should produce no output (nothing changed)';

-- Test 7: exactly the four expected datasets appear, no more
SELECT count(*) INTO v_n FROM eurostat.get_metabase_changes(s2);
ASSERT v_n = 4,
format('expected 4 changed datasets, got %s', v_n);

RAISE NOTICE 'All get_metabase_changes tests passed successfully';
EXCEPTION WHEN OTHERS THEN
RAISE NOTICE 'Test failed: %', SQLERRM;
RAISE;
END $$;
ROLLBACK;
