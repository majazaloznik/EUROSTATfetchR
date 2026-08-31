-- ============================================================================
-- Tests get_new_periods
--
-- Two snapshots. At s2:
--   ds_new_q   : gains time position 2026Q2 (already had 2026Q1)   -> "2026Q2"
--   ds_new_m   : gains two months 2026M06, 2026M07                 -> "2026M06, 2026M07"
--   ds_struct  : gains a geo position, NO new time                 -> not returned
--   ds_rm_time : a time position is REMOVED (expired), none added  -> not returned
-- ============================================================================
BEGIN;
DO $$
DECLARE
    s1 BIGINT;
    s2 BIGINT;
    v_periods TEXT;
    v_n INT;
BEGIN
    RAISE NOTICE 'Starting get_new_periods tests...';

    INSERT INTO eurostat.snapshot (file_sha256, n_rows)
    VALUES ('test_np_s1', 0) RETURNING snapshot_id INTO s1;
    INSERT INTO eurostat.snapshot (file_sha256, n_rows)
    VALUES ('test_np_s2', 0) RETURNING snapshot_id INTO s2;

    -- ds_new_q: had 2026Q1 (s1), gains 2026Q2 (s2)
    INSERT INTO eurostat.metabase (dataset, dim, pos, ord, valid_from, valid_to,
                                   from_snapshot_id, to_snapshot_id) VALUES
        ('ds_new_q', 'time', '2026Q1', 1, now(), NULL, s1, NULL),
        ('ds_new_q', 'time', '2026Q2', 2, now(), NULL, s2, NULL);

    -- ds_new_m: gains two months at s2
    INSERT INTO eurostat.metabase VALUES
        ('ds_new_m', 'time', '2026M06', 1, now(), NULL, s2, NULL),
        ('ds_new_m', 'time', '2026M07', 2, now(), NULL, s2, NULL);

    -- ds_struct: gains a geo position at s2, no new time
    INSERT INTO eurostat.metabase VALUES
        ('ds_struct', 'geo', 'ES', 1, now(), NULL, s2, NULL);

    -- ds_rm_time: a time position is removed at s2, none added
    INSERT INTO eurostat.metabase VALUES
        ('ds_rm_time', 'time', '2019Q1', 1, now(), now(), s1, s2);

    -- Test 1: ds_new_q returns "2026Q2" only (not the pre-existing 2026Q1)
    SELECT new_periods INTO v_periods
    FROM eurostat.get_new_periods(s2) WHERE dataset = 'ds_new_q';
    ASSERT v_periods = '2026Q2',
        format('ds_new_q new_periods should be 2026Q2, got %s', v_periods);

    -- Test 2: ds_new_m aggregates both months
    SELECT new_periods INTO v_periods
    FROM eurostat.get_new_periods(s2) WHERE dataset = 'ds_new_m';
    ASSERT v_periods = '2026M06, 2026M07',
        format('ds_new_m new_periods should be "2026M06, 2026M07", got %s', v_periods);

    -- Test 3: ds_struct (structural change, no new time) not returned
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.get_new_periods(s2) WHERE dataset = 'ds_struct'),
        'ds_struct has no new time period and must not be returned';

    -- Test 4: ds_rm_time (time removed, none added) not returned
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.get_new_periods(s2) WHERE dataset = 'ds_rm_time'),
        'ds_rm_time only removed a period and must not be returned';

    -- Test 5: exactly two datasets returned
    SELECT count(*) INTO v_n FROM eurostat.get_new_periods(s2);
    ASSERT v_n = 2,
        format('expected 2 datasets with new periods, got %s', v_n);

    RAISE NOTICE 'All get_new_periods tests passed successfully';
EXCEPTION WHEN OTHERS THEN
    RAISE NOTICE 'Test failed: %', SQLERRM;
    RAISE;
END $$;
ROLLBACK;
