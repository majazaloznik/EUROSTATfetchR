-- ============================================================================
-- Tests get_removed_levels
--
-- Sets up two snapshots and metabase rows where, at s2:
--   ds_a / geo    : FR and IT removed, DE survives   -> returns "FR, IT"
--   ds_a / nace   : X removed                         -> returns "X"
--   ds_b / geo    : ES removed                         -> returns "ES"
--   ds_a / time   : a time position removed            -> excluded, no row
--   ds_c / geo    : GR removed, but ds_c NOT requested -> excluded, no row
--
-- Confirms: correct codes per (dataset, dim), time exclusion, dataset filter
-- via the text[] parameter, alphabetical aggregation of codes.
-- ============================================================================
BEGIN;
DO $$
DECLARE
    s1 BIGINT;
    s2 BIGINT;
    v_removed TEXT;
    v_n INT;
BEGIN
    RAISE NOTICE 'Starting get_removed_levels tests...';

    INSERT INTO eurostat.snapshot (file_sha256, n_rows)
    VALUES ('test_rl_s1', 0) RETURNING snapshot_id INTO s1;
    INSERT INTO eurostat.snapshot (file_sha256, n_rows)
    VALUES ('test_rl_s2', 0) RETURNING snapshot_id INTO s2;

    -- ds_a / geo : DE survives, FR and IT removed at s2
    INSERT INTO eurostat.metabase (dataset, dim, pos, ord, valid_from, valid_to,
                                   from_snapshot_id, to_snapshot_id) VALUES
        ('ds_a', 'geo', 'DE', 1, now(), NULL,  s1, NULL),
        ('ds_a', 'geo', 'FR', 2, now(), now(), s1, s2),
        ('ds_a', 'geo', 'IT', 3, now(), now(), s1, s2);

    -- ds_a / nace : X removed at s2
    INSERT INTO eurostat.metabase VALUES
        ('ds_a', 'nace', 'X', 1, now(), now(), s1, s2);

    -- ds_a / time : a time position removed -> must be excluded
    INSERT INTO eurostat.metabase VALUES
        ('ds_a', 'time', '2019', 1, now(), now(), s1, s2);

    -- ds_b / geo : ES removed at s2
    INSERT INTO eurostat.metabase VALUES
        ('ds_b', 'geo', 'ES', 1, now(), now(), s1, s2);

    -- ds_c / geo : GR removed, but ds_c is not in the requested set
    INSERT INTO eurostat.metabase VALUES
        ('ds_c', 'geo', 'GR', 1, now(), now(), s1, s2);

    -- =======================================================================
    -- Assertions (requesting ds_a and ds_b only)
    -- =======================================================================

    -- Test 1: ds_a / geo returns "FR, IT" (alphabetical, DE excluded as it lives)
    SELECT removed INTO v_removed
    FROM eurostat.get_removed_levels(s2, ARRAY['ds_a','ds_b'])
    WHERE dataset = 'ds_a' AND dim = 'geo';
    ASSERT v_removed = 'FR, IT',
        format('ds_a/geo removed should be "FR, IT", got "%s"', v_removed);

    -- Test 2: ds_a / nace returns "X"
    SELECT removed INTO v_removed
    FROM eurostat.get_removed_levels(s2, ARRAY['ds_a','ds_b'])
    WHERE dataset = 'ds_a' AND dim = 'nace';
    ASSERT v_removed = 'X',
        format('ds_a/nace removed should be "X", got "%s"', v_removed);

    -- Test 3: ds_b / geo returns "ES"
    SELECT removed INTO v_removed
    FROM eurostat.get_removed_levels(s2, ARRAY['ds_a','ds_b'])
    WHERE dataset = 'ds_b' AND dim = 'geo';
    ASSERT v_removed = 'ES',
        format('ds_b/geo removed should be "ES", got "%s"', v_removed);

    -- Test 4: time dimension excluded (ds_a has no time row in the result)
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.get_removed_levels(s2, ARRAY['ds_a','ds_b'])
        WHERE dim = 'time'),
        'time dimension should be excluded from removed levels';

    -- Test 5: dataset filter works — ds_c not requested, so absent
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.get_removed_levels(s2, ARRAY['ds_a','ds_b'])
        WHERE dataset = 'ds_c'),
        'ds_c was not requested and must not appear';

    -- Test 6: exactly three rows (ds_a/geo, ds_a/nace, ds_b/geo)
    SELECT count(*) INTO v_n
    FROM eurostat.get_removed_levels(s2, ARRAY['ds_a','ds_b']);
    ASSERT v_n = 3,
        format('expected 3 removed-level rows, got %s', v_n);

    -- Test 7: empty dataset array returns nothing
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.get_removed_levels(s2, ARRAY[]::text[])),
        'empty dataset array should return no rows';

    RAISE NOTICE 'All get_removed_levels tests passed successfully';
EXCEPTION WHEN OTHERS THEN
    RAISE NOTICE 'Test failed: %', SQLERRM;
    RAISE;
END $$;
ROLLBACK;
