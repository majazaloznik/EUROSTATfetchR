-- ============================================================================
-- Tests resolve_folder_datasets
--
-- Builds a small live tree:
--   root (folder)
--     +-- sub (folder)
--     |     +-- ds_1 (dataset)
--     |     +-- tbl_1 (table)
--     +-- ds_2 (dataset)
--     +-- empty_sub (folder, no leaves)
--   other (folder)
--     +-- ds_shared (dataset)   also a child of sub  -> multi-parent
--
-- Confirms: transitive descent, dataset+table both returned, folders excluded,
-- multi-parent dedup, empty/nonexistent folder returns nothing.
-- ============================================================================
BEGIN;
DO $$
DECLARE
    s1 BIGINT;
    v_n INT;
    v_codes TEXT;
BEGIN
    RAISE NOTICE 'Starting resolve_folder_datasets tests...';

    INSERT INTO eurostat.toc_snapshot (file_sha256, n_nodes)
    VALUES ('test_rfd', 0) RETURNING snapshot_id INTO s1;

    -- nodes
    INSERT INTO eurostat.toc_node (code, type, title, valid_from, valid_to,
                                   from_snapshot_id, to_snapshot_id) VALUES
        ('root',      'folder',  'Root',       now(), NULL, s1, NULL),
        ('sub',       'folder',  'Sub',        now(), NULL, s1, NULL),
        ('empty_sub', 'folder',  'Empty',      now(), NULL, s1, NULL),
        ('other',     'folder',  'Other',      now(), NULL, s1, NULL),
        ('ds_1',      'dataset', 'Dataset 1',  now(), NULL, s1, NULL),
        ('ds_2',      'dataset', 'Dataset 2',  now(), NULL, s1, NULL),
        ('tbl_1',     'table',   'Table 1',    now(), NULL, s1, NULL),
        ('ds_shared', 'dataset', 'Shared',     now(), NULL, s1, NULL);

    -- edges
    INSERT INTO eurostat.toc_edge (parent_code, child_code, valid_from, valid_to,
                                   from_snapshot_id, to_snapshot_id) VALUES
        ('root', 'sub',        now(), NULL, s1, NULL),
        ('root', 'ds_2',       now(), NULL, s1, NULL),
        ('root', 'empty_sub',  now(), NULL, s1, NULL),
        ('sub',  'ds_1',       now(), NULL, s1, NULL),
        ('sub',  'tbl_1',      now(), NULL, s1, NULL),
        ('sub',  'ds_shared',  now(), NULL, s1, NULL),
        ('other','ds_shared',  now(), NULL, s1, NULL);   -- multi-parent

    -- =======================================================================
    -- Assertions
    -- =======================================================================

    -- Test 1: resolving 'root' returns all leaves beneath it, transitively:
    --         ds_1, ds_2, ds_shared, tbl_1 (4). Folders excluded.
    SELECT count(*), string_agg(code, ',' ORDER BY code)
      INTO v_n, v_codes
    FROM eurostat.resolve_folder_datasets('root');
    ASSERT v_n = 4,
        format('root should resolve to 4 leaves, got %s', v_n);
    ASSERT v_codes = 'ds_1,ds_2,ds_shared,tbl_1',
        format('root leaves wrong: %s', v_codes);

    -- Test 2: no folder codes appear in the result
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.resolve_folder_datasets('root')
        WHERE type = 'folder'),
        'folders must not be returned';

    -- Test 3: tables are included alongside datasets
    ASSERT EXISTS (
        SELECT 1 FROM eurostat.resolve_folder_datasets('root')
        WHERE code = 'tbl_1' AND type = 'table'),
        'tables should be returned alongside datasets';

    -- Test 4: resolving 'sub' returns only its subtree (ds_1, ds_shared, tbl_1)
    SELECT count(*) INTO v_n FROM eurostat.resolve_folder_datasets('sub');
    ASSERT v_n = 3,
        format('sub should resolve to 3 leaves, got %s', v_n);

    -- Test 5: multi-parent leaf returned once (ds_shared under sub AND other)
    SELECT count(*) INTO v_n
    FROM eurostat.resolve_folder_datasets('root')
    WHERE code = 'ds_shared';
    ASSERT v_n = 1,
        format('ds_shared should appear once, got %s', v_n);

    -- Test 6: empty folder returns nothing
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.resolve_folder_datasets('empty_sub')),
        'empty folder should resolve to no leaves';

    -- Test 7: nonexistent folder returns nothing
    ASSERT NOT EXISTS (
        SELECT 1 FROM eurostat.resolve_folder_datasets('does_not_exist')),
        'nonexistent folder should resolve to no leaves';

    RAISE NOTICE 'All resolve_folder_datasets tests passed successfully';
EXCEPTION WHEN OTHERS THEN
    RAISE NOTICE 'Test failed: %', SQLERRM;
    RAISE;
END $$;
ROLLBACK;
