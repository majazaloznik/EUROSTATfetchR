-- ============================================================================
-- Resolve a TOC folder code to its descendant dataset/table codes
--
-- Walks the live TOC edge tree from the given folder code, transitively, and
-- returns every descendant leaf (type 'dataset' or 'table') reachable beneath
-- it. Folders themselves are not returned -- only the resolvable data codes a
-- subscription should match against.
--
-- Operates on live edges/nodes only (valid_to IS NULL): it resolves against
-- the current tree at query time. A folder that does not exist, or exists but
-- contains no leaves, returns zero rows.
--
-- Multi-parenting is handled naturally (a code reachable by several paths is
-- returned once, via DISTINCT). Cycles are not expected in a navigation tree,
-- but UNION (not UNION ALL) in the recursive term prevents infinite recursion
-- were one ever to appear.
-- ============================================================================
CREATE OR REPLACE FUNCTION eurostat.resolve_folder_datasets(p_folder_code TEXT)
RETURNS TABLE (
    code TEXT,
    type TEXT
)
AS $$
    WITH RECURSIVE descendants AS (
        -- base: direct children of the requested folder
        SELECT e.child_code
        FROM eurostat.toc_edge e
        WHERE e.parent_code = p_folder_code
          AND e.valid_to IS NULL
        UNION
        -- step: children of anything already collected
        SELECT e.child_code
        FROM eurostat.toc_edge e
        JOIN descendants d ON e.parent_code = d.child_code
        WHERE e.valid_to IS NULL
    )
    SELECT DISTINCT n.code, n.type
    FROM descendants d
    JOIN eurostat.toc_node n
      ON n.code = d.child_code
     AND n.valid_to IS NULL
    WHERE n.type IN ('dataset', 'table')
    ORDER BY n.code;
$$ LANGUAGE sql STABLE;
