-- ============================================================================
-- Eurostat TOC (navigation tree) schema
--
-- Parallel to the metabase monitor: a content-hashed snapshot table plus two
-- SCD2 tables holding the navigation tree — nodes (folders / datasets /
-- tables, by code) and edges (parent -> child relationships, by code).
--
-- Kept separate from the metabase because the TOC is an independent Eurostat
-- file with its own publication cadence; the two are diffed and alerted
-- independently. Unlike the metabase file, the TOC carries a real creation
-- timestamp (the <tree creationDate> attribute), so a monotonicity guard is
-- possible here.
--
-- Node identity is `code`; `title` is tracked content (a title change expires
-- the old row and inserts a new one — which is what makes dataset/folder
-- rename detection possible later, since the TOC has titles the metabase
-- lacks). Edge identity is (parent_code, child_code); a dataset moved between
-- folders is an edge change, independent of any node change, and a code with
-- multiple parents is simply multiple live edges.
-- ============================================================================

CREATE TABLE eurostat.toc_snapshot (
  snapshot_id   bigserial PRIMARY KEY,
  file_sha256   text NOT NULL,               -- content identity (dedup key)
  creation_date timestamptz,                  -- <tree creationDate>, provenance
  observed_at   timestamptz NOT NULL DEFAULT now(),
  n_nodes       bigint NOT NULL
);
CREATE INDEX ix_toc_snap_sha ON eurostat.toc_snapshot (file_sha256);

CREATE TABLE eurostat.toc_node (
  code  text NOT NULL,
  type  text NOT NULL,                        -- 'folder' | 'dataset' | 'table' (informational)
  title text,
  valid_from timestamptz NOT NULL,
  valid_to   timestamptz,                     -- NULL = live
  from_snapshot_id bigint NOT NULL REFERENCES eurostat.toc_snapshot(snapshot_id),
  to_snapshot_id   bigint          REFERENCES eurostat.toc_snapshot(snapshot_id)
);
-- one live row per code; a title change expires the old and inserts a new row
CREATE UNIQUE INDEX ux_toc_node_live ON eurostat.toc_node (code) WHERE valid_to IS NULL;
CREATE INDEX ix_toc_node_from ON eurostat.toc_node (from_snapshot_id);
CREATE INDEX ix_toc_node_to   ON eurostat.toc_node (to_snapshot_id);

CREATE TABLE eurostat.toc_edge (
  parent_code text NOT NULL,
  child_code  text NOT NULL,
  valid_from timestamptz NOT NULL,
  valid_to   timestamptz,                     -- NULL = live
  from_snapshot_id bigint NOT NULL REFERENCES eurostat.toc_snapshot(snapshot_id),
  to_snapshot_id   bigint          REFERENCES eurostat.toc_snapshot(snapshot_id)
);
-- one live row per parent->child pair; multiple parents = multiple live edges
CREATE UNIQUE INDEX ux_toc_edge_live ON eurostat.toc_edge (parent_code, child_code) WHERE valid_to IS NULL;
CREATE INDEX ix_toc_edge_from   ON eurostat.toc_edge (from_snapshot_id);
CREATE INDEX ix_toc_edge_to     ON eurostat.toc_edge (to_snapshot_id);
CREATE INDEX ix_toc_edge_parent ON eurostat.toc_edge (parent_code) WHERE valid_to IS NULL;
CREATE INDEX ix_toc_edge_child  ON eurostat.toc_edge (child_code)  WHERE valid_to IS NULL;
