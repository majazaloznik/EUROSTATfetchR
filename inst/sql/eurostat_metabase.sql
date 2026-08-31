CREATE SCHEMA IF NOT EXISTS eurostat;

CREATE TABLE eurostat.snapshot (
  snapshot_id bigserial PRIMARY KEY,
  file_sha256 text NOT NULL UNIQUE,          -- content identity = dedup key
  observed_at timestamptz NOT NULL DEFAULT now(),
  n_rows      bigint NOT NULL);

CREATE TABLE eurostat.metabase (
  dataset text NOT NULL, dim text NOT NULL, pos text NOT NULL,
  ord int NOT NULL,                          -- protocol order within (dataset, dim)
  valid_from timestamptz NOT NULL,
  valid_to   timestamptz,                    -- NULL = live
  from_snapshot_id bigint NOT NULL REFERENCES eurostat.snapshot(snapshot_id),
  to_snapshot_id   bigint          REFERENCES eurostat.snapshot(snapshot_id));

CREATE UNIQUE INDEX ux_mb_live ON eurostat.metabase (dataset, dim, pos) WHERE valid_to IS NULL;
CREATE INDEX ix_mb_asof ON eurostat.metabase (dataset, dim, valid_from);
CREATE INDEX ix_mb_vt   ON eurostat.metabase (valid_to);
CREATE INDEX ix_mb_vfrom ON eurostat.metabase (valid_from);

CREATE TABLE eurostat.tracked_dataset (
  dataset text PRIMARY KEY, owner text, source text, contact text);


ALTER TABLE eurostat.snapshot DROP CONSTRAINT snapshot_file_sha256_key;
CREATE INDEX ix_snap_sha ON eurostat.snapshot (file_sha256);   -- keep it indexed, not unique

CREATE INDEX ix_mb_from ON eurostat.metabase (from_snapshot_id);
CREATE INDEX ix_mb_to   ON eurostat.metabase (to_snapshot_id);

DROP TABLE IF EXISTS eurostat.subscription;
CREATE TABLE eurostat.subscription (
  email   text NOT NULL,
  dataset text NOT NULL,
  scope   text NOT NULL DEFAULT 'breaking' CHECK (scope IN ('breaking','all')),
  PRIMARY KEY (email, dataset)
);

ALTER TABLE eurostat.subscription
  ADD COLUMN kind text NOT NULL DEFAULT 'dataset' CHECK (kind IN ('dataset','folder'));
