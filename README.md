# sqiff

sqiff compares PostgreSQL schemas and writes the difference as sqitch changes.

## Commands

- `sqiff table FILE [--db CONNINFO] [--write]` prints the migration for an edited table deploy file. It builds the table from the file at `--base` (master) and from your edit inside one rolled-back transaction. With `--write` it adds the migration with `sqitch add` and reworks the table's change.
- `sqiff table FILE [--db CONNINFO] --rewrite-file [--live-from CONNINFO] [--write]` rewrites a drifting table file from the live table.
- `sqiff describe`, `diff`, `plan` and `sqitch` read whole databases. `--from master` (or `rev:REV`) builds a scratch database from that revision's sqitch plan with Docker Compose.

Run it from the root of the sqitch project.

## Settings

**Databases.** `--db` and `--to` take a libpq connection string. Left out, libpq's environment names the database: `PGHOST`, `PGPORT`, `PGUSER`, `PGDATABASE`, or `PGSERVICE` with a service file.

**sqitch layout.** The top directory and plan file come from `sqitch config` (`core.top_dir`, `core.plan_file`); `--top-dir` and `--plan-file` override them.

**`sqiff.yaml`**, next to `sqitch.conf`, holds the rest. Every key is optional; an unknown key is an error.

```yaml
exclude-schemas: [hdb_catalog]       # left out of every comparison
rename-hint: "..."                   # added to the warning on a column rename
templates: _dev/sqitch/templates     # create-migration's templates: add_column, add_index, validate_constraint, migration

names:                               # change names; the defaults are shown
  table: "{schema}/table/{name}"
  function: "{schema}/function/{name}"
  view: "{schema}/view/{name}"
  trigger: "{schema}/trigger/{table}/{name}"
  index: "{schema}/table/{table}/index/{name}"
  type: "{schema}/type/{name}"
  migration: "migrations/{stamp}-{slug}"
  migration-stamp: "%Y/%m/%Y-%m-%d-%H%M%S"

baseline:                            # the scratch database --from master builds
  compose-file: docker-compose.yaml  # read from the revision's checkout
  service: postgres
  port-variable: PG_PORT             # the compose file's variable for the published port
  port: 0                            # 0 lets Docker pick a free port
  container-port: 5432
  environment: {}                    # more variables for docker compose
  user: postgres
  database: postgres
  sqitch: host                       # host, or service to run sqitch inside the service
  deploy:                            # sqitch calls that deploy the revision
    - sqitch: [deploy]
      if-exists: sqitch.plan         # skip the call when this path is missing
```

Without `templates`, `--write` uses sqitch's own templates and writes the scripts itself. Each repository gets its own baseline, shared by its worktrees and jj workspaces, under `~/.cache/sqiff`.

## Build

```bash
nix build .#sqiff
```

Inside kronor's dev shell, `cabal build` works too: the shell's GHC already has every dependency at the versions in `cabal.project.freeze`.

## Needs at run time

`git` and `sqitch`, and `docker` for `--from`. The database needs plpgsql_check for the PL/pgSQL body checks; without it they are skipped with a note.
