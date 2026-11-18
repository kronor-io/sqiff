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
  password-variable: PG_PASSWORD     # or password: ...; left out, trust auth, PGPASSWORD or ~/.pgpass
  sqitch: host                       # host, or service to run sqitch inside the service
  deploy:                            # sqitch calls that deploy the revision
    - sqitch: [deploy]
      if-exists: sqitch.plan         # skip the call when this path is missing
  status: [status]                   # the sqitch call that finds changes the plan no longer has
```

In `deploy` and `status` arguments, `{host}`, `{port}`, `{user}` and `{database}` are where that sqitch reaches the baseline: the port Docker published when sqitch runs on the host, `container-port` when it runs in the service. sqitch reads `PGHOST` and `PGPORT` only for what a target's URI leaves out, so a target with a fixed URI needs one, for example:

```yaml
  deploy:
    - sqitch: [deploy, "db:pg://{user}@{host}:{port}/{database}"]
  status: [status, "db:pg://{user}@{host}:{port}/{database}"]
```

The password reaches sqitch as `PGPASSWORD` and sqiff's own connections in the connection string; it is never put in sqitch's arguments.

Without `templates`, `--write` uses sqitch's own templates and writes the scripts itself. Each repository gets its own baseline, shared by its worktrees and jj workspaces, under `~/.cache/sqiff`.

## Build

```bash
nix build .#sqiff
```

Inside kronor's dev shell, `cabal build` works too: the shell's GHC already has every dependency at the versions in `cabal.project.freeze`.

## Tests

```bash
cabal test
```

The unit tests are pure. The integration tests start a throwaway PostgreSQL cluster (`initdb` from `PATH`, or the directory in `SQIFF_TEST_PG_BIN`), and for each directory under `test/fixtures` they build `from.sql` and `to.sql` and compare `sqiff diff` and `sqiff plan` with the golden files. They then apply the plan to `from` and expect `diff` to find nothing left. A `known-bug` file marks a fixture that fails today and says why. Refresh golden files with `cabal test --test-options=--accept`.

## Needs at run time

`git` and `sqitch`, and `docker` for `--from`. The database needs plpgsql_check for the PL/pgSQL body checks; without it they are skipped with a note.
